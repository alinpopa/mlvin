open Core.Std
open Async.Std

let start_rtm token =
  let open Async.Std.Deferred.Infix in
  let module Client = Cohttp_async.Client in
  let module Body = Cohttp_async.Body in
  let module Json = Yojson.Basic in
  let http_get = Client.get (Uri.of_string ("https://slack.com/api/rtm.start?token=" ^ token)) in
  let string_body = http_get >>= fun (response, body) -> Body.to_string body in
  string_body >>| fun s -> Json.from_string s

let rec read_data r alive_w =
  let open Data in
  Pipe.read r >>= (fun data ->
    match data with
    | `Ok d ->
        printf "Raw data: '%s'\n" d;
        let _ = match Data.of_string d with
        | Some Pong ->
            Pipe.write_without_pushback alive_w "alive"
        | Some Msg (m, u) -> printf "Got msg: %s, from user: %s\n" m u
        | Some Other t -> printf "Other type: %s\n" t
        | None -> printf "Invalid json message: %s\n" d in
        read_data r alive_w
    | `Eof ->
        printf "Reader finished...\n";
        Deferred.unit)

let rec check_alive alive_r feedback_w s =
  Clock.with_timeout (sec 10.0) (Pipe.read alive_r) >>= (fun r ->
    match r with
    | `Result _ ->
        printf "Alive...\n";
        check_alive alive_r feedback_w s
    | `Timeout ->
        let shutdown_sequence = fun () -> Async_extra.Import.Socket.(shutdown s `Both) in
        Pipe.write feedback_w (Data.KillMeNow shutdown_sequence))

let client uri feedback_w =
  let host = Option.value_exn ~message:"no host in uri" Uri.(host uri) in
  let port = Option.value ~default:443 Uri_services.(tcp_port_of_uri uri) in
  let module W = Websocket_async in
  let scheme = Option.value_exn ~message:"no scheme in uri" Uri.(scheme uri) in
  let tcp_fun s r w =
    Async_extra.Import.Socket.(setopt s Opt.nodelay true);
    (if scheme = "https" || scheme = "wss"
    then Conduit_async_ssl.ssl_connect r w
    else Deferred.return (r, w)) >>= fun (r, w) ->
      let module C = Cohttp in
      let extra_headers = C.Header.init () in
      let open Async.Std.Log.Global in
      let r, w = W.client_ez ~extra_headers ~log:Lazy.(force log) ~heartbeat:Time_ns.Span.(of_int_sec 5) uri s r w in
      let _ = Clock.every' ~continue_on_error:false (sec 9.0) (fun () ->
        Pipe.write w (Data.to_json ({id = 100; type' = "ping"; channel = ""; user = ""}))) in
      let (alive_r, alive_w) = Pipe.create () in
      let _ = check_alive alive_r feedback_w s in
      read_data r alive_w
  in
  Tcp.(with_connection (to_host_and_port host port) tcp_fun)
