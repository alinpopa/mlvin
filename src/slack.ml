open Core.Std
open Async.Std

module Logger = Log.Global

module Handler : sig
  type t = Data.Feedback.t Pipe.Reader.t Deferred.t
  val start : string -> t
end = struct
  type t = Data.Feedback.t Pipe.Reader.t Deferred.t
  let ping_timeout = sec 60.0
  let ping_freq = sec 45.0

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
          Logger.info "Raw data: '%s'" d;
          let _ =
            match Event.of_json d with
            | Some Pong _ ->
                Pipe.write_without_pushback alive_w "alive"
            | Some Msg (_, m, u) ->
                Logger.info "Got msg: '%s', from user: '%s'" m u
            | _ ->
                Logger.info "Invalid json message: '%s'" d
          in
          read_data r alive_w
      | `Eof ->
          Logger.info "Reader finished...";
          Deferred.unit)

  let rec check_alive alive_r feedback_w s =
    Clock.with_timeout ping_timeout (Pipe.read alive_r) >>= (fun r ->
      match r with
      | `Result _ ->
          Logger.info "Alive...";
          check_alive alive_r feedback_w s
      | `Timeout ->
          let shutdown_sequence = fun () -> Async_extra.Import.Socket.(shutdown s `Both) in
          Pipe.write feedback_w (Data.Feedback.KillMeNow shutdown_sequence))

  let start_pinger socket_w () =
    Clock.every' ~continue_on_error:false ping_freq (fun () ->
      Pipe.write socket_w Data.Event.(to_json (ping ())))

  let client uri feedback_w =
    let host = Option.value_exn ~message:"no host in uri" Uri.(host uri) in
    let port = Option.value ~default:443 Uri_services.(tcp_port_of_uri uri) in
    let module W = Websocket_async in
    let scheme = Option.value_exn ~message:"no scheme in uri" Uri.(scheme uri) in
    let tcp_fun socket socket_r socket_w =
      Async_extra.Import.Socket.(setopt socket Opt.nodelay true);
      (if scheme = "https" || scheme = "wss"
      then Conduit_async_ssl.ssl_connect socket_r socket_w
      else Deferred.return (socket_r, socket_w)) >>= fun (socket_r, socket_w) ->
        let module C = Cohttp in
        let extra_headers = C.Header.init () in
        let open Async.Std.Log.Global in
        let socket_r, socket_w =
          W.client_ez
          ~extra_headers
          ~log:Lazy.(force log)
          ~heartbeat:Time_ns.Span.(of_int_sec 5)
          uri socket socket_r socket_w in
        let _ = start_pinger socket_w () in
        let (alive_r, alive_w) = Pipe.create () in
        let _ = check_alive alive_r feedback_w socket in
        read_data socket_r alive_w
    in
    Tcp.(with_connection (to_host_and_port host port) tcp_fun)

  let get_rtm_url json =
    let exception InvalidAuthToken of string in
    let member = Yojson.Basic.Util.member in
    let json_to_string = Yojson.Basic.Util.to_string in
    match (member "ok" json) with
    | `Bool false ->
        raise (InvalidAuthToken (json_to_string (member "error" json)))
    | _ ->
      json |> member "url" |> json_to_string

  let start token =
    let (feedback_r, feedback_w) = Pipe.create () in
    let module AsyncHandler = Async.Std.Handler in
    let handler = AsyncHandler.create (fun x -> Logger.info "Url: '%s'" x) in
    let rtm = start_rtm token in
    let url = rtm >>| (fun r -> get_rtm_url r) in
    let _ = AsyncHandler.install handler url in
    url
    >>| (fun url -> client (Uri.of_string url) feedback_w)
    >>| (fun _ -> feedback_r)
end
