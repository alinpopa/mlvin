module Option = Mlvin.Option

module SlackHandler = struct
  type t = Lwt_io.input_channel
  let ping_timeout = 20.0
  let ping_freq = 15.0

  let try_with f =
    let open Core in
    let ok_result = fun result -> Lwt.return (Ok result) in
    let fail_result = fun exn' -> Lwt.return (Error exn') in
    Lwt.try_bind f ok_result fail_result

  let start_rtm token =
    let open Lwt.Infix in
    let module Client = Cohttp_lwt_unix.Client in
    let module Body = Cohttp_lwt_body in
    let module Json = Yojson.Basic in
    let http_get = Client.get (Uri.of_string ("https://slack.com/api/rtm.start?token=" ^ token)) in
    let string_body = http_get >>= fun (response, body) -> Body.to_string body in
    string_body >|= fun s -> Json.from_string s

  let get_rtm_url json =
    let exception InvalidAuthToken of string in
    let member = Yojson.Basic.Util.member in
    let json_to_string = Yojson.Basic.Util.to_string in
    match (member "ok" json) with
    | `Bool false ->
        raise (InvalidAuthToken (json_to_string (member "error" json)))
    | _ ->
      json |> member "url" |> json_to_string

  let client uri feedback_out =
    let exception WebsocketExit of string in
    let open Websocket_lwt.Frame in
    let open Websocket_lwt in
    let open Lwt.Infix in
    let module C = Cohttp in
    let resolved_uri = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
    let default_ctx = Conduit_lwt_unix.default_ctx in
    let create_client = resolved_uri >>= (fun endp ->
      Conduit_lwt_unix.endp_to_client ~ctx:default_ctx endp) in
    let connect_client = create_client >>= (fun client ->
      with_connection ~ctx:default_ctx client uri) in
    connect_client >>= (fun (recv, send) ->
      let (alive_in, alive_out) = Lwt_io.pipe () in
      let alive_reader () = Lwt_unix.with_timeout 90.0 (fun () -> Lwt_io.read_line alive_in) in
      let rec check_timeout () =
        try_with alive_reader >>=
          (function
            | Core.Error Lwt_unix.Timeout ->
                Lwt_log.info "Timeout" >>= fun _ ->
                send (Frame.create ~opcode:Opcode.Close ~content:"NEED TO RESTART" ()) >>= fun _ ->
                Lwt_io.write_line feedback_out "restart"
            | Core.Error _ ->
                Lwt_log.error "Other exception"
            | Core.Ok result ->
                Lwt_log.info_f "OK: '%s'; reseting the ping timeout timer." result >>= fun _ -> check_timeout ()) in
      let _ = check_timeout () in
      let react fr =
        match fr.opcode with
        | Opcode.Ping ->
            let _ = Lwt_log.info "Got ping..." in
            let _ = Lwt_io.write_line alive_out "Got ping from server!!!" in
            send @@ Frame.create ~opcode:Opcode.Pong ()
        | Opcode.Pong ->
            Lwt_log.info "Got pong..."
        | Opcode.Text
        | Opcode.Binary ->
            let _ = send @@ Frame.create ~content:"{\"type\": \"ping\", \"id\": 1001}" () in
            Lwt_io.write_line feedback_out fr.content
        | Opcode.Close ->
            Lwt.fail (WebsocketExit fr.content)
        | _ ->
            Lwt.fail (WebsocketExit "Unknown error")
      in
      let rec react_forever () = recv () >>= react >>= react_forever in
      react_forever ())

  let rec feedback_reader feedback_in f =
    let open Lwt.Infix in
    Lwt_io.read_line feedback_in >>= (fun l ->
      match l with
      | "restart" ->
          Lwt.return ()
      | m ->
          (Lwt_log.info_f "LINE: %s" m) >>= (fun _ -> feedback_reader feedback_in f))

  let rec start token =
    let open Lwt.Infix in
    let _ = Lwt_log.info "Restarting ... " >|= (fun a -> a) in
    let (feedback_in, feedback_out) = Lwt_io.pipe () in
    let open Lwt.Infix in
    let rtm = try_with (fun () -> start_rtm token) in
    let rtm_url = rtm >|= (function
      | Core.Ok json -> get_rtm_url json
      | Core.Error exc -> "") in
    let url_with_scheme = rtm_url >|= (fun url ->
      let open Option in
      let open Uri in
      with_scheme (of_string url) (some "https")) in
    let connect_client = url_with_scheme >>= (fun url ->
      try_with (fun () -> client url feedback_out)) in
    let _ = feedback_reader feedback_in (fun () -> start token) in
    connect_client

end

module Runner : (Mlvin.Run.Runner with type t = string) = struct
  type t = string

  let rec loop () =
    let open Lwt.Infix in
    let t1, _ = Lwt.wait () in
    t1 >>= fun _ -> loop ()

  let rec main token =
    let open Lwt.Infix in
    let f = SlackHandler.start token in
    f >>= function
      | Core.Ok _ ->
          Lwt_log.info "OK" >>= (fun _ ->
            Lwt_unix.sleep 10.0 >>= (fun _ -> main token))
      | Core.Error exc ->
          let msg = (Printexc.to_string exc) in
          Lwt_log.error_f "ERROR: %s" msg >>= (fun _ ->
            Lwt_unix.sleep 10.0 >>= (fun _ -> main token))

  let rec run (token : t) =
    let open Lwt.Infix in
    let _ = Lwt_log.default := Lwt_log.channel
      ~template:"$(date).$(milliseconds) [$(level)] $(message)"
      ~close_mode:`Keep
      ~channel:Lwt_io.stdout () in
    let _ = Lwt_log.add_rule "*" Lwt_log.Debug in
    Lwt_main.run (main token)
end
