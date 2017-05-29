module Option = Mlvin.Option

module SlackHandler = struct
  type t = Lwt_io.input_channel
  let ping_timeout = 20.0
  let ping_freq = 15.0

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
      let react fr =
        match fr.opcode with
        (*| Opcode.Text -> Lwt.fail Exit*)
        | Opcode.Ping -> send @@ Frame.create ~opcode:Opcode.Pong ()
        | Opcode.Pong -> Lwt.return_unit
        | Opcode.Text ->
            Lwt_io.write_line feedback_out fr.content
            (*Lwt_io.printlf "Got some text: %s" fr.content*)
        | _ -> Lwt.fail Exit
      in
      let rec react_forever () = recv () >>= react >>= react_forever in
      react_forever ())

  let try_with f =
    let open Core in
    let ok_result = fun result -> Lwt.return (Ok result) in
    let fail_result = fun exn' -> Lwt.return (Error exn') in
    Lwt.try_bind f ok_result fail_result

  let rec feedback_reader feedback_in =
    let open Lwt.Infix in
    Lwt_io.read_line feedback_in >>= (fun l ->
      Lwt_io.printlf "LINE: %s" l) >>= (fun _ -> feedback_reader feedback_in)

  let start token =
    let (feedback_in, feedback_out) = Lwt_io.pipe () in
    let open Lwt.Infix in
    let rtm = start_rtm token in
    let rtm_url = rtm >|= get_rtm_url in
    let url_with_scheme = rtm_url >|= (fun url ->
      let open Option in
      let open Uri in
      with_scheme (of_string url) (some "https")) in
    let connect_client = url_with_scheme >>= (fun url ->
      try_with (fun () -> client url feedback_out)) in
    let _ = feedback_reader feedback_in in
    connect_client 

end

module Runner : (Mlvin.Run.Runner with type t = string) = struct
  type t = string

  let rec loop () =
    let open Lwt.Infix in
    let t1, _ = Lwt.wait () in
    t1 >>= fun _ -> loop ()

  let run (token : t) =
    let open Lwt.Infix in
    let f = SlackHandler.start token in
    let r = Lwt_main.run f in
    let _ = match r with
    | Core.Ok _ -> Lwt_io.printl "OK"
    | Core.Error _ -> Lwt_io.printl "ERROR"
    in
    Lwt_main.run (loop ())
end
