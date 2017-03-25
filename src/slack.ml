open Lwt
open Cohttp
open Cohttp_lwt_unix
open Websocket_lwt

let display_body body =
  Printf.printf "Body of length: %d\n" (String.length body)
  |> fun () -> Printf.printf "Raw body: '%s'\n" body
  |> fun () -> body

let display_resp resp =
  resp
  |> Response.status
  |> Code.code_of_status
  |> fun code -> Printf.printf "Response code: %d\n" code
  |> fun () -> Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string)
  |> fun () -> resp

let start_rtm token =
  Client.get (Uri.of_string ("https://slack.com/api/rtm.start?token=" ^ token)) >>= fun (resp, body) ->
    resp
    |> display_resp
    |> fun _ -> body
    |> Cohttp_lwt_body.to_string >|= fun body ->
        body
        |> display_body
        |> Yojson.Basic.from_string

let client uri =
  let open Frame in
  let module C = Cohttp in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
    let default_ctx = Conduit_lwt_unix.default_ctx in
    (Conduit_lwt_unix.endp_to_client ~ctx:default_ctx endp >>= fun client ->
      with_connection ~ctx:default_ctx client uri) >>= fun (recv, send) ->
        let react fr =
          match fr.opcode with
          | Opcode.Ping -> send @@ Frame.create ~opcode:Opcode.Pong ()
          | Opcode.Close ->
              (if String.length fr.content >= 2 then
                send @@ Frame.create ~opcode:Opcode.Close
                ~content:(String.sub fr.content 0 2) ()
              else send @@ Frame.close 1000) >>= fun () ->
                Lwt.fail Exit
          | Opcode.Pong -> Lwt.return_unit
          | Opcode.Text -> Lwt_io.printf "Got some text: %s\n" fr.content
          | Opcode.Binary -> Lwt_io.printf "%s\n" fr.content
          | _ ->
              send @@ Frame.close 1002 >>= fun () ->
                Lwt.fail Exit
        in
        let rec react_forever () = recv () >>= react >>= react_forever
        in
        react_forever ()
