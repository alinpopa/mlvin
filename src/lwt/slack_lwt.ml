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
end

module Runner : (Mlvin.Run.Runner with type t = string) = struct
  type t = string

  let rec client () =
    let open Lwt.Infix in
    Lwt.return () >>= fun _ -> client ()

  let run (token : t) =
    Lwt_main.run (client ())
end
