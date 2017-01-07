open Lwt
open Cohttp
open Cohttp_lwt_unix

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

let body =
  Client.get (Uri.of_string "https://api.ipify.org?format=json") >>= fun (resp, body) ->
    resp
    |> display_resp
    |> fun _ -> body
    |> Cohttp_lwt_body.to_string >|= fun body ->
      body
      |> display_body
      |> Yojson.Basic.from_string

let () =
  let body = Lwt_main.run body in
  let open Yojson.Basic.Util in
  print_endline ("Ip address: " ^ (body |> member "ip" |> to_string))
