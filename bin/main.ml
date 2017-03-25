open Lwt
open Core.Std
open Mlvin
open Cmdliner

let token =
  let doc = "The Slack API access token" in
  Cmdliner.Arg.(required & opt (some string) None & info ["t"; "token"] ~docv:"TOKEN" ~doc)

let privileged_user =
  let doc = "The user ID that is allowed to talk to the bot" in
  Cmdliner.Arg.(required & opt (some string) None & info ["u"; "user"] ~docv:"USER" ~doc)

let info =
  let doc = "My own Slack Bot." in
  Cmdliner.Term.info "mlvin" ~doc

let run token user =
  "Token " ^ token ^ ", user " ^ user |> print_endline;
  let rtm = Lwt_main.run (Slack.start_rtm token) in
  let member = Yojson.Basic.Util.member in
  let to_string = Yojson.Basic.Util.to_string in
  Lwt_main.run @@ Slack.client @@ (Uri.with_scheme (Uri.of_string (rtm |> member "url" |> to_string)) (Option.some "https"))

let execute = Cmdliner.Term.(
  pure run $ token $ privileged_user)

let () =
  match Cmdliner.Term.eval (execute, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
