open Mlvin
open Cmdliner

module Runner = Slack_runner_async.Run

let token =
  let doc = "The Slack API access token" in
  Cmdliner.Arg.(required & opt (some string) None & info ["t"; "token"] ~docv:"TOKEN" ~doc)

let info =
  let doc = "My own Slack Bot." in
  Cmdliner.Term.info "mlvin" ~doc

let execute = Cmdliner.Term.(
  pure Runner.run $ token)

let () =
  match Cmdliner.Term.eval (execute, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
