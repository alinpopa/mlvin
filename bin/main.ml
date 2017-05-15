open Mlvin
open Cmdliner

module Runner = Slack_runner_async.Run

let token =
  let doc = "The Slack API access token" in
  Cmdliner.Arg.(required & opt (some string) None & info ["t"; "token"] ~docv:"TOKEN" ~doc)

let info =
  let doc = "My own Slack Bot." in
  Cmdliner.Term.info "mlvin" ~doc

let main =
  let open Core.Std in
  Random.init (int_of_float (Time.to_float (Time.now ())));
  Cmdliner.Term.(pure Runner.run $ token)

let () =
  match Cmdliner.Term.eval (main, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
