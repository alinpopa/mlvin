open Core.Std
open Mlvin
open Cmdliner

module Logger = Async.Std.Log.Global

let token =
  let doc = "The Slack API access token" in
  Cmdliner.Arg.(required & opt (some string) None & info ["t"; "token"] ~docv:"TOKEN" ~doc)

let info =
  let doc = "My own Slack Bot." in
  Cmdliner.Term.info "mlvin" ~doc

let rec restart_handler restart_r f =
  let open Async.Std in
  Pipe.read restart_r >>= (fun m ->
    match m with
    | `Ok "restart" ->
        Logger.info "Restarting listener";
        f ();
        restart_handler restart_r f
    | _ ->
        Logger.info "Got other message";
        restart_handler restart_r f)

let rec feedback_loop feedback_r restart_w =
  let open Async.Std in
  let open Data.Feedback in
  Pipe.read feedback_r >>= (fun r ->
    match r with
    | `Ok KillMeNow f ->
        f ();
        Pipe.write restart_w "restart" >>= (fun _ ->
          feedback_loop feedback_r restart_w)
    | `Ok Simple x ->
        Logger.info "Got a simple msg: %s" x;
        feedback_loop feedback_r restart_w
    | `Eof ->
        Deferred.return (Logger.info "Eof; closing the feedback loop"))

let run token =
  let open Async.Std in
  let _ = Log.Global.set_level (Log.Level.of_string "Info") in
  let _ = Log.Global.set_output [Log.Output.stdout ()] in
  let (restart_r, restart_w) = Pipe.create () in
  let (feedback_r, feedback_w) = Pipe.create () in
  let f () =
    Slack.Handler.start token feedback_w
  in
  let _ = feedback_loop feedback_r restart_w in
  let _ = restart_handler restart_r f in
  let _ = Pipe.write restart_w "restart" in
  never_returns (Scheduler.go ~raise_unhandled_exn:true ())

let execute = Cmdliner.Term.(
  pure run $ token)

let () =
  match Cmdliner.Term.eval (execute, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
