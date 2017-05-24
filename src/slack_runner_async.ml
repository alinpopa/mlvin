module Logger = Async.Log.Global

module Option = Mlvin.Option

module Runner : (Run.Runner with type t = string) = struct
  type t = string

  let try_kill kill_f =
    try kill_f ()
    with _ -> Logger.error "Got error while running the kill function, but will continue..."

  let start opt_feedback_r f =
    let open Async in
    let sec = Core.sec in
    let feedback = Option.(opt_feedback_r >>| Deferred.return) in
    Option.or_else feedback (fun () ->
      try_with f >>= function
        | Ok r -> Deferred.return r
        | Error _ ->
            let msg = Data.Feedback.Retry "got error, restarting handler" in
            let (restart_r, restart_w) = Pipe.create () in
            let _ = Clock.after (sec 10.0) >>= fun () ->
              Pipe.write restart_w msg in
            Deferred.return restart_r)

  let rec loop opt_feedback_r f =
    let open Async in
    let open Data.Feedback in
    start opt_feedback_r f >>= (fun feedback_r ->
      Pipe.read feedback_r >>= function
        | `Ok KillMeNow kill_f ->
            try_kill kill_f;
            Logger.info "Restarting Slack handler.";
            Pipe.close_read feedback_r;
            loop None f
        | `Ok Simple x ->
            Logger.info "Got a simple msg: %s" x;
            loop (Some feedback_r) f
        | `Ok Retry s ->
            Logger.info "Retrying with ... %s" s;
            Pipe.close_read feedback_r;
            loop None f
        | `Eof ->
            Deferred.return (Logger.info "Eof; closing the feedback loop"))

  let run (token : t) =
    let open Core in
    let open Async in
    let _ = Log.Global.set_level (Log.Level.of_string "Info") in
    let _ = Log.Global.set_output [Log.Output.stdout ()] in
    let f () =
      Slack.Handler.start token
    in
    let _ = loop None f in
    never_returns (Scheduler.go ~raise_unhandled_exn:true ())
end
