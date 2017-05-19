module Logger = Async.Std.Log.Global

module Run : Slack_runner.Run = struct
  let rec loop opt_feedback_r f =
    let open Async.Std in
    let open Core.Std in
    let open Data.Feedback in
    let start =
      match opt_feedback_r with
      | Some r ->
          Deferred.return (r)
      | None ->
          try_with (fun () -> f ()) >>= fun response ->
            match response with
            | Ok r ->
                Deferred.return (r)
            | Error _ ->
                let msg = Data.Feedback.Retry "got error, restarting handler" in
                let (restart_r, restart_w) = Pipe.create () in
                let _ = Clock.after (sec 10.0) >>= fun () ->
                  Pipe.write restart_w msg in
                Deferred.return (restart_r)
    in
    start >>= (fun feedback_r ->
      Pipe.read feedback_r >>= (fun r ->
        match r with
        | `Ok KillMeNow kill_f ->
            let _ =
              try
                kill_f ();
              with
              | _ -> Logger.error "Got error while running the kill function, but will continue..."
            in
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
            Deferred.return (Logger.info "Eof; closing the feedback loop")))

  let run token =
    let open Core.Std in
    let open Async.Std in
    let _ = Log.Global.set_level (Log.Level.of_string "Info") in
    let _ = Log.Global.set_output [Log.Output.stdout ()] in
    let f () =
      Slack.Handler.start token
    in
    let _ = loop None f in
    never_returns (Scheduler.go ~raise_unhandled_exn:true ())
end
