module Logger = Async.Std.Log.Global

module Run : sig
  val run : string -> 'a
end = struct
  let rec loop opt_feedback_r f =
    let open Async.Std in
    let open Data.Feedback in
    let start =
      match opt_feedback_r with
      | Some r -> Deferred.return (r)
      | None -> f ()
    in
    start >>= (fun feedback_r ->
      Pipe.read feedback_r >>= (fun r ->
        match r with
        | `Ok KillMeNow kill_f ->
            kill_f ();
            Logger.info "Restarting Slack handler.";
            Pipe.close_read feedback_r;
            loop None f
        | `Ok Simple x ->
            Logger.info "Got a simple msg: %s" x;
            loop (Some feedback_r) f
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
