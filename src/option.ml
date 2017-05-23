include Core.Std.Option

let or_else opt f =
  match opt with
  | Some a -> a
  | None -> f ()
