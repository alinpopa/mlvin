module type Run = sig
  type t = string
  val run : t -> 'a
end

module Make (Runner : Run) : Run = struct
  type t = Runner.t
  let run x = Runner.run x
end
