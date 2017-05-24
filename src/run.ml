module type Runner = sig
  type t

  val run : t -> 'a
end
