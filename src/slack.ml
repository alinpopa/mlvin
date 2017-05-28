module MakeRunner (Runner : Run.Runner) : (Run.Runner with type t = Runner.t) = struct
  type t = Runner.t
  let run x = Runner.run x
end
