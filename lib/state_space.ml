type t =
  { term : Kernel.Term.t; variables : string array; stats : Kernel.stats }

let initial_var variables =
  let term = Kernel.var variables.(0) in
  { term; variables; stats = Kernel.stats term }

let initial_cst c variables =
  let term = Kernel.const c in
  { term; variables; stats = Kernel.stats term }

let compare state1 state2 = Lib_rewriting.Term.compare state1.term state2.term

let pp fmtr { term; stats; _ } =
  Format.fprintf
    fmtr
    "@[<hov 6>term = %a@]@;stats = %a@;"
    Kernel.Term.pp
    term
    Kernel.pp_stats
    stats
