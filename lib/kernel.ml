open Lib_rewriting

module Prim = struct
  type t = Add | Mul | Neg | Var of string | Const of float

  let compare = Stdlib.compare

  let hash = Hashtbl.hash

  let pp fmtr (prim : t) =
    let open Format in
    match prim with
    | Add -> fprintf fmtr "Add"
    | Mul -> fprintf fmtr "Mul"
    | Neg -> fprintf fmtr "Neg"
    | Var s -> fprintf fmtr "var(%s)" s
    | Const f -> fprintf fmtr "%f" f
end

module Term = Term.Make (Prim)
module Path = Path.With_hash_consing
module Patt = Pattern.Make_with_hash_consing (Prim) (Term) (Path)
module Rewrite = Rewrite.Make (Prim) (Term) (Path) (Patt)

type stats = { nodes : int; variables : string list }

let rec stats : Term.t -> stats -> stats =
 fun term acc ->
  match term.desc with
  | Prim (Var v, []) ->
      { variables = v :: acc.variables; nodes = 1 + acc.nodes }
  | Prim (Const _, []) -> { acc with nodes = 1 + acc.nodes }
  | Prim ((Add | Mul), [l; r]) ->
      let acc = { acc with nodes = 1 + acc.nodes } in
      stats l (stats r acc)
  | Prim (Neg, [term]) ->
      let acc = { acc with nodes = 1 + acc.nodes } in
      stats term acc
  | _ ->
      Format.eprintf "Kernel.stats: invalid term %a@." Term.pp term ;
      Stdlib.failwith "stats"

let stats term = stats term { nodes = 0; variables = [] }

let pp_stats fmtr { nodes; _ } = Format.fprintf fmtr "@[{ nodes = %d }@]" nodes

let binop op x y = Term.prim op [x; y]

let add x y = Term.prim Prim.Add [x; y]

let mul x y = Term.prim Prim.Mul [x; y]

let neg x = Term.prim Prim.Neg [x]

let var s = Term.prim (Prim.Var s) []

let const f = Term.prim (Prim.Const f) []
