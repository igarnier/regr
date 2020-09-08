(* -------------------- *)

type native =
  | Add of native * native
  | Sub of native * native
  | Mul of native * native
  | Div of native * native
  | Var of string
  | Const of float

let rec pp_native fmtr (term : native) =
  match term with
  | Add (l, r) -> Format.fprintf fmtr "@[(%a + %a)@]" pp_native l pp_native r
  | Sub (l, r) -> Format.fprintf fmtr "@[(%a - %a)@]" pp_native l pp_native r
  | Mul (l, r) -> Format.fprintf fmtr "@[(%a * %a)@]" pp_native l pp_native r
  | Div (l, r) -> Format.fprintf fmtr "@[(%a / %a)@]" pp_native l pp_native r
  | Var v -> Format.fprintf fmtr "@[%s@]" v
  | Const f -> Format.fprintf fmtr "@[%.3f@]" f

(* -------------------- *)

module Prim = struct
  type t = Add | Sub | Mul | Div | Var of string | Float of float

  let compare (x : t) (y : t) = Stdlib.compare x y

  let hash = Hashtbl.hash

  let pp fmtr = function
    | Add -> Format.fprintf fmtr "Add"
    | Sub -> Format.fprintf fmtr "Sub"
    | Mul -> Format.fprintf fmtr "Mul"
    | Div -> Format.fprintf fmtr "Div"
    | Var s -> Format.fprintf fmtr "Var(%s)" s
    | Float f -> Format.fprintf fmtr "%f" f
end

(* -------------------- *)

module Term = Regr.Term.Make (Prim)
module Patt =
  Regr.Pattern.Make_with_hash_consing (Prim) (Term)
    (Regr.Path.With_hash_consing)
module Rewrite =
  Regr.Rewrite.Make (Prim) (Term) (Regr.Path.With_hash_consing) (Patt)

let add x y = Term.prim Add [x; y]

let sub x y = Term.prim Sub [x; y]

let mul x y = Term.prim Mul [x; y]

let div x y = Term.prim Div [x; y]

let float f = Term.prim (Prim.Float f) []

let var s = Term.prim (Prim.Var s) []

(* -------------------- *)

let rec to_native : Term.t -> native =
 fun { desc; _ } ->
  match desc with
  | Prim (Prim.Add, [lhs; rhs]) -> Add (to_native lhs, to_native rhs)
  | Prim (Prim.Sub, [lhs; rhs]) -> Sub (to_native lhs, to_native rhs)
  | Prim (Prim.Mul, [lhs; rhs]) -> Mul (to_native lhs, to_native rhs)
  | Prim (Prim.Div, [lhs; rhs]) -> Div (to_native lhs, to_native rhs)
  | Prim (Prim.Var v, []) -> Var v
  | Prim (Float f, []) -> Const f
  | _ -> assert false

(* -------------------- *)

let add_pattern =
  let open Patt in
  prim Prim.Add list_any

let pattern =
  let open Patt in
  focus @@ prim Prim.Mul (any @. add_pattern @. list_empty)

let rewrite_at term path =
  let target = Rewrite.get_subterm ~term ~path in
  match target.desc with
  | Prim (Prim.Mul, [something; { desc = Prim (Prim.Add, [lhs; rhs]); _ }]) ->
      let replacement = add (mul something lhs) (mul something rhs) in
      Rewrite.subst ~term ~path ~replacement
  | _ -> assert false

(* -------------------- *)

let expression =
  let subexpr = mul (float 2.) (add (var "x") (var "y")) in
  sub subexpr (div subexpr (div subexpr (var "z")))

let () = Format.eprintf "%a@." pp_native (to_native expression)

(* Matches are produced in a depth-first fashion, hence matches
   closer to the root are closer to the beginning of the list of
   matches. *)
let matches = Rewrite.all_matches pattern expression

let () =
  List.iter
    (fun path ->
      Format.printf "%s@." (Regr.Path.With_hash_consing.to_string path))
    matches

(* Rewrite deeper matches first *)
let rewritten =
  List.fold_right (fun path acc -> rewrite_at acc path) matches expression

let target =
  let subexpr = add (mul (float 2.) (var "x")) (mul (float 2.) (var "y")) in
  sub subexpr (div subexpr (div subexpr (var "z")))

let () = Format.eprintf "%a@." pp_native (to_native rewritten)

(* Thanks to hash-consing, structural equality is physical equality *)
let () = assert (target.tag = rewritten.tag)
