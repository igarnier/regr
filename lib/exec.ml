type t =
  | EAdd of t * t
  | EMul of t * t
  | ENeg of t
  | EVar of string
  | EConst of float

module String_bij = Stats.Finbij.Make (String)
module String_set = Set.Make (String)

type signature = { variables : String_bij.t }

type arguments = { variables_args : float array }

let mkfreshgen () =
  let x = ref 0 in
  fun () ->
    let v = !x in
    incr x ;
    v

let variables term =
  let rec loop : t -> String_set.t -> String_set.t =
   fun term acc ->
    match term with
    | EAdd (l, r) | EMul (l, r) -> loop l (loop r acc)
    | ENeg x -> loop x acc
    | EVar s -> String_set.add s acc
    | EConst _ -> acc
  in
  loop term String_set.empty

let prepare : Kernel.Term.t -> t * signature =
 fun term ->
  let rec loop (term : Kernel.Term.t) =
    match term.desc with
    | Prim (Var v, []) -> EVar v
    | Prim (Const f, []) -> EConst f
    | Prim (Add, [l; r]) -> EAdd (loop l, loop r)
    | Prim (Mul, [l; r]) -> EMul (loop l, loop r)
    | Prim (Neg, [term]) -> ENeg (loop term)
    | _ ->
        Format.eprintf "%a" Kernel.Term.pp term ;
        assert false
  in
  let term = loop term in
  let variables = String_bij.of_list (String_set.elements (variables term)) in
  (term, { variables })

let rec normalize term =
  match term with
  | EAdd (l, r) -> (
      let l = normalize l in
      let r = normalize r in
      match (l, r) with
      | (EConst x, EConst y) -> EConst (x +. y)
      | _ -> EAdd (l, r))
  | EMul (l, r) -> (
      let l = normalize l in
      let r = normalize r in
      match (l, r) with
      | (EConst x, EConst y) -> EConst (x *. y)
      | _ -> EMul (l, r))
  | ENeg l -> (
      let l = normalize l in
      match l with EConst x -> EConst ~-.x | ENeg x -> x | _ -> ENeg l)
  | term -> term

let rec evaluate term signature args =
  match term with
  | EAdd (l, r) ->
      let l = evaluate l signature args in
      let r = evaluate r signature args in
      l +. r
  | EMul (l, r) ->
      let l = evaluate l signature args in
      let r = evaluate r signature args in
      l *. r
  | ENeg l ->
      let l = evaluate l signature args in
      ~-.l
  | EVar s ->
      let index = String_bij.idx_exn signature.variables s in
      args.variables_args.(index)
  | EConst f -> f

let rec pp fmtr term =
  let open Format in
  match term with
  | EAdd (l, r) -> fprintf fmtr "(%a + %a)" pp l pp r
  | EMul (l, r) -> fprintf fmtr "(%a * %a)" pp l pp r
  | ENeg t -> fprintf fmtr "(- %a)" pp t
  | EVar s -> fprintf fmtr "var(%s)" s
  | EConst f -> pp_print_float fmtr f

let pp fmtr term = pp fmtr term (* (normalize term) *)
