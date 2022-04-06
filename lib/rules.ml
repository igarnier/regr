open Dagger
module Lang = Smc_inference

type 'n rule_set =
  { uid : int;
    rule_name : 'n;
    rule_patt : pattern;
    replacement : unit -> replacement Lang.t
  }

and replacement =
  | Context_aware of (State_space.t -> Kernel.Term.t -> Kernel.Term.t)
  | Context_blind of (State_space.t -> Kernel.Term.t)

and pattern = Pattern of Kernel.Patt.t | Root

type transition = { path : Kernel.Path.t; replacement : replacement Lang.t }

let pp_transition fmtr transition = Kernel.Path.pp fmtr transition.path

let matches_without_consing pattern term =
  match pattern with
  | Root -> [Kernel.Path.root]
  | Pattern patt -> Kernel.Rewrite.all_matches patt term

(* let perform_transition state { path; replacement } =
 *   match replacement with
 *   | Context_blind rewriter ->
 *       let replacement = rewriter state in
 *       Kernel.Rewrite.subst ~term:state.term ~path ~replacement
 *   | Context_aware rewriter ->
 *       let replaced = Kernel.Rewrite.get_subterm ~term:state.term ~path in
 *       let replacement = rewriter state replaced in
 *       Kernel.Rewrite.subst ~term:state.term ~path ~replacement *)

(* let rewriting (state : State_space.t) (rule : 'n rule_set) =
 *   let open Traced.Infix in
 *   let matches = matches_without_consing rule.rule_patt state.term in
 *   let+ replacement = rule.replacement () in
 *   match replacement with
 *   | Context_blind rewriter ->
 *       let result = rewriter state in
 *       List.map (fun path -> (path, result)) matches
 *   | Context_aware rewriter ->
 *       List.map
 *         (fun path ->
 *           let term = Kernel.Rewrite.get_subterm ~term:state.term ~path in
 *           let result = rewriter state term in
 *           (path, result))
 *         matches *)

(* Basic patterns *)

let is_var = function Kernel.Prim.Var _ -> true | _ -> false

let is_const = function Kernel.Prim.Const _ -> true | _ -> false

let is_neg = function Kernel.Prim.Neg -> true | _ -> false

let is_leaf = function
  | Kernel.Prim.Const _ | Kernel.Prim.Var _ -> true
  | _ -> false

let is_binop = function Kernel.Prim.Add | Kernel.Prim.Mul -> true | _ -> false

let is_unop = function Kernel.Prim.Neg -> true | _ -> false

let match_any =
  let open Kernel.Patt in
  focus any

let match_var =
  let open Kernel.Patt in
  focus (prim_pred is_var list_empty)

let match_neg =
  let open Kernel.Patt in
  focus (prim_pred is_neg list_any)

let match_var_unfocused =
  let open Kernel.Patt in
  prim_pred is_var list_empty

let match_const =
  let open Kernel.Patt in
  focus (prim_pred is_const list_empty)

let match_leaf_unfocused =
  let open Kernel.Patt in
  prim_pred is_leaf list_empty

let match_leaf_binop =
  let open Kernel.Patt in
  focus
    (prim_pred
       is_binop
       (match_leaf_unfocused @. match_leaf_unfocused @. list_empty))

let match_any_binop =
  let open Kernel.Patt in
  focus (prim_pred is_binop (any @. any @. list_empty))

let match_leaf_unop =
  let open Kernel.Patt in
  focus (prim_pred is_unop (match_leaf_unfocused @. list_empty))

let match_any_unop =
  let open Kernel.Patt in
  focus (prim_pred is_binop (any @. list_empty))

(* Rules *)

let some x = Some x

let float_model () = Lang.sample (Stats_dist.float 1_000.0)
(* Format.printf "c = %f@." c ; *)

(* let+ c = sample (Stats_dist.brownian ~start:(return 1.0) ~std:(return 1.0))
 * and+ scale =
 *   map
 *     (sample (Stats_dist.brownian ~start:(return 1.0) ~std:(return 1.0)))
 *     abs_float
 * in
 * c *. (10. ** scale) *)

module Rules = struct
  (*
    splice_op_var_l = term -> op(term, var)
    splice_op_var_r = term -> op(var, term) (for non commutative op)
    splice_op_cst_l = term -> op(term, cst)
    splice_op_cst_r = term -> op(cst, term) (for non commutative op)
    replace_by_cst = term -> cst
    switch_op = op1(l, r) <-> op2(l, r)
    proj_l = op(l, r) -> l
    proj_r = op(l, r) -> r
    neg = term <-> neg(term)
  *)
  type name =
    | Mul_var
    | Mul_cst
    | Add_var
    | Add_cst
    | Replace_by_var
    | Replace_by_cst
    | Switch_op
    | Proj_left
    | Proj_right
    | Negate
    | Unnegate

  let string_of_name (n : name) =
    match n with
    | Mul_var -> "mul_var"
    | Mul_cst -> "mul_cst"
    | Add_var -> "add_var"
    | Add_cst -> "add_cst"
    | Replace_by_var -> "replace_by_var"
    | Replace_by_cst -> "replace_by_cst"
    | Switch_op -> "switch_op"
    | Proj_left -> "proj_left"
    | Proj_right -> "proj_right"
    | Negate -> "negate"
    | Unnegate -> "unnegate"

  let name_of op x =
    let open Kernel in
    let open Prim in
    match op with
    | Add -> ( match x with `Var -> Add_var | `Const -> Add_cst)
    | Mul -> ( match x with `Var -> Mul_var | `Const -> Mul_cst)
    | _ -> assert false

  let uid =
    let x = ref 0 in
    fun () ->
      let v = !x in
      incr x ;
      v

  let splice_op_l op x body =
    let rule_name = name_of op x in
    { uid = uid ();
      rule_name;
      rule_patt = Pattern match_any;
      replacement =
        (fun () ->
          Lang.return (Context_aware (fun _state term -> body op term)))
    }

  (* let splice_op_r op x =
   *   let rule_name = name_of op x in
   *   { rule_name;
   *     rule_patt = Pattern match_any;
   *     replacements = [Context_aware (fun _state term -> Kernel.binop op x term)]
   *   } *)

  let replace_by_cst =
    let rule_name = Replace_by_cst in
    { uid = uid ();
      rule_name;
      rule_patt = Pattern match_var;
      replacement =
        (fun () ->
          Lang.map (float_model ()) (fun c ->
              Context_blind (fun _state -> Kernel.const c)))
    }

  let replace_by_var x =
    let rule_name = Replace_by_var in
    { uid = uid ();
      rule_name;
      rule_patt = Pattern match_const;
      replacement =
        (fun () -> Lang.return (Context_blind (fun _state -> Kernel.var x)))
    }

  let switch_op =
    { uid = uid ();
      rule_name = Switch_op;
      rule_patt = Pattern match_any_binop;
      replacement =
        (fun () ->
          Lang.return
          @@ Context_aware
               (fun _state term ->
                 match term.desc with
                 | Prim (Add, [l; r]) -> Kernel.mul l r
                 | Prim (Mul, [l; r]) -> Kernel.add l r
                 | _ -> assert false))
    }

  let proj_l =
    { uid = uid ();
      rule_name = Proj_left;
      rule_patt = Pattern match_any_binop;
      replacement =
        (fun () ->
          Lang.return
          @@ Context_aware
               (fun _state term ->
                 match term.desc with
                 | Prim ((Add | Mul), [l; _r]) -> l
                 | _ -> assert false))
    }

  let proj_r =
    { uid = uid ();
      rule_name = Proj_left;
      rule_patt = Pattern match_any_binop;
      replacement =
        (fun () ->
          Lang.return
          @@ Context_aware
               (fun _state term ->
                 match term.desc with
                 | Prim ((Add | Mul), [_l; r]) -> r
                 | _ -> assert false))
    }

  let neg =
    { uid = uid ();
      rule_name = Negate;
      rule_patt = Pattern match_any;
      replacement =
        (fun () ->
          Cgraph.Internal.set_next_debug_info "neg" ;
          Lang.return @@ Context_aware (fun _state term -> Kernel.neg term))
    }

  let unneg =
    { uid = uid ();
      rule_name = Negate;
      rule_patt = Pattern match_neg;
      replacement =
        (fun () ->
          Cgraph.Internal.set_next_debug_info "unneg" ;
          Lang.return @@ Context_aware (fun _state term -> Kernel.neg term))
    }

  let fixed = [switch_op; proj_l; proj_r; neg; unneg]
end

let fixed_rules variables =
  Array.fold_left
    (fun acc var ->
      Rules.splice_op_l Add `Var (fun op body ->
          Kernel.binop op (Kernel.var var) body)
      :: Rules.splice_op_l Mul `Var (fun op body ->
             Kernel.binop op (Kernel.var var) body)
      (* :: Rules.replace_by_var var *) :: acc)
    Rules.fixed
    variables

let dynamic_rules =
  let open Lang in
  let open Infix in
  let add_const =
    { uid = Rules.uid ();
      rule_name = Rules.name_of Add `Const;
      rule_patt = Pattern match_any;
      replacement =
        (fun () ->
          let+ c = float_model () in
          Context_aware
            (fun _state body -> Kernel.binop Add (Kernel.const c) body))
    }
  in
  let mul_const =
    { uid = Rules.uid ();
      rule_name = Rules.name_of Mul `Const;
      rule_patt = Pattern match_any;
      replacement =
        (fun () ->
          let+ c = float_model () in
          Context_aware
            (fun _state body -> Kernel.binop Mul (Kernel.const c) body))
    }
  in
  [add_const; mul_const; Rules.replace_by_cst]
