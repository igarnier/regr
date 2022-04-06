open Dagger

module type Parameters_sig = sig
  val initial : State_space.t

  val energy : State_space.t -> float

  val rules : Rules.Rules.name Rules.rule_set list

  val verbosity : [ `Silent | `Progress | `Trace ]
end

let rng_state = RNG.make [| 0x1337; 0x533D; 1291741937 |]

(* module Bandit = Ucb1.Make (Basic_structures.Std.Int) *)

(* let bandit actions =
 *   let ready_to_move = ref (Bandit.create actions) in
 *   let waiting_for_reward = ref None in
 *   ( Traced.dist0
 *       ~sampler:(fun _rng ->
 *         let (act, bandit) = Bandit.next_action !ready_to_move in
 *         waiting_for_reward := Some bandit ;
 *         act)
 *       ~log_pdf:(fun _ ->
 *         Log_space.of_float (1. /. float_of_int (Array.length actions))),
 *     ready_to_move,
 *     waiting_for_reward ) *)

(* let uniform (l : State_space.t list) : State_space.t Dagger.Traced.sampleable =
 *   match l with
 *   | [] -> assert false
 *   | _ ->
 *       let arr = Array.of_list l in
 *       Stats_dist.uniform arr *)

let l2_squared arr1 arr2 =
  let len = Array.length arr1 in
  assert (len = Array.length arr2) ;
  if len = 0 then 0.0
  else
    let acc = ref 0.0 in
    for i = 0 to len - 1 do
      acc := !acc +. ((arr1.(i) -. arr2.(i)) ** 2.)
    done ;
    1. /. float_of_int len *. !acc

let l2 arr1 arr2 = sqrt (l2_squared arr1 arr2)

let linf arr1 arr2 =
  let len = Array.length arr1 in
  assert (len = Array.length arr2) ;
  if len = 0 then 0.0
  else
    let acc = ref 0.0 in
    for i = 0 to len - 1 do
      let delta = abs_float (arr1.(i) -. arr2.(i)) in
      let v = !acc in
      acc := if delta > v then delta else v
    done ;
    !acc

let predicted inputs (state : State_space.t) =
  let term = state.term in
  let (term, sg) = Exec.prepare term in
  Array.map
    (fun input -> Exec.evaluate term sg { variables_args = [| input |] })
    inputs

let error inputs outputs (state : State_space.t) =
  let predicted = predicted inputs state in
  sqrt @@ l2_squared outputs predicted

let l2_error inputs outputs (state : State_space.t) =
  let predicted = predicted inputs state in
  l2 outputs predicted

(* let error inputs outputs (state : State_space.t) =
 *   let predicted = predicted inputs state in
 *   let ratio =
 *     Array.map2
 *       (fun o p ->
 *         let r = o /. p in
 *         match Float.classify_float r with
 *         | FP_normal | FP_subnormal | FP_zero -> r
 *         | FP_infinite | FP_nan -> 0.0)
 *       outputs
 *       predicted
 *   in
 *   (\* low stddev quantifies how much constant is the ratio *\)
 *   let stddev = Stats.Emp.Float.empirical_variance ratio in
 *   stddev
 * (\* l2_squared ratio (Array.init (Array.length ratio) (Fun.const 1.)) *\) *)

let gamma = 0.9

let max_error = error

let gaussian_score ~std outputs predicted =
  let acc = ref 1.0 in
  for i = 0 to Array.length outputs - 1 do
    acc := !acc *. Stats.Pdfs.gaussian ~mean:outputs.(i) ~std predicted.(i)
  done ;
  !acc

let pp_float_array fmtr array =
  Format.fprintf
    fmtr
    "[| %a |]"
    (Format.pp_print_list
       ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ", ")
       Format.pp_print_float)
    (Array.to_list array)

module Rule :
  Basic_structures.Basic_intf.Std with type t = Rules.Rules.name Rules.rule_set =
struct
  type t = Rules.Rules.name Rules.rule_set

  let compare (x : t) (y : t) = Int.compare x.uid y.uid

  let equal (x : t) (y : t) = Int.equal x.uid y.uid

  let hash (x : t) = Hashtbl.hash x.uid

  let pp fmtr (x : t) =
    Format.pp_print_string fmtr
    @@
    match x.rule_name with
    | Rules.Rules.Mul_var -> "mul_var"
    | Rules.Rules.Mul_cst -> "mul_cst"
    | Rules.Rules.Add_var -> "add_var"
    | Rules.Rules.Add_cst -> "add_cst"
    | Rules.Rules.Replace_by_var -> "replace_by_var"
    | Rules.Rules.Replace_by_cst -> "replace_by_cst"
    | Rules.Rules.Switch_op -> "switch_op"
    | Rules.Rules.Proj_left -> "proj_left"
    | Rules.Rules.Proj_right -> "proj_right"
    | Rules.Rules.Negate -> "negate"
    | Rules.Rules.Unnegate -> "unnegate"
end

(*

X1 -> X2 -> X3 -> X4 -> ... -> XN

at each resampling, the tail will be resampled using uniform distributions and hence become crap
ie crap is a strong attractor for this MC

what we want is to make it very unlikely for a resampling to make the chain crap

can we use a non-uniform distribution? learn somehow a relationship {list/mset of past rewrites -> measure on rewrites}
eg a Dirichlet process M(rewrites) -> GG(rewrites)

*)

(* let rewriter inputs outputs rules state rewrites =
 *   let rec loop ({ State_space.term; _ } as current) min_error i =
 *     let open Traced.Infix in
 *     let open Traced in
 *     if i = 0 then return current
 *     else
 *       let* rewriting_options = Rules.rewriting current rule in
 *       (\* let rewritings =
 *        *   List.map (fun (path, replacement) -> ()) rewritings_options
 *        * in *\)
 *       let rewritings =
 *         List.fold_left
 *           (fun rewritings (path, replacement) ->
 *             (fun () ->
 *               map (replacement ()) (fun replacement ->
 *                   let result = Kernel.Rewrite.subst ~term ~path ~replacement in
 *                   let stats = Kernel.stats result in
 *                   { current with State_space.term = result; stats }))
 *             :: rewritings)
 *           []
 *           rewriting_options
 *       in
 *       let rewritings = Array.of_list rewritings in
 *       let length = Array.length rewritings in
 *       let dist = Stats_dist.uniform (Array.init length Fun.id) in
 *       let state_with_error =
 *         let* index = Traced.sample dist in
 *         let* state = rewritings.(index) () in
 *         let error = error inputs outputs state in
 *         let min_error = Float.min error min_error in
 *         let new_error = error in
 *         Traced.return (state, min_error, new_error)
 *       in
 *       let scored =
 *         Traced.map_score state_with_error (fun (_state, min_error, new_error) ->
 *             (\*  in
 *              * let p = predicted inputs state in
 *              * let s = gaussian_score ~std:1000000000. outputs p in
 *              * Format.printf
 *              *   "predicted: %a, target: %a@."
 *              *   pp_float_array
 *              *   p
 *              *   pp_float_array
 *              *   outputs ;
 *              * let () = Format.printf "score: %.20f@." s in *\)
 *             (\* let _discount = gamma ** float (rewrites - i) in
 *              * let _p = predicted inputs state in *\)
 *             let ratio = min_error /. new_error in
 *             (\* let () = Format.printf "ratio: %f@." ratio in *\)
 *             (\* discount +. ((1. -. discount) *. ratio) *\)
 *             ratio
 *             (\* let s = Stats.Pdfs.gaussian ~mean:0.0 ~std:1.0 ratio in
 *              * Format.printf "ratio: %f, score: %f@." ratio s ;
 *              * s *\)
 *             (\* (\\* Format.printf
 *              *  *   "%f predicted: %a, target: %a@."
 *              *  *   error
 *              *  *   pp_float_array
 *              *  *   p
 *              *  *   pp_float_array
 *              *  *   outputs ; *\\)
 *              * let s =
 *              *   (\\* let error = error /. (1. +. error) in *\\)
 *              *   (\\* let error = log (error +. 1.) in *\\)
 *              *   let score = Stats.Pdfs.exponential ~rate:4.0 error in
 *              *   discount +. ((1. -. discount) *. score)
 *              * in
 *              * s *\))
 *       in
 *       let* (scored, min_error, _error) = scored in
 *       loop scored min_error (i - 1)
 *   in
 *   let initial_error = error inputs outputs state in
 *   loop state initial_error rewrites *)

(* let rewriter_precondition inputs outputs rules state rewrites =
 *   let open Traced.Infix in
 *   let open Traced in
 *   let rec loop ({ State_space.term; _ } as current) min_error rule_list =
 *     match rule_list with
 *     | [] -> return current
 *     | rule :: next_rules ->
 *         let* rewriting_options = Rules.rewriting current rule in
 *         (\* let rewritings =
 *          *   List.map (fun (path, replacement) -> ()) rewritings_options
 *          * in *\)
 *         let rewritings =
 *           List.fold_left
 *             (fun rewritings (path, replacement) ->
 *               (fun () ->
 *                 let result = Kernel.Rewrite.subst ~term ~path ~replacement in
 *                 let stats = Kernel.stats result in
 *                 { current with State_space.term = result; stats })
 *               :: rewritings)
 *             []
 *             rewriting_options
 *         in
 *         let rewritings = Array.of_list rewritings in
 *         let length = Array.length rewritings in
 *         if length = 0 then map_score (return current) (fun _ -> 0.0)
 *         else
 *           let dist = Stats_dist.uniform (Array.init length Fun.id) in
 *           let state_with_error =
 *             let* index = Traced.sample dist in
 *             let state = rewritings.(index) () in
 *             let error = error inputs outputs state in
 *             let min_error = Float.min error min_error in
 *             let new_error = error in
 *             Traced.return (state, min_error, new_error)
 *           in
 *           let scored =
 *             Traced.map_score
 *               state_with_error
 *               (fun (_state, min_error, new_error) ->
 *                 (\*  in
 *                  * let p = predicted inputs state in
 *                  * let s = gaussian_score ~std:1000000000. outputs p in
 *                  * Format.printf
 *                  *   "predicted: %a, target: %a@."
 *                  *   pp_float_array
 *                  *   p
 *                  *   pp_float_array
 *                  *   outputs ;
 *                  * let () = Format.printf "score: %.20f@." s in *\)
 *                 (\* let _discount = gamma ** float (rewrites - i) in
 *                  * let _p = predicted inputs state in *\)
 *                 let ratio = min_error /. new_error in
 *                 (\* let () = Format.printf "ratio: %f@." ratio in *\)
 *                 (\* discount +. ((1. -. discount) *. ratio) *\)
 *                 ratio
 *                 (\* let s = Stats.Pdfs.gaussian ~mean:0.0 ~std:1.0 ratio in
 *                  * Format.printf "ratio: %f, score: %f@." ratio s ;
 *                  * s *\)
 *                 (\* (\\* Format.printf
 *                  *  *   "%f predicted: %a, target: %a@."
 *                  *  *   error
 *                  *  *   pp_float_array
 *                  *  *   p
 *                  *  *   pp_float_array
 *                  *  *   outputs ; *\\)
 *                  * let s =
 *                  *   (\\* let error = error /. (1. +. error) in *\\)
 *                  *   (\\* let error = log (error +. 1.) in *\\)
 *                  *   let score = Stats.Pdfs.exponential ~rate:4.0 error in
 *                  *   discount +. ((1. -. discount) *. score)
 *                  * in
 *                  * s *\))
 *           in
 *           let* (scored, min_error, _error) = scored in
 *           loop scored min_error next_rules
 *   in
 *   let initial_error = error inputs outputs state in
 *   let rule_dist = Stats_dist.uniform (Array.of_list rules) in
 *   let* rules = sample (iid rewrites rule_dist) in
 *   loop state initial_error (Array.to_list rules)
 *
 * let rewriter_precondition_dirichlet alpha inputs outputs rules state rewrites =
 *   let open Traced.Infix in
 *   let open Traced in
 *   let rec loop ({ State_space.term; _ } as current) min_error rule_list =
 *     match rule_list with
 *     | [] -> return current
 *     | rule :: next_rules ->
 *         let* rewriting_options = Rules.rewriting current rule in
 *         let rewritings =
 *           List.fold_left
 *             (fun rewritings (path, replacement) ->
 *               (fun () ->
 *                 let result = Kernel.Rewrite.subst ~term ~path ~replacement in
 *                 let stats = Kernel.stats result in
 *                 { current with State_space.term = result; stats })
 *               :: rewritings)
 *             []
 *             rewriting_options
 *         in
 *         let rewritings = Array.of_list rewritings in
 *         let length = Array.length rewritings in
 *         if length = 0 then map_score (return current) (fun _ -> 0.0)
 *         else
 *           let dist = Stats_dist.uniform (Array.init length Fun.id) in
 *           let state_with_error =
 *             let* index = Traced.sample dist in
 *             let state = rewritings.(index) () in
 *             let error = error inputs outputs state in
 *             let min_error = Float.min error min_error in
 *             let new_error = error in
 *             Traced.return (state, min_error, new_error)
 *           in
 *           let scored =
 *             Traced.map_score
 *               state_with_error
 *               (fun (_state, min_error, new_error) ->
 *                 (\*  in
 *                  * let p = predicted inputs state in
 *                  * let s = gaussian_score ~std:1000000000. outputs p in
 *                  * Format.printf
 *                  *   "predicted: %a, target: %a@."
 *                  *   pp_float_array
 *                  *   p
 *                  *   pp_float_array
 *                  *   outputs ;
 *                  * let () = Format.printf "score: %.20f@." s in *\)
 *                 (\* let _discount = gamma ** float (rewrites - i) in
 *                  * let _p = predicted inputs state in *\)
 *                 let ratio = min_error /. new_error in
 *                 (\* let () = Format.printf "ratio: %f@." ratio in *\)
 *                 (\* discount +. ((1. -. discount) *. ratio) *\)
 *                 ratio
 *                 (\* let s = Stats.Pdfs.gaussian ~mean:0.0 ~std:1.0 ratio in
 *                  * Format.printf "ratio: %f, score: %f@." ratio s ;
 *                  * s *\)
 *                 (\* (\\* Format.printf
 *                  *  *   "%f predicted: %a, target: %a@."
 *                  *  *   error
 *                  *  *   pp_float_array
 *                  *  *   p
 *                  *  *   pp_float_array
 *                  *  *   outputs ; *\\)
 *                  * let s =
 *                  *   (\\* let error = error /. (1. +. error) in *\\)
 *                  *   (\\* let error = log (error +. 1.) in *\\)
 *                  *   let score = Stats.Pdfs.exponential ~rate:4.0 error in
 *                  *   discount +. ((1. -. discount) *. score)
 *                  * in
 *                  * s *\))
 *           in
 *           let* (scored, min_error, _error) = scored in
 *           loop scored min_error next_rules
 *   in
 *   let initial_error = error inputs outputs state in
 *   let* weights = sample (Gsl_dist.dirichlet ~alpha:(return alpha)) in
 *   let rule_dist =
 *     Stats_dist.categorical
 *       (module Rule)
 *       (List.combine rules (Array.to_list weights))
 *   in
 *   let* selected_rules = sample (iid_list rewrites rule_dist) in
 *   let+ final = loop state initial_error selected_rules in
 *   let mset =
 *     List.map
 *       (fun rule ->
 *         (\* count how many times it was sampled *\)
 *         List.fold_left
 *           (fun c selected ->
 *             if selected.Rules.uid = rule.Rules.uid then c + 1 else c)
 *           0
 *           selected_rules)
 *       rules
 *   in
 *   (final, Array.of_list mset) *)

(* let rewriter_no_score rules state rewrites =
 *   let rec loop ({ State_space.term; _ } as current) i =
 *     let open Traced.Infix in
 *     let open Traced in
 *     if i = 0 then return current
 *     else
 *       let rewriting_options = Rules.rewriting current rules in
 *       let rewritings =
 *         List.fold_left
 *           (fun rewritings (path, replacement) ->
 *             (fun () ->
 *               map (replacement ()) (fun replacement ->
 *                   let result = Kernel.Rewrite.subst ~term ~path ~replacement in
 *                   let stats = Kernel.stats result in
 *                   { current with State_space.term = result; stats }))
 *             :: rewritings)
 *           []
 *           rewriting_options
 *       in
 *       let rewritings = Array.of_list rewritings in
 *       let length = Array.length rewritings in
 *       let dist = Stats_dist.uniform (Array.init length Fun.id) in
 *       let* index = Traced.sample dist in
 *       let* state = rewritings.(index) () in
 *       loop state (i - 1)
 *   in
 *   loop state rewrites *)

let last = ref 0

let rec rewriter_no_score_precondition inputs outputs
    (rules : Rules.Rules.name Rules.rule_set list)
    ({ State_space.term; _ } as current) niter n =
  let open Rules.Lang in
  let open Infix in
  let () =
    if !last >= n then () else Format.printf "%d@." n ;
    last := n
  in
  if n = niter then return current
  else
    let* () = fork 100 in
    let* rule = sample @@ Stats_dist.uniform (Array.of_list rules) in
    let* replacement = rule.replacement () in
    let matches =
      Rules.matches_without_consing rule.Rules.rule_patt term |> Array.of_list
    in
    let length = Array.length matches in
    if length = 0 then return current
    else
      let* path = sample (Stats_dist.uniform matches) in
      let state =
        match replacement with
        | Rules.Context_blind rewriter ->
            let replacement = rewriter current in
            let result = Kernel.Rewrite.subst ~term ~path ~replacement in
            let stats = Kernel.stats result in
            { current with State_space.term = result; stats }
        | Rules.Context_aware rewriter ->
            let target = Kernel.Rewrite.get_subterm ~term ~path in
            let replacement = rewriter current target in
            let result = Kernel.Rewrite.subst ~term ~path ~replacement in
            let stats = Kernel.stats result in
            { current with State_space.term = result; stats }
      in
      let pred = predicted inputs state in
      let dist = l2 pred outputs in
      let dist = log dist in
      let* () = score (Stats.Pdfs.exponential ~rate:4.0 dist) in
      rewriter_no_score_precondition inputs outputs rules state niter (n + 1)
