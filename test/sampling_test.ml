open Stats
open Regr

let pp_float_array fmtr array =
  Format.fprintf
    fmtr
    "[| %a |]"
    (Format.pp_print_list
       ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ", ")
       Format.pp_print_float)
    (Array.to_list array)

let truth x = 12.0 +. (3. *. x *. x *. x) +. (2. *. x)

let _truth _x = 512.0

let inputs = Array.init 10 (fun i -> float_of_int (i * 10))

let noisy_outputs =
  let _gaussian = Gen.gaussian ~mean:0.0 ~std:10.0 in
  Array.map (fun x -> (* gaussian seed +.  *) truth x) inputs

let initial = Regr.State_space.initial_var [| "x" |]

let rng_state = Sampler.rng_state

let population =
  Dagger.Smc_inference.Non_interruptible.run
    1_000
    (Sampler.rewriter_no_score_precondition
       inputs
       noisy_outputs
       (Rules.fixed_rules initial.variables @ Rules.dynamic_rules)
       initial
       10
       0)
    rng_state

let (state, best) =
  let pop = Array.of_list population in
  Array.sort (fun (_, s) (_, s') -> Dagger.Log_space.compare s s') pop ;
  pop.(Array.length pop - 1)

let () = Format.printf "Best score: %a@." Dagger.Log_space.pp best

let () = Format.printf "Best term: %a@." State_space.pp state

let pred = Sampler.predicted inputs state

let () = Format.printf "predicted: %a@." Sampler.pp_float_array pred

let () = Format.printf "expected: %a@." Sampler.pp_float_array noisy_outputs

(* let sampler_global_error =
 *   let open Dagger.Traced in
 *   (\* let open Infix in *\)
 *   map_score
 *     ((\* let* nrewrites = sample @@ Gsl_dist.poisson ~rate:(return 10.) in *\)
 *      Regr.Sampler.rewriter_no_score
 *        (Rules.fixed_rules initial.variables @ Rules.dynamic_rules)
 *        initial
 *        nrewrites)
 *     (fun state ->
 *       let error = Sampler.error inputs noisy_outputs state in
 *       let error = error /. (1. +. error) in
 *       Stats.Pdfs.exponential ~rate:4.0 error) *)

let n = ref 0

(* let sampler_global_error_gaussian_score =
 *   let open Dagger.Smc_inference in
 *   map_log_score
 *     ((\* let* nrewrites = sample @@ Gsl_dist.poisson ~rate:(return 10.) in *\)
 *      Regr.Sampler.rewriter_no_score_precondition
 *        (Rules.fixed_rules initial.variables @ Rules.dynamic_rules)
 *        initial
 *        nrewrites)
 *     (fun state ->
 *       (\* let error = Sampler.error inputs noisy_outputs state in
 *        * let s = Stats.Pdfs.exponential_ln ~rate:4.0 error in
 *        * incr n ;
 *        * Dagger.Log_space.unsafe_cast s *\)
 *       (\* let s = Gsl.Randist.gamma_pdf ~a:1.0 ~b:2.0 error in *\)
 *       (\* let s = 1. /. (error +. 0.1) in *\)
 *       (\* if !n mod 100_000 = 0 then
 *        *   Format.printf "error = %f, score = %f@." error s ; *\)
 *       (\* s *\)
 *       (\* let (term', _) = Exec.prepare state.term in
 *        * let () = Format.printf "%a@." Exec.pp term' in *\)
 *
 *       (\* *\)
 *       let predicted = Sampler.predicted inputs state in
 *       let acc = ref Dagger.Log_space.one in
 *       for i = 0 to Array.length predicted - 1 do
 *         acc :=
 *           Dagger.Log_space.mul
 *             !acc
 *             (Dagger.Log_space.unsafe_cast
 *             @@ Stats.Pdfs.gaussian_ln
 *                  ~mean:noisy_outputs.(i)
 *                  ~std:1.0
 *                  predicted.(i))
 *       done ;
 *       !acc
 *       (\* if Float.classify_float !acc = Float.FP_zero then Float.epsilon else !acc *\)) *)

(* let sampler_global_error_precondition =
 *   let open Dagger.Traced in
 *   map_score
 *     (Regr.Sampler.rewriter_no_score_precondition
 *        (Rules.fixed_rules initial.variables @ Rules.dynamic_rules)
 *        initial
 *        nrewrites)
 *     (fun state ->
 *       let error = Sampler.error inputs noisy_outputs state in
 *       let error = error /. (1. +. error) in
 *       Stats.Pdfs.exponential ~rate:4.0 error) *)

(* let sampler_local_error =
 *   Regr.Sampler.rewriter
 *     inputs
 *     noisy_outputs
 *     (Rules.fixed_rules initial.variables @ Rules.dynamic_rules)
 *     initial
 *     nrewrites *)

(* let sampler_local_error_precondition =
 *   Regr.Sampler.rewriter_precondition
 *     inputs
 *     noisy_outputs
 *     (Rules.fixed_rules initial.variables @ Rules.dynamic_rules)
 *     initial
 *     nrewrites *)

(* let sampler_local_and_global_error_precondition =
 *   let open Dagger.Traced in
 *   let open Infix in
 *   map_score
 *     (let* initial =
 *        let flip = sample Stats_dist.bool in
 *        if_
 *          flip
 *          (return initial)
 *          (let+ c = Rules.float_model () in
 *           State_space.initial_cst c [| "x" |])
 *      in
 *      Regr.Sampler.rewriter_precondition
 *        inputs
 *        noisy_outputs
 *        (Rules.fixed_rules initial.variables @ Rules.dynamic_rules)
 *        initial
 *        nrewrites)
 *     (fun state ->
 *       let error = Sampler.error inputs noisy_outputs state in
 *       let error = error /. (1. +. error) in
 *       Stats.Pdfs.exponential ~rate:4.0 error)
 *
 * let sampler_local_error_precondition_dirichlet alpha =
 *   Regr.Sampler.rewriter_precondition_dirichlet
 *     alpha
 *     inputs
 *     noisy_outputs
 *     (Rules.fixed_rules initial.variables @ Rules.dynamic_rules)
 *     initial
 *     nrewrites *)

(* let sampler = sampler_global_error_gaussian_score *)

(* let _ = Sampler.K.run sampler
 *
 * let () =
 *   Dagger.Traced.printer :=
 *     fun _ o ->
 *       (\* let () =
 *        *   Cgraph.Internal.to_dot
 *        *     ~mode:Cgraph.Internal.Full
 *        *     (Cgraph.ex sampler)
 *        *     (open_out "/tmp/term.dot")
 *        * in *\)
 *       let s : State_space.t = Obj.obj o in
 *       let (term', _) = Exec.prepare s.term in
 *       Format.asprintf "%a@." Exec.pp term' *)

(* let () =
 *   let least_error = ref max_float in
 *   let samples = 1_000_000 in
 *   let rec loop n s =
 *     if n = samples then s
 *     else
 *       let state = Dagger.Traced.run s in
 *       let term = state.State_space.term in
 *       let (term, _) = Exec.prepare term in
 *
 *       let error = Sampler.error inputs noisy_outputs state in
 *
 *       let () =
 *         if n mod 100_000 = 0 then
 *           (let total = !Sampler.K.total in
 *            let rejected = !Sampler.K.rejected in
 *            Sampler.K.total := 0 ;
 *            Sampler.K.rejected := 0 ;
 *            Format.printf
 *              "rejection rate: %f, error = %f, term = %a@."
 *              (float rejected /. float total))
 *             error
 *             Exec.pp
 *             term
 *       in
 *       if error < !least_error then (
 *         least_error := error ;
 *         Format.printf "%a@." Exec.pp term ;
 *         Format.printf
 *           "err = %f@.@."
 *           (Sampler.l2_error inputs noisy_outputs state)) ;
 *       loop (n + 1) (Sampler.K.sample_step s rng_state)
 *   in
 *   let t0 = Unix.gettimeofday () in
 *   let res = loop 0 sampler in
 *   let t1 = Unix.gettimeofday () in
 *   let () =
 *     Cgraph.Internal.to_dot
 *       ~mode:Cgraph.Internal.Full
 *       (Cgraph.ex res)
 *       (open_out "/tmp/term.dot")
 *   in
 *
 *   Format.printf
 *     "performed %d samples in %f seconds (%f samples/sec)"
 *     samples
 *     (t1 -. t0)
 *     (float_of_int samples /. (t1 -. t0)) ;
 *   () *)
