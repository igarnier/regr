open Numerics
open StaTz
open Regr
open Mcts

let truth x = 2187.0 +. (0.12 *. x *. x)

(* This is our base space. *)
let inputs = Array.init 1000 (fun i -> float_of_int i -. 500.)

let noisy_outputs =
  let gaussian = Stats.gaussian ~mean:0.0 ~std:10.0 in
  Float64.Vec.of_array
  @@ Array.map (fun x -> Stats.sample_gen gaussian +. truth x) inputs

let lpnorm p vec =
  let v = Float64.Vec.pow p vec in
  let s = Float64.Vec.sum v in
  s ** (1.0 /. p)

let loss nsamples (s : State_space.t) =
  let (term, signature) = Exec.prepare s.term in
  let term = Exec.normalize term in
  let predicted =
    Float64.Vec.of_array
    @@ Array.map
         (fun input ->
           let empirical_output =
             Stats.empirical_of_generative
               ~nsamples
               (Stats.generative ~sampler:(fun () ->
                    let args =
                      Exec.sample_arguments s.rng_state [| input |] signature
                    in
                    Exec.evaluate term signature args))
           in
           Stats.mean (module Structures.Float) empirical_output)
         inputs
  in
  lpnorm 2. (Float64.Vec.sub predicted noisy_outputs)

module Spec : Mcts.S = struct
  type terminal = State_space.t

  type nonterminal = State_space.t

  type state = Terminal of terminal | Nonterminal of nonterminal

  type action = Rules.transition

  let actions (state : nonterminal) =
    Array.of_list (Rules.list_transitions state Rules.rules)

  let next (nt : nonterminal) (a : action) : state =
    let term = Rules.perform_transition nt a in
    let stats = Kernel.stats term in
    let state = { nt with State_space.term; stats } in
    let holes = stats.holes in
    if holes = 0 then Terminal state else Nonterminal state

  let reward (state : terminal) = assert false

  let exploration_depth = `Unbounded

  let exploration_kernel = `Uniform

  let pp_action = Rules.pp_transition
end
