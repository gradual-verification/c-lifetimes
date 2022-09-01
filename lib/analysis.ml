open Abstract
open GoblintCil
module DF = Dataflow

let handle_inst (i : instr) (state : Sigma.sigma) : Sigma.sigma = match i with
  | _ -> state
let handle_stmt (i : stmt) (state : Sigma.sigma) : Sigma.sigma = match i with
  | _ -> state

(*
module LifetimeInferenceDataflow = struct
  type t = Sigma.sigma
  let name = "Lifetime Inference"
  let debug = false
  let copy = Sigma.copy

  let combinePredecessors (s : stmt) (old : t) (ll : t) = Sigma.join old ll
  let doInstr (i : instr) (ll : t) = let action = handle_inst i in DF.Post action
  let doStmt (s : stmt) (ll: t) = let action = handle_stmt s in DF.Post action
  let filterStmt = true

end 

module LifetimeInference = DF.ForwardsDataFlow(LifetimeInferenceDataflow)

*)