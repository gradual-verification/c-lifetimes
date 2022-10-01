open GoblintCil
open Abstract
module DF = Dataflow

let flowInst _instr state = state

module LifetimeChecking = struct
  type t = AbstractState.t

  let name = "Lifetime Inference"
  let debug = ref false
  let copy = AbstractState.copy

  (** For each statement id, the data at the start. Not found in the hash
   table means nothing is known about the state at this point. At the end
    of the analysis this means that the block is not reachable. *)
  let stmtStartData = Inthash.create 64

  (** Give the first value for a predecessors, compute the value to be set for the block *)
  let computeFirstPredecessor _stmt state = state

  (** Take some old data for the start of a statement, and some new data for
    the same point. Return None if the combination is identical to the old
    data. Otherwise, compute the combination, and return it. *)
  let combinePredecessors (_s : stmt) ~old (state : t) =
    Some (AbstractState.join old state)

  (** Whether to put this statement in the worklist. This is called when a
    block would normally be put in the worklist. *)
  let filterStmt _stmt = true

  let pretty _unit _s = Pretty.text "N/A"

  (** The (forwards) transfer function for an instruction. The
     {!Cil.currentLoc} is set before calling this. The default action is to
     continue with the state unchanged. *)
  let doInstr (i : instr) (_state : t) =
    let update = flowInst i in
    DF.Post update

  (** The (forwards) transfer function for a statement. The {!Cil.currentLoc}
     is set before calling this. The default action is to do the instructions
     in this statement, if applicable, and continue with the successors. *)
  let doStmt _stmt _state = DF.SDefault

  let doGuard _exp _state = DF.GDefault

  (** Generate the successor to an If statement assuming the given expression
      is nonzero.  Analyses that don't need guard information can return
      GDefault; this is equivalent to returning GUse of the input.
      A return value of GUnreachable indicates that this half of the branch
      will not be taken and should not be explored.  This will be called
      twice per If, once for "then" and once for "else".
    *)
end

module LifetimeCheckingDF = DF.ForwardsDataFlow (LifetimeChecking)

let analyze_function (fd : Cil.fundec):unit =
  let first_stmt = List.hd fd.sbody.bstmts in
  let initialState = AbstractState.initial fd in
  print_string ((AbstractState.string_of ~width:1 initialState)^"\n");

  Inthash.clear LifetimeChecking.stmtStartData;
  Inthash.add LifetimeChecking.stmtStartData first_stmt.sid initialState;
  LifetimeCheckingDF.compute[first_stmt]
