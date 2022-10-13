open Core
open GoblintCil
open Abstract_loc
open Abstract_state
open Assigned
open Liveness
open Points_to
include Cil_wrappers
module DF = Dataflow
module IH = Inthash

let flowAssignment (lv : lval) (expr : exp) (lloc : location) (_rloc : location)
    (state : AbstractState.t) : AbstractState.t =
  let pointers = MayPtsTo.locations_of_lval state.mayptsto lv in
  let pointees =
    MayPtsTo.get_points_to state.mayptsto
      (MayPtsTo.locations_of_exp state.mayptsto expr)
  in
  let updatedMayPtsTo =
    MayPtsTo.set_points_to state.mayptsto pointers pointees
  in
  let updatedAssignment = AssignedAt.update state.reassignment pointees lloc in
  {
    liveness = state.liveness;
    mayptsto = updatedMayPtsTo;
    variables = state.variables;
    reassignment = updatedAssignment;
  }

let flowFree (params : exp list) (state : AbstractState.t) : AbstractState.t =
  let pointers =
    List.fold params ~init:AbstractLoc.Set.empty ~f:(fun s e ->
        AbstractLoc.Set.union s (MayPtsTo.locations_of_exp state.mayptsto e))
  in
  let pointees = MayPtsTo.get_points_to state.mayptsto pointers in
  let updatedLiveness = Liveness.kill state.liveness pointees in
  {
    liveness = updatedLiveness;
    mayptsto = state.mayptsto;
    variables = state.variables;
    reassignment = state.reassignment;
  }

let functionCalled (ex : exp) =
  match ex with
  | Lval lv -> (
      match lv with
      | host, _offset -> (
          match host with Var vinfo -> Some vinfo.vname | _ -> None))
  | _ -> None

let flowInst (instr : Cil.instr) (state : AbstractState.t) : AbstractState.t =
  match instr with
  | Set (lv, expr, lloc, rloc) -> flowAssignment lv expr lloc rloc state
  | Call (_lvop, ex, params, _lloc, _rloc) -> (
      match functionCalled ex with
      | Some fn -> (
          match fn with "free" -> flowFree params state | _ -> state)
      | None -> state)
  | _ -> state

module LifetimeInference = struct
  type t = AbstractState.t

  let name = "Lifetime Inference"
  let debug = ref true
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

  let pretty _unit s = AbstractState.pretty s

  (** The (forwards) transfer function for an instruction. The
     {!Cil.currentLoc} is set before calling this. The default action is to
     continue with the state unchanged. *)

  let printFlowCall ~(width : int) (i : instr) : unit =
    let string_of_inst =
      Pretty.sprint ~width (Cil.printInstr Cil.defaultCilPrinter () i)
    in
    print_endline ("\n\n" ^ string_of_inst);
    print_endline "-----------------------"

  let doInstr (i : instr) (_state : t) =
    let update = flowInst i in
    printFlowCall ~width:4 i;
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

module LifetimeInferenceDF = DF.ForwardsDataFlow (LifetimeInference)

let analyze_function (fd : Cil.fundec) : unit =
  let first_stmt = List.hd_exn fd.sbody.bstmts in
  let initialState = AbstractState.initial fd in
  print_string (AbstractState.string_of ~width:1 initialState ^ "\n");

  Inthash.clear LifetimeInference.stmtStartData;

  Inthash.add LifetimeInference.stmtStartData first_stmt.sid initialState;
  LifetimeInferenceDF.compute [ first_stmt ]

let getResultsForStmt (sid : int) : AbstractState.t option =
  try Some (IH.find LifetimeInference.stmtStartData sid)
  with Not_found_s _ -> None

let getResultsForInstList (il : instr list) (stmtStartData : AbstractState.t) :
    AbstractState.t list =
  let proc_one hil i =
    match hil with
    | [] -> flowInst i stmtStartData :: hil
    | hd :: tl -> flowInst i hd :: tl
  in List.fold il ~f:proc_one ~init:[stmtStartData] |> List.tl_exn |> List.rev

class stateVisitorClass =
  object
    inherit nopCilVisitor as super
    val mutable sid = -1
    val mutable state_list = []
    val mutable curr_state = None

    method! vstmt stmt =
      let sid = stmt.sid in
      (match getResultsForStmt sid with
      | None -> curr_state <- None
      | Some vml -> (
          match stmt.skind with
          | Instr il ->
              curr_state <- None;
              state_list <- getResultsForInstList il vml
          | _ -> curr_state <- None));
      super#vstmt stmt

    method! vinst _ =
      try
        let data = List.hd state_list in
        curr_state <- Some data;
        state_list <- List.tl_exn state_list;
        DoChildren
      with Failure _ -> DoChildren

    method get_cur_state () =
      match curr_state with None -> getResultsForStmt sid | Some st -> st
  end

class lifetimeReporterClass =
  object
    inherit stateVisitorClass as _super
    method! vvrbl (_ : varinfo) = SkipChildren
  end

let lifetimeInference (fd : fundec) (_loc : location) : unit =
  analyze_function fd;
  let vis = (new lifetimeReporterClass :> nopCilVisitor) in
  ignore (visitCilFunction vis fd)
