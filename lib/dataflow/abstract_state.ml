open Core
open GoblintCil
open Assigned
open Liveness
open Points_to
open Cil_wrappers

module AbstractState = struct
  type t = {
    liveness : Liveness.t;
    mayptsto : MayPtsTo.t;
    reassignment : AssignedAt.t;
    variables : VarMap.t;
  }

  let copy t =
    {
      liveness = t.liveness;
      mayptsto = t.mayptsto;
      reassignment = t.reassignment;
      variables = t.variables;
    }

  let join t1 t2 =
    {
      variables = t1.variables;
      liveness = Liveness.join t1.liveness t2.liveness;
      mayptsto = MayPtsTo.join t1.mayptsto t2.mayptsto;
      reassignment = AssignedAt.join t1.reassignment t2.reassignment;
    }

  let initial fd =
    let vm = VarMap.initialize fd in
    {
      variables = vm;
      liveness = Liveness.initial vm;
      mayptsto = MayPtsTo.initial vm;
      reassignment = AssignedAt.initial vm;
    }

  let title text doc = Pretty.concat (Pretty.text (text ^ ":\n")) doc

  let pretty state =
    let varmap_pretty = VarMap.pretty state.variables in
    let mayptsto_pretty = MayPtsTo.pretty state.mayptsto state.variables in
    let liveness_pretty = Liveness.pretty state.liveness state.variables in
    let assignedat_pretty = AssignedAt.pretty state.reassignment state.variables in
    Pretty.indent 4
      (Pretty.docList ~sep:(Pretty.text "\n") Fun.id ()
         [
           title "Variables" varmap_pretty;
           title "May-Points-To" mayptsto_pretty;
           title "Liveness" liveness_pretty;
           title "Last Assigned At" assignedat_pretty;
         ])

  let string_of ~width state = Pretty.sprint ~width (pretty state)
end
