(* open Core
open Abstract
open Cil_wrappers

module Delta = struct
  type indirection_index = int
  type lvar_origin = (VarInfo.t * indirection_index)
  type t = {
    var_association: AbstractValue.T.t list Int.Map.t;
    lookup : (int * AbstractValue.Set.t) AbstractValue.Map.t;
    relation : Int.Set.t Int.Map.t;
  }
  let equate (v1:lvar_origin) (v2:lvar_origin) = ()
  let outlives (v1:lvar_origin) (v2:lvar_origin) = ()
end*)