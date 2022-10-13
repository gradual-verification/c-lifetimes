open Core
open Lattice
open Helpers
open Abstract_loc
open Cil_wrappers


module AssignedAt : sig
  include LatticeState
  val update: t -> AbstractLoc.Set.t -> SourceLocation.t -> t
end = struct
  module V = struct
    type v = SourceLocation.Set.t

    let join = SourceLocation.Set.union
  end

  include V
  module L = Lattice (V)
  include L

  let pretty ?(indent = 4) s vm =
    let pretty_value (locs : SourceLocation.Set.t) (_vm : VarMap.t) =
      PrettyExtensions.pretty_list
        (SourceLocation.Set.to_list locs)
        SourceLocation.pretty
    in
    L.pretty ~indent ~vm s pretty_value

  let initial vm =
    let to_empty abv_list =
      List.map abv_list ~f:(fun abv -> (abv, SourceLocation.Set.empty))
    in
    L.initial ~vm ~locals:to_empty ~param:to_empty

    let update prev abslocs loc =
      AbstractLoc.Set.fold abslocs ~init:prev ~f:(
      
      fun m key -> let prevSet = AbstractLoc.Map.find prev key in 
      let toUpdate = (match prevSet with
          | Some(set) -> set 
          | None -> SourceLocation.Set.empty) 
      in let updated = SourceLocation.Set.add toUpdate loc in
          AbstractLoc.Map.set m ~key ~data:updated)
end