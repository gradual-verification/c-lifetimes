open Core
open GoblintCil
open Cil_wrappers
open Lattice
open Abstract_loc
open Vitality


(* The liveness map, which maps abstract locations to their vitality *)
module Liveness : sig
  include LatticeState
  val kill :
  t -> AbstractLoc.Set.t -> t
end = struct
  module V = struct
    type v = vitality

    let join = join_vitality
  end

  include V
  module L = Lattice (V)
  include L

  let pretty ?(indent = 4) s vm =
    let pretty_value (v : vitality) (_vm : VarMap.t) =
      Pretty.text (string_of_vitality v)
    in
    L.pretty ~indent ~vm s pretty_value

    let initial vm =
      let to_alive abv_list = List.map abv_list ~f:(fun abv -> (abv, Alive)) in
      L.initial ~vm ~locals:to_alive ~param:to_alive

  let kill liveness locs =
    AbstractLoc.Set.fold locs ~init:liveness ~f:(fun m key ->
        AbstractLoc.Map.set m ~key ~data:Dead)
end