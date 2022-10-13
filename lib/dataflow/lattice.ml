open Cil_wrappers
open GoblintCil
open Abstract_loc

module type LatticeState = sig
  type v
  type t

  val join : t -> t -> t
  val initial : VarMap.t -> t
  val pretty : ?indent:int -> t -> VarMap.t -> Pretty.doc
end

module type LatticeDefn = sig
  type v

  val join : v -> v -> v
end

module Lattice (V : LatticeDefn) = struct
  type t = V.v AbstractLoc.Map.t

  let join (a : t) (b : t) =
    AbstractLoc.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        V.join av bv)

  let initial ~(vm : VarMap.t)
      ~(locals : AbstractLoc.t list -> (AbstractLoc.t * 'a) list)
      ~(param : AbstractLoc.t list -> (AbstractLoc.t * 'a) list) : t =
    AbstractLoc.initialize_absloc_map ~vm ~map_to:param ~locals_map_to:locals

  let pretty ?(indent = 4) ~vm (map : t)
      (pretty_value : V.v -> VarMap.t -> Pretty.doc) =
    AbstractLoc.pretty_map ~indent ~vm ~map ~pretty_value:(fun v ->
        pretty_value v vm)
end