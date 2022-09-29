open Core
open GoblintCil

type liveness = Alive | Zombie | Dead

let ( * ) (a : liveness) (b : liveness) =
  match a with
  | Alive -> (
      match b with Zombie -> Zombie | Dead -> Zombie | Alive -> Alive)
  | Dead -> ( match b with Zombie -> Zombie | Dead -> Dead | Alive -> Zombie)
  | Zombie -> Zombie
  
(** `
  Both lifetime variables and abstract locations can be 
  represented by a pair of ints, where the first value is 
  the unique ID for a reference-type variable, and the second
  is the level of indirection into its type.
**)

module AbstractValue = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare]
  end

  include Comparable.Make (T)

  let copy_set s = List.fold_left (Set.to_list s) ~init:Set.empty ~f:Set.add

  let copy_map map copy =
    List.fold_left (Map.keys map) ~init:Map.empty ~f:(fun m k ->
        Map.add_exn m ~key:k ~data:(copy (Map.find_exn m k)))
end

module type AbstractMap = sig
  type value
  type t = value AbstractValue.Map.t

  val join : t -> t -> t
  val copy : t -> t
  val initial : Cil.fundec -> t
end


module Sigma : AbstractMap = struct
  type value = AbstractValue.Set.t
  type t = value AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        AbstractValue.Set.union av bv)

  let copy c = AbstractValue.copy_map c AbstractValue.copy_set
  let initial _fd = AbstractValue.Map.empty
end

module Chi : AbstractMap = struct
  type value = liveness
  type t = value AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv -> av * bv)

  let copy c = AbstractValue.copy_map c Fn.id
  let initial _fd = AbstractValue.Map.empty
end

module Phi : AbstractMap = struct
  type value = Int.Set.t
  type t = value AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        Int.Set.union av bv)

  let copy c =
    AbstractValue.copy_map c (fun s ->
        List.fold_left (Int.Set.to_list s) ~init:Int.Set.empty ~f:(fun s v ->
            Int.Set.add s v))

  let initial _fd = AbstractValue.Map.empty
end

module AbstractState = struct
  type t = {
    liveness: Chi.t;
    mayptsto: Sigma.t;
    reassignment: Phi.t;
  }

  let initial fd = {
    liveness = Chi.initial fd;
    mayptsto = Sigma.initial fd;
    reassignment = Phi.initial fd;
  }

  let copy t = {
    liveness = Chi.copy t.liveness;
    mayptsto = Sigma.copy t.mayptsto;
    reassignment = Phi.copy t.reassignment;
  }

  let join t1 t2 = {
    liveness = Chi.join t1.liveness t2.liveness;
    mayptsto = Sigma.join t1.mayptsto t2.mayptsto;
    reassignment = Phi.join t1.reassignment t2.reassignment;
  }

end