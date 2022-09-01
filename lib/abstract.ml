open Core
open GoblintCil
module DF = Dataflow


module PointsTo = struct
  type points_to_map = (Int.Set.t) Int.Map.t 

  let join (a: points_to_map) (b: points_to_map) = Int.Map.merge a b ~f:(fun ~key:_ -> function
    | `Left v -> Some(v)
    | `Right v -> Some(v)
    | `Both(v1, v2) -> Some(Set.union v1 v2)
  )

  let copy (a: points_to_map) = a
end

module Liveness = struct
  type liveness = Alive | Zombie | Dead
  let liveness_str = function
  | Alive -> "Alive"
  | Zombie -> "Zombie"
  | Dead -> "Dead"

  let join_liveness_lattice (a:liveness) (b:liveness) = match a with 
    | Alive -> (match b with 
    | Zombie -> Zombie | Dead -> Zombie | Alive -> Alive)
    | Dead -> (match b with 
        | Zombie -> Zombie | Dead -> Dead | Alive -> Zombie)
    | Zombie -> Zombie

  type liveness_map = liveness Int.Map.t

  let copy (a: liveness_map) = a

  let join (a: liveness_map) (b: liveness_map) = Int.Map.merge a b ~f:(fun ~key:_ -> function
  | `Left v -> Some(v)
  | `Right v -> Some(v)
  | `Both(v1, v2) -> Some(join_liveness_lattice v1 v2)
)
end

module Lifetimes = struct
  type lifetime_variable = {
    scope: int option;
    id: int option;
  }

  type lifetime_map = lifetime_variable Int.Map.t

  let join (a: lifetime_map) (b: lifetime_map) = Int.Map.merge a b ~f:(fun ~key:_ -> function
  | `Left v -> Some(v)
  | `Right v -> Some(v)
  | `Both(v1, _) -> Some(v1)
  )

  let copy (a: lifetime_map) = a
end

module Sigma = struct
  type sigma = {
    points_to: PointsTo.points_to_map;
    liveness_map: Liveness.liveness_map;
    lifetime_map: Lifetimes.lifetime_variable Int.Map.t
  }

  let join (a: sigma) (b: sigma) = {
    points_to = PointsTo.join a.points_to b.points_to;
    liveness_map = Liveness.join a.liveness_map b.liveness_map; 
    lifetime_map = Lifetimes.join a.lifetime_map b.lifetime_map
  }

  let copy (a: sigma) = {
    points_to = PointsTo.copy a.points_to;
    liveness_map = Liveness.copy a.liveness_map;
    lifetime_map = Lifetimes.copy a.lifetime_map;
  }
end



