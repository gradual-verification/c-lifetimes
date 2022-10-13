(* liveness values. Alive indicates valid memory, 
 * Dead is invalid memory, and Zombie is the join.*)
 type vitality = Alive | Zombie | Dead [@@deriving equal]

 (* The definition of join for liveness values. *)
let join_vitality (a : vitality) (b : vitality) =
  if equal_vitality a b then a else Zombie

let string_of_vitality l =
  match l with Alive -> "Alive" | Dead -> "Dead" | Zombie -> "Zombie"