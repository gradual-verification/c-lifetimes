open Lifeline

let%test "Alive * Dead = Zombie" = Zombie == (join_vitality Alive Dead)
let%test "Alive * Alive = Alive" =  Alive == (join_vitality Alive Alive)
let%test "Dead * Dead = Dead" = Dead == (join_vitality Dead Dead)
let%test "Zombie * Alive = Zombie" = (join_vitality Zombie Alive) == Zombie 
let%test "Zombie * Dead = Zombie" = (join_vitality Zombie  Dead) == Zombie