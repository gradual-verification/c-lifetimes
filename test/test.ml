open Lifeline

let%test "Alive * Dead = Zombie" = compare Zombie (Alive * Dead) == 0
let%test "Alive * Alive = Alive" = compare Alive (Alive * Alive) == 0
let%test "Dead * Dead = Dead" = compare Dead (Dead * Dead) == 0
let%test "Zombie * Alive = Zombie" = compare (Zombie * Alive) Zombie == 0
let%test "Zombie * Dead = Zombie" = compare (Zombie * Dead) Zombie == 0