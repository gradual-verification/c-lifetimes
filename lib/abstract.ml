type liveness = Alive | Zombie | Dead
let ( * ) (a:liveness) (b:liveness) =  match a with 
  | Alive -> (match b with 
  | Zombie -> Zombie | Dead -> Zombie | Alive -> Alive)
  | Dead -> (match b with 
      | Zombie -> Zombie | Dead -> Dead | Alive -> Zombie)
  | Zombie -> Zombie


