open Core
open Variables
open GoblintCil
type liveness = Alive | Zombie | Dead

let ( * ) (a : liveness) (b : liveness) =
  match a with
  | Alive -> (
      match b with Zombie -> Zombie | Dead -> Zombie | Alive -> Alive)
  | Dead -> ( match b with Zombie -> Zombie | Dead -> Dead | Alive -> Zombie)
  | Zombie -> Zombie

let string_of_liveness l =
  match l with Alive -> "Alive" | Dead -> "Dead" | Zombie -> "Zombie"

module Location = struct
  module T = struct
    type t = Cil.location = {
      line : int;
      file : string;
      byte : int;
      column : int;
      endLine : int;
      endByte : int;
      endColumn : int;
      synthetic : bool;
    }
    [@@deriving sexp, compare]
  end
  
  let string_of_loc (t:T.t):string = (t.file)^":"^(string_of_int t.line)^":"^(string_of_int t.column)
  
  include Comparable.Make (T)
end

module AbstractValue = struct
  module T = struct
    type t = { vid : int; indirection : int } [@@deriving sexp, compare]
  end

  include Comparable.Make (T)

  let copy_set s = List.fold_left (Set.to_list s) ~init:Set.empty ~f:Set.add

  let copy_map map copy =
    List.fold_left (Map.keys map) ~init:Map.empty ~f:(fun m k ->
        Map.add_exn m ~key:k ~data:(copy (Map.find_exn m k)))

  let pretty (prefix : string) (varmap : VarMap.t) (abstract : T.t)
      =
    Pretty.text(prefix
    ^ VarMap.name_of_vid ~vm:varmap ~vid:abstract.vid
    ^ string_of_int abstract.indirection)
end

module type AbstractMapping = sig
  type value
  type t = value AbstractValue.Map.t

  val join : t -> t -> t
  val copy : t -> t
  val empty : t
end

module Sigma : AbstractMapping = struct
  type value = AbstractValue.Set.t
  type t = value AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        AbstractValue.Set.union av bv)

  let copy c = AbstractValue.copy_map c AbstractValue.copy_set
  let empty = AbstractValue.Map.empty

  let _pretty ~vm sigma = (sigma |> AbstractValue.Map.to_alist)
  |> List.map ~f:(fun kv -> Pretty.concat (AbstractValue.pretty "l_" vm (fst kv)) ((Pretty.text " -> ")))
  |> Pretty.docList ~sep:Pretty.line Fun.id ()

end

module Chi : AbstractMapping = struct
  type value = liveness
  type t = value AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv -> av * bv)

  let copy c = AbstractValue.copy_map c Fn.id
  let empty = AbstractValue.Map.empty
end

module Phi : AbstractMapping = struct
  type value = Location.Set.t
  type t = value AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        Location.Set.union av bv)

  let copy c =
    AbstractValue.copy_map c (fun s ->
        List.fold_left (Location.Set.to_list s)
          ~init:Location.Set.empty ~f:(fun s v ->
            Location.Set.add s v))

  let empty = AbstractValue.Map.empty

  
end

module Delta = struct
  type t = {
    lookup : (int * AbstractValue.Set.t) AbstractValue.Map.t;
    relation : Int.Set.t Int.Map.t;
  }

  let equate _v1 _v2 = ()
  let empty = { lookup = AbstractValue.Map.empty; relation = Int.Map.empty }

  let copy d =
    {
      lookup =
        AbstractValue.copy_map d.lookup (fun s ->
            (fst s, AbstractValue.copy_set (snd s)));
      relation =
        List.fold_left (Int.Map.keys d.relation) ~init:Int.Map.empty
          ~f:(fun m k ->
            Int.Map.add_exn m ~key:k
              ~data:
                (List.fold_left
                   (Int.Set.to_list (Map.find_exn m k))
                   ~init:Int.Set.empty
                   ~f:(fun s v -> Int.Set.add s v)));
    }
end

module AbstractState = struct
  type t = {
    liveness : Chi.t;
    mayptsto : Sigma.t;
    reassignment : Phi.t;
    variables : VarMap.t;
  }

  let copy t =
    {
      liveness = Chi.copy t.liveness;
      mayptsto = Sigma.copy t.mayptsto;
      reassignment = Phi.copy t.reassignment;
      variables = t.variables;
    }

  let join t1 t2 =
    {
      variables = t1.variables;
      liveness = Chi.join t1.liveness t2.liveness;
      mayptsto = Sigma.join t1.mayptsto t2.mayptsto;
      reassignment = Phi.join t1.reassignment t2.reassignment;
    }

  let initial fd = let vars = (VarMap.initialize fd) in {
    variables = vars;
    liveness = Chi.empty;
    mayptsto = Sigma.empty;
    reassignment = Phi.empty;
  } 


  let pretty state = Pretty.docList ~sep:Pretty.line Fun.id () [VarMap.pretty state.variables]

  let string_of ~width state = Pretty.sprint ~width (pretty state) 
end