open Core
open Cil_wrappers
open GoblintCil

type liveness = Alive | Zombie | Dead
type vid = int [@@deriving sexp, compare]

let ( * ) (a : liveness) (b : liveness) =
  match a with
  | Alive -> (
      match b with Zombie -> Zombie | Dead -> Zombie | Alive -> Alive)
  | Dead -> ( match b with Zombie -> Zombie | Dead -> Dead | Alive -> Zombie)
  | Zombie -> Zombie

let string_of_liveness l =
  match l with Alive -> "Alive" | Dead -> "Dead" | Zombie -> "Zombie"

module AbstractValue = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare]
  end

  include Comparable.Make (T)

  let gen_local (vi : VarInfo.t) = (vi.vinfo.vid, 0)

  let gen_all (vi : VarInfo.t) : T.t list =
    let rec lift index (vi : VarInfo.t) lst =
      match lst with
      | _ :: tl -> [ (vi.vinfo.vid, index) ] @ lift (index + 1) vi tl
      | [] -> []
    in
    lift 0 vi vi.unrolled_type

  let copy_set s = List.fold_left (Set.to_list s) ~init:Set.empty ~f:Set.add

  let copy_map map copy =
    List.fold_left (Map.keys map) ~init:Map.empty ~f:(fun m k ->
        match Map.find m k with
        | Some v -> Map.add_exn m ~key:k ~data:(copy v)
        | None -> m)

  let pretty (prefix : string) (varmap : VarMap.t) (abstract : T.t) =
    Pretty.text
      (prefix
      ^ VarMap.name_of_vid ~vm:varmap ~vid:(fst abstract)
      ^ string_of_int (snd abstract))

  let pretty_list (prefix : string) (vm : VarMap.t) (ps : 'a list) =
    let rec concatenate prefix vm ls =
      match ls with
      | h :: [] -> pretty prefix vm h
      | h :: tl ->
          Pretty.concat
            (Pretty.concat (pretty prefix vm h) (Pretty.text ","))
            (concatenate prefix vm tl)
      | [] -> Pretty.nil
    in
    Pretty.concat (Pretty.text "{")
      (Pretty.concat (concatenate prefix vm ps) (Pretty.text "}"))
end

module Sigma = struct
  type value = AbstractValue.Set.t
  type t = value AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        AbstractValue.Set.union av bv)

  let copy c = AbstractValue.copy_map c AbstractValue.copy_set
  let empty = AbstractValue.Map.empty

  let rec split_pairs (tlist : AbstractValue.T.t list) :
      (AbstractValue.T.t * AbstractValue.Set.t) list =
    match tlist with
    | h1 :: h2 :: tl ->
        [ (h1, AbstractValue.Set.add AbstractValue.Set.empty h2) ]
        @ split_pairs ([ h2 ] @ tl)
    | h :: [] -> [ (h, AbstractValue.Set.empty) ]
    | [] -> []

  let initial vm : value AbstractValue.Map.t =
    AbstractValue.Map.of_alist_exn
      (List.fold_left (VarMap.parameter_data vm) ~init:[]
         ~f:(fun l (v : VarInfo.t) -> l @ split_pairs (AbstractValue.gen_all v))
      @ List.map (VarMap.local_data vm) ~f:(fun vi ->
            (AbstractValue.gen_local vi, AbstractValue.Set.empty)))

  let pretty ?(indent = 4) ~vm sigma =
    Pretty.indent indent
      (sigma |> AbstractValue.Map.to_alist
      |> List.map ~f:(fun kv ->
             Pretty.concat
               (AbstractValue.pretty "l_" vm (fst kv))
               (Pretty.concat (Pretty.text " -> ")
                  (AbstractValue.pretty_list "l_" vm
                     (AbstractValue.Set.to_list (snd kv)))))
      |> Pretty.docList ~sep:Pretty.line Fun.id ())

  let string_of ~vm ~width sigma = Pretty.sprint ~width (pretty ~vm sigma)
end

module Chi = struct
  type value = liveness
  type t = value AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv -> av * bv)

  let copy c = AbstractValue.copy_map c Fn.id
  let empty = AbstractValue.Map.empty

  let initial vm =
    AbstractValue.Map.of_alist_exn
      (List.map (VarMap.local_data vm) ~f:(fun vi ->
           (AbstractValue.gen_local vi, Alive))
      @ List.fold_left
          (List.map (VarMap.parameter_data vm) ~f:(fun vi ->
               List.map (AbstractValue.gen_all vi) ~f:(fun abv -> (abv, Alive))))
          ~init:[] ~f:List.append)

  let pretty ?(indent = 4) ~vm chi =
    Pretty.indent indent
      (chi |> AbstractValue.Map.to_alist
      |> List.map ~f:(fun kv ->
             Pretty.concat
               (AbstractValue.pretty "l_" vm (fst kv))
               (Pretty.concat (Pretty.text " -> ")
                  (Pretty.text (string_of_liveness (snd kv)))))
      |> Pretty.docList ~sep:Pretty.line Fun.id ())

  let string_of ~width ~vm chi = Pretty.sprint ~width (pretty ~vm chi)

  let initialize (abs_locs : AbstractValue.T.t list) =
    List.fold abs_locs ~init:AbstractValue.Map.empty ~f:(fun m key ->
        AbstractValue.Map.add_exn m ~key ~data:Alive)
end

module Phi = struct
  type value = Location.Set.t
  type t = value AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        Location.Set.union av bv)

  let copy c =
    AbstractValue.copy_map c (fun s ->
        List.fold_left (Location.Set.to_list s) ~init:Location.Set.empty
          ~f:(fun s v -> Location.Set.add s v))

  let initial vm =
    AbstractValue.Map.of_alist_exn
      (List.map (VarMap.local_data vm) ~f:(fun vi ->
           (AbstractValue.gen_local vi, Location.Set.empty))
      @ List.fold_left
          (List.map (VarMap.parameter_data vm) ~f:(fun vi ->
               List.map (AbstractValue.gen_all vi) ~f:(fun abv ->
                   (abv, Location.Set.empty))))
          ~init:[] ~f:List.append)

  let pretty ?(indent = 4) ~vm phi =
    Pretty.indent indent
      (phi |> AbstractValue.Map.to_alist
      |> List.map ~f:(fun kv ->
             Pretty.concat
               (AbstractValue.pretty "l_" vm (fst kv))
               (Pretty.concat (Pretty.text " -> ")
                  (Location.pretty_list (Location.Set.to_list (snd kv)))))
      |> Pretty.docList ~sep:Pretty.line Fun.id ())

  let string_of ~width ~vm phi = Pretty.sprint ~width (pretty ~vm phi)
  let empty = AbstractValue.Map.empty
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

  let initial fd =
    let vars = VarMap.initialize fd in
    {
      variables = vars;
      liveness = Chi.initial vars;
      mayptsto = Sigma.initial vars;
      reassignment = Phi.initial vars;
    }

  let title text doc = Pretty.concat (Pretty.text (text^":\n")) (doc)
  let pretty state =
    let varmap_pretty = VarMap.pretty state.variables in
    let sigma_pretty = Sigma.pretty ~vm:state.variables state.mayptsto in
    let chi_pretty = Chi.pretty ~vm:state.variables state.liveness in
    let phi_pretty = Phi.pretty ~vm:state.variables state.reassignment in
    Pretty.indent 4 (Pretty.docList ~sep:(Pretty.text "\n") Fun.id ()
      [ title "Variables" varmap_pretty; title "May-Points-To" sigma_pretty; title "Liveness" chi_pretty; title "Last Assigned At" phi_pretty ])

  let string_of ~width state = Pretty.sprint ~width (pretty state)
end