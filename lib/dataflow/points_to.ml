open Core
open GoblintCil
open Lattice
open Helpers
open Abstract_loc
open Cil_wrappers

module MayPtsTo : sig
  include LatticeState
  val get_points_to : t -> AbstractLoc.Set.t -> AbstractLoc.Set.t
  val set_points_to : t -> AbstractLoc.Set.t -> AbstractLoc.Set.t -> t
  val dereference : t -> AbstractLoc.Set.t -> indirection -> AbstractLoc.Set.t
  val locations_of_exp : ?indir:int -> t -> exp -> AbstractLoc.Set.t
  val locations_of_lval : ?indir:int -> t -> lval -> AbstractLoc.Set.t
end = struct
  (* The may-points-to map, which maps abstract locations to sets of abstract locations *)
  module V = struct
    type v = AbstractLoc.Set.t

    let join = AbstractLoc.Set.union
  end

  include V
  module L = Lattice (V)
  include L

  let pretty ?(indent = 4) s vm =
    let pretty_abv abv = AbstractLoc.pretty vm abv in
    let pretty_abv_set (locs : AbstractLoc.Set.t) (_vm : VarMap.t) =
      PrettyExtensions.pretty_list (AbstractLoc.Set.to_list locs) pretty_abv
    in
    L.pretty ~indent ~vm s pretty_abv_set

  (* The helper function map_pair produces a set of mappings for a list of abstract values.
     * For a variable x:int** with vid = 5, we have the types [int **, int *, int]. If x is a parameter,
     * we use gen_all to produce [(5, 0), (5, 1), (5, 2)]. Then, we use map_pairs to produce the mappings
     * [(5, 0) -> {(5, 1)}, (5, 1) -> {(5, 2)}], which are used to populate a may-points-to map.*)
  let rec map_pairs (tlist : AbstractLoc.t list) :
      (AbstractLoc.t * AbstractLoc.Set.t) list =
    match tlist with
    | h1 :: h2 :: tl ->
        [ (h1, AbstractLoc.Set.add AbstractLoc.Set.empty h2) ]
        @ map_pairs ([ h2 ] @ tl)
    | h :: [] -> [ (h, AbstractLoc.Set.empty) ]
    | [] -> []

  let initial vm =
    let initial_local abv_list =
      List.map abv_list ~f:(fun abv -> (abv, AbstractLoc.Set.empty))
    in
    L.initial ~vm ~locals:initial_local ~param:map_pairs

  (* if we have a mapping l_1 -> {l_2, l_3, l_4}, then l_1 is the pointer location,
   * while l_2-4 are the pointee locations. *)

  (* For a set of pointer locations, get the merged set of locations pointed to by each *)
  let get_points_to (mayptsto : t) (pointers : AbstractLoc.Set.t) =
    let pointedTo =
      List.map (AbstractLoc.Set.to_list pointers) ~f:(fun abv ->
          let found_value = AbstractLoc.Map.find mayptsto abv in
          match found_value with
          | Some set -> set
          | None -> AbstractLoc.Set.empty)
    in
    List.fold pointedTo ~init:AbstractLoc.Set.empty ~f:AbstractLoc.Set.union

  (* Set each provided pointer location to point to a given set of pointee locations *)
  let set_points_to (mayptsto : t) (pointers : AbstractLoc.Set.t)
      (pointees : AbstractLoc.Set.t) : t =
    AbstractLoc.Set.fold pointers ~init:mayptsto ~f:(fun s key ->
        AbstractLoc.Map.set s ~key ~data:pointees)

  (* for a given pointer location corresponding to a local variable, get the set of pointee
   * locations at a given level of indirection in mayptsto *)
  let rec dereference (mayptsto : t) (initial : AbstractLoc.Set.t)
      (indir : indirection) : AbstractLoc.Set.t =
    if indir > 0 then
      let dereferenced_once =
        Set.fold initial ~init:AbstractLoc.Set.empty ~f:(fun acc v ->
            let pointing_to = Map.find mayptsto v in
            match pointing_to with
            | Some mptsto -> AbstractLoc.Set.union acc mptsto
            | None -> AbstractLoc.Set.empty)
      in
      dereference mayptsto dereferenced_once (indir - 1)
    else initial

  (* get the set of pointer locations for a given expression, where a
   * pointee location is pointed to by a pointer location *)
  let rec locations_of_exp ?(indir = 0) (mayptsto : t) (e : exp) :
      AbstractLoc.Set.t =
    match e with
    | AddrOf lva ->
        let adjusted = if indir > 0 then indir - 1 else indir in
        locations_of_lval ~indir:adjusted mayptsto lva
        (* unary operations consist of logical and bitwise not,
           * as well as decrement. So, we treat these as an equivalence
           * class over the locations for the base expression *)
    | UnOp (_, iex, _) -> locations_of_exp ~indir mayptsto iex
    | Lval lvl -> locations_of_lval ~indir mayptsto lvl
    (* when we encounter a conditional, we merge the sets of locations, 
     * from each branch. *)
    | Question (_, tex, fex, _) ->
        AbstractLoc.Set.union
          (locations_of_exp ~indir mayptsto tex)
          (locations_of_exp ~indir mayptsto fex)
    | _ -> AbstractLoc.Set.empty

  (* if an lval corresponds to an expression of the form *x, then it will contain 'Mem', so we
   * increment indirection by one. Else, it's just x, so we find the expressions corresponding to
   * x at the provided level of indirection from prior recursive calls *)
  and locations_of_lval ?(indir = 0) (mayptsto : t) (lv : lval) :
      AbstractLoc.Set.t =
    match lv with
    | lhost, _offset -> (
        match lhost with
        | Var vi ->
            let starting_point =
              AbstractLoc.Set.of_list
                [ AbstractLoc.gen_local (VarInfo.initialize vi) ]
            in
            dereference mayptsto starting_point indir
        | Mem ex -> locations_of_exp ~indir:(indir + 1) mayptsto ex)
end
