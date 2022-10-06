open Core
open Cil_wrappers
open GoblintCil

(* liveness values. Alive indicates valid memory, 
 * Dead is invalid memory, and Zombie is the join.*)
type vitality = Alive | Zombie | Dead [@@deriving equal]

(* CIL assigns a unique integer ID to each variable using the, 
 * struct 'varinfo', which has the field 'vid' *)
type vid = int [@@deriving sexp, compare]

(* We extend 'varinfo' by unrolling a variable's type into a list,
 * where each element of the list is the type at each level of
 * indirection. For abstract values that correspond to variables,
 * we use an 'indirection' field for type lookup. 
 * struct 'varinfo', which has the field 'vid' *)
type indirection = int [@@deriving sexp, compare]

(* The definition of join for liveness values. *)
let ( * ) (a : vitality) (b : vitality) =
  if equal_vitality a b then a else Zombie

let string_of_vitality l =
  match l with Alive -> "Alive" | Dead -> "Dead" | Zombie -> "Zombie"

(* We use a single struct, AbstractValue, to represent both abstract locations
 * and lifetime variables. *)
module AbstractValue = struct
  module T = struct
    type t = vid * indirection [@@deriving sexp, compare]
  end

  let prefix_abs_loc = "l_"
  let prefix_ltvar = "`"

  include Comparable.Make (T)

  (* Given a variable, we can choose to generate abstract values for each level of indirection
   * in its type, or just the first level. For lifetime variables, which are assigned to each
   *  type, we use gen_all. However, for abstract locations, we only use gen_all for parameters,
   *  which are initialized on entry to a function, while we use gen_local for local variables,
   *  which are uninitialized. *)
  let gen_local (vi : VarInfo.t) : T.t = (vi.vinfo.vid, 0)

  let gen_all (vi : VarInfo.t) : T.t list =
    List.mapi vi.unrolled_type ~f:(fun i _ -> (vi.vinfo.vid, i))

  (* using a given variable map, initialize a mapping from abstract locations for parameters
   *  and locals to a given type of value, which is specified in functions passed as parameters.   
   *  by default, locals and parameters are handled the same, though this can be overridden *)
  let initialize_absloc_map ~(vm : VarMap.t)
      ~(map_to : T.t list -> (T.t * 'a) list)
      ~(locals_map_to : T.t list -> (T.t * 'a) list) =
    Map.of_alist_exn
      (List.fold_left (VarMap.parameter_data vm) ~init:[]
         ~f:(fun l (v : VarInfo.t) -> l @ map_to (gen_all v))
      @ locals_map_to
          (List.map (VarMap.local_data vm) ~f:(fun vi -> gen_local vi)))

  (* produce a Pretty.doc object for a given abstract value *)
  let pretty (prefix : string) (varmap : VarMap.t) (abstract : T.t) =
    Pretty.text
      (prefix
      ^ VarMap.name_of_vid ~vm:varmap ~vid:(fst abstract)
      ^ string_of_int (snd abstract))

  (* produce a Pretty.doc object for a map of type AbstractValue.Map *)
  let pretty_map ~indent ~vm ~map ~prefix ~pretty_value =
    Pretty.indent indent
      (map |> Map.to_alist
      |> List.map ~f:(fun kv ->
             Pretty.concat
               (pretty prefix vm (fst kv))
               (Pretty.concat (Pretty.text " -> ") (pretty_value (snd kv))))
      |> Pretty.docList ~sep:Pretty.line Fun.id ())

  (* produce a Pretty.doc object for a map of type AbstractValue.Map which is 
   * being used to map abstract locations *)
  let pretty_absloc_map ?(indent = 4) vm ~map ~pretty_value =
    pretty_map ~indent ~vm ~map ~prefix:prefix_abs_loc ~pretty_value

  (* produce a Pretty.doc object for a map of type AbstractValue.Map which is 
   * being used to map lifetime variables *)
  let pretty_ltvar_map ?(indent = 4) vm ~map ~pretty_value =
    pretty_map ~indent ~vm ~map ~prefix:prefix_ltvar ~pretty_value

  (* produce a Pretty.doc object for a list of AbstractValues *)
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

  (* produce a Pretty.doc object for a list of AbstractValues being used to
   * represent abstract locations being *)
  let pretty_absloc_list (vm : VarMap.t) (ps : 'a list) =
    pretty_list prefix_abs_loc vm ps
end

(* The may-points-to map, which maps abstract locations to sets of abstract locations *)
module Sigma = struct
  type t = AbstractValue.Set.t AbstractValue.Map.t

  (* Joining two 'sigma' states merges their mappings. If a variable is mapped by each state,
   * then we take the union of the two may-points-to sets.*)
  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        AbstractValue.Set.union av bv)

  (* if we have a mapping l_1 -> {l_2, l_3, l_4}, then l_1 is the pointer location,
   * while l_2-4 are the pointee locations. *)

  (* For a set of pointer locations, get the merged set of locations pointed to by each *)
  let get_points_to (sigma : t) (pointers : AbstractValue.Set.t) :
      AbstractValue.Set.t =
    let pointedTo =
      List.map (AbstractValue.Set.to_list pointers) ~f:(fun abv ->
          let found_value = AbstractValue.Map.find sigma abv in
          match found_value with
          | Some set -> set
          | None -> AbstractValue.Set.empty)
    in
    List.fold pointedTo ~init:AbstractValue.Set.empty ~f:AbstractValue.Set.union

  (* Set each provided pointer location to point to a given set of pointee locations *)
  let set_points_to (sigma : t) (pointers : AbstractValue.Set.t)
      (pointees : AbstractValue.Set.t) : t =
    AbstractValue.Set.fold pointers ~init:sigma ~f:(fun s key ->
        AbstractValue.Map.set s ~key ~data:pointees)

  (* for a given pointer location corresponding to a local variable, get the set of pointee
   * locations at a given level of indirection in sigma *)
  let rec dereference (sigma : t) (initial : AbstractValue.T.t list)
      (ind : indirection) : AbstractValue.Set.t =
    if ind > 0 then
      let dereferenced_once =
        List.fold initial ~init:[] ~f:(fun acc v ->
            let pointing_to = AbstractValue.Map.find sigma v in
            match pointing_to with
            | Some mptsto -> acc @ AbstractValue.Set.to_list mptsto
            | None -> [])
      in
      dereference sigma dereferenced_once (ind - 1)
    else AbstractValue.Set.of_list initial

  (* get the set of pointer locations for a given expression, where a
   * pointee location is pointed to by a pointer location *)
  let rec locations_of_exp ?(ind = 0) (sigma : t) (e : exp) :
      AbstractValue.Set.t =
    match e with
    | AddrOf lva ->
        let adjusted = if ind > 0 then ind - 1 else ind in
        locations_of_lval ~ind:adjusted sigma lva
        (* unary operations consist of logical and bitwise not,
           * as well as decrement. So, we treat these as an equivalence
           * class over the locations for the base expression *)
    | UnOp (_, iex, _) -> locations_of_exp ~ind sigma iex
    | Lval lvl -> locations_of_lval ~ind sigma lvl
    (* when we encounter a conditional, we merge the sets of locations, 
     * from each branch. *)
    | Question (_, tex, fex, _) ->
        AbstractValue.Set.union
          (locations_of_exp ~ind sigma tex)
          (locations_of_exp ~ind sigma fex)
    | _ -> AbstractValue.Set.empty

  (* if an lval corresponds to an expression of the form *x, then it will contain 'Mem', so we
   * increment indirection by one. Else, it's just x, so we find the expressions corresponding to
   * x at the provided level of indirection from prior recursive calls *)
  and locations_of_lval ?(ind = 0) (sigma : t) (lv : lval) : AbstractValue.Set.t
      =
    match lv with
    | lhost, _offset -> (
        match lhost with
        | Var vi ->
            let starting_point =
              [ AbstractValue.gen_local (VarInfo.initialize vi) ]
            in
            dereference sigma starting_point ind
        | Mem ex -> locations_of_exp ~ind:(ind + 1) sigma ex)

  (* The helper function map_pair produces a set of mappings for a list of abstract values.
   * For a variable x:int** with vid = 5, we have the types [int **, int *, int]. If x is a parameter,
   * we use gen_all to produce [(5, 0), (5, 1), (5, 2)]. Then, we use map_pairs to produce the mappings
   * [(5, 0) -> {(5, 1)}, (5, 1) -> {(5, 2)}], which are used to populate a may-points-to map.*)
  let rec map_pairs (tlist : AbstractValue.T.t list) :
      (AbstractValue.T.t * AbstractValue.Set.t) list =
    match tlist with
    | h1 :: h2 :: tl ->
        [ (h1, AbstractValue.Set.add AbstractValue.Set.empty h2) ]
        @ map_pairs ([ h2 ] @ tl)
    | h :: [] -> [ (h, AbstractValue.Set.empty) ]
    | [] -> []

  (* Creates the initial may-points-to map, which contains full mappings for all parameters as generated
   * by may_pairs, and single mappings to empty sets for the locations of local variables.*)
  let initial ~vm : t =
    let local_emptyset abs_local_list =
      List.map abs_local_list ~f:(fun abs -> (abs, AbstractValue.Set.empty))
    in
    AbstractValue.initialize_absloc_map ~vm ~map_to:map_pairs
      ~locals_map_to:local_emptyset

  let pretty ?(indent = 4) ~vm sigma =
    let pretty_value (s : AbstractValue.Set.t) =
      AbstractValue.pretty_absloc_list vm (AbstractValue.Set.to_list s)
    in
    AbstractValue.pretty_absloc_map ~indent vm ~map:sigma ~pretty_value
end

(* The liveness map, which maps abstract locations to their vitality *)
module Chi = struct
  type t = vitality AbstractValue.Map.t

  (* Joining two mappings for an abstract location produces a new map, where
   * its new vitality is the join of its two previous vitalities.*)
  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv -> av * bv)

  (* Each abstract location maps to Alive by default. *)
  let initial ~(vm : VarMap.t) : t =
    let all_to_alive abv_list =
      List.map abv_list ~f:(fun abv -> (abv, Alive))
    in
    AbstractValue.initialize_absloc_map ~vm ~map_to:all_to_alive
      ~locals_map_to:all_to_alive

  let pretty ?(indent = 4) ~vm chi =
    let pretty_value s = Pretty.text (string_of_vitality s) in
    AbstractValue.pretty_absloc_map ~indent vm ~map:chi ~pretty_value
end

(* A mapping from abstract locations to sets of SourceLocations where they were last modified in Sigma *)
(* this is similar enough to Sigma that we could merge the two, or create a functor. *)
module Phi = struct
  type t = Location.Set.t AbstractValue.Map.t

  (* Similar to Sigma, joining two Phi values merges the sets mapped to by abstract location *)
  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        Location.Set.union av bv)

  (* Each abstract location maps to an empty set by default. *)
  let initial ~vm : t =
    let all_to_empty abv_list =
      List.map abv_list ~f:(fun abv -> (abv, Location.Set.empty))
    in
    AbstractValue.initialize_absloc_map ~vm ~map_to:all_to_empty
      ~locals_map_to:all_to_empty

  let pretty ?(indent = 4) ~vm phi =
    let pretty_value s = Location.pretty_list (Location.Set.to_list s) in
    AbstractValue.pretty_absloc_map ~indent vm ~map:phi ~pretty_value
end

module AbstractState = struct
  type t = {
    liveness : Chi.t;
    mayptsto : Sigma.t;
    reassignment : Phi.t;
    variables : VarMap.t;
  }

  let updateSigma (state : t) (newSigma : Sigma.t) =
    {
      liveness = state.liveness;
      mayptsto = newSigma;
      reassignment = state.reassignment;
      variables = state.variables;
    }

  let copy t =
    {
      liveness = t.liveness;
      mayptsto = t.mayptsto;
      reassignment = t.reassignment;
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
    let vm = VarMap.initialize fd in
    {
      variables = vm;
      liveness = Chi.initial ~vm;
      mayptsto = Sigma.initial ~vm;
      reassignment = Phi.initial ~vm;
    }

  let title text doc = Pretty.concat (Pretty.text (text ^ ":\n")) doc

  let pretty state =
    let varmap_pretty = VarMap.pretty state.variables in
    let sigma_pretty = Sigma.pretty ~vm:state.variables state.mayptsto in
    let chi_pretty = Chi.pretty ~vm:state.variables state.liveness in
    let phi_pretty = Phi.pretty ~vm:state.variables state.reassignment in
    Pretty.indent 4
      (Pretty.docList ~sep:(Pretty.text "\n") Fun.id ()
         [
           title "Variables" varmap_pretty;
           title "May-Points-To" sigma_pretty;
           title "Liveness" chi_pretty;
           title "Last Assigned At" phi_pretty;
         ])

  let string_of ~width state = Pretty.sprint ~width (pretty state)
end

module Delta = struct
  type lvar_origin = VarInfo.t

  type t = {
    var_association : AbstractValue.T.t list Int.Map.t;
    lookup : (int * AbstractValue.Set.t) AbstractValue.Map.t;
    relation : Int.Set.t Int.Map.t;
  }

  let empty =
    {
      var_association = Int.Map.empty;
      lookup = AbstractValue.Map.empty;
      relation = Int.Map.empty;
    }

  let equate (_v1 : lvar_origin) (_v2 : lvar_origin) = ()
  let outlives (_v1 : lvar_origin) (_v2 : lvar_origin) = ()
end