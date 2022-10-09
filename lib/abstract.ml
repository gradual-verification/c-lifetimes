open Core
open Cil_wrappers
open GoblintCil
open Helpers

(* liveness values. Alive indicates valid memory, 
 * Dead is invalid memory, and Zombie is the join.*)
type vitality = Alive | Zombie | Dead [@@deriving equal]

(* CIL assigns a unique integer ID to each variable in the, 
 * struct 'varinfo', which has the field 'vid' *)
type vid = int [@@deriving sexp, compare]

(* We extend 'varinfo' by unrolling a variable's type into a list,
 * where each element of the list is the type at each level of
 * indirection. For abstract values that correspond to variables,
 * we use an 'indirection' field for type lookup. 
 * struct 'varinfo', which has the field 'vid' *)
type indirection = int [@@deriving sexp, compare]

(* CIL assigns a unique integer ID to each statement using the, 
 * struct 'stmt', which has the field 'sid' *)
type sid = int [@@deriving sexp, compare]

(* The definition of join for liveness values. *)
let join_vitality (a : vitality) (b : vitality) =
  if equal_vitality a b then a else Zombie

let string_of_vitality l =
  match l with Alive -> "Alive" | Dead -> "Dead" | Zombie -> "Zombie"

type abstract_kind = LTVar | AbsLoc [@@deriving sexp, compare, equal]

let string_of_abstract_value_kind l =
  match l with LTVar -> "'" | AbsLoc -> "l_"

module AbstractLocation = struct
  type t = LTVar
end

(* We use a single struct, AbstractValue, to represent both abstract locations
 * and lifetime variables. *)
module AbstractValue = struct
  (* an abstract location can correspond to a local variable 
   * at a given level of indirection, or memory allocated by a given statement.*)

  (* TODO: refactor t to use the abstract_value type above *)

  module T = struct
    type t = abstract_kind * vid * indirection [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let prefix_abs_loc = "l_"

  (* Given a variable, we can choose to generate abstract values for each level of indirection
   * in its type, or just the first level. For lifetime variables, which are assigned to each
   *  type, we use gen_all. However, for abstract locations, we only use gen_all for parameters,
   *  which are initialized on entry to a function, while we use gen_local for local variables,
   *  which are uninitialized. *)
  let gen_local (vi : VarInfo.t) : T.t = (AbsLoc, vi.vinfo.vid, 0)

  let gen_all (vi : VarInfo.t) : T.t list =
    List.mapi vi.unrolled_type ~f:(fun i _ -> (AbsLoc, vi.vinfo.vid, i))

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
  let pretty (varmap : VarMap.t) (abstract : t) =
    let prefix = string_of_abstract_value_kind (fst3 abstract) in
    Pretty.text
      (prefix
      ^ VarMap.name_of_vid ~vm:varmap ~vid:(snd3 abstract)
      ^ string_of_int (trd3 abstract))

  (* produce a Pretty.doc object for a map of type AbstractValue.Map *)
  let pretty_map ~indent ~vm ~map ~pretty_value =
    let pretty_key k = pretty vm k in
    PrettyExtensions.pretty_map ~indent ~map ~pretty_key ~pretty_value

  (* produce a Pretty.doc object for a list of AbstractValues *)
  let pretty_list (vm : VarMap.t) (ps : 'a list) =
    let pretty_closure v = pretty vm v in
    PrettyExtensions.pretty_list ps pretty_closure
end

module type Prettyable = sig
  type t

  val pretty : t -> VarMap.t -> Pretty.doc
end

module type LatticeState = sig
  type v
  type t

  val join : t -> t -> t
  val initial : VarMap.t -> t
  val pretty : ?indent:int -> t -> VarMap.t -> Pretty.doc
end

module type LatticeDefn = sig
  type v

  val join : v -> v -> v
end

module Lattice (V : LatticeDefn) = struct
  type t = V.v AbstractValue.Map.t

  let join (a : t) (b : t) =
    AbstractValue.Map.merge_skewed a b ~combine:(fun ~key:_ av bv ->
        V.join av bv)

  let initial ~(vm : VarMap.t)
      ~(locals : AbstractValue.t list -> (AbstractValue.t * 'a) list)
      ~(param : AbstractValue.t list -> (AbstractValue.t * 'a) list) : t =
    AbstractValue.initialize_absloc_map ~vm ~map_to:param ~locals_map_to:locals

  let pretty ?(indent = 4) ~vm (map : t)
      (pretty_value : V.v -> VarMap.t -> Pretty.doc) =
    AbstractValue.pretty_map ~indent ~vm ~map ~pretty_value:(fun v ->
        pretty_value v vm)
end

module AssignedAt : sig
  include LatticeState
  val update: t -> AbstractValue.Set.t -> SourceLocation.t -> t
end = struct
  module V = struct
    type v = SourceLocation.Set.t

    let join = SourceLocation.Set.union
  end

  include V
  module L = Lattice (V)
  include L

  let pretty ?(indent = 4) s vm =
    let pretty_value (locs : SourceLocation.Set.t) (_vm : VarMap.t) =
      PrettyExtensions.pretty_list
        (SourceLocation.Set.to_list locs)
        SourceLocation.pretty
    in
    L.pretty ~indent ~vm s pretty_value

  let initial vm =
    let to_empty abv_list =
      List.map abv_list ~f:(fun abv -> (abv, SourceLocation.Set.empty))
    in
    L.initial ~vm ~locals:to_empty ~param:to_empty

    let update prev abslocs loc =
      AbstractValue.Set.fold abslocs ~init:prev ~f:(
      
      fun m key -> let prevSet = AbstractValue.Map.find prev key in 
      let toUpdate = (match prevSet with
          | Some(set) -> set 
          | None -> SourceLocation.Set.empty) 
      in let updated = SourceLocation.Set.add toUpdate loc in
          AbstractValue.Map.set m ~key ~data:updated)
end

module MayPtsTo : sig
  include LatticeState
  val get_points_to : t -> AbstractValue.Set.t -> AbstractValue.Set.t
  val set_points_to : t -> AbstractValue.Set.t -> AbstractValue.Set.t -> t
  val dereference : t -> AbstractValue.Set.t -> indirection -> AbstractValue.Set.t
  val locations_of_exp : ?indir:int -> t -> exp -> AbstractValue.Set.t
  val locations_of_lval : ?indir:int -> t -> lval -> AbstractValue.Set.t
end = struct
  (* The may-points-to map, which maps abstract locations to sets of abstract locations *)
  module V = struct
    type v = AbstractValue.Set.t

    let join = AbstractValue.Set.union
  end

  include V
  module L = Lattice (V)
  include L

  let pretty ?(indent = 4) s vm =
    let pretty_abv abv = AbstractValue.pretty vm abv in
    let pretty_abv_set (locs : AbstractValue.Set.t) (_vm : VarMap.t) =
      PrettyExtensions.pretty_list (AbstractValue.Set.to_list locs) pretty_abv
    in
    L.pretty ~indent ~vm s pretty_abv_set

  (* The helper function map_pair produces a set of mappings for a list of abstract values.
     * For a variable x:int** with vid = 5, we have the types [int **, int *, int]. If x is a parameter,
     * we use gen_all to produce [(5, 0), (5, 1), (5, 2)]. Then, we use map_pairs to produce the mappings
     * [(5, 0) -> {(5, 1)}, (5, 1) -> {(5, 2)}], which are used to populate a may-points-to map.*)
  let rec map_pairs (tlist : AbstractValue.t list) :
      (AbstractValue.t * AbstractValue.Set.t) list =
    match tlist with
    | h1 :: h2 :: tl ->
        [ (h1, AbstractValue.Set.add AbstractValue.Set.empty h2) ]
        @ map_pairs ([ h2 ] @ tl)
    | h :: [] -> [ (h, AbstractValue.Set.empty) ]
    | [] -> []

  let initial vm =
    let initial_local abv_list =
      List.map abv_list ~f:(fun abv -> (abv, AbstractValue.Set.empty))
    in
    L.initial ~vm ~locals:initial_local ~param:map_pairs

  (* if we have a mapping l_1 -> {l_2, l_3, l_4}, then l_1 is the pointer location,
   * while l_2-4 are the pointee locations. *)

  (* For a set of pointer locations, get the merged set of locations pointed to by each *)
  let get_points_to (mayptsto : t) (pointers : AbstractValue.Set.t) =
    let pointedTo =
      List.map (AbstractValue.Set.to_list pointers) ~f:(fun abv ->
          let found_value = AbstractValue.Map.find mayptsto abv in
          match found_value with
          | Some set -> set
          | None -> AbstractValue.Set.empty)
    in
    List.fold pointedTo ~init:AbstractValue.Set.empty ~f:AbstractValue.Set.union

  (* Set each provided pointer location to point to a given set of pointee locations *)
  let set_points_to (mayptsto : t) (pointers : AbstractValue.Set.t)
      (pointees : AbstractValue.Set.t) : t =
    AbstractValue.Set.fold pointers ~init:mayptsto ~f:(fun s key ->
        AbstractValue.Map.set s ~key ~data:pointees)

  (* for a given pointer location corresponding to a local variable, get the set of pointee
   * locations at a given level of indirection in mayptsto *)
  let rec dereference (mayptsto : t) (initial : AbstractValue.Set.t)
      (indir : indirection) : AbstractValue.Set.t =
    if indir > 0 then
      let dereferenced_once =
        Set.fold initial ~init:AbstractValue.Set.empty ~f:(fun acc v ->
            let pointing_to = Map.find mayptsto v in
            match pointing_to with
            | Some mptsto -> AbstractValue.Set.union acc mptsto
            | None -> AbstractValue.Set.empty)
      in
      dereference mayptsto dereferenced_once (indir - 1)
    else initial

  (* get the set of pointer locations for a given expression, where a
   * pointee location is pointed to by a pointer location *)
  let rec locations_of_exp ?(indir = 0) (mayptsto : t) (e : exp) :
      AbstractValue.Set.t =
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
        AbstractValue.Set.union
          (locations_of_exp ~indir mayptsto tex)
          (locations_of_exp ~indir mayptsto fex)
    | _ -> AbstractValue.Set.empty

  (* if an lval corresponds to an expression of the form *x, then it will contain 'Mem', so we
   * increment indirection by one. Else, it's just x, so we find the expressions corresponding to
   * x at the provided level of indirection from prior recursive calls *)
  and locations_of_lval ?(indir = 0) (mayptsto : t) (lv : lval) :
      AbstractValue.Set.t =
    match lv with
    | lhost, _offset -> (
        match lhost with
        | Var vi ->
            let starting_point =
              AbstractValue.Set.of_list
                [ AbstractValue.gen_local (VarInfo.initialize vi) ]
            in
            dereference mayptsto starting_point indir
        | Mem ex -> locations_of_exp ~indir:(indir + 1) mayptsto ex)
end

(* The liveness map, which maps abstract locations to their vitality *)
module Liveness : sig
  include LatticeState
  val kill :
  t -> AbstractValue.Set.t -> t
end = struct
  module V = struct
    type v = vitality

    let join = join_vitality
  end

  include V
  module L = Lattice (V)
  include L

  let pretty ?(indent = 4) s vm =
    let pretty_value (v : vitality) (_vm : VarMap.t) =
      Pretty.text (string_of_vitality v)
    in
    L.pretty ~indent ~vm s pretty_value

  let initial vm =
    let to_alive abv_list = List.map abv_list ~f:(fun abv -> (abv, Alive)) in
    L.initial ~vm ~locals:to_alive ~param:to_alive

  let kill liveness locs =
    AbstractValue.Set.fold locs ~init:liveness ~f:(fun m key ->
        AbstractValue.Map.set m ~key ~data:Dead)
end

module AbstractState = struct
  type t = {
    liveness : Liveness.t;
    mayptsto : MayPtsTo.t;
    reassignment : AssignedAt.t;
    variables : VarMap.t;
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
      liveness = Liveness.join t1.liveness t2.liveness;
      mayptsto = MayPtsTo.join t1.mayptsto t2.mayptsto;
      reassignment = AssignedAt.join t1.reassignment t2.reassignment;
    }

  let initial fd =
    let vm = VarMap.initialize fd in
    {
      variables = vm;
      liveness = Liveness.initial vm;
      mayptsto = MayPtsTo.initial vm;
      reassignment = AssignedAt.initial vm;
    }

  let title text doc = Pretty.concat (Pretty.text (text ^ ":\n")) doc

  let pretty state =
    let varmap_pretty = VarMap.pretty state.variables in
    let mayptsto_pretty = MayPtsTo.pretty state.mayptsto state.variables in
    let liveness_pretty = Liveness.pretty state.liveness state.variables in
    let assignedat_pretty = AssignedAt.pretty state.reassignment state.variables in
    Pretty.indent 4
      (Pretty.docList ~sep:(Pretty.text "\n") Fun.id ()
         [
           title "Variables" varmap_pretty;
           title "May-Points-To" mayptsto_pretty;
           title "Liveness" liveness_pretty;
           title "Last Assigned At" assignedat_pretty;
         ])

  let string_of ~width state = Pretty.sprint ~width (pretty state)
end

module Delta = struct
  type lvar_origin = VarInfo.t

  type t = {
    var_association : AbstractValue.t list Int.Map.t;
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