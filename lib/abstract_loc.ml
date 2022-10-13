open Core
open GoblintCil
open Cil_wrappers
open Helpers

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

module AbstractLoc = struct
  (* an abstract location can correspond to a local variable 
   * at a given level of indirection, or memory allocated by a given statement.*)

  module T = struct
    type t = vid * indirection [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let prefix_abs_loc = "l_"

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
  let pretty (varmap : VarMap.t) (abstract : t) =
    Pretty.text
      (VarMap.name_of_vid ~vm:varmap ~vid:(fst abstract)
      ^ string_of_int (snd abstract))

  (* produce a Pretty.doc object for a map of type AbstractLoc.Map *)
  let pretty_map ~indent ~vm ~map ~pretty_value =
    let pretty_key k = pretty vm k in
    PrettyExtensions.pretty_map ~indent ~map ~pretty_key ~pretty_value

  (* produce a Pretty.doc object for a list of AbstractLocs *)
  let pretty_list (vm : VarMap.t) (ps : 'a list) =
    let pretty_closure v = pretty vm v in
    PrettyExtensions.pretty_list ps pretty_closure
end
