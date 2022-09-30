open Core
open GoblintCil

let isConst attrs = hasAttribute "const" attrs

type indirection = typ * bool list

let rec compute_indirection t =
  match t with
  | TPtr (inner, attrs) ->
      let result = compute_indirection inner in
      (fst result, snd result @ [ isConst attrs ])
  | TVoid attrs -> (t, [ isConst attrs ])
  | TInt (_, attrs) -> (t, [ isConst attrs ])
  | TFloat (_, attrs) -> (t, [ isConst attrs ])
  | TArray (_, _, attrs) -> (t, [ isConst attrs ])
  | TFun (_, _, _, attrs) -> (t, [ isConst attrs ])
  | TNamed (_, attrs) -> (t, [ isConst attrs ])
  | TComp (_, attrs) -> (t, [ isConst attrs ])
  | TEnum (_, attrs) -> (t, [ isConst attrs ])
  | TBuiltin_va_list attrs -> (t, [ isConst attrs ])

module VarInfo = struct
  type t = { vinfo : varinfo; btype : typ; clookup : bool list }

  let initialize vi =
    let indirection = compute_indirection vi.vtype in
    match indirection with t, bl -> { vinfo = vi; btype = t; clookup = bl }
end

module VarMap = struct
  type varmap = VarInfo.t Int.Map.t
  type t = { locals : varmap; parameters : varmap }

  let info_of_vid ~vm ~vid =
    let local = Int.Map.find vm.locals vid in
    match local with
    | Some info -> info
    | None -> Int.Map.find_exn vm.parameters vid

  let name_of_vid ~vm ~vid = (info_of_vid ~vm ~vid).vinfo.vname
  let type_of_vid ~vm ~vid = (info_of_vid ~vm ~vid).vinfo.vtype
  let loc_of_vid ~vm ~vid = (info_of_vid ~vm ~vid).vinfo.vdecl

  let initialize_varmap (vl : varinfo list) =
    List.fold_left vl ~init:Int.Map.empty ~f:(fun m vi ->
        Int.Map.add_exn m ~key:vi.vid ~data:(VarInfo.initialize vi))

  let initialize fd =
    {
      locals = initialize_varmap fd.slocals;
      parameters = initialize_varmap fd.sformals;
    }
end
