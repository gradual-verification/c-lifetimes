open Core
open GoblintCil
open Cil

module Type = struct
  type t = typ

  let rec unroll (t1 : typ) : typ list =
    match t1 with TPtr (t, _) -> [ t ] @ unroll t | _ -> [ t1 ]

  let compare t1 t2 =
    let sig1 = typeSig t1 in
    let sig2 = typeSig t2 in
    Poly.compare sig1 sig2
end

module VarInfo = struct
  type t = { vinfo : varinfo; unrolled_type : typ list }

  let initialize vi = { vinfo = vi; unrolled_type = Type.unroll vi.vtype }

  let pretty vi =
    Pretty.concat
      (Pretty.text (vi.vinfo.vname ^ ":"))
      (printType Cil.defaultCilPrinter () vi.vinfo.vtype)
end

module VarMap = struct
  type varmap = VarInfo.t Int.Map.t
  type t = { locals : varmap; parameters : varmap }

  let local_data (vm : t) = Int.Map.data vm.locals
  let parameter_data (vm : t) = Int.Map.data vm.parameters

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

  let pretty ?(indent = 4) vm =
    Pretty.indent indent
      ((vm.locals |> Int.Map.to_alist) @ (vm.parameters |> Int.Map.to_alist)
      |> List.map ~f:(fun kv ->
             Pretty.concat
               (Pretty.text (string_of_int (fst kv) ^ " -> "))
               (VarInfo.pretty (snd kv)))
      |> Pretty.docList ~sep:Pretty.line Fun.id ())

  let string_of ~width ~vm = Pretty.sprint ~width (pretty vm)

  let initialize fd =
    {
      locals = initialize_varmap fd.slocals;
      parameters = initialize_varmap fd.sformals;
    }
end

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

  let pretty (t : T.t) : Pretty.doc =
    Pretty.text
      (t.file ^ ":" ^ string_of_int t.line ^ ":" ^ string_of_int t.column)

  let pretty_list (ps : 'a list) =
    let rec concatenate ls =
      match ls with
      | h :: [] -> pretty h
      | h :: tl ->
          Pretty.concat
            (Pretty.concat (pretty h) (Pretty.text ","))
            (concatenate tl)
      | [] -> Pretty.nil
    in
    Pretty.concat (Pretty.text "{")
      (Pretty.concat (concatenate ps) (Pretty.text "}"))

  let string_of ~width loc = Pretty.sprint ~width (pretty loc)

  include Comparable.Make (T)
end