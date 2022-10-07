open GoblintCil
open Core

module PrettyExtensions = struct
  let pretty_list (ps : 'a list) (pretty: 'a -> Pretty.doc) =
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

  let pretty_map ~indent ~map ~pretty_key ~pretty_value =
    Pretty.indent indent
      (map |> Map.to_alist
      |> List.map ~f:(fun kv ->
              Pretty.concat
                (pretty_key (fst kv))
                (Pretty.concat (Pretty.text " -> ") (pretty_value (snd kv))))
      |> Pretty.docList ~sep:Pretty.line Fun.id ())
end