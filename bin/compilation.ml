open GoblintCil
open GoblintCil.Cil
open Lifeline.Config
open Core

let create_dump_location config files = match config.dump_loc with
  | Some loc -> (if(List.length files > 1) then (Core_unix.mkdir_p loc)); files
  | None -> files

let resolve_to_dir optional_dir existing_path = match optional_dir with
| Some dir -> Some(String.concat ~sep:Filename.dir_sep [dir; (Filename.basename existing_path)])
| None -> None


let create_io_pairs config sources =
  match sources with
  | _::_ -> List.map sources ~f:(fun s -> resolve_to_dir config.dump_loc s, s)
  | [] ->  [(config.dump_loc, 
      (let temp_dest = Filename_unix.temp_file "" ".c" in
        Core.Out_channel.write_all 
          temp_dest ~data:(Core.In_channel.input_all Core.In_channel.stdin); 
        temp_dest
        ))]

let dump_cil dest file ext = 
  let dest_w_ext = (String.concat [dest; ext]) in
  (dumpFile defaultCilPrinter (Out_channel.create (dest_w_ext)) (Filename.basename dest_w_ext) file)

let compile_cil config io_pairs = List.map io_pairs ~f:(fun p -> 
  let pair = (fst p, Frontc.parse (snd p) ()) in
    (if(match_dump_type config.dump_type CIL) then
      match fst pair with 
      | Some dest -> dump_cil dest (snd pair) ".cil" | None -> ()
    ); pair 
  )

let compile_cfg config pairs = 
  let add_cfg = fun f -> (Cfg.computeFileCFG f); f in
  let compiled = add_cfg (Mergecil.merge(List.map pairs ~f:snd) "merged") in
    (
      if(phys_same config.dump_type CFG) then
        match config.dump_loc with
        | Some dest -> dump_cil dest compiled ".cfg.cil" | None -> ()
      ); compiled

let run_analysis (config: config_t) (files : string list) = create_dump_location config files 
  |> create_io_pairs config 
  |> compile_cil config 
  |> compile_cfg config 
  |> Lifeline.analyze config
