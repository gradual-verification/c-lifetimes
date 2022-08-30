open Cmdliner
open Compilation
open Lifeline.Config

let config_options = 
  let dump_loc = (
    let doc = "Dump intermediate files to the specified directory." in
    Arg.(value & opt (some file) None & info ["d"; "dump"] ~docv:"DUMP" ~doc))
  in
  let output = (
    let doc = "Output file for analysis results." in
    Arg.(value & opt (some file) None & info ["o"; "output"] ~doc))
  in let verbose = (
    let doc = "Increase verbosity." in
    Arg.(value & flag & info ["v"; "verbose"] ~doc))
  in let chosen_dump_type = (
    let doc = "Chose the type of intermediate file to dump (cfg | cil)" in
      let dt = Arg.enum [dump_type_str CIL, CIL; dump_type_str CFG, CFG] in
        Arg.(value & opt (some dt) ~vopt:(Some CFG) None & info ["dt"; "dump-type"] ~docv:"DUMP" ~doc))
  in
  Term.(const analysis_config $ chosen_dump_type $ dump_loc $ output $ verbose)

let files = Arg.(value & pos_all file [] & info [] ~docv:"FILE")

let cmd =
  let info = Cmd.info "lifeline" ~version: (match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v) in
  Cmd.v info Term.(const run_analysis $ config_options $ files)

let main () = exit (Cmd.eval cmd)
let () = main ()