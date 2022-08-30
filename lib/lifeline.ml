module Config = struct
  type dump_type = CIL | CFG
  let dump_type_str = function
    | CIL -> "cil"
    | CFG -> "cfg"

  let (=) (dt1:dump_type) (dt2:dump_type) = String.equal (dump_type_str dt1) (dump_type_str dt2)

  let match_dump_type (dt:(dump_type option)) (desired:dump_type) = match dt with
  | Some t -> (t = desired)
  | None -> true

  type config_t = {
    dump_type: (dump_type option); 
    dump_loc: (string option); 
    output: (string option);
    verbose: bool
  }
  let analysis_config dt dl op v = {dump_type = dt; dump_loc = dl; output = op; verbose = v}
end

let analyze (_config: Config.config_t) (_file:GoblintCil.file) = ()


