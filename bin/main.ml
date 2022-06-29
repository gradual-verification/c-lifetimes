open Cil
module E = Errormsg 

let main () = 
  let f = Frontc.parse Sys.argv.(1) () in
  let d_globals = Pretty.d_list "\n" d_global in 
  E.log "%a" (d_globals) f.globals;
  ()

let _ = main ()