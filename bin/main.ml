open Cil
module E = Errormsg 

let main () = 
  let f = Frontc.parse Sys.argv.(1) () in
  let d_list = Pretty.d_list "\n" d_global in 
  E.log "%a" (d_list) f.globals;
  ()

let _ = main ()