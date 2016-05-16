let display_debug = ref false
let display_log = ref false
let display_warning = ref false

let start_time = Unix.gettimeofday ()

let time () =
  (* let t = Unix.times () in 
  Printf.printf "[%.3fs, %.3fs]" t.tms_utime t.tms_stime*)
  let t = Unix.gettimeofday () -. start_time in
  Printf.printf "[%.3fs]" t

let warning string =
  if !display_warning 
  then ( time (); Printf.printf " Warning: %s\n" string)

let debug string =
  if !display_debug 
  then ( time (); Printf.printf " Debug: %s\n" string)

let log string = 
  if !display_log 
  then ( time (); Printf.printf " Log: %s\n" string )
