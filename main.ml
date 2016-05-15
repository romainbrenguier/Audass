let test_value (ax,ay) (bx,by) = 
  let open Arithm in
  let open Filter in
  let f = (var "x") >= (var "y") * (var "a") + (var "b") in
  let a v = 
    if v = "x" then Some (string_of_int ax)
    else if v = "y" then Some (string_of_int ay) else None
  in
  let b v = 
    if v = "x" then Some (string_of_int bx) 
    else if v = "y" then Some (string_of_int by) else None 
  in
  (f,a,b)
    
let test = 
  try 
    while true 
    do
      print_endline "a.x";
      let ax = read_int () in
      print_endline "a.y";
      let ay = read_int () in
      print_endline "b.x";
      let bx = read_int () in
      print_endline "b.y";
      let by = read_int () in
      let f,a,b = test_value (ax,ay) (bx,by) in
      match Filter.gen_compare f a b with
      | Smaller -> print_endline "smaller"
      | Greater -> print_endline "smaller"
      | Equivalent -> print_endline "equivalent"
      | Incomparable -> print_endline "incomparable"

    done;
  with
  | End_of_file
  | Failure _ -> Printf.printf "Exiting.\n"
