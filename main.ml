(*let test_value (ax,ay) (bx,by) = 
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
*)  

module Set = Tree.Make(Tree.PositiveIntFiltering)

let test = 
  let rec loop accu = 
    print_endline "int?";
    let res = 
      try 
	let x = read_int () in
	Some (Set.add x accu)
      with
      | End_of_file | Failure _ -> None
    in 
    match res with Some a -> loop a
    | None ->
      List.iter (fun x -> Printf.printf "%d; " x) (Set.elements accu);
      Printf.printf "Tree:\n";
      Set.to_string string_of_int accu;
      Printf.printf "Filtered:\n";
      List.iter (fun x -> Printf.printf "%d; " x) (Set.elements (Set.filter (fun x -> None) accu));
      Printf.printf "Exiting.\n"
  in loop Set.empty
  
(*kprint_endline "a.x";
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
  | Incomparable -> print_endline "incomparable"*)
  

