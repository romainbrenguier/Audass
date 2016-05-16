module Point = 
struct
  type t = {x : int; y : int }
  let filter p =
    let open Arithm in
    let open Filter in
    (cst p.x) >= (cst p.y) * (var "a") + (var "b")
  let compare = compare

end


module PointSet = Tree.Make(Point)
module BasicPointSet = Set.Make(Point)
module ToTest = BasicPointSet

let test = 
  Log.display_debug := true;
  print_endline "a";
  let a = read_int () in
  print_endline "b";
  let b = read_int () in

  let rec loop1 accu = 
    let res = 
      try 
	print_endline "x";
	let x = read_int () in
	print_endline "y";
	let y = read_int () in
	Some (ToTest.add {Point.x=x;Point.y=y} accu)
      with
      | End_of_file | Failure _ -> None
    in 
    match res with Some a -> loop1 a
    | None ->
      let point_to_string p = Printf.sprintf "%d , %d; " p.Point.x p.Point.y in
      let print_point p = print_endline (point_to_string p) in
      List.iter print_point (ToTest.elements accu);
      Printf.printf "Tree:\n";
      (* ToTest.to_string point_to_string accu;*)
      Printf.printf "Filtered:\n";
      Log.time ();
      let filtered = ToTest.filter 
	(fun p -> p.x >= p.y * a + b) 
	(* (fun s -> if s = "a" then Some a else if s = "b" then Some b else None) *)
	accu 
      in
      Log.time ();
      ToTest.elements filtered |> List.iter print_point;
      Printf.printf "Exiting.\n"
  in


  loop1 ToTest.empty
  
  

