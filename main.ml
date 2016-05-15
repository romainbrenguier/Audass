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
  in
  ()
  (* loop Set.empty *)


module Point = 
struct
  type t = {x : int; y : int }
  let filter p =
    let open Arithm in
    let open Filter in
    (cst p.x) >= (cst p.y) * (var "a") + (var "b")
end


module PointSet = Tree.Make(Point)

let test = 

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
	Some (PointSet.add {Point.x=x;Point.y=y} accu)
      with
      | End_of_file | Failure _ -> None
    in 
    match res with Some a -> loop1 a
    | None ->
      let point_to_string p = Printf.sprintf "%d , %d; " p.Point.x p.Point.y in
      let print_point p = print_endline (point_to_string p) in
      List.iter print_point (PointSet.elements accu);
      Printf.printf "Tree:\n";
      PointSet.to_string point_to_string accu;
      Printf.printf "Filtered:\n";
      List.iter print_point (PointSet.elements (PointSet.filter (fun s -> if s = "a" then Some a else if s = "b" then Some b else None) accu));
      Printf.printf "Exiting.\n"
  in


  loop1 PointSet.empty
  
  

