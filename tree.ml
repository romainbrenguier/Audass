module type Filtering =
sig 
  type t
  val filter : t -> Filter.t
end

(* Example of a filtering *)
module PositiveIntFiltering =
struct
  open Arithm
  open Filter
  type t = int
  let filter x = (cst x) >= (cst 0)
end

(* Makes a filtering into a tree structure *)
module Make(F:Filtering) =
struct

  type elt = F.t

  type t =
    { value: F.t option; 
      smaller: t option; 
      greater: t option; 
      incomparable: t option; 
      equivalent: t option}
      
  let empty = {value=None; smaller=None; greater=None; 
	       incomparable=None; equivalent=None }

  let leaf v = {empty with value=Some v }
  let singleton = leaf

    
  let rec add v tree = 
    let open Filter in
    match tree.value with
    | None -> leaf v
    | Some value ->
      match gen_compare F.filter value v with
      | Smaller -> 
	(match tree.smaller with 
	| None -> {tree with smaller = Some (leaf v)} 
	| Some x -> {tree with smaller = Some (add v x)})
      | Greater -> 
	(match tree.greater with
	| None -> {tree with greater = Some (leaf v)} 
	| Some x -> {tree with greater = Some (add v x)})
      | Incomparable -> 
	(match tree.incomparable with
	| None -> {tree with incomparable = Some (leaf v)} 
	| Some x -> {tree with incomparable = Some (add v x)})
      | Equivalent -> 
	(match tree.equivalent with
	| None -> {tree with equivalent = Some (leaf v) } 
	| Some x -> {tree with equivalent = Some (add v x)})

  let elements tree = 
    let rec add accu = function 
      | None -> accu
      | Some t -> aux accu t
    and aux accu t = match t.value with 
      | None -> accu
      | Some value -> 
	List.fold_left add (value::accu) [t.smaller;t.greater;t.incomparable;t.equivalent]
    in aux [] tree
	

  (* warning: very inefficient way to do it *)
  let union a b = 
    List.fold_left (fun set x -> add x set) a (elements b)

  let to_string ts =
    let rec opt_aux t = match t with 
      | None -> ()
      | Some t -> aux t
    and aux t = match t.value with
      | None -> Printf.printf "empty"
      | Some value -> 
	Printf.printf "{ val : %s\n[\n" (ts value);
	Printf.printf "< : "; opt_aux t.smaller;
	Printf.printf "> : "; opt_aux t.greater;
	Printf.printf "~ : "; opt_aux t.equivalent;
	Printf.printf "|| : "; opt_aux t.incomparable;
	Printf.printf "\n]\n}\n"
    in aux 

  let filter substitution m = 
    let rec opt_aux = function
      | None -> None
      | Some x -> Some (aux x)
    and aux t = match t.value with
      | None -> empty
      | Some value -> 
	if Filter.is_valid (Filter.substitute substitution (F.filter value))
	then 
	  {t with greater = opt_aux t.greater; incomparable = opt_aux t.incomparable}
	else
	  match t.incomparable, t.smaller with
	  | None, None -> empty
	  | Some a, None | None, Some a -> a
	  | Some a, Some b ->
	    union (aux a) (aux b)
	  
    in aux m


end
  
