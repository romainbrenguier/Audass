module type Filtering =
sig 
  type t
  val filter : Filter.t
  val field : t -> string -> int option
end

(* Example of a filtering *)
module PositiveIntFiltering =
struct
  open Arithm
  open Filter
  type t = int
  let filter = (var "x") >= (cst 0)
  let field i s = if s = "x" then Some i else None    
end

(* Makes a filtering into a tree structure *)
module Make(F:Filtering) =
struct
  type t =
    { value: F.t; 
      smaller: t option; 
      greater: t option; 
      incomparable: t option; 
      equivalent: t option}
      
  let leaf v = {value=v; smaller=None; greater=None; 
		incomparable=None; equivalent=None }
    
  let rec insert tree v = 
    let open Filter in
    match gen_compare F.filter (F.field tree.value) (F.field v) with
    | Smaller -> 
      (match tree.smaller with 
      | None -> {tree with smaller = Some (leaf v)} 
      | Some x -> {tree with smaller = Some (insert x v)})
    | Greater -> 
      (match tree.greater with
      | None -> {tree with greater = Some (leaf v)} 
      | Some x -> {tree with greater = Some (insert x v)})
    | Incomparable -> 
      (match tree.incomparable with
      | None -> {tree with incomparable = Some (leaf v)} 
      | Some x -> {tree with incomparable = Some (insert x v)})
    | Equivalent -> 
      (match tree.equivalent with
      | None -> {tree with equivalent = Some (leaf v) } 
      | Some x -> {tree with equivalent = Some (insert x v)})
	
end
  
