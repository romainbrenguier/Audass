type t = 
| Prop of (string * Arithm.t * Arithm.t) 
| Logic of (string * t * t)
| Bool of bool
| Forall of (string * t)

let prop s a b = Prop (s,a,b)
let logic s a b = Logic (s,a,b)
let forall s a = Forall (s,a)

let rec substitute map = function 
  | Prop (s,a,b) -> prop s (Arithm.substitute map a) (Arithm.substitute map b)
  | Logic (s,a,b) -> logic s (substitute map a) (substitute map b)
  | Bool b -> Bool b
  | Forall (s,a) -> forall s (substitute map a)

let rec free_variables = function 
  | Bool b -> []
  | Forall (s,x) -> free_variables x
  | Logic (p,a,b) -> List.merge compare (free_variables a) (free_variables b)
  | Prop (s,a,b) ->  List.merge compare (Arithm.free_variables a) (Arithm.free_variables b)

let ( => ) = logic "=>"
let ( == ) = prop "=="
let ( >= ) = prop ">="

let rec to_smt buf = function
  | Bool b -> Printf.fprintf buf "%b" b
  | Prop (s,a,b) -> Printf.fprintf buf "(%s %a %a)" s Arithm.to_smt a Arithm.to_smt b
  | Logic (s,a,b) -> Printf.fprintf buf "(%s %a %a)" s to_smt a to_smt b
  | Forall (s,a) -> Printf.fprintf buf "(forall ((%s Real)) %a)" s to_smt a

    
(* remove successive equal values in a list *)
let remove_duplicates l = 
  let rec aux accu = function
  | [] -> accu | [a] -> a :: accu
  | a :: b :: tl when a = b -> aux accu (b :: tl)
  | a :: tl -> aux (a :: accu) tl
  in List.rev (aux [] l)
  

let implication f a b =
  (* let s1 = substitute a f in*)
  (* let s2 = substitute b f in*)
  let s1, s2 = f a, f b in
  let free = 
    List.merge compare (free_variables s1) (free_variables s2) 
  |> remove_duplicates
  in
  let p = 
    List.fold_left
      (fun accu x -> forall x accu)
      (logic "=>" s1 s2) free
  in
  p
  

let is_valid formula = 
  let tmp = open_out "selector_tmp.smt" in
  Printf.fprintf tmp "(assert %a)\n(check-sat)\n; sat\n" to_smt formula;
  close_out tmp;
  let inch = Unix.open_process_in "z3 -smt2 selector_tmp.smt" in
  let res = input_line inch in
  if res = "sat" then true
  else if res = "unsat" then false
  else failwith (Printf.sprintf "unknown result: %s\n" res)


type order = Smaller | Greater | Equivalent | Incomparable

let gen_compare f a b =
  match is_valid (implication f a b),(is_valid (implication f b a)) with
  | true, true -> Equivalent
  | false, true -> Greater
  | true, false -> Smaller
  | false, false -> Incomparable


