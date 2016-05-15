type t = Const of string | Var of string | Arithm of (string * t * t)
    
let cst s = Const s
let var s = Var s
let arithm s a b = Arithm (s,a,b)
let (+) a b = arithm "+" a b
let (-) a b = arithm "-" a b
let ( * ) a b = arithm "*" a b
let (/) a b = arithm "/" a b
let (%) a b = arithm "%" a b
  
let rec to_smt buf = function
  | Const s | Var s -> output_string buf s
  | Arithm (s,a,b) -> Printf.fprintf buf "(%s %a %a)" s to_smt a to_smt b
    
let rec substitute map = function
  | Const s -> Const s
  | Var s -> (match map s with Some x -> Const x | None -> Var s)
  | Arithm (s,a,b) -> Arithm (s,substitute map a, substitute map b)
    
let rec free_variables = function 
  | Const s -> []
  | Var s -> [s]
  | Arithm (s,a,b) -> List.merge compare (free_variables a) (free_variables b)

