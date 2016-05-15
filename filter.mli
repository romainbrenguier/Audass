type t
val prop : string -> Arithm.t -> Arithm.t -> t
val logic : string -> t -> t -> t
val forall : string -> t -> t
val substitute : (string -> int option) -> t -> t
val free_variables : t -> string list
val ( => ) : t -> t -> t
val ( == ) : Arithm.t -> Arithm.t -> t 
val ( >= ) : Arithm.t -> Arithm.t -> t 
val to_smt : out_channel -> t -> unit
val is_valid : t -> bool

exception QuantifiedFormula
(** optimazed is_valid function for unquantified function *)
val eval : t -> bool

type order = Smaller | Greater | Equivalent | Incomparable
(** term is greater if it is more often true *)
val gen_compare : ('a -> t) -> 'a -> 'a -> order

