type t
val prop : string -> Arithm.t -> Arithm.t -> t
val logic : string -> t -> t -> t
val forall : string -> t -> t
val substitute : (string -> string option) -> t -> t
val free_variables : t -> string list
val ( => ) : t -> t -> t
val ( == ) : Arithm.t -> Arithm.t -> t 
val ( >= ) : Arithm.t -> Arithm.t -> t 
val to_smt : out_channel -> t -> unit
val is_valid : t -> bool
type order = Smaller | Greater | Equivalent | Incomparable
val gen_compare :
  t -> (string -> string option) -> (string -> string option) -> order

