type t
val cst : int -> t
val var : string -> t
val arithm : string -> t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( % ) : t -> t -> t
exception UnsubstitedVariables
val eval : t -> int
val to_smt : out_channel -> t -> unit
val substitute : (string -> int option) -> t -> t
val free_variables : t -> string list
