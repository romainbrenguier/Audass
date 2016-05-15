type t
val cst : string -> t
val var : string -> t
val arithm : string -> t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( % ) : t -> t -> t
val to_smt : out_channel -> t -> unit
val substitute : (string -> string option) -> t -> t
val free_variables : t -> string list
