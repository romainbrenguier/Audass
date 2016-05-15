module type Filtering = 
sig
  type t
  val filter : t -> Filter.t 
end

module PositiveIntFiltering : Filtering with type t = int

module Make :
  functor (F : Filtering) ->
sig
  type elt = F.t
  type t
  val empty : t
  val singleton : elt -> t
  val add : elt -> t -> t
  val elements : t -> elt list
  val filter : (string -> int option) -> t -> t
  val to_string : (elt -> string) -> t -> unit
end
