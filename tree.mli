module type Filtering =
sig
  type t
  val filter : Filter.t
  val field : t -> string -> int option 
end

(** This is an example of a very simple filtering. 
    It distinguishes between positive and negative integers *)
module PositiveIntFiltering : Filtering

module Make :
  functor (F : Filtering) ->
sig
  type t
  val leaf : F.t -> t
  val insert : t -> F.t -> t
end
