(** abstract type eatery to represent eateries*)
type eatery
(** [search_food food] is a list of eateries that is serving [food] for that day*)

val create_eatery : string -> string list -> eatery
(** [create_eatery] creates an eatery *)

val contains_helper : string -> eatery -> bool
(** [contains_helper] checks if a food is in one specific eatery *)

val contains : string -> eatery list -> bool
(** [contains] checks if a food is in the eateries *)

val search_food : string -> eatery list -> string list
(** [search_food] returns a list of eateries that contains the desired food and
    returns and empty list if nothing is found. *)

val run_contains : string -> eatery list -> unit
(** [run_contains] runs the [contains] function. *)

val run_search_food : string -> eatery list -> unit
(** [run_serach_food] runs the [search_food] function]. *)
