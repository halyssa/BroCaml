(** abstract type eatery to represent eateries*)
type eatery
(** [search_food food] is a list of eateries that is serving [food] for that day*)

val create_eatery : string -> string list -> eatery
(** [create_eatery] creates an eatery *)

val contains : string -> eatery list -> bool
(** [contains] checks if a food is in the eateries *)

(* val search_food : string -> eatery list *)

(* val rate_food : string -> int -> string list *)

(* val get_data : unit -> eatery list Lwt.t *)

(* val parse_eateries : Yojson.Safe.t -> { name : string; menu : string list
   } *)
