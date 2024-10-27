(* open Lwt open Cohttp_lwt_unix open Yojson.Safe *)

type eatery = {
  name : string;
  menu : string list;
}

(* creates a new eatery *)
let create_eatery name menu =
  if String.trim name = "" then failwith "Eatery name cannot be empty."
  else if List.length menu = 0 then failwith "Menu cannot be empty."
  else { name; menu }

(* helper for contains: checks if a food item is in an eatery*)
let contains_helper food eatery =
  List.exists
    (fun item -> String.lowercase_ascii item = String.lowercase_ascii food)
    eatery.menu

(* checks if food is found in any of the eateries *)
let contains (food : string) (eateries : eatery list) : bool =
  List.exists (fun eatery -> contains_helper food eatery) eateries

(* search and return eateries with the desired food*)
let search_food food eateries =
  List.fold_right
    (fun eatery acc ->
      if contains_helper food eatery then eatery.name :: acc else acc)
    eateries []
