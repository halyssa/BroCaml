type eatery = {
  name : string;
  menu : string list;
}

let get_name eatery = eatery.name
let get_menu eatery = eatery.menu

(* creates a new eatery *)
let create_eatery name menu =
  if String.trim name = "" then
    raise (Invalid_argument "Eatery name cannot be empty.")
  else if List.length menu = 0 then
    raise (Invalid_argument "Menu cannot be empty.")
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

let run_search_food food eateries =
  let result = search_food food eateries in
  if result = [] then
    Printf.printf "Unfortunately, %s is not served in the eateries today. " food
  else List.iter (fun p -> Printf.printf "%s\n" p) result

let run_contains food eateries =
  let result = contains food eateries in
  match result with
  | true -> run_search_food food eateries
  | false ->
      Printf.printf "Unfortunately, %s is not served in the eateries today. "
        food

let eatery_exists name eateries =
  List.exists
    (fun eatery ->
      String.lowercase_ascii eatery.name = String.lowercase_ascii name)
    eateries
