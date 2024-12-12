type eatery = {
  name : string;
  menu : string list;
}

let get_name eatery = eatery.name
let get_menu eatery = eatery.menu

let create_eatery name menu =
  if String.trim name = "" then
    raise (Invalid_argument "Eatery name cannot be empty.")
  else if List.length menu = 0 then
    raise (Invalid_argument "Menu cannot be empty.")
  else { name; menu }

let contains_helper food eatery =
  List.exists
    (fun item -> String.lowercase_ascii item = String.lowercase_ascii food)
    eatery.menu

let contains (food : string) (eateries : eatery list) : bool =
  List.exists (fun eatery -> contains_helper food eatery) eateries

let search_food food eateries =
  List.fold_right
    (fun eatery acc ->
      if contains_helper food eatery then eatery.name :: acc else acc)
    eateries []

let eatery_exists name eateries =
  List.exists
    (fun eatery ->
      String.lowercase_ascii eatery.name = String.lowercase_ascii name)
    eateries
