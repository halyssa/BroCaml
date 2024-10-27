(* open Lwt open Cohttp_lwt_unix open Yojson.Safe *)

type eatery = {
  name : string;
  menu : string list;
}

let create_eatery name menu =
  if String.trim name = "" then failwith "Eatery name cannot be empty."
  else if List.length menu = 0 then failwith "Menu cannot be empty."
  else { name; menu }

(** [contains food eateries] returns true if any eatery in [eateries] has [food]
    in its menu, false otherwise. *)
let contains (food : string) (eateries : eatery list) : bool =
  List.exists
    (fun eatery ->
      List.exists
        (fun item -> String.lowercase_ascii item = String.lowercase_ascii food)
        eatery.menu)
    eateries
