open User
open Cohttp_lwt_unix

type eatery = {
  name : string;
  menu : string list;
}
(** type [eatery] stores a string [name] and string list [menu]*)

(** [fetch_json] is the json of [url]. Raises:
    - Fails with "HTTP request failed with error" if the HTTP request is
      unsuccessful.
    - Fails with "JSON parsing error" if the response body cannot be parsed as
      JSON.
    - Fails with "Unexpected error: <msg>" if an unexpected error occurs during
      the request or parsing.*)
let fetch_json url =
  try%lwt
    let%lwt response, body = Client.get (Uri.of_string url) in
    let%lwt body_string = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_string in
    print_endline "Successfully fetched and parsed JSON.";
    Lwt.return json
  with
  | Failure _ -> Lwt.fail_with "HTTP request failed with error"
  | Yojson.Json_error e -> Lwt.fail_with "JSON parsing error"
  | e ->
      Lwt.fail_with
        (Printf.sprintf "Unexpected error: %s" (Printexc.to_string e))

(** [parse_eateries] parses the given JSON object [json] and returns an Lwt list
    of eateries. *)
let parse_eateries json =
  let open Yojson.Safe.Util in
  try
    match json |> member "data" |> member "eateries" with
    | `Null -> Lwt.return [] (* Return an empty list if "eateries" is null *)
    | eateries_json ->
        let eateries =
          eateries_json |> to_list
          |> List.map (fun eatery ->
                 let name = eatery |> member "name" |> to_string in
                 let menu_items =
                   let dining_items =
                     match eatery |> member "diningItems" with
                     | `Null -> [] (* Handle case where diningItems is null *)
                     | items_json -> items_json |> to_list
                   in
                   let operating_hours =
                     match eatery |> member "operatingHours" with
                     | `Null ->
                         [] (* Handle case where operatingHours is null *)
                     | hours_json -> hours_json |> to_list
                   in
                   (* Collect all menu items from operating hours and their
                      events *)
                   let menu_from_events =
                     operating_hours
                     |> List.fold_left
                          (fun acc hour ->
                            match hour |> member "events" with
                            | `Null -> acc
                            | events_json ->
                                let events_list = events_json |> to_list in
                                (* Extract items from each event's menu *)
                                acc
                                @ (events_list
                                  |> List.fold_left
                                       (fun acc event ->
                                         match event |> member "menu" with
                                         | `Null -> acc
                                         | menu_json ->
                                             let menu_list =
                                               menu_json |> to_list
                                             in
                                             (* Extract items from each
                                                category's items in the menu *)
                                             acc
                                             @ (menu_list
                                               |> List.fold_left
                                                    (fun acc category ->
                                                      match
                                                        category
                                                        |> member "items"
                                                      with
                                                      | `Null -> acc
                                                      | items_json ->
                                                          let items_list =
                                                            items_json
                                                            |> to_list
                                                          in
                                                          acc
                                                          @ (items_list
                                                            |> List.map
                                                                 (fun item ->
                                                                   item
                                                                   |> member
                                                                        "item"
                                                                   |> to_string)
                                                            ))
                                                    []))
                                       []))
                          []
                   in
                   if List.length menu_from_events > 0 then menu_from_events
                   else if List.length dining_items > 0 then
                     dining_items
                     |> List.map (fun item ->
                            item |> member "item" |> to_string)
                   else [] (* Return an empty list if both are empty *)
                 in
                 { name; menu = menu_items })
        in
        Lwt.return eateries
  with e ->
    print_endline ("Unexpected error in parse_eateries: " ^ Printexc.to_string e);
    Lwt.return []

(** [get_data] returns a Lwt list of eateries from
    "https://now.dining.cornell.edu/api/1.0/dining/eateries.json" *)
let get_data () : eatery list Lwt.t =
  let url = "https://now.dining.cornell.edu/api/1.0/dining/eateries.json" in
  try%lwt
    let%lwt json = fetch_json url in
    let%lwt eateries = parse_eateries json in
    Lwt.return eateries
  with
  | Failure msg when msg = "HTTP request failed with error" ->
      print_endline "Error: Could not fetch data from the dining API.";
      Lwt.return []
  | Failure msg when msg = "JSON parsing error" ->
      print_endline
        "Error: Failed to parse the JSON response from the dining API.";
      Lwt.return []
  | Failure msg ->
      print_endline (Printf.sprintf "Unexpected error in get_data: %s" msg);
      Lwt.return []
