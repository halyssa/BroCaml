open User
open Cohttp_lwt_unix

type eatery = {
  name : string;
  menu : string list;
}

let fetch_json url =
  try%lwt
    let%lwt response, body = Client.get (Uri.of_string url) in
    let%lwt body_string = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_string in
    Lwt.return json
  with e ->
    Lwt.fail_with
      (Printf.sprintf "Unexpected error: %s" (Printexc.to_string e))
    [@coverage off]
(* coverage off because branch will not be reached since url is always valid *)

let parse_eateries (json : Yojson.Safe.t) : User.eatery list Lwt.t =
  try
    match
      json
      |> Yojson.Safe.Util.member "data"
      |> Yojson.Safe.Util.member "eateries"
    with
    | `Null -> Lwt.return []
    | eateries_json ->
        let eateries =
          eateries_json |> Yojson.Safe.Util.to_list
          |> List.map (fun eatery ->
                 let name =
                   eatery
                   |> Yojson.Safe.Util.member "name"
                   |> Yojson.Safe.Util.to_string
                 in
                 let menu_items =
                   let dining_items =
                     match eatery |> Yojson.Safe.Util.member "diningItems" with
                     | `Null -> []
                     | items_json ->
                         items_json |> Yojson.Safe.Util.to_list
                         |> List.map (fun item ->
                                item
                                |> Yojson.Safe.Util.member "item"
                                |> Yojson.Safe.Util.to_string)
                   in
                   let operating_hours =
                     match
                       eatery |> Yojson.Safe.Util.member "operatingHours"
                     with
                     | `Null -> []
                     | hours_json -> hours_json |> Yojson.Safe.Util.to_list
                   in
                   let menu_from_events =
                     operating_hours
                     |> List.fold_left
                          (fun acc hour ->
                            match hour |> Yojson.Safe.Util.member "events" with
                            | `Null -> acc
                            | events_json ->
                                let events_list =
                                  events_json |> Yojson.Safe.Util.to_list
                                in
                                acc
                                @ (events_list
                                  |> List.fold_left
                                       (fun acc event ->
                                         match
                                           event
                                           |> Yojson.Safe.Util.member "menu"
                                         with
                                         | `Null -> acc
                                         | menu_json ->
                                             let menu_list =
                                               menu_json
                                               |> Yojson.Safe.Util.to_list
                                             in
                                             acc
                                             @ (menu_list
                                               |> List.fold_left
                                                    (fun acc category ->
                                                      match
                                                        category
                                                        |> Yojson.Safe.Util
                                                           .member "items"
                                                      with
                                                      | `Null -> acc
                                                      | items_json ->
                                                          let items_list =
                                                            items_json
                                                            |> Yojson.Safe.Util
                                                               .to_list
                                                          in
                                                          acc
                                                          @ (items_list
                                                            |> List.map
                                                                 (fun item ->
                                                                   item
                                                                   |> Yojson
                                                                      .Safe
                                                                      .Util
                                                                      .member
                                                                        "item"
                                                                   |> Yojson
                                                                      .Safe
                                                                      .Util
                                                                      .to_string)
                                                            ))
                                                    []))
                                       []))
                          []
                   in
                   if List.length menu_from_events > 0 then menu_from_events
                   else if List.length dining_items > 0 then dining_items
                   else
                     match
                       eatery |> Yojson.Safe.Util.member "diningCuisines"
                     with
                     | `Null -> []
                     | cuisines_json ->
                         cuisines_json |> Yojson.Safe.Util.to_list
                         |> List.map (fun cuisine ->
                                cuisine
                                |> Yojson.Safe.Util.member "name"
                                |> Yojson.Safe.Util.to_string)
                 in
                 let safe_menu =
                   if List.length menu_items = 0 then [ "No menu available" ]
                   else menu_items
                 in
                 User.create_eatery name safe_menu)
        in
        Lwt.return eateries
  with e ->
    print_endline ("Unexpected error in parse_eateries: " ^ Printexc.to_string e);
    Lwt.return []

let get_data () : User.eatery list Lwt.t =
  let url = "https://now.dining.cornell.edu/api/1.0/dining/eateries.json" in
  try%lwt
    let%lwt json = fetch_json url in
    let%lwt eateries = parse_eateries json in
    Lwt.return eateries
  with Failure msg ->
    print_endline (Printf.sprintf "Unexpected error in get_data: %s" msg);
    Lwt.return []
