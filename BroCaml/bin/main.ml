open BroCaml.Data
open BroCaml.User
open Lwt
open Cohttp_lwt_unix
open Sqlite3


let run_search_food food eateries =
  let result = search_food food eateries in
  if result = [] then
    Printf.printf "Unfortunately, %s is not served in the eateries today. " food
  else List.iter (fun p -> Printf.printf "%s\n" p) result

let run_contains food eateries =
  let result = contains food eateries in
  match result with
  | true ->
      Printf.printf
        "%s is served in the eateries today! Would you like to see where it is \
         served (y/n)? "
        food;
      let response = read_line () in
      if response = "y" then run_search_food food eateries
  | false ->
      Printf.printf "Unfortunately, %s is not served in the eateries today. "
        food

let quit_program () =
  print_endline "Thanks for using FindMyFood!";
  exit 0

let rec prompt_user (eateries : eatery list) =
  print_endline "Please choose a number that best fits your desired action";
  print_endline "1. Check if a <food> is served at any of the eateries (ex. 1 pizza)";
  (* print_endline "2. Check if a <food> is served at a specific <eatery>"; *)
  print_endline "2. Search where a <food> is being served (ex. 2 pizza)";
  print_endline "3. Quit";
  let action = read_line () in
  let parts = String.split_on_char ' ' action in
  match parts with
  | [ "1"; food ] ->
      run_contains food eateries;
      prompt_user eateries
  (* | ["2"; food; eatery] -> contains_helper food (create_eatery eatery); *)
  | [ "2"; food ] ->
      run_search_food food eateries;
      prompt_user eateries
  | [ "3" ] -> quit_program ()
  | _ ->
      print_endline
        "That action does not exist or is incorrectly formatted.";
      prompt_user eateries

let main () =
  print_endline "Hey! Welcome to FindMyFood";
  let%lwt eateries = get_data () in
  let () = prompt_user eateries in
  Lwt.return_unit

(* let () = Lwt_main.run (main ()) *)



(* Connect to the database *)
(* let connect_db db_file = db_open db_file *)

(* Define a custom exception for database errors *)
exception BindingError of string

(* Create a new user with enhanced error handling *)
let create_user db username password =
  let query = "INSERT INTO Users (username, password_hash) VALUES (?, ?);" in
  let stmt = prepare db query in
  (* Bind the username *)
  (match bind_text stmt 1 username with
   | Rc.OK -> ()
   | Rc.ERROR -> raise (BindingError ("Error binding username: " ^ errmsg db))
   | Rc.MISUSE -> raise (BindingError "SQLite MISUSE detected while binding username")
   | _ -> raise (BindingError "Unexpected SQLite result while binding username"));
  (* Bind the password *)
  (match bind_text stmt 2 password with
   | Rc.OK -> ()
   | Rc.ERROR -> raise (BindingError ("Error binding password: " ^ errmsg db))
   | Rc.MISUSE -> raise (BindingError "SQLite MISUSE detected while binding password")
   | _ -> raise (BindingError "Unexpected SQLite result while binding password"));
  (* Execute the statement *)
  (match step stmt with
   | Rc.DONE -> print_endline "User created successfully!"
   | Rc.ERROR -> raise (BindingError ("Error inserting user: " ^ errmsg db))
   | Rc.MISUSE -> raise (BindingError "SQLite MISUSE detected during step")
   | _ -> raise (BindingError "Unexpected SQLite result during step"));
  (* Finalize the statement to clean up resources *)
  finalize stmt |> ignore


(* Fetch and print all users *)
(* Fetch and print all users, handling all possible step results *)
(* Simplified fetch_users for debugging *)
let fetch_users db =
  let query = "SELECT id, username FROM Users;" in
  print_endline "Debug: Preparing query...";
  
  (* Prepare the statement with error handling *)
  let stmt =
    try
      prepare db query
    with
    | Sqlite3.Error msg ->
        failwith ("Failed to prepare statement: " ^ msg)
  in

  print_endline "Debug: Statement prepared successfully.";

  (* Process rows *)
  let rec process_rows () =
    print_endline "Debug: Executing step...";
    let result = step stmt in
    Printf.printf "Debug: Step result -> %s\n" (Rc.to_string result);

    match result with
    | Rc.ROW ->
        (* Extract and print row data *)
        let id = column stmt 0 |> Data.to_string |> Option.value ~default:"NULL" in
        let username = column stmt 1 |> Data.to_string |> Option.value ~default:"NULL" in
        Printf.printf "ID: %s, Username: %s\n" id username;
        process_rows ()
    | Rc.DONE ->
        print_endline "Debug: All rows processed successfully."
    | Rc.ERROR ->
        print_endline ("Debug: Rc.ERROR returned. " ^ errmsg db);
        raise (Failure ("Error during fetch: " ^ errmsg db))
    | Rc.BUSY ->
        print_endline "Debug: Rc.BUSY returned.";
        raise (Failure "Database is busy. Try again later.")
    | Rc.MISUSE ->
        print_endline "Debug: Rc.MISUSE returned.";
        raise (Failure "SQLite MISUSE detected during fetch.")
    | other ->
        Printf.printf "Debug: Unexpected Rc -> %s\n" (Rc.to_string other);
        raise (Failure ("Unexpected SQLite result during step: " ^ Rc.to_string other))
  in

  (* Start processing rows *)
  process_rows ();

  (* Finalize statement *)
  finalize stmt |> ignore;
  db_close db |> ignore

  let connect_db_checked db_file =
    if not (Sys.file_exists db_file) then
      failwith ("Database file not found: " ^ db_file);
    db_open db_file




(* Main function *)
let () =
  let db_file = "findmyfood.db" in
  let db = connect_db_checked db_file in
  print_endline ("Debug: Using database file -> " ^ db_file);
  (* Add a test user *)
  create_user db "testuser" "hashedpassword123";
  (* Fetch and print users *)
  fetch_users db;
  db_close db |> ignore