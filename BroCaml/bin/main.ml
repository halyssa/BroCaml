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
         served (y/n)? \n"
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
  print_endline "\n Please choose a number that best fits your desired action";
  print_endline
    "1. Check if a <food> is served at any of the eateries (ex. 1 pizza)";
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
      print_endline "That action does not exist or is incorrectly formatted.";
      prompt_user eateries

let user_entered () =
  let%lwt eateries = get_data () in
  let () = prompt_user eateries in
  Lwt.return_unit

(* let () = Lwt_main.run (main ()) *)

(* Connect to the database *)
(* let connect_db db_file = db_open db_file *)

(* Define a custom exception for database errors *)
exception BindingError of string

(* Check if a user exists in the database *)
let user_exists db username =
  let query = "SELECT COUNT(*) FROM Users WHERE username = ?;" in
  let stmt = prepare db query in
  bind_text stmt 1 username |> ignore;
  match step stmt with
  | Rc.ROW ->
      let count = column stmt 0 |> Data.to_int |> Option.value ~default:0 in
      finalize stmt |> ignore;
      count > 0
  | _ ->
      finalize stmt |> ignore;
      raise (BindingError "Failed to check if user exists.")

(* Validate a user's credentials (username and password) *)
let validate_user db username password =
  let%lwt result =
    (* Example: Check the database asynchronously *)
    let query = "SELECT password_hash FROM Users WHERE username = ?;" in
    let stmt = prepare db query in
    bind_text stmt 1 username |> ignore;
    match step stmt with
    | Rc.ROW ->
        let stored_hash = column stmt 0 |> Data.to_string |> Option.get in
        Lwt.return (stored_hash = password)
        (* Compare password *)
    | Rc.DONE -> Lwt.return false (* Username not found *)
    | _ -> Lwt.fail_with "Database error during user validation"
  in
  Lwt.return result

let finalize_statement stmt db =
  match finalize stmt with
  | Rc.OK -> () (* Finalize succeeded *)
  | Rc.ERROR -> failwith ("Failed to finalize statement: " ^ errmsg db)
  | Rc.MISUSE -> failwith "SQLite MISUSE detected during finalize."
  | other ->
      failwith ("Unexpected result during finalize: " ^ Rc.to_string other)
(* Create a new user in the database *)

let create_user db username password : unit =
  let query = "INSERT INTO Users (username, password_hash) VALUES (?, ?);" in
  let stmt = prepare db query in
  try
    (* Bind the username *)
    bind_text stmt 1 username |> ignore;
    (* Bind the password *)
    bind_text stmt 2 password |> ignore;
    (* Execute the statement *)
    match step stmt with
    | Rc.DONE -> print_endline "User created successfully!"
    | Rc.ERROR -> raise (BindingError ("Error creating user: " ^ errmsg db))
    | _ -> raise (BindingError "Unexpected result during user creation")
    (* Finalize the statement (always clean up, even if an error occurs) *)
  with exn ->
    (* Ensure finalization in case of an exception *)
    finalize_statement stmt db;
    raise exn

(* Fetch and print all users *)
let fetch_users db =
  let query = "SELECT id, username FROM Users;" in
  let stmt = prepare db query in
  try
    print_endline "Users in the database:";
    while step stmt = Rc.ROW do
      let id =
        column stmt 0 |> Data.to_string |> Option.value ~default:"NULL"
      in
      let username =
        column stmt 1 |> Data.to_string |> Option.value ~default:"NULL"
      in
      Printf.printf "ID: %s, Username: %s\n" id username
    done;
    finalize_statement stmt db
  with exn ->
    (* Ensure finalization in case of an exception *)
    finalize_statement stmt db;
    raise exn

(* Prompt the user to log in or create an account *)
let rec login_or_create_account db =
  print_endline "Welcome! Please choose an action:";
  print_endline "1. Log in";
  print_endline "2. Create an account";
  print_endline "3. Quit";
  let choice = read_line () in
  match choice with
  | "1" ->
      (* Login flow *)
      print_string "Enter username: ";
      let username = read_line () in
      print_string "Enter password: ";
      let password = read_line () in
      if%lwt validate_user db username password then (
        Printf.printf "Welcome back, %s!\n" username;
        Lwt.return_unit (* Exit recursion on success *))
      else (
        print_endline "Invalid username or password. Please try again. \n";
        login_or_create_account db (* Recursive call *))
  | "2" ->
      (* Account creation flow *)
      print_string "Choose a username: ";
      let username = read_line () in
      if%lwt Lwt.return (user_exists db username) then (
        print_endline
          "This username is already taken. Please choose another. \n";
        login_or_create_account db)
      else (
        print_string "Choose a password: ";
        let password = read_line () in
        Lwt.return (create_user db username password))
  | "3" -> quit_program ()
  | _ ->
      (* Invalid input, prompt again *)
      print_endline "Invalid choice. Please try again. \n";
      login_or_create_account db

let connect_db_checked db_file =
  if not (Sys.file_exists db_file) then
    failwith ("Database file not found: " ^ db_file);
  db_open db_file

(* main *)
let () =
  let db_file = "findmyfood.db" in
  let db = connect_db_checked db_file in
  print_endline ("Debug: Using database file -> " ^ db_file);

  (* Run Lwt computation *)
  Lwt_main.run
    (let%lwt () = login_or_create_account db in
     (* Fetch and print all users (optional for debugging) *)
     (* let%lwt () = Lwt.return (fetch_users db) in *)
     Lwt.return_unit);

  (* Close the database *)
  db_close db |> ignore
