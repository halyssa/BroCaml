open Sqlite3

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
  Lwt.return
    (let query = "SELECT password_hash FROM Users WHERE username = ?;" in
     let stmt = prepare db query in
     bind_text stmt 1 username |> ignore;
     match step stmt with
     | Rc.ROW ->
         let stored_hash = column stmt 0 |> Data.to_string |> Option.get in
         finalize stmt |> ignore;
         stored_hash = password
     | Rc.DONE ->
         finalize stmt |> ignore;
         false
     | _ ->
         finalize stmt |> ignore;
         raise (BindingError "Database error during user validation."))

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
    finalize_statement stmt db;
    raise exn

let connect_db_checked db_file =
  if not (Sys.file_exists db_file) then
    failwith ("Database file not found: " ^ db_file);
  db_open db_file
