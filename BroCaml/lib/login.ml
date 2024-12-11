open Sqlite3

exception BindingError of string

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

(** [finalize_statement] is a function to clean up SQL statement after
    execution. *)
let finalize_statement ~finalize stmt db =
  match finalize stmt db with
  | Sqlite3.Rc.OK -> ()
  | Sqlite3.Rc.ERROR ->
      failwith ("Failed to finalize statement: " ^ Sqlite3.errmsg db)
  | Sqlite3.Rc.MISUSE -> failwith "SQLite MISUSE detected during finalize."
  | other ->
      failwith
        ("Unexpected result during finalize: " ^ Sqlite3.Rc.to_string other)

let create_user ~finalize db username password =
  let query = "INSERT INTO Users (username, password_hash) VALUES (?, ?);" in
  let stmt = Sqlite3.prepare db query in
  try
    Sqlite3.bind_text stmt 1 username |> ignore;
    Sqlite3.bind_text stmt 2 password |> ignore;
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> print_endline "User created successfully!"
    | Sqlite3.Rc.ERROR ->
        raise (Failure ("Error creating user: " ^ Sqlite3.errmsg db))
    | _ -> raise (Failure "Unexpected result during user creation")
  with exn ->
    finalize stmt db;
    raise exn

let fetch_users ~finalize db =
  let query = "SELECT id, username FROM Users;" in
  let stmt = prepare db query in
  try
    print_endline "Users in the database:";
    while step stmt = Sqlite3.Rc.ROW do
      let id =
        column stmt 0 |> Data.to_string |> Option.value ~default:"NULL"
      in
      let username =
        column stmt 1 |> Data.to_string |> Option.value ~default:"NULL"
      in
      Printf.printf "ID: %s, Username: %s\n" id username
    done;
    finalize_statement ~finalize stmt db (* Pass finalize function *)
  with exn ->
    finalize_statement ~finalize stmt db;
    (* Pass finalize function *)
    raise exn

let connect_db_checked db_file =
  if not (Sys.file_exists db_file) then
    failwith ("Database file not found: " ^ db_file);
  db_open db_file
