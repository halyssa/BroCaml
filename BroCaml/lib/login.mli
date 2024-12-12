exception BindingError of string
(* [BindingError] is raised when there is an error binding values to SQL
   statements. *)

val user_exists : Sqlite3.db -> string -> bool
(** [user_exists db username] checks if a user [username] exists in the database
    [db]. *)

val validate_user : Sqlite3.db -> string -> string -> bool Lwt.t
(** [validate_user db username password] validates the user's credentials using
    [username] and [password] in the database [db] *)

val create_user :
  finalize:(Sqlite3.stmt -> Sqlite3.db -> unit) ->
  Sqlite3.db ->
  string ->
  string ->
  unit
(** [create_user ~finalize db username password] creates a new user with
    username [username] and password [password] in the database [db]. *)

val connect_db_checked : string -> Sqlite3.db
(** [connect_db_checked db_file] opens a connection to the database file
    [db_file] *)
