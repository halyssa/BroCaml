val rate_food :
  Sqlite3.db ->
  Sqlite3.db ->
  string ->
  string ->
  int ->
  bool ref ->
  string ->
  bool ->
  User.eatery list ->
  unit Lwt.t
(** [rate_food public_db personal_db food eatery rating is_guest current_user is_anon eateries]
    adds a rating for [food] at [eatery] to [public_db] and [personal_db] if not
    a guest. *)

val view_food_rating :
  Sqlite3.db -> string -> string -> User.eatery list -> unit Lwt.t
(** [view_food_rating public_db food eatery eateries] displays ratings for
    [food] at [eatery] from [public_db]. *)

val show_personal_ratings : Sqlite3.db -> string -> bool ref -> unit Lwt.t
(** [show_personal_ratings db username is_guest] displays personal ratings from
    [db] for [username] if not a guest. *)

val show_public_ratings : Sqlite3.db -> Sqlite3.header -> Sqlite3.header -> unit
(** [show_public_ratings db username is_guest] displays public ratings from
    [db].*)

val sort_by_highest_rating : Sqlite3.db -> string -> unit Lwt.t
(** [sort_by_highest_rating db table] sorts and displays ratings from [table] in
    [db] by highest rating. *)

val sort_by_lowest_rating : Sqlite3.db -> string -> unit Lwt.t
(** [sort_by_lowest_rating db table] sorts and displays ratings from [table] in
    [db] by lowest rating. *)

val sort_by_eatery_alphabetical : Sqlite3.db -> string -> unit Lwt.t
(** [sort_by_eatery_alphabetical db table] sorts and displays ratings from
    [table] in [db] by eatery name in alphabetical order. *)

val sort_by_eatery_reverse_alphabetical : Sqlite3.db -> string -> unit Lwt.t
(** [sort_by_eatery_reverse_alphabetical db table] sorts and displays ratings
    from [table] in [db] by eatery name in reverse alphabetical order. *)

val sort_by_food_alphabetical : Sqlite3.db -> string -> unit Lwt.t
(** [sort_by_food_alphabetical db table] sorts and displays ratings from [table]
    in [db] by food name in alphabetical order. *)

val sort_by_food_reverse_alphabetical : Sqlite3.db -> string -> unit Lwt.t
(** [sort_by_food_reverse_alphabetical db table] sorts and displays ratings from
    [table] in [db] by food name in reverse alphabetical order. *)

val sort_by_date_asc : Sqlite3.db -> string -> unit Lwt.t
(** [sort_by_date_asc db table] sorts and displays ratings from [table] in [db]
    by date in ascending order. *)

val sort_by_date_desc : Sqlite3.db -> string -> unit Lwt.t
(** [sort_by_date_desc db table] sorts and displays ratings from [table] in [db]
    by date in descending order. *)
