open BroCaml.Data
open BroCaml.User
open BroCaml.Login
open Lwt
open Cohttp_lwt_unix
open Sqlite3
open BroCaml.Rating

let current_user : string option ref = ref None
let is_guest : bool ref = ref false

let quit_program () =
  print_endline "Thanks for using FindMyFood!";
  exit 0

exception BindingError of string

let rec prompt_user_find public_db personal_db eateries =
  print_endline "\n Which number best fits your desired action? ";
  print_endline
    "1. Check if <food> is served at any of the eateries (ex. 1 pizza)";
  print_endline "2. Search where <food> is being served (ex. 2 pizza)";
  print_endline "3. Quit";
  let action = read_line () in
  let parts = String.split_on_char ' ' action in
  match parts with
  | [ "1"; food ] ->
      run_contains food eateries;
      prompt_user_find public_db personal_db eateries
  | [ "2"; food ] ->
      run_search_food food eateries;
      prompt_user_find public_db personal_db eateries
  | [ "3" ] -> Lwt.return (quit_program ())
  | _ ->
      print_endline "That action does not exist or is incorrectly formatted.";
      prompt_user_find public_db personal_db eateries

let rec prompt_user_rate public_db personal_db eateries =
  print_endline "\n Which number best fits your desired action? ";
  print_endline "1. Rate <food> offered by <eatery> (ex. 1 pizza Okenshields 5)";
  print_endline
    "2. View the rating of <food> at <eatery> (ex. 2 pizza Okenshields)";

  print_endline "3. View your personal ratings";
  print_endline "4. View all ratings for <food> (ex. 4 pizza)";
  print_endline "5. Quit";
  let action = read_line () in
  let parts = String.split_on_char ' ' action in
  match parts with
  | [ "1"; food; eatery; rating ] -> (
      try
        let rating = int_of_string rating in
        if rating < 1 || rating > 5 then (
          print_endline "Rating must be between 1 and 5.";
          prompt_user_rate public_db personal_db eateries)
        else
          let%lwt () =
            rate_food public_db personal_db food eatery rating is_guest
              current_user eateries
          in
          prompt_user_rate public_db personal_db eateries
      with Failure _ ->
        print_endline "Invalid rating. Please enter a number between 1 and 5.";
        prompt_user_rate public_db personal_db eateries)
  | [ "2"; food; eatery ] ->
      let%lwt () = view_food_rating public_db food eatery eateries in
      prompt_user_rate public_db personal_db eateries
  | [ "3" ] ->
      show_personal_ratings personal_db;
      prompt_user_rate public_db personal_db eateries
  | [ "4"; food ] ->
      show_public_ratings public_db food;
      prompt_user_rate public_db personal_db eateries
  | [ "5" ] -> Lwt.return (quit_program ())
  | _ ->
      print_endline "That action does not exist or is incorrectly formatted.";
      prompt_user_rate public_db personal_db eateries

let rec prompt_user public_db personal_db eateries =
  print_endline "\nPlease choose a number that best fits your desired action:";
  print_endline "1. Find foods";
  print_endline "2. Rate foods";
  print_endline "3. Quit";

  let action = read_line () in
  let parts = String.split_on_char ' ' action in
  match parts with
  | [ "1" ] -> prompt_user_find public_db personal_db eateries
  | [ "2" ] -> prompt_user_rate public_db personal_db eateries
  | [ "3" ] -> Lwt.return (quit_program ())
  | _ ->
      print_endline "That action does not exist or is incorrectly formatted.";
      prompt_user public_db personal_db eateries

let user_entered public_db personal_db =
  let%lwt eateries = get_data () in
  let%lwt () = prompt_user public_db personal_db eateries in
  Lwt.return_unit

(* Prompt the user to log in or create an account *)
let rec login_or_create_account db =
  print_endline "Welcome! Please choose an action:";
  print_endline "1. Log in";
  print_endline "2. Create an account";
  print_endline "3. Proceed as a guest";
  print_endline "4. Quit";
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
        current_user := Some username;
        (* Set the current user *)
        Lwt.return_unit
        (* Exit recursion on success *))
      else (
        print_endline "Invalid username or password. Please try again.\n";
        login_or_create_account db (* Recursive call *))
  | "2" ->
      print_string "Choose a username: ";
      let username = read_line () in
      let%lwt exists = Lwt.return (user_exists db username) in
      if exists then (
        print_endline "This username is already taken. Please choose another.\n";
        login_or_create_account db)
      else (
        print_string "Choose a password: ";
        let password = read_line () in
        (* Create the user asynchronously *)
        let finalize_fn stmt db = ignore (Sqlite3.finalize stmt) in
        Lwt.ignore_result
          (create_user ~finalize:finalize_fn db username password;
           print_endline "Account created successfully!";
           current_user := Some username;
           Lwt.return_unit (* To ensure we're returning a proper Lwt value *));
        Lwt.return_unit)
  (* Set the current user *)
  | "3" ->
      (* Proceed as a guest *)
      print_endline "You are now proceeding as a guest.";
      is_guest := true;
      Lwt.return_unit
  | "4" -> quit_program ()
  | _ ->
      (* Invalid input, prompt again *)
      print_endline "Invalid choice. Please try again.\n";
      login_or_create_account db

(* main *)
let () =
  let public_db_file = "findmyfood.db" in
  let personal_db_file = "personal_ratings.db" in

  let public_db = connect_db_checked public_db_file in
  let personal_db = connect_db_checked personal_db_file in
  Lwt_main.run
    (let%lwt () = login_or_create_account public_db in
     user_entered public_db personal_db);

  db_close public_db |> ignore;
  db_close personal_db |> ignore
