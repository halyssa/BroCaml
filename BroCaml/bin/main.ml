open BroCaml.Data
open BroCaml.User
open BroCaml.Login
open Lwt
open Cohttp_lwt_unix
open Sqlite3

let current_user : string option ref = ref None

let quit_program () =
  print_endline "Thanks for using FindMyFood!";
  exit 0

let rate_food public_db personal_db food eatery rating =
  match !current_user with
  | None ->
      Lwt.return
        (print_endline
           "Error: No user logged in. Please log in first to submit ratings.")
  | Some username ->
      print_string "Would you like to submit your rating anonymously? (y/n): ";
      let anon = read_line () in
      let public_username = if anon = "y" then "anonymous" else username in

      (* Function to insert into the public database *)
      let insert_public () =
        let query =
          "INSERT INTO Ratings (eatery_name, food_item, username, rating)\n\
          \   VALUES (?, ?, ?, ?);"
        in
        let stmt = Sqlite3.prepare public_db query in
        Lwt.finalize
          (fun () ->
            Sqlite3.bind_text stmt 1 eatery |> ignore;
            Sqlite3.bind_text stmt 2 food |> ignore;
            Sqlite3.bind_text stmt 3 public_username |> ignore;
            Sqlite3.bind_int stmt 4 rating |> ignore;

            match Sqlite3.step stmt with
            | Sqlite3.Rc.DONE ->
                Lwt.return
                  (print_endline "Rating submitted to the public database!")
            | Sqlite3.Rc.ERROR ->
                Lwt.return
                  (print_endline
                     ("Error submitting public rating: "
                    ^ Sqlite3.errmsg public_db))
            | _ ->
                Lwt.return
                  (print_endline
                     "Unexpected error during public rating submission."))
          (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))
      in

      (* Function to insert into the personal database *)
      let insert_personal () =
        let query =
          "INSERT INTO PersonalRatings (eatery_name, food_item, rating)\n\
          \   VALUES (?, ?, ?);"
        in
        let stmt = Sqlite3.prepare personal_db query in
        Lwt.finalize
          (fun () ->
            Sqlite3.bind_text stmt 1 eatery |> ignore;
            Sqlite3.bind_text stmt 2 food |> ignore;
            Sqlite3.bind_int stmt 3 rating |> ignore;

            match Sqlite3.step stmt with
            | Sqlite3.Rc.DONE ->
                Lwt.return
                  (print_endline "Rating submitted to your personal database!")
            | Sqlite3.Rc.ERROR ->
                Lwt.return
                  (print_endline
                     ("Error submitting personal rating: "
                    ^ Sqlite3.errmsg personal_db))
            | _ ->
                Lwt.return
                  (print_endline
                     "Unexpected error during personal rating submission."))
          (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))
      in

      if%lwt Lwt.return (user_exists public_db username) then
        let%lwt _ = insert_public () in
        let%lwt _ = insert_personal () in
        Lwt.return_unit
      else
        Lwt.return
          (print_endline
             "Username not found in the public database. Please log in first.")

(* function to view food ratings *)
let view_food_rating public_db food eatery =
  let query =
    "SELECT AVG(rating) FROM Ratings WHERE food_item = ? AND eatery_name = ? \
     AND rating IS NOT NULL;"
  in
  let stmt = Sqlite3.prepare public_db query in
  Lwt.catch
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          (* No ratings found *)
          print_endline "No ratings found for this food item at this eatery.";
          Lwt.return () (* Ensure this returns Lwt.t *)
      | Sqlite3.Rc.ROW ->
          (* Extract rating values *)
          let rating = Sqlite3.column_int stmt 0 in
          Printf.printf "The rating for %s at %s is %d.\n" food eatery rating;
          Lwt.return () (* Ensure this returns Lwt.t *)
      | _ ->
          (* Handle unexpected SQL error *)
          print_endline "Error while fetching food ratings.";
          Lwt.return ()) (* Ensure this returns Lwt.t *)
    (fun exn ->
      (* Handle exceptions *)
      print_endline ("Error: " ^ Printexc.to_string exn);
      Lwt.return ())
  >>= fun () ->
  (* Finalize the statement to release resources *)
  Lwt.return (Sqlite3.finalize stmt |> ignore)

exception BindingError of string

let rec prompt_user public_db personal_db eateries =
  print_endline "\nPlease choose a number that best fits your desired action:";
  print_endline
    "1. Check if a <food> is served at any of the eateries (ex. 1 pizza)";
  print_endline "2. Search where a <food> is being served (ex. 2 pizza)";
  print_endline "3. Quit";
  print_endline
    "4. Rate a food item offered by an eatery (ex. 4 pizza EateryName 5)";
  print_endline
    "5. View the rating of a <food> item at an <eatery> (ex. 5 pizza \
     Okenshields)";
  let action = read_line () in
  let parts = String.split_on_char ' ' action in
  match parts with
  | [ "1"; food ] ->
      run_contains food eateries;
      prompt_user public_db personal_db eateries
  | [ "2"; food ] ->
      run_search_food food eateries;
      prompt_user public_db personal_db eateries
  | [ "3" ] -> Lwt.return (quit_program ())
  | [ "4"; food; eatery; rating ] -> (
      try
        let rating = int_of_string rating in
        if rating < 1 || rating > 5 then (
          print_endline "Rating must be between 1 and 5.";
          prompt_user public_db personal_db eateries)
        else
          let%lwt () = rate_food public_db personal_db food eatery rating in
          prompt_user public_db personal_db eateries
      with Failure _ ->
        print_endline "Invalid rating. Please enter a number between 1 and 5.";
        prompt_user public_db personal_db eateries)
  | [ "5"; food; eatery ] ->
      let%lwt () = view_food_rating public_db food eatery in
      prompt_user public_db personal_db eateries
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
        current_user := Some username;
        (* Set the current user *)
        Lwt.return_unit
        (* Exit recursion on success *))
      else (
        print_endline "Invalid username or password. Please try again.\n";
        login_or_create_account db (* Recursive call *))
  | "2" ->
      (* Account creation flow *)
      print_string "Choose a username: ";
      let username = read_line () in
      if%lwt Lwt.return (user_exists db username) then (
        print_endline "This username is already taken. Please choose another.\n";
        login_or_create_account db)
      else (
        print_string "Choose a password: ";
        let password = read_line () in
        Lwt.return
          (create_user db username password;
           print_endline "Account created successfully!";
           current_user := Some username))
      (* Set the current user *)
  | "3" -> quit_program ()
  | _ ->
      (* Invalid input, prompt again *)
      print_endline "Invalid choice. Please try again.\n";
      login_or_create_account db

let show_ratings db =
  let query = "SELECT * FROM PersonalRatings;" in
  let stmt = prepare db query in
  try
    print_endline "Displaying all ratings in PersonalRatings:";
    print_endline "------------------------------------------";
    while step stmt = Rc.ROW do
      let eatery_name =
        column stmt 0 |> Data.to_string |> Option.value ~default:"NULL"
      in
      let food_item =
        column stmt 1 |> Data.to_string |> Option.value ~default:"NULL"
      in
      let rating = column stmt 2 |> Data.to_int |> Option.value ~default:0 in
      Printf.printf "Eatery: %s | Food: %s | Rating: %d\n" eatery_name food_item
        rating
    done;
    finalize stmt |> ignore
  with exn ->
    finalize stmt |> ignore;
    raise exn

let show_ratings db =
  let query = "SELECT * FROM PersonalRatings;" in
  let stmt = prepare db query in
  try
    print_endline "Displaying all ratings in PersonalRatings:";
    print_endline "------------------------------------------";
    while step stmt = Rc.ROW do
      let eatery_name =
        column stmt 0 |> Data.to_string |> Option.value ~default:"NULL"
      in
      let food_item =
        column stmt 1 |> Data.to_string |> Option.value ~default:"NULL"
      in
      let rating = column stmt 2 |> Data.to_int |> Option.value ~default:0 in
      Printf.printf "Eatery: %s | Food: %s | Rating: %d\n" eatery_name food_item
        rating
    done;
    finalize stmt |> ignore
  with exn ->
    finalize stmt |> ignore;
    raise exn

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
