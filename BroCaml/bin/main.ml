open BroCaml.Data
open BroCaml.User
open BroCaml.Login
open Lwt
open Cohttp_lwt_unix
open Sqlite3

let current_user : string option ref = ref None
let is_guest : bool ref = ref false

let quit_program () =
  print_endline "Thanks for using FindMyFood!";
  exit 0

let rate_food public_db personal_db food eatery rating =
  if !is_guest then
    Lwt.return
      (print_endline
         "Error: Guests cannot rate foods. Please log in or create an account \
          to submit ratings.")
  else
    match !current_user with
    | None ->
        Lwt.return
          (print_endline
             "Error: No user logged in. Please log in first to submit ratings.")
    | Some username ->
        print_string "Would you like to submit your rating anonymously? (y/n): ";
        let anon = read_line () in
        let public_username = if anon = "y" then "anonymous" else username in

        let get_current_date () =
          let open Unix in
          let tm = localtime (time ()) in
          Printf.sprintf "%02d/%02d/%04d" (tm.tm_mon + 1) tm.tm_mday
            (tm.tm_year + 1900)
        in
        let current_date = get_current_date () in

        (* Function to insert into the public database *)
        let insert_public () =
          let query =
            if anon = "y" then
              "INSERT INTO Ratings (eatery_name, food_item, username, rating, \
               date)\n\
               VALUES (?, ?, ?, ?, ?);"
            else
              "INSERT INTO Ratings (eatery_name, food_item, username, rating, \
               date)\n\
               VALUES (?, ?, ?, ?, ?)\n\
               ON CONFLICT(eatery_name, food_item, username, date)\n\
               DO UPDATE SET rating = excluded.rating;"
          in
          let stmt = Sqlite3.prepare public_db query in
          Lwt.finalize
            (fun () ->
              Sqlite3.bind_text stmt 1 eatery |> ignore;
              Sqlite3.bind_text stmt 2 food |> ignore;
              Sqlite3.bind_text stmt 3 public_username |> ignore;
              Sqlite3.bind_int stmt 4 rating |> ignore;
              Sqlite3.bind_text stmt 5 current_date |> ignore;

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
            "INSERT INTO PersonalRatings (eatery_name, food_item, rating, date)\n\
             VALUES (?, ?, ?, ?)\n\
             ON CONFLICT(eatery_name, food_item, date)\n\
             DO UPDATE SET rating = excluded.rating;"
          in
          let stmt = Sqlite3.prepare personal_db query in
          Lwt.finalize
            (fun () ->
              Sqlite3.bind_text stmt 1 eatery |> ignore;
              Sqlite3.bind_text stmt 2 food |> ignore;
              Sqlite3.bind_int stmt 3 rating |> ignore;
              Sqlite3.bind_text stmt 4 current_date |> ignore;

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

        (* Ask if the user wants to leave a comment *)
        print_string
          "Would you like to leave a comment with your rating? (y/n): ";
        let leave_comment = read_line () in
        let comment =
          if leave_comment = "y" then (
            print_string "Please enter your comment: ";
            read_line ())
          else ""
        in

        (* Handle comment insertion into the database if the user provided
           one *)
        let insert_comment () =
          if comment <> "" then
            let query =
              "UPDATE Ratings SET comment = ? WHERE eatery_name = ? AND \
               food_item = ? AND username = ? AND date = ?;"
            in
            let stmt = Sqlite3.prepare public_db query in
            Lwt.finalize
              (fun () ->
                Sqlite3.bind_text stmt 1 comment |> ignore;
                Sqlite3.bind_text stmt 2 eatery |> ignore;
                Sqlite3.bind_text stmt 3 food |> ignore;
                Sqlite3.bind_text stmt 4 public_username |> ignore;
                Sqlite3.bind_text stmt 5 current_date |> ignore;

                match Sqlite3.step stmt with
                | Sqlite3.Rc.DONE ->
                    Lwt.return (print_endline "Comment submitted!")
                | Sqlite3.Rc.ERROR ->
                    Lwt.return
                      (print_endline
                         ("Error submitting comment: "
                        ^ Sqlite3.errmsg public_db))
                | _ ->
                    Lwt.return
                      (print_endline "Unexpected error submitting comment."))
              (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))
          else Lwt.return ()
        in

        if%lwt Lwt.return (user_exists public_db username) then
          let%lwt _ = insert_public () in
          let%lwt _ = insert_personal () in
          let%lwt _ = insert_comment () in
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
      (* Bind parameters to the query *)
      Sqlite3.bind_text stmt 1 food |> ignore;
      Sqlite3.bind_text stmt 2 eatery |> ignore;

      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          (* Extract the average rating as a float *)
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.NULL ->
              (* Handle the case where there are no ratings *)
              Printf.printf "No ratings found for %s at %s.\n" food eatery;
              Lwt.return ()
          | Sqlite3.Data.FLOAT avg_rating ->
              Printf.printf "The average rating for %s at %s is %.2f.\n" food
                eatery avg_rating;
              (* After showing the rating, ask if the user wants to view
                 comments *)
              print_string
                "Would you like to view comments for this food item? (y/n): ";
              let view_comments = read_line () in
              if view_comments = "y" then
                (* Fetch and display comments if the user chooses to view
                   them *)
                let comment_query =
                  "SELECT username, comment FROM Ratings WHERE food_item = ? \
                   AND eatery_name = ? AND comment IS NOT NULL;"
                in
                let comment_stmt = Sqlite3.prepare public_db comment_query in
                Lwt.finalize
                  (fun () ->
                    Sqlite3.bind_text comment_stmt 1 food |> ignore;
                    Sqlite3.bind_text comment_stmt 2 eatery |> ignore;
                    match Sqlite3.step comment_stmt with
                    | Sqlite3.Rc.ROW ->
                        (* Loop to fetch and print all comments *)
                        let rec print_comments () =
                          match Sqlite3.column comment_stmt 1 with
                          | Sqlite3.Data.TEXT comment ->
                              (* Safely extract the comment as a string *)
                              let username =
                                match Sqlite3.column comment_stmt 0 with
                                | Sqlite3.Data.TEXT username -> username
                                | _ ->
                                    "" (* Handle unexpected data types safely *)
                              in
                              Printf.printf "%s: %s\n" username comment;
                              if Sqlite3.step comment_stmt = Sqlite3.Rc.ROW then
                                print_comments ()
                          | _ -> ()
                        in
                        print_comments ();
                        Lwt.return ()
                    | _ ->
                        Lwt.return
                          (print_endline "No comments found for this food item."))
                  (fun () ->
                    Lwt.return (ignore (Sqlite3.finalize comment_stmt)))
              else Lwt.return ()
          | _ ->
              (* Handle unexpected data types *)
              print_endline "Unexpected data type while fetching food ratings.";
              Lwt.return ())
      | _ ->
          (* Handle unexpected SQL errors *)
          print_endline "Error while fetching food ratings.";
          Lwt.return ())
    (fun exn ->
      (* Handle exceptions *)
      print_endline ("Error: " ^ Printexc.to_string exn);
      Lwt.return ())
  >>= fun () ->
  (* Finalize the statement to release resources *)
  Lwt.return (Sqlite3.finalize stmt |> ignore)

exception BindingError of string

let show_ratings db =
  let query = "SELECT * FROM PersonalRatings;" in
  let stmt = prepare db query in
  try
    print_endline "Displaying all ratings in PersonalRatings:";
    print_endline "------------------------------------------";
    while step stmt = Rc.ROW do
      let eatery_name =
        column stmt 1 |> Data.to_string |> Option.value ~default:"NULL"
      in
      let food_item =
        column stmt 2 |> Data.to_string |> Option.value ~default:"NULL"
      in
      let rating = column stmt 3 |> Data.to_int |> Option.value ~default:0 in
      let date =
        column stmt 4 |> Data.to_string |> Option.value ~default:"NULL"
      in
      Printf.printf "Eatery: %s | Food: %s | Rating: %d | Date: %s\n"
        eatery_name food_item rating date
    done;
    finalize stmt |> ignore
  with exn ->
    finalize stmt |> ignore;
    raise exn

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
  print_endline "4. Quit";
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
          let%lwt () = rate_food public_db personal_db food eatery rating in
          prompt_user_rate public_db personal_db eateries
      with Failure _ ->
        print_endline "Invalid rating. Please enter a number between 1 and 5.";
        prompt_user_rate public_db personal_db eateries)
  | [ "2"; food; eatery ] ->
      let%lwt () = view_food_rating public_db food eatery in
      prompt_user_rate public_db personal_db eateries
  | [ "3" ] ->
      show_ratings personal_db;
      prompt_user_rate public_db personal_db eateries
  | [ "4" ] -> Lwt.return (quit_program ())
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
