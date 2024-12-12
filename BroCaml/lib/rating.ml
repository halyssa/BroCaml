open Lwt
open Cohttp_lwt_unix
open Sqlite3
open Login
open User

let rate_food public_db personal_db food eatery rating is_guest current_user
    is_anon eateries =
  if !is_guest then
    Lwt.return
      (print_endline
         "Error: Guests cannot rate foods. Please log in or create an account \
          to submit ratings.")
  else if not (eatery_exists eatery eateries) then
    Lwt.return (print_endline "Error: Eatery does not exist.")
  else
    match !current_user with
    | None ->
        Lwt.return
          (print_endline
             "Error: No user logged in. Please log in first to submit ratings.")
    | Some username ->
        if not (contains food eateries) then
          Lwt.return
            (print_endline
               "Error: The food item is not available at this eatery.")
        else
          let current_timestamp = Unix.localtime (Unix.time ()) in
          let current_date =
            Printf.sprintf "%04d-%02d-%02d"
              (current_timestamp.Unix.tm_year + 1900)
              (current_timestamp.Unix.tm_mon + 1)
              current_timestamp.Unix.tm_mday
          in
          let current_time =
            Printf.sprintf "%02d:%02d:%02d" current_timestamp.Unix.tm_hour
              current_timestamp.Unix.tm_min current_timestamp.Unix.tm_sec
          in

          (* Function to insert or update the public database *)
          let insert_public () =
            let query =
              if is_anon then
                "INSERT INTO Ratings (eatery_name, food_item, username, \
                 rating, date, time)\n\
                 VALUES (?, ?, 'anonymous', ?, ?, ?);"
              else
                "INSERT INTO Ratings (eatery_name, food_item, username, \
                 rating, date, time)\n\
                 VALUES (?, ?, ?, ?, ?, ?)\n\
                 ON CONFLICT(eatery_name, food_item, username, date)\n\
                 DO UPDATE SET rating = excluded.rating, time = excluded.time;"
            in
            let stmt = Sqlite3.prepare public_db query in
            Lwt.finalize
              (fun () ->
                Sqlite3.bind_text stmt 1 eatery |> ignore;
                Sqlite3.bind_text stmt 2 food |> ignore;
                if not is_anon then Sqlite3.bind_text stmt 3 username |> ignore;
                Sqlite3.bind_int stmt (if is_anon then 3 else 4) rating
                |> ignore;
                Sqlite3.bind_text stmt (if is_anon then 4 else 5) current_date
                |> ignore;
                Sqlite3.bind_text stmt (if is_anon then 5 else 6) current_time
                |> ignore;

                match Sqlite3.step stmt with
                | Sqlite3.Rc.DONE ->
                    Lwt.return
                      (print_endline
                         "Rating submitted or updated successfully in the \
                          public database!")
                | Sqlite3.Rc.ERROR ->
                    Lwt.return
                      (print_endline
                         ("Error submitting or updating public rating: "
                        ^ Sqlite3.errmsg public_db))
                | _ ->
                    Lwt.return
                      (print_endline
                         "Unexpected error during public rating submission."))
              (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))
          in

          (* Function to insert or update the personal database *)
          let insert_personal () =
            let query =
              "INSERT INTO PersonalRatings (eatery_name, food_item, rating, \
               date, time)\n\
               VALUES (?, ?, ?, ?, ?)\n\
               ON CONFLICT(eatery_name, food_item, date)\n\
               DO UPDATE SET rating = excluded.rating, time = excluded.time;"
            in
            let stmt = Sqlite3.prepare personal_db query in
            Lwt.finalize
              (fun () ->
                Sqlite3.bind_text stmt 1 eatery |> ignore;
                Sqlite3.bind_text stmt 2 food |> ignore;
                Sqlite3.bind_int stmt 3 rating |> ignore;
                Sqlite3.bind_text stmt 4 current_date |> ignore;
                Sqlite3.bind_text stmt 5 current_time |> ignore;

                match Sqlite3.step stmt with
                | Sqlite3.Rc.DONE ->
                    Lwt.return
                      (print_endline
                         "Rating submitted or updated successfully in your \
                          personal database!")
                | Sqlite3.Rc.ERROR ->
                    Lwt.return
                      (print_endline
                         ("Error submitting or updating personal rating: "
                        ^ Sqlite3.errmsg personal_db))
                | _ ->
                    Lwt.return
                      (print_endline
                         "Unexpected error during personal rating submission."))
              (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))
          in

          (* Execute both public and personal database updates *)
          let%lwt () = insert_public () in
          let%lwt () = insert_personal () in
          Lwt.return ()

let view_food_rating public_db food eatery eateries =
  if not (contains food eateries) then
    Lwt.return
      (print_endline "Error: The food item is not available at this eatery.")
  else if not (eatery_exists eatery eateries) then
    Lwt.return (print_endline "Error: Eatery does not exist.")
  else
    let query =
      "SELECT AVG(rating), MAX(date), MAX(time) FROM Ratings WHERE food_item = \
       ? AND eatery_name = ? AND rating IS NOT NULL;"
    in
    let stmt = Sqlite3.prepare public_db query in
    Lwt.catch
      (fun () ->
        (* Bind parameters to the query *)
        Sqlite3.bind_text stmt 1 food |> ignore;
        Sqlite3.bind_text stmt 2 eatery |> ignore;

        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            (* Extract the average rating, date, and time *)
            match
              ( Sqlite3.column stmt 0,
                Sqlite3.column stmt 1,
                Sqlite3.column stmt 2 )
            with
            | ( Sqlite3.Data.FLOAT avg_rating,
                Sqlite3.Data.TEXT date,
                Sqlite3.Data.TEXT time ) ->
                Printf.printf
                  "The average rating for %s at %s is %.2f. (Last rated on %s \
                   at %s)\n"
                  food eatery avg_rating date time;
                (* Ask if the user wants to view comments *)
                print_string
                  "Would you like to view comments for this food item? (y/n): ";
                let view_comments = read_line () in
                if view_comments = "y" then
                  (* Fetch and display comments if the user chooses to view
                     them *)
                  let comment_query =
                    "SELECT username, comment, time FROM Ratings WHERE \
                     food_item = ? AND eatery_name = ? AND comment IS NOT \
                     NULL;"
                  in
                  let comment_stmt = Sqlite3.prepare public_db comment_query in
                  Lwt.finalize
                    (fun () ->
                      Sqlite3.bind_text comment_stmt 1 food |> ignore;
                      Sqlite3.bind_text comment_stmt 2 eatery |> ignore;
                      let rec print_comments () =
                        match Sqlite3.step comment_stmt with
                        | Sqlite3.Rc.ROW ->
                            let username =
                              match Sqlite3.column comment_stmt 0 with
                              | Sqlite3.Data.TEXT username -> username
                              | _ -> "anonymous"
                            in
                            let comment =
                              match Sqlite3.column comment_stmt 1 with
                              | Sqlite3.Data.TEXT comment -> comment
                              | _ -> ""
                            in
                            let time =
                              match Sqlite3.column comment_stmt 2 with
                              | Sqlite3.Data.TEXT time -> time
                              | _ -> "unknown time"
                            in
                            Printf.printf "%s (%s): %s\n" username time comment;
                            print_comments ()
                        | _ -> ()
                      in
                      print_comments ();
                      Lwt.return ())
                    (fun () ->
                      Lwt.return (ignore (Sqlite3.finalize comment_stmt)))
                else Lwt.return ()
            | Sqlite3.Data.NULL, _, _ ->
                (* Handle no ratings *)
                Printf.printf "No ratings found for %s at %s.\n" food eatery;
                Lwt.return ()
            | _ ->
                (* Handle unexpected data types *)
                print_endline
                  "Unexpected data type while fetching food ratings.";
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

let show_personal_ratings db is_guest =
  if !is_guest then
    Lwt.return
      (print_endline
         "Error: Guests do not have personal ratings. Please log in or create \
          an account to see your past ratings.")
  else
    let query =
      "SELECT eatery_name, food_item, rating, date, time FROM PersonalRatings;"
    in
    let stmt = Sqlite3.prepare db query in
    Lwt.finalize
      (fun () ->
        print_endline "Displaying all ratings in PersonalRatings:";
        print_endline "------------------------------------------";
        let rec fetch_rows () =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let eatery_name =
                Sqlite3.column stmt 0 |> Sqlite3.Data.to_string
                |> Option.value ~default:"NULL"
              in
              let food_item =
                Sqlite3.column stmt 1 |> Sqlite3.Data.to_string
                |> Option.value ~default:"NULL"
              in
              let rating =
                Sqlite3.column stmt 2 |> Sqlite3.Data.to_int
                |> Option.value ~default:0
              in
              let date =
                Sqlite3.column stmt 3 |> Sqlite3.Data.to_string
                |> Option.value ~default:"NULL"
              in
              let time =
                Sqlite3.column stmt 4 |> Sqlite3.Data.to_string
                |> Option.value ~default:"NULL"
              in
              Printf.printf
                "Eatery: %s | Food: %s | Rating: %d | Date: %s | Time: %s\n"
                eatery_name food_item rating date time;
              fetch_rows () (* Recursive call to fetch next row *)
          | Sqlite3.Rc.DONE -> Lwt.return_unit (* End of rows *)
          | _ ->
              print_endline "Error while processing personal ratings.";
              Lwt.return_unit
        in
        fetch_rows ())
      (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))

let show_public_ratings db food choice =
  let query =
    "SELECT eatery_name, rating, date, time FROM Ratings WHERE food_item = ?"
  in
  let sorted_query =
    match choice with
    | "1" -> query ^ " ORDER BY rating DESC"
    | "2" -> query ^ " ORDER BY rating ASC"
    | "3" -> query ^ " ORDER BY eatery_name DESC"
    | "4" -> query ^ " ORDER BY eatery_name ASC"
    | "5" -> query ^ " ORDER BY date ASC, time ASC"
    | "6" -> query ^ " ORDER BY date DESC, time DESC"
    | _ -> query (* Default case, no sorting *)
  in
  let stmt = Sqlite3.prepare db sorted_query in
  try
    Sqlite3.bind_text stmt 1 food |> ignore;
    print_endline
      ("Displaying all ratings for \"" ^ food ^ "\" in the public database:");
    print_endline "------------------------------------------------------------";
    while Sqlite3.step stmt = Sqlite3.Rc.ROW do
      let eatery_name =
        Sqlite3.column stmt 0 |> Sqlite3.Data.to_string
        |> Option.value ~default:"NULL"
      in
      let rating =
        Sqlite3.column stmt 1 |> Sqlite3.Data.to_int |> Option.value ~default:0
      in
      let date =
        Sqlite3.column stmt 2 |> Sqlite3.Data.to_string
        |> Option.value ~default:"NULL"
      in
      let time =
        Sqlite3.column stmt 3 |> Sqlite3.Data.to_string
        |> Option.value ~default:"NULL"
      in
      Printf.printf "Eatery: %s | Rating: %d | Date: %s | Time: %s\n"
        eatery_name rating date time
    done;
    Sqlite3.finalize stmt |> ignore
  with exn ->
    Sqlite3.finalize stmt |> ignore;
    raise exn

(**[print_results] is a helper function to print the results from sorting a
   database *)
let print_results stmt =
  let rec print_rows () =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let eatery = Sqlite3.column stmt 0 |> Sqlite3.Data.to_string_exn in
        let food = Sqlite3.column stmt 1 |> Sqlite3.Data.to_string_exn in
        let rating = Sqlite3.column stmt 2 |> Sqlite3.Data.to_int_exn in
        let date = Sqlite3.column stmt 3 |> Sqlite3.Data.to_string_exn in
        let time = Sqlite3.column stmt 4 |> Sqlite3.Data.to_string_exn in
        Printf.printf
          "Eatery: %s | Food: %s | Rating: %d | Date: %s | Time: %s\n" eatery
          food rating date time;
        print_rows ()
    | _ -> ()
  in
  print_rows ()

(* sorting by rating *)
let sort_by_highest_rating db table =
  let query =
    Printf.sprintf
      "SELECT eatery_name, food_item, rating, date, time FROM %s ORDER BY \
       rating DESC;"
      table
  in
  let stmt = Sqlite3.prepare db query in
  Lwt.finalize
    (fun () ->
      print_results stmt;
      Lwt.return ())
    (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))

let sort_by_lowest_rating db table =
  let query =
    Printf.sprintf
      "SELECT eatery_name, food_item, rating, date, time FROM %s ORDER BY \
       rating ASC;"
      table
  in
  let stmt = Sqlite3.prepare db query in
  Lwt.finalize
    (fun () ->
      print_results stmt;
      Lwt.return ())
    (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))

(* sort abc *)
let sort_by_eatery_alphabetical db table =
  let query =
    Printf.sprintf
      "SELECT eatery_name, food_item, rating, date, time FROM %s ORDER BY \
       eatery_name ASC;"
      table
  in
  let stmt = Sqlite3.prepare db query in
  Lwt.finalize
    (fun () ->
      print_results stmt;
      Lwt.return ())
    (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))

let sort_by_eatery_reverse_alphabetical db table =
  let query =
    Printf.sprintf
      "SELECT eatery_name, food_item, rating, date, time FROM %s ORDER BY \
       eatery_name DESC;"
      table
  in
  let stmt = Sqlite3.prepare db query in
  Lwt.finalize
    (fun () ->
      print_results stmt;
      Lwt.return ())
    (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))

let sort_by_food_alphabetical db table =
  let query =
    Printf.sprintf
      "SELECT eatery_name, food_item, rating, date, time FROM %s ORDER BY \
       food_item ASC;"
      table
  in
  let stmt = Sqlite3.prepare db query in
  Lwt.finalize
    (fun () ->
      print_results stmt;
      Lwt.return ())
    (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))

let sort_by_food_reverse_alphabetical db table =
  let query =
    Printf.sprintf
      "SELECT eatery_name, food_item, rating, date, time FROM %s ORDER BY \
       food_item DESC;"
      table
  in
  let stmt = Sqlite3.prepare db query in
  Lwt.finalize
    (fun () ->
      print_results stmt;
      Lwt.return ())
    (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))

(* chronological *)
let sort_by_date_asc db table =
  let query =
    Printf.sprintf
      "SELECT eatery_name, food_item, rating, date, time FROM %s ORDER BY date \
       ASC, time ASC;"
      table
  in
  let stmt = Sqlite3.prepare db query in
  Lwt.finalize
    (fun () ->
      print_results stmt;
      Lwt.return ())
    (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))

let sort_by_date_desc db table =
  let query =
    Printf.sprintf
      "SELECT eatery_name, food_item, rating, date, time FROM %s ORDER BY date \
       DESC, time DESC;"
      table
  in
  let stmt = Sqlite3.prepare db query in
  Lwt.finalize
    (fun () ->
      print_results stmt;
      Lwt.return ())
    (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))
