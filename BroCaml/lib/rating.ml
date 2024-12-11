open Lwt
open Cohttp_lwt_unix
open Sqlite3
open Login

let rate_food public_db personal_db food eatery rating is_guest current_user =
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

        (* SQL query to insert or update the rating *)
        let query =
          "INSERT INTO Ratings (eatery_name, food_item, username, rating, \
           date, time)\n\
           VALUES (?, ?, ?, ?, ?, ?)\n\
           ON CONFLICT(eatery_name, food_item, username, date)\n\
           DO UPDATE SET rating = excluded.rating, time = excluded.time;"
        in
        let stmt = Sqlite3.prepare public_db query in
        Lwt.finalize
          (fun () ->
            Sqlite3.bind_text stmt 1 eatery |> ignore;
            Sqlite3.bind_text stmt 2 food |> ignore;
            Sqlite3.bind_text stmt 3 username |> ignore;
            Sqlite3.bind_int stmt 4 rating |> ignore;
            Sqlite3.bind_text stmt 5 current_date |> ignore;
            Sqlite3.bind_text stmt 6 current_time |> ignore;

            match Sqlite3.step stmt with
            | Sqlite3.Rc.DONE ->
                Lwt.return
                  (print_endline "Rating submitted or updated successfully!")
            | Sqlite3.Rc.ERROR ->
                Lwt.return
                  (print_endline
                     ("Error submitting or updating rating: "
                    ^ Sqlite3.errmsg public_db))
            | _ ->
                Lwt.return
                  (print_endline "Unexpected error during rating submission."))
          (fun () -> Lwt.return (ignore (Sqlite3.finalize stmt)))

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

let show_personal_ratings db =
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

let show_public_ratings db food =
  let query =
    "SELECT eatery_name, rating, date FROM Ratings WHERE food_item = ?;"
  in
  let stmt = Sqlite3.prepare db query in
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
      Printf.printf "Eatery: %s | Rating: %d | Date: %s\n" eatery_name rating
        date
    done;
    Sqlite3.finalize stmt |> ignore
  with exn ->
    Sqlite3.finalize stmt |> ignore;
    raise exn
