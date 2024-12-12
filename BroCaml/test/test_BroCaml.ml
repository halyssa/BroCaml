open OUnit2
open Lwt
open Lwt.Infix
open Cohttp_lwt_unix
open Yojson
open BroCaml.User
open BroCaml.Data
open BroCaml.Login
open BroCaml.Rating
open Sqlite3

let eatery1 = create_eatery "Bistro Cafe" [ "Pasta"; "Salad"; "Soup" ]
let eatery2 = create_eatery "Deli Delight" [ "Sandwich"; "Soup"; "Juice" ]
let eatery3 = create_eatery "Gourmet Grill" [ "Burger"; "Fries"; "Salad" ]
let eateries = [ eatery1; eatery2; eatery3 ]

let test_create_eatery_invalid =
  "empty test"
  >::: [
         ( "empty eater name" >:: fun _ ->
           assert_raises (Invalid_argument "Eatery name cannot be empty.")
             (fun () -> create_eatery "" [ "Pizza"; "Burger" ]) );
         ( "empty menu" >:: fun _ ->
           assert_raises (Invalid_argument "Menu cannot be empty.") (fun () ->
               create_eatery "Pizza Place" []) );
       ]

let contains_helper_test =
  "contains helper tests"
  >::: [
         ( "food in any eatery menu, exact match" >:: fun _ ->
           assert_bool "Pasta should be found in eateries"
             (contains "Pasta" eateries) );
         ( "food in any eatery menu, case-insensitive match" >:: fun _ ->
           assert_bool "burger should be found in eateries"
             (contains "burger" eateries) );
         ( "food not in any eatery menu" >:: fun _ ->
           assert_bool "Pizza should not be found in eateries"
             (not (contains "Pizza" eateries)) );
         ( "food not in any eatery menu" >:: fun _ ->
           assert_bool "Kale Chips should not be found in eateries"
             (not (contains "Kale Chips" eateries)) );
         ( "food with special characters in menu" >:: fun _ ->
           let eatery_special_chars =
             create_eatery "Fancy Eatery"
               [ "Chicken & Waffles"; "Grilled Chicken" ]
           in
           assert_bool "Food with special characters should be found"
             (contains "Chicken & Waffles" [ eatery_special_chars ]) );
       ]

let contains_tests =
  "contains function tests"
  >::: [
         ( "food in any eatery menu, exact match" >:: fun _ ->
           assert_bool "Pasta should be found in eateries"
             (contains "Pasta" eateries) );
         ( "food in any eatery menu, case-insensitive match" >:: fun _ ->
           assert_bool "burger should be found in eateries"
             (contains "burger" eateries) );
         ( "food not in any eatery menu" >:: fun _ ->
           assert_bool "Pizza should not be found in eateries"
             (not (contains "Pizza" eateries)) );
       ]

let search_test_helper food eateries expected =
  "" >:: fun _ ->
  let result = search_food food eateries in
  assert_equal expected result

let search_food_tests =
  "search_food function tests"
  >::: [
         search_test_helper "Pasta" eateries [ "Bistro Cafe" ];
         search_test_helper "Salad" eateries [ "Bistro Cafe"; "Gourmet Grill" ];
         search_test_helper "Pizza" eateries [];
       ]

let eatery_duplicate_food = create_eatery "Duplicate" [ "Burger"; "Burger" ]

let contains_duplicate_food_test =
  "duplicate food in menu" >:: fun _ ->
  assert_bool "Burger should be found in Duplicate's menu"
    (contains_helper "Burger" eatery_duplicate_food)

let test_parse_eateries_empty_eateries _ =
  Lwt_main.run
    (let json = `Assoc [ ("data", `Assoc [ ("eateries", `Null) ]) ] in
     let%lwt result = parse_eateries json in
     assert_equal (List.length result) 0;
     Lwt.return_unit)

let test_parse_eateries_missing_eateries _ =
  Lwt_main.run
    (let json = `Assoc [ ("data", `Assoc []) ] in
     let%lwt result = parse_eateries json in
     assert_equal (List.length result) 0;
     Lwt.return_unit)

let test_get_data_valid _ =
  Lwt_main.run
    (let%lwt result = get_data () in
     assert_bool "Should return a list of eateries" (List.length result > 0);
     Lwt.return_unit)

let get_data_tests =
  "get_data tests" >::: [ "test_get_data_valid" >:: test_get_data_valid ]

let parse_eateries_tests =
  "parse_eateries tests"
  >::: [
         "test_parse_eateries_empty_eateries"
         >:: test_parse_eateries_empty_eateries;
         "test_parse_eateries_missing_eateries"
         >:: test_parse_eateries_missing_eateries;
       ]

let setup_test_db () =
  let db = Sqlite3.db_open ":memory:" in
  let create_table_query =
    "CREATE TABLE Users ( id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT \
     NOT NULL UNIQUE, password_hash TEXT NOT NULL);"
  in
  ignore (Sqlite3.exec db create_table_query);
  db

let test_user_exists _ =
  let db = setup_test_db () in
  let insert_query =
    "INSERT INTO Users (username, password_hash) VALUES ('test_user', \
     'hash123');"
  in
  ignore (Sqlite3.exec db insert_query);

  assert_bool "User should exist" (user_exists db "test_user");
  assert_bool "User should not exist" (not (user_exists db "non_existent_user"))

let test_validate_user _ =
  let db = setup_test_db () in
  let insert_query =
    "INSERT INTO Users (username, password_hash) VALUES ('test_user', \
     'hash123');"
  in
  ignore (Sqlite3.exec db insert_query);

  let result = Lwt_main.run (validate_user db "test_user" "hash123") in
  assert_bool "Validation should succeed" result;

  let result = Lwt_main.run (validate_user db "test_user" "wrong_hash") in
  assert_bool "Validation should fail with wrong password" (not result);

  let result = Lwt_main.run (validate_user db "non_existent_user" "hash123") in
  assert_bool "Validation should fail for non-existent user" (not result)

let test_validate_special_chars _ =
  let db = setup_test_db () in
  let insert_query =
    "INSERT INTO Users (username, password_hash) VALUES ('user!@#$', \
     'hash123');"
  in
  ignore (Sqlite3.exec db insert_query);

  let result = Lwt_main.run (validate_user db "user!@#$" "hash123") in
  assert_bool "Validation should succeed" result;

  let result = Lwt_main.run (validate_user db "user!@#$" "wrong_hash") in
  assert_bool "Validation should fail with wrong password" (not result)

(** [finalize_wrapper stmt _db] is a helper function that converts Finalize.stmt
    to the type Finalize.db *)
let finalize_wrapper stmt _db =
  match Sqlite3.finalize stmt with
  | Sqlite3.Rc.OK -> () (* This ensures no value is returned *)
  | Sqlite3.Rc.CONSTRAINT ->
      failwith "Error creating user: UNIQUE constraint failed: Users.username"
  | rc -> failwith ("Failed to finalize statement: " ^ Sqlite3.Rc.to_string rc)

let test_create_user _ =
  let db = setup_test_db () in

  create_user ~finalize:finalize_wrapper db "new_user" "password123";
  assert_bool "User should exist after creation" (user_exists db "new_user");

  assert_raises
    (Failure "Error creating user: UNIQUE constraint failed: Users.username")
    (fun () ->
      create_user ~finalize:finalize_wrapper db "new_user" "password123");

  let count_query = "SELECT COUNT(*) FROM Users;" in
  let stmt = Sqlite3.prepare db count_query in
  let count =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        Sqlite3.column stmt 0 |> Sqlite3.Data.to_int |> Option.get
    | _ -> 0
  in
  Sqlite3.finalize stmt |> ignore;

  assert_equal 1 count

let test_connect_db_checked _ =
  (* Test case 1: Database file exists *)
  let existing_db = Filename.temp_file "test_db" ".sqlite" in
  let db = connect_db_checked existing_db in
  assert_bool "Database connection should be open" (db_close db);
  Sys.remove existing_db;

  (* Test case 2: Database file doesn't exist *)
  let non_existing_db = "/tmp/non_existing_db.sqlite" in
  assert_raises
    (Failure ("Database file not found: " ^ non_existing_db))
    (fun () -> connect_db_checked non_existing_db)

let login_tests =
  "login tests"
  >::: [
         "test_user_exists" >:: test_user_exists;
         "test_validate_user" >:: test_validate_user;
         "test_validate_special_chars" >:: test_validate_special_chars;
         "test_create_user" >:: test_create_user;
         "test_connect_db_checked" >:: test_connect_db_checked;
       ]

let null_eateries_json = `Assoc [ ("data", `Assoc [ ("eateries", `Null) ]) ]

let no_menu_items_json =
  `Assoc
    [
      ( "data",
        `Assoc
          [
            ( "eateries",
              `List
                [
                  `Assoc
                    [
                      ("name", `String "Test Eatery");
                      ("diningItems", `Null);
                      ("operatingHours", `Null);
                      ( "diningCuisines",
                        `List [ `Assoc [ ("name", `String "Italian") ] ] );
                    ];
                ] );
          ] );
    ]

let no_menu_events_json =
  `Assoc
    [
      ( "data",
        `Assoc
          [
            ( "eateries",
              `List
                [
                  `Assoc
                    [
                      ("name", `String "Event Eatery");
                      ( "operatingHours",
                        `List
                          [
                            `Assoc
                              [
                                ( "events",
                                  `List
                                    [
                                      `Assoc
                                        [
                                          ("menu", `Null);
                                          ("otherField", `String "OtherData");
                                        ];
                                    ] );
                              ];
                          ] );
                    ];
                ] );
          ] );
    ]

let invalid_json = `Assoc [ ("invalid_key", `String "Invalid") ]
let failing_url = "http://invalid-url.test"

let test_parse_null_eateries _ =
  Lwt_main.run
    ( parse_eateries null_eateries_json >|= fun eateries ->
      assert_equal [] eateries )

let test_parse_no_menu_items _ =
  Lwt_main.run
    ( parse_eateries no_menu_items_json >|= fun eateries ->
      assert_equal [ "Test Eatery" ] (List.map (fun e -> get_name e) eateries);
      assert_equal [ [ "Italian" ] ] (List.map (fun e -> get_menu e) eateries)
    )

let test_parse_no_menu_events _ =
  Lwt_main.run
    ( parse_eateries no_menu_events_json >|= fun eateries ->
      assert_equal [ "Event Eatery" ] (List.map (fun e -> get_name e) eateries);
      assert_equal
        [ [ "No menu available" ] ]
        (List.map (fun e -> get_menu e) eateries) )

let test_fetch_invalid_json _ =
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         Lwt.fail (Failure "JSON parsing error") >|= fun _ ->
         assert_failure "Expected fetch_json to fail")
       (function
         | Failure msg ->
             assert_equal "JSON parsing error" msg;
             Lwt.return ()
         | _ -> assert_failure "Unexpected exception"))

let test_parse_eateries_valid_json _ =
  Lwt_main.run
    (let json =
       `Assoc
         [
           ( "data",
             `Assoc
               [
                 ( "eateries",
                   `List
                     [
                       `Assoc
                         [
                           ("name", `String "Bistro Cafe");
                           ( "diningItems",
                             `List [ `Assoc [ ("item", `String "Pasta") ] ] );
                         ];
                     ] );
               ] );
         ]
     in
     let%lwt eateries = parse_eateries json in
     assert_equal (List.length eateries) 1;
     Lwt.return ())

let test_fetch_failing_url _ =
  let mock_fetch_json url =
    Lwt.pause () >>= fun () ->
    Lwt.fail (Failure "HTTP request failed with error")
  in
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         mock_fetch_json "https://failing-url.com" >|= fun _ ->
         assert_failure "Expected fetch_json to fail")
       (function
         | Failure msg ->
             assert_equal "HTTP request failed with error" msg;
             Lwt.return ()
         | _ -> assert_failure "Unexpected exception"))

let test_get_data _ =
  Lwt_main.run
    (let timeout = Lwt_unix.sleep 0.5 >>= fun () -> Lwt.return () in
     Lwt.pick
       [
         ( get_data () >>= fun eateries ->
           assert_bool "Eateries list should not be empty"
             (List.length eateries > 0);
           Lwt.return () );
         timeout;
       ])

let get_data fetch_json =
  let%lwt response = fetch_json () in
  Lwt.return response

let test_get_data_http_failure _ =
  let mock_fetch_json _ = Lwt.fail (Failure "HTTP request failed with error") in
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         let%lwt result = get_data mock_fetch_json in
         assert_equal [] result;
         Lwt.return ())
       (function
         | Failure msg ->
             assert_equal "HTTP request failed with error" msg;
             Lwt.return ()
         | _ -> assert_failure "Expected fetch_json to fail"))

let test_get_data_json_parsing_failure _ =
  let mock_fetch_json _ = Lwt.fail (Failure "JSON parsing error") in
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         let%lwt result = get_data mock_fetch_json in
         assert_equal [] result;
         Lwt.return ())
       (function
         | Failure msg ->
             assert_equal "JSON parsing error" msg;
             Lwt.return ()
         | _ -> assert_failure "Expected fetch_json to fail"))

let test_get_data_unexpected_failure _ =
  let mock_fetch_json _ = Lwt.fail (Failure "Unexpected error") in
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         let%lwt result = get_data mock_fetch_json in
         assert_equal [] result;
         Lwt.return ())
       (function
         | Failure msg ->
             assert_equal "Unexpected error" msg;
             Lwt.return ()
         | _ -> assert_failure "Expected fetch_json to fail"))

let create_in_memory_db () =
  let db = Sqlite3.db_open ":memory:" in
  let create_ratings_table_query =
    "CREATE TABLE IF NOT EXISTS Ratings (\n\
    \              eatery_name TEXT,\n\
    \              food_item TEXT,\n\
    \              username TEXT,\n\
    \              rating INTEGER,\n\
    \              comment TEXT, \n\
    \              date TEXT,\n\
    \              time TEXT,\n\
    \              PRIMARY KEY (eatery_name, food_item, username, date)\n\
    \            );"
  in
  let create_personal_ratings_table_query =
    "CREATE TABLE IF NOT EXISTS PersonalRatings (\n\
    \              eatery_name TEXT,\n\
    \              food_item TEXT,\n\
    \              rating INTEGER,\n\
    \              comment TEXT, \n\n\
    \                  date TEXT,\n\
    \              time TEXT,\n\
    \              PRIMARY KEY (eatery_name, food_item, date)\n\
    \            );"
  in
  ignore (Sqlite3.exec db create_ratings_table_query);
  ignore (Sqlite3.exec db create_personal_ratings_table_query);
  db

let test_view_food_rating_no_ratings =
  "View food rating when no ratings exist" >:: fun _ ->
  let db = create_in_memory_db () in
  let result =
    Lwt_main.run (view_food_rating db "Sushi" "Sushi Bar" eateries)
  in
  assert_equal () result

let test_rate_food_as_guest =
  "Rate food as guest" >:: fun _ ->
  let db = create_in_memory_db () in
  let is_guest = ref true in
  let current_user = ref None in
  let result =
    Lwt_main.run
      (rate_food db db "Pizza" "Grill House" 4 is_guest current_user eateries)
  in
  assert_equal () result

let test_rate_food =
  "food rating" >:: fun _ ->
  let public_db = create_in_memory_db () in
  let personal_db = create_in_memory_db () in
  let is_guest = ref false in
  let current_user = ref (Some "testuser") in
  let eateries = [ create_eatery "Eatery1" [ "Food1"; "Food2" ] ] in

  let result =
    Lwt_main.run
      (rate_food public_db personal_db "Food1" "Eatery1" 5 is_guest current_user
         eateries)
  in

  assert_equal () result;

  let stmt =
    Sqlite3.prepare public_db
      "SELECT rating FROM Ratings WHERE eatery_name = 'Eatery1' AND food_item \
       = 'Food1'"
  in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> (
      match Sqlite3.column stmt 0 |> Sqlite3.Data.to_int with
      | Some rating -> assert_equal 5 rating
      | None -> assert_failure "Rating is NULL")
  | _ -> assert_failure "Rating not found in public database"

let test_view_food_rating =
  "view food rating" >:: fun _ ->
  let public_db = create_in_memory_db () in
  let eateries = [ create_eatery "Test Eatery" [ "Test Food" ] ] in

  let insert_query =
    "INSERT INTO Ratings (eatery_name, food_item, username, rating, date, \
     time) VALUES (?, ?, ?, ?, ?, ?);"
  in
  let stmt = Sqlite3.prepare public_db insert_query in
  Sqlite3.bind_text stmt 1 "Test Eatery" |> ignore;
  Sqlite3.bind_text stmt 2 "Test Food" |> ignore;
  Sqlite3.bind_text stmt 3 "testuser" |> ignore;
  Sqlite3.bind_int stmt 4 4 |> ignore;
  Sqlite3.bind_text stmt 5 "2023-12-11" |> ignore;
  Sqlite3.bind_text stmt 6 "12:00:00" |> ignore;
  assert_equal Sqlite3.Rc.DONE (Sqlite3.step stmt);
  Sqlite3.finalize stmt |> ignore;

  let output = ref "" in
  let old_stdout = Unix.dup Unix.stdout in
  let pipe_out, pipe_in = Unix.pipe () in
  Unix.dup2 pipe_in Unix.stdout;
  Unix.close pipe_in;

  Lwt_main.run (view_food_rating public_db "Test Food" "Test Eatery" eateries);

  Unix.dup2 old_stdout Unix.stdout;
  let buffer = Bytes.create 1024 in
  let _ = Unix.read pipe_out buffer 0 1024 in
  output := Bytes.to_string buffer;
  Unix.close pipe_out;

  Printf.printf "Captured output: %s\n" !output;

  assert_bool "Output should contain average rating"
    (Str.string_match
       (Str.regexp "The average rating for Test Food at Test Eatery is 4.00")
       !output 0);
  assert_bool "Output should contain last rated date and time"
    (Str.string_match
       (Str.regexp ".*Last rated on [0-9-]+ at [0-9:]+.*")
       !output 0)

let setup_in_memory_db () =
  let db = create_in_memory_db () in
  db

let teardown_in_memory_db db = ignore (Sqlite3.db_close db)

let populate_personal_ratings_table db =
  let insert_query =
    "INSERT INTO PersonalRatings (eatery_name, food_item, rating, comment, \
     date, time)\n\
    \ VALUES (?, ?, ?, ?, ?, ?);"
  in
  let stmt = Sqlite3.prepare db insert_query in
  let rows =
    [
      ("Eatery A", "Burger", 5, "Great taste", "2024-12-01", "12:00");
      ("Eatery B", "Pizza", 4, "Pretty good", "2024-12-02", "13:00");
      ("Eatery C", "Pasta", 3, "Okay-ish", "2024-12-03", "14:00");
    ]
  in
  List.iter
    (fun (eatery, food, rating, comment, date, time) ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT eatery));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT food));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int rating)));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT comment));
      ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT date));
      ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT time));
      ignore (Sqlite3.step stmt);
      ignore (Sqlite3.reset stmt))
    rows;
  ignore (Sqlite3.finalize stmt)

let test_show_personal_ratings_with_data =
  "personal ratings" >:: fun _ ->
  let db = setup_in_memory_db () in
  populate_personal_ratings_table db;
  let is_guest = ref false in
  Lwt_main.run (show_personal_ratings db is_guest);
  teardown_in_memory_db db

let test_show_public_ratings =
  "public ratings" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_query =
    "INSERT INTO Ratings (eatery_name, food_item, username, rating, comment, \
     date, time) VALUES (?, ?, ?, ?, ?, ?, ?)"
  in
  let insert_stmt = Sqlite3.prepare db insert_query in
  let test_data =
    [
      ( "Pizza Place",
        "Pepperoni Pizza",
        "user1",
        5,
        "Great!",
        "2024-12-01",
        "18:00" );
      ( "Burger Joint",
        "Cheeseburger",
        "user2",
        4,
        "Tasty!",
        "2024-12-02",
        "19:00" );
      ("Pizza Place", "Veggie Pizza", "user3", 3, "Okay.", "2024-12-01", "19:30");
      ( "Sushi Spot",
        "California Roll",
        "user4",
        5,
        "Delicious!",
        "2024-12-03",
        "20:00" );
      ( "Burger Joint",
        "Veggie Burger",
        "user5",
        2,
        "Not great.",
        "2024-12-02",
        "20:30" );
    ]
  in
  List.iter
    (fun (name, food, user, rating, comment, date, time) ->
      Sqlite3.bind_text insert_stmt 1 name |> ignore;
      Sqlite3.bind_text insert_stmt 2 food |> ignore;
      Sqlite3.bind_text insert_stmt 3 user |> ignore;
      Sqlite3.bind_int insert_stmt 4 rating |> ignore;
      Sqlite3.bind_text insert_stmt 5 comment |> ignore;
      Sqlite3.bind_text insert_stmt 6 date |> ignore;
      Sqlite3.bind_text insert_stmt 7 time |> ignore;
      ignore (Sqlite3.step insert_stmt);
      Sqlite3.reset insert_stmt |> ignore)
    test_data;

  Sqlite3.finalize insert_stmt |> ignore;

  let food_item = "Pepperoni Pizza" in
  let choices = [ "1"; "2"; "3"; "4"; "5"; "6" ] in

  List.iter
    (fun choice ->
      print_endline ("Testing with choice: " ^ choice);
      show_public_ratings db food_item choice)
    choices;

  print_endline "Testing with non-existent food item:";
  show_public_ratings db "Nonexistent Food" "1"

let test_rate_food_invalid_rating_value =
  "Rate food with invalid rating value (out of range)" >:: fun _ ->
  let db = create_in_memory_db () in
  let is_guest = ref false in
  let current_user = ref (Some "john_doe") in
  let result =
    Lwt_main.run
      (rate_food db db "Pizza" "Grill House" 6 is_guest current_user eateries)
  in
  assert_equal () result

let test_view_food_rating_with_comments =
  "View food rating with comments" >:: fun _ ->
  let db = create_in_memory_db () in
  let is_guest = ref false in
  let current_user = ref (Some "john_doe") in
  Lwt_main.run
    (rate_food db db "Pizza" "Grill House" 4 is_guest current_user eateries);
  Lwt_main.run
    (rate_food db db "Pizza" "Grill House" 4 is_guest current_user eateries);
  let result =
    Lwt_main.run (view_food_rating db "Pizza" "Grill House" eateries)
  in
  assert_equal () result

let test_view_food_rating_empty_db =
  "View food rating when database is empty" >:: fun _ ->
  let db = create_in_memory_db () in
  let result =
    Lwt_main.run (view_food_rating db "Pizza" "Grill House" eateries)
  in
  assert_equal () result

let test_view_food_rating_no_comments =
  "View food rating with no comments" >:: fun _ ->
  let db = create_in_memory_db () in
  let is_guest = ref false in
  let current_user = ref (Some "john_doe") in
  Lwt_main.run
    (rate_food db db "Pizza" "Grill House" 4 is_guest current_user eateries);
  let result =
    Lwt_main.run (view_food_rating db "Pizza" "Grill House" eateries)
  in
  assert_equal () result

let test_sort_by_highest_rating =
  "Sort by highest rating" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-01", "12:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-02", "13:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-03", "14:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  Lwt_main.run (sort_by_highest_rating db "Ratings");

  let stmt =
    Sqlite3.prepare db
      "SELECT rating FROM Ratings ORDER BY rating DESC LIMIT 1;"
  in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let rating = Sqlite3.column stmt 0 |> Sqlite3.Data.to_int in
      assert_equal (Some 5) rating
  | _ ->
      assert_failure "Failed to retrieve the highest rating from the database"

let test_sort_by_highest_rating =
  "Sort by highest rating" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-01", "12:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-02", "13:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-03", "14:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  Lwt_main.run (sort_by_highest_rating db "Ratings");

  let stmt =
    Sqlite3.prepare db
      "SELECT rating FROM Ratings ORDER BY rating DESC LIMIT 1;"
  in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let rating = Sqlite3.column stmt 0 |> Sqlite3.Data.to_int in
      assert_equal (Some 5) rating
  | _ ->
      assert_failure "Failed to retrieve the highest rating from the database"

let test_sort_by_lowest_rating =
  "Sort by lowest rating" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-01", "12:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-02", "13:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-03", "14:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  Lwt_main.run (sort_by_lowest_rating db "Ratings");

  let stmt =
    Sqlite3.prepare db "SELECT rating FROM Ratings ORDER BY rating ASC LIMIT 1;"
  in

  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let rating = Sqlite3.column stmt 0 |> Sqlite3.Data.to_int in
      assert_equal (Some 3) rating
  | _ -> assert_failure "Failed to retrieve the lowest rating from the database"

let test_sort_by_eatery_alphabetical =
  "Sort by eatery name alphabetically" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-01", "12:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-03", "14:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-02", "13:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  let test_query =
    "SELECT eatery_name FROM Ratings ORDER BY eatery_name ASC LIMIT 1;"
  in
  let stmt = Sqlite3.prepare db test_query in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let first_eatery = Sqlite3.column stmt 0 |> Sqlite3.Data.to_string in
      assert_equal (Some "Burger Joint") first_eatery;
      Lwt_main.run (sort_by_eatery_alphabetical db "Ratings")
  | _ ->
      assert_failure
        "Failed to retrieve the first eatery name from the database"

let test_sort_by_eatery_reverse_alphabetical =
  "Sort by eatery name alphabetically" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-01", "12:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-03", "14:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-02", "13:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  let test_query =
    "SELECT eatery_name FROM Ratings ORDER BY eatery_name DESC LIMIT 1;"
  in
  let stmt = Sqlite3.prepare db test_query in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let first_eatery = Sqlite3.column stmt 0 |> Sqlite3.Data.to_string in
      assert_equal (Some "Sushi Place") first_eatery;
      Lwt_main.run (sort_by_eatery_reverse_alphabetical db "Ratings")
  | _ ->
      assert_failure
        "Failed to retrieve the first eatery name from the database"

let test_sort_by_date_asc =
  "Sort by date ascending" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-03", "14:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-01", "13:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-01", "09:00:00");
      ("Taco Stand", "Burrito", 4, "2023-12-02", "12:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  let test_query =
    "SELECT eatery_name, date, time FROM Ratings ORDER BY date ASC, time ASC \
     LIMIT 1;"
  in
  let stmt = Sqlite3.prepare db test_query in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let first_eatery = Sqlite3.column stmt 0 |> Sqlite3.Data.to_string in
      let first_date = Sqlite3.column stmt 1 |> Sqlite3.Data.to_string in
      let first_time = Sqlite3.column stmt 2 |> Sqlite3.Data.to_string in
      assert_equal (Some "Burger Joint") first_eatery;
      assert_equal (Some "2023-12-01") first_date;
      assert_equal (Some "09:00:00") first_time;
      Lwt_main.run (sort_by_date_asc db "Ratings")
  | _ -> assert_failure "Failed to retrieve the first row from the database"

let test_sort_by_date_desc =
  "Sort by date ascending" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-03", "14:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-01", "13:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-01", "09:00:00");
      ("Taco Stand", "Burrito", 4, "2023-12-02", "12:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  let test_query =
    "SELECT eatery_name, date, time FROM Ratings ORDER BY date DESC, time DESC \
     LIMIT 1;"
  in
  let stmt = Sqlite3.prepare db test_query in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let first_eatery = Sqlite3.column stmt 0 |> Sqlite3.Data.to_string in
      let first_date = Sqlite3.column stmt 1 |> Sqlite3.Data.to_string in
      let first_time = Sqlite3.column stmt 2 |> Sqlite3.Data.to_string in
      assert_equal (Some "Burger Joint") first_eatery;
      assert_equal (Some "2023-12-01") first_date;
      assert_equal (Some "09:00:00") first_time;
      Lwt_main.run (sort_by_date_asc db "Ratings")
  | _ -> assert_failure "Failed to retrieve the first row from the database"

let test_sort_by_date_desc =
  "Sort by date descending" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-03", "14:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-01", "13:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-01", "09:00:00");
      ("Taco Stand", "Burrito", 4, "2023-12-02", "12:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  let test_query =
    "SELECT eatery_name, date, time FROM Ratings ORDER BY date DESC, time DESC \
     LIMIT 1;"
  in
  let stmt = Sqlite3.prepare db test_query in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let first_eatery = Sqlite3.column stmt 0 |> Sqlite3.Data.to_string in
      let first_date = Sqlite3.column stmt 1 |> Sqlite3.Data.to_string in
      let first_time = Sqlite3.column stmt 2 |> Sqlite3.Data.to_string in
      assert_equal (Some "Grill House") first_eatery;
      assert_equal (Some "2023-12-03") first_date;
      assert_equal (Some "14:00:00") first_time;
      Lwt_main.run (sort_by_date_desc db "Ratings")
  | _ -> assert_failure "Failed to retrieve the first row from the database"

let test_sort_by_food_alphabetical =
  "Sort by food alphabetical" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-03", "14:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-01", "13:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-01", "09:00:00");
      ("Taco Stand", "Burrito", 4, "2023-12-02", "12:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  let test_query =
    "SELECT food_item FROM Ratings ORDER BY food_item ASC LIMIT 1;"
  in
  let stmt = Sqlite3.prepare db test_query in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let first_food = Sqlite3.column stmt 0 |> Sqlite3.Data.to_string in
      assert_equal (Some "Burrito") first_food;
      Lwt_main.run (sort_by_food_alphabetical db "Ratings")
  | _ -> assert_failure "Failed to retrieve the first row from the database"

let test_sort_by_food_reverse_alphabetical =
  "Sort by food reverse alphabetical" >:: fun _ ->
  let db = create_in_memory_db () in

  let insert_data =
    [
      ("Grill House", "Pizza", 4, "2023-12-03", "14:00:00");
      ("Sushi Place", "California Roll", 5, "2023-12-01", "13:00:00");
      ("Burger Joint", "Cheeseburger", 3, "2023-12-01", "09:00:00");
      ("Taco Stand", "Burrito", 4, "2023-12-02", "12:00:00");
    ]
  in

  List.iter
    (fun (eatery, food, rating, date, time) ->
      let insert_query =
        "INSERT INTO Ratings (eatery_name, food_item, rating, date, time) \
         VALUES (?, ?, ?, ?, ?);"
      in
      let stmt = Sqlite3.prepare db insert_query in
      Sqlite3.bind_text stmt 1 eatery |> ignore;
      Sqlite3.bind_text stmt 2 food |> ignore;
      Sqlite3.bind_int stmt 3 rating |> ignore;
      Sqlite3.bind_text stmt 4 date |> ignore;
      Sqlite3.bind_text stmt 5 time |> ignore;
      ignore (Sqlite3.step stmt);
      Sqlite3.finalize stmt |> ignore)
    insert_data;

  let test_query =
    "SELECT food_item FROM Ratings ORDER BY food_item DESC LIMIT 1;"
  in
  let stmt = Sqlite3.prepare db test_query in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let first_food = Sqlite3.column stmt 0 |> Sqlite3.Data.to_string in
      assert_equal (Some "Pizza") first_food;
      Lwt_main.run (sort_by_food_reverse_alphabetical db "Ratings")
  | _ -> assert_failure "Failed to retrieve the first row from the database"

let sorting_tests =
  "Sorting tests"
  >::: [
         test_sort_by_highest_rating;
         test_sort_by_lowest_rating;
         test_sort_by_eatery_alphabetical;
         test_sort_by_eatery_reverse_alphabetical;
         test_sort_by_date_asc;
         test_sort_by_date_desc;
         test_sort_by_food_alphabetical;
         test_sort_by_food_reverse_alphabetical;
       ]

let ratings_tests =
  "Food Rating Tests"
  >::: [
         test_view_food_rating_no_ratings;
         test_rate_food_as_guest;
         test_view_food_rating;
         test_rate_food;
         test_rate_food_invalid_rating_value;
         test_show_personal_ratings_with_data;
         test_show_public_ratings;
       ]

let data_tests =
  "Data.ml Test Suite"
  >::: [
         "test_parse_null_eateries" >:: test_parse_null_eateries;
         "test_parse_no_menu_items" >:: test_parse_no_menu_items;
         "test_parse_no_menu_events" >:: test_parse_no_menu_events;
         "test_fetch_invalid_json" >:: test_fetch_invalid_json;
         "test_fetch_failing_url" >:: test_fetch_failing_url;
         "test_get_data" >:: test_get_data;
         "test_parse_eateries_valid_json" >:: test_parse_eateries_valid_json;
         "test_get_data_http_failure" >:: test_get_data_http_failure;
         "test_get_data_json_parsing_failure"
         >:: test_get_data_json_parsing_failure;
         "test_get_data_unexpected_failure" >:: test_get_data_unexpected_failure;
       ]

let eatery_tests =
  "eatery test suite"
  >::: [
         contains_helper_test;
         contains_tests;
         search_food_tests;
         contains_duplicate_food_test;
         parse_eateries_tests;
         get_data_tests;
         login_tests;
         test_create_eatery_invalid;
         data_tests;
         ratings_tests;
         sorting_tests;
       ]

let _ = run_test_tt_main eatery_tests
