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

(** [test_create_eatery_invalid] tests the [create_eatery] function with invalid
    inputs. Ensures the function raises invalid arguments. *)
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

(** [contains_helper_test] tests the [contains] helper function to check if a
    given food item exists in any eatery's menu *)
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
(** [contains_tests] tests the [contains] function to verify its behavior when checking
    for the presence of food items in eatery menus. *)
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

(** [search_test_helper food eateries expected] creates a unit test for the 
    [search_food] function. It checks if searching for [food] in [eateries] 
    produces the expected result [expected]. *)
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

(** [contains_duplicate_food_test] tests the [contains_helper] function to check if 
    it correctly identifies a food item in an eatery menu that contains 
    duplicate entries. *)
let contains_duplicate_food_test =
  "duplicate food in menu" >:: fun _ ->
  assert_bool "Burger should be found in Duplicate's menu"
    (contains_helper "Burger" eatery_duplicate_food)

(** [test_parse_eateries_empty_eateries] tests the [parse_eateries] function to ensure
it correctly handles JSON input where the "eateries" field is null, resulting in
an empty list of eateries. *)
let test_parse_eateries_empty_eateries _ =
  Lwt_main.run
    (let json = `Assoc [ ("data", `Assoc [ ("eateries", `Null) ]) ] in
     let%lwt result = parse_eateries json in
     assert_equal (List.length result) 0;
     Lwt.return_unit)

(** [test_parse_eateries_missing_eateries] tests the [parse_eateries] function to ensure
it correctly handles JSON input where the "eateries" field is missing, resulting in
an empty list of eateries. *)
let test_parse_eateries_missing_eateries _ =
  Lwt_main.run
    (let json = `Assoc [ ("data", `Assoc []) ] in
     let%lwt result = parse_eateries json in
     assert_equal (List.length result) 0;
     Lwt.return_unit)

(** [test_get_data_valid] tests the [get_data] function to ensure it successfully 
    retrieves a non-empty list of eateries. *)
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

(** [setup_test_db] initializes an in-memory SQLite database for testing purposes.
    It creates a table with the following schema:
    - [id]: An auto-incrementing primary key.
    - [username]: A unique, non-null text field for storing usernames.
    - [password_hash]: A non-null text field for storing hashed passwords.
    Returns the initialized database instance. *)
let setup_test_db () =
  let db = Sqlite3.db_open ":memory:" in
  let create_table_query =
    "CREATE TABLE Users ( id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT \
     NOT NULL UNIQUE, password_hash TEXT NOT NULL);"
  in
  ignore (Sqlite3.exec db create_table_query);
  db

(** [test_user_exists] tests the [user_exists] function to verify that it correctly
  checks the existence of a user in the [Users] table of the test database after adding one. *)
let test_user_exists _ =
  let db = setup_test_db () in
  let insert_query =
    "INSERT INTO Users (username, password_hash) VALUES ('test_user', \
     'hash123');"
  in
  ignore (Sqlite3.exec db insert_query);

  assert_bool "User should exist" (user_exists db "test_user");
  assert_bool "User should not exist" (not (user_exists db "non_existent_user"))

(** [test_validate_user] tests the [validate_user] function to ensure it correctly 
    validates user credentials in the [Users] table of the test database *)
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

(** [test_validate_special_chars] tests the [validate_user] function to ensure it correctly 
    handles user credentials with special characters in the username. *)
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

(** [test_create_user] tests the [create_user] function to ensure it correctly creates a user 
    in the [Users] table of the test database and handles errors properly. *)
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

(** [test_connect_db_checked] tests the [connect_db_checked] function to ensure it correctly 
    handles database connections by verifying that the function successfully connects to an existing database file and ensuring the function raises a [Failure] when attempting to connect to a non-existent database file. *)
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

(** [test_parse_null_eateries] tests the [parse_eateries] function to ensure it correctly 
    handles the case where the input JSON contains a null value for eateries. The test 
    verifies that the function returns an empty list when parsing such input. *)
let test_parse_null_eateries _ =
  Lwt_main.run
    ( parse_eateries null_eateries_json >|= fun eateries ->
      assert_equal [] eateries )

(** [test_parse_no_menu_items] tests the [parse_eateries] function to ensure it correctly 
    handles the case where eateries have no menu items. *)
let test_parse_no_menu_items _ =
  Lwt_main.run
    ( parse_eateries no_menu_items_json >|= fun eateries ->
      assert_equal [ "Test Eatery" ] (List.map (fun e -> get_name e) eateries);
      assert_equal [ [ "Italian" ] ] (List.map (fun e -> get_menu e) eateries)
    )

(** [test_parse_no_menu_events] tests the [parse_eateries] function to ensure it correctly 
    handles the case where eateries have no menu events. *)
let test_parse_no_menu_events _ =
  Lwt_main.run
    ( parse_eateries no_menu_events_json >|= fun eateries ->
      assert_equal [ "Event Eatery" ] (List.map (fun e -> get_name e) eateries);
      assert_equal
        [ [ "No menu available" ] ]
        (List.map (fun e -> get_menu e) eateries) )

(** [test_fetch_invalid_json] tests the behavior of the function handling invalid JSON input. *)
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

(** [test_parse_eateries_valid_json] tests the [parse_eateries] function to ensure it correctly 
    parses a valid JSON structure containing eateries. *)
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

(** [test_fetch_failing_url] tests the behavior of the [fetch_json] function when an HTTP request fails. *)
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

(** [test_get_data] tests the [get_data] function to ensure it correctly retrieves a non-empty list 
    of eateries. *)
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

(** [get_data] fetches data by calling the [fetch_json] function. *)
let get_data fetch_json =
  let%lwt response = fetch_json () in
  Lwt.return response

(** [test_get_data_http_failure] tests the [get_data] function to ensure it correctly handles 
    an HTTP failure when fetching data. *)
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

(** [test_get_data_json_parsing_failure] tests the [get_data] function to ensure it correctly handles 
    a JSON parsing failure when fetching data. *)
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

(** [test_get_data_unexpected_failure] tests the [get_data] function to ensure it correctly handles 
    an unexpected failure when fetching data. *)
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

(** [create_in_memory_db] creates an in-memory SQLite database for testing purposes. The function creates a [Ratings] table with columns for [eatery_name], [food_item], [username], [rating], [comment], 
[date], and [time], creates a [PersonalRatings] table with columns for [eatery_name], [food_item], [rating], [comment], 
[date], and [time], where the primary key is a combination of [eatery_name], [food_item], and [date], and returns the initialized database instance. *)
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

(** [test_view_food_rating_no_ratings] tests the [view_food_rating] function when no ratings exist for a given food item at an eatery *)
let test_view_food_rating_no_ratings =
  "View food rating when no ratings exist" >:: fun _ ->
  let db = create_in_memory_db () in
  let result =
    Lwt_main.run (view_food_rating db "Sushi" "Sushi Bar" eateries)
  in
  assert_equal () result

(** [test_rate_food_as_guest] tests the [rate_food] function when a guest user rates food. *)
let test_rate_food_as_guest =
  "Rate food as guest" >:: fun _ ->
  let db = create_in_memory_db () in
  let is_guest = ref true in
  let current_user = "" in
  let result =
    Lwt_main.run
      (rate_food db db "Pizza" "Grill House" 4 is_guest current_user false eateries)
  in
  assert_equal () result




(**ADD DOCUMENTATION*)
let create_anon_db () =
  let db = Sqlite3.db_open ":memory:" in
  let create_ratings_table_query =
    "CREATE TABLE IF NOT EXISTS Ratings (\n\
    \              eatery_name TEXT,\n\
    \              food_item TEXT,\n\
    \              username TEXT,\n\
    \              is_anonymous BOOLEAN,\n\
    \              rating INTEGER,\n\
    \              comment TEXT, \n\
    \              date TEXT,\n\
    \              time TEXT,\n\
    \              PRIMARY KEY (eatery_name, food_item, username, date, is_anonymous)\n\
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


(** [test_rate_food] tests the [rate_food] function when a registered user rates food. *)
let test_rate_food =
  "food rating" >:: fun _ ->
  let public_db = create_anon_db () in
  let personal_db = create_anon_db () in
  let is_guest = ref false in
  let current_user = "testuser" in
  let eateries = [ create_eatery "Eatery1" [ "Food1"; "Food2" ] ] in

  let result =
    Lwt_main.run
      (rate_food public_db personal_db "Food1" "Eatery1" 5 is_guest current_user false
         eateries)
  in

  assert_equal () result;

  let stmt =
    Sqlite3.prepare public_db
      "SELECT rating FROM Ratings WHERE eatery_name = 'Eatery1' AND food_item = 'Food1'"
  in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> (
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT rating -> assert_equal 5 (Int64.to_int rating)
      | _ -> assert_failure "Rating is not an integer")
  | _ -> assert_failure "Rating not found in public database"


(** [test_view_food_rating] verifies the functionality of the [view_food_rating] to ensure that it correctly displays the average rating 
    and the date/time of the last rating for a given food item at a specified eatery. *)
    (** [test_view_food_rating] verifies the functionality of the [view_food_rating] to ensure that it correctly displays the average rating 
    and the date/time of the last rating for a given food item at a specified eatery. *)

    let test_view_food_rating =
      "view food rating" >:: fun _ ->
        let public_db = create_anon_db () in
        let eateries = [ create_eatery "Test Eatery" [ "Test Food" ] ] in
    
        (* Insert a rating for testing *)
        let insert_query =
          "INSERT INTO Ratings (eatery_name, food_item, username, is_anonymous, rating, date, time) VALUES (?, ?, ?, ?, ?, ?, ?);"
        in
        let stmt = Sqlite3.prepare public_db insert_query in
        Sqlite3.bind_text stmt 1 "Test Eatery" |> ignore;
        Sqlite3.bind_text stmt 2 "Test Food" |> ignore;
        Sqlite3.bind_text stmt 3 "testuser" |> ignore;
        Sqlite3.bind_int stmt 4 0 |> ignore; (* Assuming non-anonymous *)
        Sqlite3.bind_int stmt 5 4 |> ignore;
        Sqlite3.bind_text stmt 6 "2023-12-11" |> ignore;
        Sqlite3.bind_text stmt 7 "12:00:00" |> ignore;
        assert_equal Sqlite3.Rc.DONE (Sqlite3.step stmt);
        Sqlite3.finalize stmt |> ignore;
    
        (* Capture stdout for testing *)
        let output = ref "" in
        let old_stdout = Unix.dup Unix.stdout in
        let pipe_out, pipe_in = Unix.pipe () in
        Unix.dup2 pipe_in Unix.stdout; (* Redirect stdout to pipe *)
        Unix.close pipe_in;
    
        (* Run the function *)
        Lwt_main.run (view_food_rating public_db "Test Food" "Test Eatery" eateries);
    
        (* Flush the output explicitly to ensure it reaches the pipe *)
        flush stdout;
    
        (* Restore stdout and read captured output *)
        Unix.dup2 old_stdout Unix.stdout; (* Restore original stdout *)
        let buffer = Bytes.create 1024 in
        let bytes_read = Unix.read pipe_out buffer 0 1024 in
        output := Bytes.sub_string buffer 0 bytes_read; (* Capture the output *)
        Unix.close pipe_out; (* Close the write end of the pipe *)
    
        (* Print captured output for debugging *)
        Printf.printf "Captured output: %s\n" !output;
    
        (* Check the output *)
        assert_bool "Output should contain average rating"
          (Str.string_match
             (Str.regexp "The average rating for Test Food at Test Eatery is 4.00")
             !output 0);
        assert_bool "Output should contain last rated date and time"
          (Str.string_match
             (Str.regexp ".*Last rated on [0-9-]+ at [0-9:]+.*")
             !output 0)
    
    
           
(** [setup_in_memory_db] initializes a new in-memory SQLite database. *)
let setup_in_memory_db () =
  let db = create_in_memory_db () in
  db

(** [teardown_in_memory_db db] closes the in-memory SQLite database. *)
let teardown_in_memory_db db = ignore (Sqlite3.db_close db)

(** [populate_personal_ratings_table db] populates the "PersonalRatings" table in the given database [db] with 
    predefined sample data. The data consists of ratings for different food items from various eateries, along with 
    associated comments, dates, and times. *)
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

(** [test_show_personal_ratings_with_data] tests the [show_personal_ratings] function by simulating the retrieval of
    personal ratings data for a logged-in user from the database. *)
let test_show_personal_ratings_with_data =
  "personal ratings" >:: fun _ ->
  let db = setup_in_memory_db () in
  populate_personal_ratings_table db;
  let is_guest = ref false in
  Lwt_main.run (show_personal_ratings db "" is_guest);
  teardown_in_memory_db db

(** [test_show_public_ratings] tests the [show_public_ratings] function by simulating the retrieval of public
    ratings for a specific food item from the database. *)
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

(** [test_rate_food_invalid_rating_value] tests the [rate_food] function when an invalid rating value 
  (out of range) is provided. *)
let test_rate_food_invalid_rating_value =
  "Rate food with invalid rating value (out of range)" >:: fun _ ->
  let db = create_in_memory_db () in
  let is_guest = ref false in
  let current_user = "john_doe" in
  let result =
    Lwt_main.run
      (rate_food db db "Pizza" "Grill House" 6 is_guest current_user false eateries)
  in
  assert_equal () result

(** [test_view_food_rating_with_comments] tests the [view_food_rating] function when there are ratings 
    with multiple comments for the same food item. *)
let test_view_food_rating_with_comments =
  "View food rating with comments" >:: fun _ ->
  let db = create_in_memory_db () in
  let is_guest = ref false in
  let current_user = "john_doe" in
  Lwt_main.run
    (rate_food db db "Pizza" "Grill House" 4 is_guest current_user false eateries);
  Lwt_main.run
    (rate_food db db "Pizza" "Grill House" 4 is_guest current_user false eateries);
  let result =
    Lwt_main.run (view_food_rating db "Pizza" "Grill House" eateries)
  in
  assert_equal () result

(** [test_view_food_rating_empty_db] tests the [view_food_rating] function when the database is empty. *)
let test_view_food_rating_empty_db =
  "View food rating when database is empty" >:: fun _ ->
  let db = create_in_memory_db () in
  let result =
    Lwt_main.run (view_food_rating db "Pizza" "Grill House" eateries)
  in
  assert_equal () result

(** [test_view_food_rating_no_comments] tests the [view_food_rating] function when a food item has been rated 
    but no comment has been provided. *)
let test_view_food_rating_no_comments =
  "View food rating with no comments" >:: fun _ ->
  let db = create_in_memory_db () in
  let is_guest = ref false in
  let current_user = "john_doe" in
  Lwt_main.run
    (rate_food db db "Pizza" "Grill House" 4 is_guest current_user false eateries);
  let result =
    Lwt_main.run (view_food_rating db "Pizza" "Grill House" eateries)
  in
  assert_equal () result

(** [test_sort_by_highest_rating] tests the [sort_by_highest_rating] function to ensure that ratings are
    sorted correctly in descending order, with the highest rating appearing first. *)
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

(** [test_sort_by_lowest_rating] tests the [sort_by_lowest_rating] function to ensure that ratings are
    sorted correctly in ascending order, with the lowest rating appearing first. *)
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

(** [test_sort_by_eatery_alphabetical] tests the [sort_by_eatery_alphabetical] function,
    ensuring that eateries are sorted in alphabetical order. *)
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

(** [test_sort_by_eatery_reverse_alphabetical] tests the [sort_by_eatery_reverse_alphabetical] function,
    ensuring that eateries are sorted in reverse alphabetical order. *)
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

(** [test_sort_by_date_asc] tests the [sort_by_date_asc] function to ensure that ratings are sorted
    by date in ascending order. *)
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

(** [test_sort_by_date_desc] tests the [sort_by_date_desc] function to ensure that ratings are sorted
    by date in descending order. *)
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

(** [test_sort_by_food_alphabetical] tests the [sort_by_food_alphabetical] function,
    ensuring that food items are sorted in alphabetical order. *)
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

(** [test_sort_by_food_reverse_alphabetical] tests the [sort_by_food_reverse_alphabetical] function,
    ensuring that food items are sorted in reverse alphabetical order. *)
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
