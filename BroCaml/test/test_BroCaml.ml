open OUnit2
open Lwt
open Lwt.Infix
open Cohttp_lwt_unix
open Yojson
open BroCaml.User
open BroCaml.Data
open BroCaml.Login

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

let test_finalize_statement_rc_ok _ =
  let db = setup_test_db () in

  let mock_finalize stmt db = ignore (Sqlite3.finalize stmt) in

  assert_bool "Rc.OK should succeed without exceptions"
    (try
       create_user ~finalize:mock_finalize db "test_user" "password";
       true
     with _ -> false)

let login_tests =
  "login tests"
  >::: [
         "test_user_exists" >:: test_user_exists;
         "test_validate_user" >:: test_validate_user;
         "test_validate_special_chars" >:: test_validate_special_chars;
         "test Rc.OK" >:: test_finalize_statement_rc_ok;
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
       ]

let _ = run_test_tt_main eatery_tests
