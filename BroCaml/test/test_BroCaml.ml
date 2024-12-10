open OUnit2
open Lwt
open Lwt.Infix
open Cohttp_lwt_unix
open Yojson
open BroCaml.User
open BroCaml.Data
open BroCaml.Login

(* Sample eateries for testing *)
let eatery1 = create_eatery "Bistro Cafe" [ "Pasta"; "Salad"; "Soup" ]
let eatery2 = create_eatery "Deli Delight" [ "Sandwich"; "Soup"; "Juice" ]
let eatery3 = create_eatery "Gourmet Grill" [ "Burger"; "Fries"; "Salad" ]
let eateries = [ eatery1; eatery2; eatery3 ]

(* Test contains_helper *)
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

(* Test contains function *)
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

(* Test search_food *)
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

(* Test duplicate food in menu *)
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

(*testing for login*)
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

let login_tests =
  "login tests"
  >::: [
         "test_user_exists" >:: test_user_exists;
         "test_validate_user" >:: test_validate_user;
         "test_validate_special_chars" >:: test_validate_special_chars;
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
       ]

let _ = run_test_tt_main eatery_tests
