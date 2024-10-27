open OUnit2
open BroCaml.User

let eatery1 = create_eatery "Bistro Cafe" [ "Pasta"; "Salad"; "Soup" ]
let eatery2 = create_eatery "Deli Delight" [ "Sandwich"; "Soup"; "Juice" ]
let eatery3 = create_eatery "Gourmet Grill" [ "Burger"; "Fries"; "Salad" ]
let eateries : eatery list = [ eatery1; eatery2; eatery3 ]

let eateries =
  [
    create_eatery "Bistro Cafe" [ "Pasta"; "Salad"; "Soup" ];
    create_eatery "Deli Delight" [ "Sandwich"; "Soup"; "Juice" ];
    create_eatery "Gourmet Grill" [ "Burger"; "Fries"; "Salad" ];
  ]

let contains_helper_test =
  "contains helper tests"
  >::: [
         ( "test_pasta_in_bistro" >:: fun _ ->
           assert_bool "Pasta should be in Bistro Cafe's menu"
             (contains_helper "Pasta" eatery1) );
         ( "test_burger_not_in_bistro" >:: fun _ ->
           assert_bool "Burger should not be in Bistro Cafe's menu"
             (not (contains_helper "Burger" eatery1)) );
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

(** [search_test_helper food eateries expected] is a helper function for testing
    the [search_food] function. It checks whether the result of
    [search_food food eateries] matches the expected list of eatery names. *)
let search_test_helper food eateries (expected : string list) bool_expected =
  "" >:: fun _ ->
  let result = search_food food eateries in
  assert_equal bool_expected (result = expected)

let search_food_tests =
  "serach_food function tests"
  >::: [
         search_test_helper "Pasta" eateries [ "Bistro Cafe" ] true;
         search_test_helper "Salad" eateries
           [ "Bistro Cafe"; "Gourmet Grill" ]
           true;
         search_test_helper "Pizza" eateries [] true;
       ]

let tests =
  "test suite" >::: [ contains_helper_test; contains_tests; search_food_tests ]

let _ = run_test_tt_main tests
