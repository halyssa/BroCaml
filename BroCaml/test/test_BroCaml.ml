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

let tests =
  "test suite" >::: [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]

let _ = run_test_tt_main tests
