open OUnit2
(* open BroCaml.User *)

type eatery = { name: string; menu: string list }

(* let eatery1 = { name = "Bistro Cafe"; menu = ["Pasta"; "Salad"; "Soup"] }
let eatery2 = { name = "Deli Delight"; menu = ["Sandwich"; "Soup"; "Juice"] }
let eatery3 = { name = "Gourmet Grill"; menu = ["Burger"; "Fries"; "Salad"] }
let eateries = [eatery1; eatery2; eatery3] *)

(* let contains_tests = "contains function tests" >::: [
  "food in menu, exact match" >:: (fun _ ->
    assert_bool "Expected to find 'Pasta'" (contains "Pasta" eatery1.menu));
  "food in menu, case-insensitive match" >:: (fun _ ->
    assert_bool "Expected to find 'burger' (case insensitive)" (contains "burger" eatery3.menu));
  "food not in menu" >:: (fun _ ->
    assert_bool "Expected not to find 'Pizza'" (not (contains "Pizza" eatery2.menu)));
] *)

let tests = "test suite" >::: [
  "a trivial test" >:: (fun _ -> assert_equal 0 0)
]

let _ = run_test_tt_main tests