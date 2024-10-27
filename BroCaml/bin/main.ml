open BroCaml.Data
open BroCaml.User
open Lwt
open Cohttp_lwt_unix

let run_search_food food eateries =
  let result = search_food food eateries in
  if result = [] then
    Printf.printf "Unfortunately, %s is not served in the eateries today. " food
  else List.iter (fun p -> Printf.printf "%s\n" p) result

let run_contains food eateries =
  let result = contains food eateries in
  match result with
  | true ->
      Printf.printf
        "%s is served in the eateries today! Would you like to see where it is \
         served (y/n)? "
        food;
      let response = read_line () in
      if response = "y" then run_search_food food eateries
  | false ->
      Printf.printf "Unfortunately, %s is not served in the eateries today. "
        food

let quit_program () =
  print_endline "Thanks for using FindMyFood!";
  exit 0

let rec prompt_user (eateries : eatery list) =
  print_endline "Please choose a number that best fits your desired action";
  print_endline "1. Check if a <food> is served at any of the eateries";
  (* print_endline "2. Check if a <food> is served at a specific <eatery>"; *)
  print_endline "2. Search where a <food> is being served";
  print_endline "3. Quit";
  let action = read_line () in
  let parts = String.split_on_char ' ' action in
  match parts with
  | [ "1"; food ] ->
      run_contains food eateries;
      prompt_user eateries
  (* | ["2"; food; eatery] -> contains_helper food (create_eatery eatery); *)
  | [ "2"; food ] ->
      run_search_food food eateries;
      prompt_user eateries
  | [ "3" ] -> quit_program ()
  | _ ->
      print_endline
        "That action does not exist or is incorrectly formatted.";
      prompt_user eateries

let main () =
  print_endline "Hey! Welcome to FindMyFood";
  let%lwt eateries = get_data () in
  let () = prompt_user eateries in
  Lwt.return_unit

let () = Lwt_main.run (main ())
