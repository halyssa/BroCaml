open BroCaml.Data
open Lwt
open Cohttp_lwt_unix

let main () =
  let%lwt eateries = get_data () in

  List.iter
    (fun eatery ->
      Printf.printf "Eatery: %s\n" eatery.name;
      Printf.printf "Menu Items:\n";
      List.iter (fun item -> Printf.printf "  - %s\n" item) eatery.menu;
      Printf.printf "\n" (* For better separation between eateries *))
    eateries;
  Lwt.return_unit

let () = Lwt_main.run (main ())
