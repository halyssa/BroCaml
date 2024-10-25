open BroCaml.Data
open Lwt
open Cohttp_lwt_unix

let main () =
  let%lwt eateries = get_data () in

  List.iter (fun eatery ->
    Printf.printf "Eatery: %s\n" eatery.name
  ) eateries;
  Lwt.return_unit

let () = Lwt_main.run (main ())
  