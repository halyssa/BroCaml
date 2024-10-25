(* open Lwt
open Cohttp_lwt_unix
open Yojson.Safe *)

type eatery = {
  name : string;
  menu : string list;
}
