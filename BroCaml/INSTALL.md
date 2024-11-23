opam install lwt cohttp-lwt-unix yojson lwt_ppx
opam install lwt_ssl
cd BroCaml
dune build

brew install sqlite3
opam install sqlite3