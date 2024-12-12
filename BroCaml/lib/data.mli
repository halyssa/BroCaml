type eatery
(** abstract type [eatery] stores a name and menu*)

val fetch_json : string -> Yojson.Safe.t Lwt.t
(** [fetch_json] is the json of [url]. Raises:
    - Fails with "HTTP request failed with error" if the HTTP request is
      unsuccessful.
    - Fails with "JSON parsing error" if the response body cannot be parsed as
      JSON.
    - Fails with "Unexpected error: <msg>" if an unexpected error occurs during
      the request or parsing.*)

val parse_eateries : Yojson.Safe.t -> User.eatery list Lwt.t
(** [parse_eateries] parses the given JSON object [json] and returns an Lwt list
    of eateries. *)

val get_data : unit -> User.eatery list Lwt.t
(** [get_data] returns a Lwt list of eateries from
    "https://now.dining.cornell.edu/api/1.0/dining/eateries.json" *)
