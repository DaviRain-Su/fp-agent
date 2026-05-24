type t = Success of { output : string } | Error of { message : string }

val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result

val to_observation : t -> string
(** The observation text fed back to the model after a tool runs. *)
