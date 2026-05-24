type t =
  | Initializing
  | Waiting_for_model
  | Executing_tool
  | Observing_result
  | Completed
  | Failed

val to_string : t -> string
val pp : Stdlib.Format.formatter -> t -> unit
val equal : t -> t -> bool
val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result

val transition : t -> t -> (t, string) result
(** [transition from to_] returns [Ok to_] if the state machine permits the
    transition, otherwise [Error reason]. *)
