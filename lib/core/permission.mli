type t = Allow | Deny of string | Ask_user of string

val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
val to_string : t -> string
val is_allow : t -> bool
val equal : t -> t -> bool
val pp : Stdlib.Format.formatter -> t -> unit
