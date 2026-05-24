type t =
  | Tool_call of Tool_call.t
  | Tool_calls of Tool_call.t list
  | Final_answer of { answer : string }

val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
