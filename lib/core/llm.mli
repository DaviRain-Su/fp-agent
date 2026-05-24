type content =
  | Text of string
  | Thinking of { text : string; signature : string }
  | Tool_use of { id : string; name : string; input : Yojson.Safe.t }
  | Tool_result of { id : string; content : string }

type role = User | Assistant
type turn = { role : role; content : content list }
type usage = { input_tokens : int; output_tokens : int }

val zero_usage : usage
val text : string -> content
val user : string -> turn
val assistant : content list -> turn

val tool_uses : content list -> (string * Tool_call.t) list
(** Extract tool calls while preserving provider tool ids. *)

val final_text : content list -> string option
(** Concatenate text blocks when the assistant did not request tools. *)

val content_to_json : content -> Yojson.Safe.t
val content_of_json : Yojson.Safe.t -> content
val turn_to_json : turn -> Yojson.Safe.t
val turn_of_json : Yojson.Safe.t -> turn
val usage_to_json : usage -> Yojson.Safe.t
val usage_of_json : Yojson.Safe.t -> usage
val yojson_of_content : content -> Yojson.Safe.t
val content_of_yojson : Yojson.Safe.t -> content
val yojson_of_role : role -> Yojson.Safe.t
val role_of_yojson : Yojson.Safe.t -> role
val yojson_of_turn : turn -> Yojson.Safe.t
val turn_of_yojson : Yojson.Safe.t -> turn
val yojson_of_usage : usage -> Yojson.Safe.t
val usage_of_yojson : Yojson.Safe.t -> usage

val turn_to_message : turn -> Message.t
(** Lossy text view for transcript/UI compatibility. *)
