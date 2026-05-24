type t =
  | User_message of { content : string }
  | Model_delta of { content : string }
  | Assistant_message of { content : Llm.content list; usage : Llm.usage }
  | Model_response of { action : Model_action.t }
  | Policy_decision of { tool_call : Tool_call.t; permission : Permission.t }
  | Tool_call of Tool_call.t
  | Tool_result_message of { id : string; result : Tool_result.t }
  | Tool_result of Tool_result.t
  | Graph_event of Graph_event.t
  | State_transition of { from_state : Agent_state.t; to_state : Agent_state.t }

val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result

val to_display : t -> string option
(** A concise one-line rendering for live display, or [None] to omit the event
    from the live view (it is still written to the event log). *)

val describe_tool : Tool_call.t -> string
(** A short human-readable description of a tool call, e.g. "read_file a.ml". *)
