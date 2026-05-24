type t =
  | User_message of { content : string }
  | Model_response of { action : Model_action.t }
  | Tool_call of Tool_call.t
  | Tool_result of Tool_result.t
  | State_transition of { from_state : Agent_state.t; to_state : Agent_state.t }

val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
