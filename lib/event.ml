open! Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t =
  | User_message of { content : string }
  | Model_response of { action : Model_action.t }
  | Tool_call of Tool_call.t
  | Tool_result of Tool_result.t
  | State_transition of { from_state : Agent_state.t; to_state : Agent_state.t }
[@@deriving yojson_of, of_yojson]

let to_yojson = yojson_of_t

let of_yojson json =
  match t_of_yojson json with
  | t -> Ok t
  | exception exn -> Error (Exn.to_string exn)
