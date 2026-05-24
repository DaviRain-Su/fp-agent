open! Base

type t =
  | Initializing
  | Waiting_for_model
  | Executing_tool
  | Observing_result
  | Completed
  | Failed
[@@deriving yojson_of, of_yojson]

let to_string = function
  | Initializing -> "Initializing"
  | Waiting_for_model -> "Waiting_for_model"
  | Executing_tool -> "Executing_tool"
  | Observing_result -> "Observing_result"
  | Completed -> "Completed"
  | Failed -> "Failed"

let pp fmt t = Stdlib.Format.pp_print_string fmt (to_string t)
let equal (a : t) (b : t) = Poly.equal a b
let to_yojson = yojson_of_t

let of_yojson json =
  match t_of_yojson json with
  | t -> Ok t
  | exception exn -> Error (Exn.to_string exn)

(* Allowed transitions of the agent loop state machine. Any pair not listed
   here is rejected so that illegal flow is caught in tests rather than at
   runtime. *)
let transition from_state to_state =
  let ok = Ok to_state in
  match (from_state, to_state) with
  | Initializing, Waiting_for_model
  | Initializing, Failed
  | Waiting_for_model, Executing_tool
  | Waiting_for_model, Completed
  | Waiting_for_model, Failed
  | Executing_tool, Observing_result
  | Executing_tool, Failed
  | Observing_result, Waiting_for_model
  | Observing_result, Completed
  | Observing_result, Failed ->
      ok
  | _ ->
      Error
        ("invalid transition: " ^ to_string from_state ^ " -> "
       ^ to_string to_state)
