open! Base

(* The model-visible conversation for resuming a run, derived from the event
   log via the event-sourced {!Session_state}. *)
let of_session ~session_dir =
  Result.map (Journal.read ~session_dir) ~f:(fun events ->
      Session_state.turns (Session_state.replay events))

let messages_of_session ~session_dir =
  Result.map (of_session ~session_dir) ~f:(List.map ~f:Llm.turn_to_message)
