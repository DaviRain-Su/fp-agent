open! Base

(* The model-visible conversation for resuming a run, derived from the event
   log via the event-sourced {!Session_state}. *)
let of_session ~session_dir =
  Result.map (Journal.read ~session_dir) ~f:(fun events ->
      Session_state.messages (Session_state.replay events))
