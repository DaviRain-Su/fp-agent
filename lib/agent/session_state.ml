open! Base

(* The agent's state, defined as a fold over the event log. This is the
   event-sourced core: live state is not the source of truth, the log is, and
   [replay] reconstructs the state by reducing the events. *)
type t = { messages : Message.t list; agent_state : Agent_state.t; steps : int }

let empty = { messages = []; agent_state = Agent_state.Initializing; steps = 0 }

let reduce (st : t) (event : Event.t) =
  match event with
  | User_message { content } ->
      { st with messages = st.messages @ [ Message.user content ] }
  | Model_response { action } ->
      {
        st with
        messages =
          st.messages
          @ [
              Message.assistant
                (Yojson.Safe.to_string (Model_action.to_yojson action));
            ];
        steps = st.steps + 1;
      }
  | Tool_result result ->
      {
        st with
        messages =
          st.messages @ [ Message.user (Tool_result.to_observation result) ];
      }
  | State_transition { to_state; _ } -> { st with agent_state = to_state }
  | Tool_call _ | Policy_decision _ -> st

let replay events = List.fold events ~init:empty ~f:reduce
let messages t = t.messages
let agent_state t = t.agent_state
let steps t = t.steps
