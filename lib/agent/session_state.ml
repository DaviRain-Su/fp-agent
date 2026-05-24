open! Base

(* The agent's state, defined as a fold over the event log. This is the
   event-sourced core: live state is not the source of truth, the log is, and
   [replay] reconstructs the state by reducing the events. *)
type t = { turns : Llm.turn list; agent_state : Agent_state.t; steps : int }

let empty = { turns = []; agent_state = Agent_state.Initializing; steps = 0 }

let content_of_action = function
  | Model_action.Final_answer { answer } -> [ Llm.Text answer ]
  | Tool_call tc ->
      [ Llm.Tool_use { id = "legacy_tool_0"; name = tc.name; input = tc.args } ]
  | Tool_calls calls ->
      List.mapi calls ~f:(fun i (tc : Tool_call.t) ->
          Llm.Tool_use
            {
              id = Printf.sprintf "legacy_tool_%d" i;
              name = tc.name;
              input = tc.args;
            })

let reduce (st : t) (event : Event.t) =
  match event with
  | User_message { content } ->
      { st with turns = st.turns @ [ Llm.user content ] }
  | Model_delta _ -> st
  | Assistant_message { content; _ } ->
      {
        st with
        turns = st.turns @ [ Llm.assistant content ];
        steps = st.steps + 1;
      }
  | Model_response { action } ->
      {
        st with
        turns = st.turns @ [ Llm.assistant (content_of_action action) ];
        steps = st.steps + 1;
      }
  | Tool_result_message { id; result } ->
      {
        st with
        turns =
          st.turns
          @ [
              {
                role = Llm.User;
                content =
                  [
                    Llm.Tool_result
                      { id; content = Tool_result.to_observation result };
                  ];
              };
            ];
      }
  | Tool_result result ->
      {
        st with
        turns = st.turns @ [ Llm.user (Tool_result.to_observation result) ];
      }
  | State_transition { to_state; _ } -> { st with agent_state = to_state }
  | Tool_call _ | Policy_decision _ | Graph_event _ -> st

let replay events = List.fold events ~init:empty ~f:reduce
let turns t = t.turns
let messages t = List.map t.turns ~f:Llm.turn_to_message
let agent_state t = t.agent_state
let steps t = t.steps
