open! Base

(* The agent's state, defined as a fold over the event log. This is the
   event-sourced core: live state is not the source of truth, the log is, and
   [replay] reconstructs the state by reducing the events. *)
type t = { turns : Llm.turn list; agent_state : Agent_state.t; steps : int }

let empty = { turns = []; agent_state = Agent_state.Initializing; steps = 0 }
let append_turn st turn = { st with turns = st.turns @ [ turn ] }
let is_tool_result = function Llm.Tool_result _ -> true | _ -> false

let append_tool_result st block =
  match List.rev st.turns with
  | ({ role = Llm.User; content } as last) :: rev_prefix
    when (not (List.is_empty content)) && List.for_all content ~f:is_tool_result
    ->
      {
        st with
        turns =
          List.rev ({ last with content = content @ [ block ] } :: rev_prefix);
      }
  | _ -> append_turn st { role = Llm.User; content = [ block ] }

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
  | User_message { content } -> append_turn st (Llm.user content)
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
      append_tool_result st
        (Llm.Tool_result { id; content = Tool_result.to_observation result })
  | Tool_result result ->
      append_turn st (Llm.user (Tool_result.to_observation result))
  | Context_compacted { summary; recent } ->
      {
        st with
        turns =
          {
            role = Llm.User;
            content =
              [ Llm.Text ("[Earlier conversation summary]\n" ^ summary) ];
          }
          :: recent;
      }
  | Workspace_snapshot _ | Turn_completed _ -> st
  | State_transition { to_state; _ } -> { st with agent_state = to_state }
  | Tool_call _ | Policy_decision _ | Plan_updated _ | Graph_event _ -> st

let replay events = List.fold events ~init:empty ~f:reduce
let turns t = t.turns
let messages t = List.map t.turns ~f:Llm.turn_to_message
let agent_state t = t.agent_state
let steps t = t.steps
