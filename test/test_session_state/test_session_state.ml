open! Base
open Fp_agent

let events =
  [
    Event.User_message { content = "do it" };
    Event.State_transition
      {
        from_state = Agent_state.Initializing;
        to_state = Agent_state.Waiting_for_model;
      };
    Event.Model_response
      { action = Model_action.Tool_call (Tool_call.Read_file { path = "f" }) };
    Event.Tool_call (Tool_call.Read_file { path = "f" });
    Event.Policy_decision
      {
        tool_call = Tool_call.Read_file { path = "f" };
        permission = Permission.Allow;
      };
    Event.Tool_result (Tool_result.Success { output = "contents" });
    Event.State_transition
      {
        from_state = Agent_state.Observing_result;
        to_state = Agent_state.Waiting_for_model;
      };
  ]

let test_replay () =
  let st = Session_state.replay events in
  Alcotest.(check int) "one model step" 1 (Session_state.steps st);
  Alcotest.(check bool)
    "agent_state from last transition" true
    (Agent_state.equal
       (Session_state.agent_state st)
       Agent_state.Waiting_for_model);
  let roles =
    List.map (Session_state.messages st) ~f:(fun m -> m.Message.role)
  in
  Alcotest.(check (list string))
    "messages: user, assistant, observation"
    [ "user"; "assistant"; "user" ]
    roles

let test_incremental_matches_replay () =
  (* reducing one-by-one equals replaying the whole list *)
  let folded =
    List.fold events ~init:Session_state.empty ~f:Session_state.reduce
  in
  let replayed = Session_state.replay events in
  Alcotest.(check int)
    "same step count"
    (Session_state.steps folded)
    (Session_state.steps replayed)

let test_prefix_is_earlier_state () =
  (* replaying a prefix gives the state at that point — the basis for forking *)
  let prefix = List.take events 3 in
  let st = Session_state.replay prefix in
  Alcotest.(check int) "prefix step count" 1 (Session_state.steps st);
  Alcotest.(check int)
    "prefix has fewer messages" 2
    (List.length (Session_state.messages st))

let () =
  Alcotest.run "session_state"
    [
      ( "replay",
        [
          Alcotest.test_case "replay" `Quick test_replay;
          Alcotest.test_case "incremental" `Quick
            test_incremental_matches_replay;
          Alcotest.test_case "prefix" `Quick test_prefix_is_earlier_state;
        ] );
    ]
