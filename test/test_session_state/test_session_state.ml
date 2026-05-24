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
      { action = Model_action.Tool_call (Tool_call.read_file "f") };
    Event.Tool_call (Tool_call.read_file "f");
    Event.Policy_decision
      { tool_call = Tool_call.read_file "f"; permission = Permission.Allow };
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

let test_batch_tool_results_reduce_to_observations () =
  let events =
    [
      Event.User_message { content = "batch" };
      Event.Assistant_message
        {
          content =
            [
              Llm.Tool_use
                {
                  id = "call-a";
                  name = "read_file";
                  input = `Assoc [ ("path", `String "a") ];
                };
              Llm.Tool_use
                {
                  id = "call-b";
                  name = "read_file";
                  input = `Assoc [ ("path", `String "b") ];
                };
            ];
          usage = Llm.zero_usage;
        };
      Event.Tool_call (Tool_call.read_file "a");
      Event.Tool_result_message
        { id = "call-a"; result = Tool_result.Success { output = "a" } };
      Event.Tool_call (Tool_call.read_file "b");
      Event.Tool_result_message
        { id = "call-b"; result = Tool_result.Success { output = "b" } };
    ]
  in
  let st = Session_state.replay events in
  Alcotest.(check int) "one model step" 1 (Session_state.steps st);
  let roles =
    List.map (Session_state.messages st) ~f:(fun m -> m.Message.role)
  in
  Alcotest.(check (list string))
    "messages: user, assistant, observation batch"
    [ "user"; "assistant"; "user" ]
    roles;
  match List.last_exn (Session_state.turns st) with
  | { role = Llm.User; content = [ Llm.Tool_result _; Llm.Tool_result _ ] } ->
      ()
  | turn ->
      Alcotest.failf "expected batched tool results: %s"
        (Yojson.Safe.to_string (Llm.turn_to_json turn))

let test_normalized_events_preserve_tool_ids () =
  let events =
    [
      Event.User_message { content = "inspect f" };
      Event.Assistant_message
        {
          content =
            [
              Llm.Tool_use
                {
                  id = "provider-call-7";
                  name = "read_file";
                  input = `Assoc [ ("path", `String "f") ];
                };
            ];
          usage = { input_tokens = 11; output_tokens = 5 };
        };
      Event.Tool_call (Tool_call.read_file "f");
      Event.Tool_result_message
        {
          id = "provider-call-7";
          result = Tool_result.Success { output = "contents" };
        };
      Event.Assistant_message
        {
          content = [ Llm.Text "done" ];
          usage = { input_tokens = 20; output_tokens = 3 };
        };
    ]
  in
  let st = Session_state.replay events in
  Alcotest.(check int) "two model steps" 2 (Session_state.steps st);
  match Session_state.turns st with
  | [
   { role = Llm.User; content = [ Llm.Text "inspect f" ] };
   {
     role = Llm.Assistant;
     content = [ Llm.Tool_use { id = requested_id; name; input } ];
   };
   {
     role = Llm.User;
     content = [ Llm.Tool_result { id = result_id; content } ];
   };
   { role = Llm.Assistant; content = [ Llm.Text "done" ] };
  ] ->
      Alcotest.(check string) "assistant tool id" "provider-call-7" requested_id;
      Alcotest.(check string) "tool result id" requested_id result_id;
      Alcotest.(check string) "tool name" "read_file" name;
      Alcotest.(check (testable Yojson.Safe.pp Yojson.Safe.equal))
        "tool input"
        (`Assoc [ ("path", `String "f") ])
        input;
      Alcotest.(check bool)
        "observation content" true
        (String.is_substring content ~substring:"contents")
  | turns ->
      Alcotest.failf "unexpected turns: %s"
        (Yojson.Safe.to_string (`List (List.map turns ~f:Llm.turn_to_json)))

let test_context_compaction_replaces_visible_turns () =
  let st =
    Session_state.replay
      [
        Event.User_message { content = "old task" };
        Event.Assistant_message
          { content = [ Llm.Text "old answer" ]; usage = Llm.zero_usage };
        Event.Context_compacted
          {
            summary = "old task was answered";
            recent = [ Llm.user "recent task" ];
          };
      ]
  in
  match Session_state.turns st with
  | [
   {
     role = Llm.User;
     content =
       [ Llm.Text "[Earlier conversation summary]\nold task was answered" ];
   };
   { role = Llm.User; content = [ Llm.Text "recent task" ] };
  ] ->
      Alcotest.(check int)
        "model steps survive compaction" 1 (Session_state.steps st)
  | turns ->
      Alcotest.failf "unexpected compacted turns: %s"
        (Yojson.Safe.to_string (`List (List.map turns ~f:Llm.turn_to_json)))

let test_plan_events_do_not_enter_model_transcript () =
  let st =
    Session_state.replay
      [
        Event.User_message { content = "build feature" };
        Event.Plan_updated
          {
            items =
              [
                { Event.status = Event.Todo; text = "inspect code" };
                { Event.status = Event.Doing; text = "implement command" };
              ];
          };
        Event.Assistant_message
          { content = [ Llm.Text "done" ]; usage = Llm.zero_usage };
      ]
  in
  Alcotest.(check int) "one model step" 1 (Session_state.steps st);
  Alcotest.(check int)
    "plan is not a turn" 2
    (List.length (Session_state.turns st))

let () =
  Alcotest.run "session_state"
    [
      ( "replay",
        [
          Alcotest.test_case "replay" `Quick test_replay;
          Alcotest.test_case "incremental" `Quick
            test_incremental_matches_replay;
          Alcotest.test_case "prefix" `Quick test_prefix_is_earlier_state;
          Alcotest.test_case "batch_results" `Quick
            test_batch_tool_results_reduce_to_observations;
          Alcotest.test_case "normalized_events" `Quick
            test_normalized_events_preserve_tool_ids;
          Alcotest.test_case "context_compaction" `Quick
            test_context_compaction_replaces_visible_turns;
          Alcotest.test_case "plan_events" `Quick
            test_plan_events_do_not_enter_model_transcript;
        ] );
    ]
