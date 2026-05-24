open Base
open Fp_agent

let test_valid_transitions () =
  let cases =
    [
      ( "Initializing -> Waiting_for_model",
        Agent_state.Initializing,
        Agent_state.Waiting_for_model );
      ("Initializing -> Failed", Agent_state.Initializing, Agent_state.Failed);
      ( "Waiting_for_model -> Executing_tool",
        Agent_state.Waiting_for_model,
        Agent_state.Executing_tool );
      ( "Waiting_for_model -> Completed",
        Agent_state.Waiting_for_model,
        Agent_state.Completed );
      ( "Waiting_for_model -> Failed",
        Agent_state.Waiting_for_model,
        Agent_state.Failed );
      ( "Executing_tool -> Observing_result",
        Agent_state.Executing_tool,
        Agent_state.Observing_result );
      ( "Executing_tool -> Failed",
        Agent_state.Executing_tool,
        Agent_state.Failed );
      ( "Observing_result -> Waiting_for_model",
        Agent_state.Observing_result,
        Agent_state.Waiting_for_model );
      ( "Observing_result -> Completed",
        Agent_state.Observing_result,
        Agent_state.Completed );
      ( "Observing_result -> Failed",
        Agent_state.Observing_result,
        Agent_state.Failed );
    ]
  in
  List.iter cases ~f:(fun (name, from_state, to_state) ->
      match Agent_state.transition from_state to_state with
      | Ok result ->
          Alcotest.(check (testable Agent_state.pp Agent_state.equal))
            name to_state result
      | Error msg -> Alcotest.fail (name ^ " unexpected error: " ^ msg))

let test_invalid_transitions () =
  let cases =
    [
      ( "Initializing -> Executing_tool",
        Agent_state.Initializing,
        Agent_state.Executing_tool );
      ( "Initializing -> Observing_result",
        Agent_state.Initializing,
        Agent_state.Observing_result );
      ( "Initializing -> Completed",
        Agent_state.Initializing,
        Agent_state.Completed );
      ( "Waiting_for_model -> Initializing",
        Agent_state.Waiting_for_model,
        Agent_state.Initializing );
      ( "Waiting_for_model -> Observing_result",
        Agent_state.Waiting_for_model,
        Agent_state.Observing_result );
      ( "Waiting_for_model -> Waiting_for_model",
        Agent_state.Waiting_for_model,
        Agent_state.Waiting_for_model );
      ( "Executing_tool -> Waiting_for_model",
        Agent_state.Executing_tool,
        Agent_state.Waiting_for_model );
      ( "Executing_tool -> Completed",
        Agent_state.Executing_tool,
        Agent_state.Completed );
      ( "Executing_tool -> Executing_tool",
        Agent_state.Executing_tool,
        Agent_state.Executing_tool );
      ( "Observing_result -> Initializing",
        Agent_state.Observing_result,
        Agent_state.Initializing );
      ( "Observing_result -> Executing_tool",
        Agent_state.Observing_result,
        Agent_state.Executing_tool );
      ( "Observing_result -> Observing_result",
        Agent_state.Observing_result,
        Agent_state.Observing_result );
      ( "Completed -> Initializing",
        Agent_state.Completed,
        Agent_state.Initializing );
      ( "Completed -> Waiting_for_model",
        Agent_state.Completed,
        Agent_state.Waiting_for_model );
      ("Completed -> Failed", Agent_state.Completed, Agent_state.Failed);
      ("Failed -> Initializing", Agent_state.Failed, Agent_state.Initializing);
      ( "Failed -> Waiting_for_model",
        Agent_state.Failed,
        Agent_state.Waiting_for_model );
      ("Failed -> Completed", Agent_state.Failed, Agent_state.Completed);
    ]
  in
  List.iter cases ~f:(fun (name, from_state, to_state) ->
      match Agent_state.transition from_state to_state with
      | Ok _ -> Alcotest.fail (name ^ ": expected error but got Ok")
      | Error _ -> ())

let test_state_json_roundtrip () =
  let cases =
    [
      Agent_state.Initializing;
      Agent_state.Waiting_for_model;
      Agent_state.Executing_tool;
      Agent_state.Observing_result;
      Agent_state.Completed;
      Agent_state.Failed;
    ]
  in
  List.iter cases ~f:(fun state ->
      let json = Agent_state.to_yojson state in
      match Agent_state.of_yojson json with
      | Ok decoded ->
          Alcotest.(check (testable Agent_state.pp Agent_state.equal))
            ("roundtrip " ^ Agent_state.to_string state)
            state decoded
      | Error msg -> Alcotest.fail ("decode failed: " ^ msg))

let test_invalid_state_json () =
  let invalid = `Assoc [ ("tag", `String "Unknown_state") ] in
  match Agent_state.of_yojson invalid with
  | Ok _ -> Alcotest.fail "expected error for unknown state"
  | Error _ -> ()

let () =
  Alcotest.run "agent_state"
    [
      ( "valid_transitions",
        [ Alcotest.test_case "valid_transitions" `Quick test_valid_transitions ]
      );
      ( "invalid_transitions",
        [
          Alcotest.test_case "invalid_transitions" `Quick
            test_invalid_transitions;
        ] );
      ( "json_roundtrip",
        [ Alcotest.test_case "json_roundtrip" `Quick test_state_json_roundtrip ]
      );
      ( "invalid_json",
        [ Alcotest.test_case "invalid_json" `Quick test_invalid_state_json ] );
    ]
