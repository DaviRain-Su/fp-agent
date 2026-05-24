open Base
open Fp_agent

let yojson_roundtrip name of_yojson to_yojson value =
  let json = to_yojson value in
  match of_yojson json with
  | Ok decoded ->
      Alcotest.(check (testable Yojson.Safe.pp Yojson.Safe.equal))
        (name ^ " roundtrip") json (to_yojson decoded)
  | Error msg -> Alcotest.fail (name ^ " decode failed: " ^ msg)

let test_tool_call_roundtrips () =
  let cases =
    [
      ("Read_file", Tool_call.Read_file { path = "lib/foo.ml" });
      ( "Write_file",
        Tool_call.Write_file { path = "lib/foo.ml"; content = "let x = 42" } );
      ( "Edit_file",
        Tool_call.Edit_file
          {
            path = "lib/foo.ml";
            old_text = "let x = 42";
            new_text = "let x = 43";
          } );
      ( "Run_command",
        Tool_call.Run_command { command = "dune build"; cwd = None } );
      ( "Run_command_with_cwd",
        Tool_call.Run_command { command = "make test"; cwd = Some "/tmp" } );
      ("List_files", Tool_call.List_files { path = "lib" });
    ]
  in
  List.iter cases ~f:(fun (name, tc) ->
      yojson_roundtrip name Tool_call.of_yojson Tool_call.to_yojson tc)

let test_tool_result_roundtrips () =
  let cases =
    [
      ("Success", Tool_result.Success { output = "hello" });
      ("Error", Tool_result.Error { message = "not found" });
    ]
  in
  List.iter cases ~f:(fun (name, tr) ->
      yojson_roundtrip name Tool_result.of_yojson Tool_result.to_yojson tr)

let test_model_action_roundtrips () =
  let cases =
    [
      ( "Tool_call",
        Model_action.Tool_call (Tool_call.Read_file { path = "lib/foo.ml" }) );
      ( "Final_answer",
        Model_action.Final_answer { answer = "The fix is complete." } );
    ]
  in
  List.iter cases ~f:(fun (name, ma) ->
      yojson_roundtrip name Model_action.of_yojson Model_action.to_yojson ma)

let test_event_roundtrips () =
  let cases =
    [
      ("User_message", Event.User_message { content = "Fix the bug" });
      ( "Model_response",
        Event.Model_response
          { action = Model_action.Final_answer { answer = "Done" } } );
      ( "Tool_call",
        Event.Tool_call (Tool_call.Read_file { path = "lib/foo.ml" }) );
      ("Tool_result", Event.Tool_result (Tool_result.Success { output = "42" }));
      ( "State_transition",
        Event.State_transition
          {
            from_state = Agent_state.Initializing;
            to_state = Agent_state.Waiting_for_model;
          } );
    ]
  in
  List.iter cases ~f:(fun (name, ev) ->
      yojson_roundtrip name Event.of_yojson Event.to_yojson ev)

let test_invalid_json () =
  let invalid_cases =
    [
      ("Tool_call: missing tag", `Assoc [ ("path", `String "foo.ml") ]);
      ("Tool_call: unknown tag", `Assoc [ ("tag", `String "Delete_file") ]);
      ( "Tool_call: wrong shape",
        `Assoc [ ("tag", `String "Read_file"); ("path", `Int 42) ] );
      ("Tool_result: empty", `Assoc []);
      ("Model_action: empty", `Assoc []);
      ("Event: empty", `Assoc []);
    ]
  in
  List.iter invalid_cases ~f:(fun (name, json) ->
      match Tool_call.of_yojson json with
      | Ok _ -> Alcotest.fail (name ^ ": expected error but got Ok")
      | Error _ -> ())

let () =
  Alcotest.run "tool_call"
    [
      ( "roundtrips",
        [
          Alcotest.test_case "tool_call" `Quick test_tool_call_roundtrips;
          Alcotest.test_case "tool_result" `Quick test_tool_result_roundtrips;
          Alcotest.test_case "model_action" `Quick test_model_action_roundtrips;
          Alcotest.test_case "event" `Quick test_event_roundtrips;
        ] );
      ( "invalid_json",
        [ Alcotest.test_case "invalid_json" `Quick test_invalid_json ] );
    ]
