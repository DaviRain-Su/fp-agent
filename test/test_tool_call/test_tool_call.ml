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
      ("Read_file", Tool_call.read_file "lib/foo.ml");
      ( "Write_file",
        Tool_call.write_file ~path:"lib/foo.ml" ~content:"let x = 42" );
      ( "Edit_file",
        Tool_call.edit_file ~path:"lib/foo.ml" ~old_text:"let x = 42"
          ~new_text:"let x = 43" );
      ("Run_command", Tool_call.run_command "dune build");
      ("Run_command_with_cwd", Tool_call.run_command ~cwd:"/tmp" "make test");
      ("List_files", Tool_call.list_files "lib");
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

let test_graph_event_roundtrips () =
  let cases =
    [
      ( "Node_started",
        Graph_event.Node_started { node_id = "n"; kind = Graph_event.Tool } );
      ( "Node_completed",
        Graph_event.Node_completed
          { node_id = "n"; kind = Graph_event.Agent; output = Some "done" } );
      ( "Edge_selected",
        Graph_event.Edge_selected
          { node_id = "r"; label = "yes"; target_node_id = "child" } );
    ]
  in
  List.iter cases ~f:(fun (name, event) ->
      yojson_roundtrip name Graph_event.of_yojson Graph_event.to_yojson event)

let test_model_action_roundtrips () =
  let cases =
    [
      ("Tool_call", Model_action.Tool_call (Tool_call.read_file "lib/foo.ml"));
      ( "Tool_calls",
        Model_action.Tool_calls
          [ Tool_call.read_file "a.ml"; Tool_call.list_files "lib" ] );
      ( "Final_answer",
        Model_action.Final_answer { answer = "The fix is complete." } );
    ]
  in
  List.iter cases ~f:(fun (name, ma) ->
      yojson_roundtrip name Model_action.of_yojson Model_action.to_yojson ma)

let test_llm_roundtrips () =
  let content =
    [
      Llm.Text "result:";
      Llm.Thinking { text = "inspect"; signature = "sig-1" };
      Llm.Tool_use
        {
          id = "call-123";
          name = "read_file";
          input = `Assoc [ ("path", `String "lib/foo.ml") ];
        };
      Llm.Tool_result { id = "call-123"; content = "let x = 1" };
    ]
  in
  List.iteri content ~f:(fun i block ->
      yojson_roundtrip
        (Printf.sprintf "Llm.content.%d" i)
        (fun json -> Ok (Llm.content_of_yojson json))
        Llm.yojson_of_content block);
  yojson_roundtrip "Llm.turn"
    (fun json -> Ok (Llm.turn_of_yojson json))
    Llm.yojson_of_turn
    { role = Llm.Assistant; content };
  yojson_roundtrip "Llm.usage"
    (fun json -> Ok (Llm.usage_of_yojson json))
    Llm.yojson_of_usage
    { input_tokens = 12; output_tokens = 34 }

let test_event_roundtrips () =
  let cases =
    [
      ("User_message", Event.User_message { content = "Fix the bug" });
      ( "Assistant_message",
        Event.Assistant_message
          {
            content =
              [
                Llm.Tool_use
                  {
                    id = "call-123";
                    name = "read_file";
                    input = `Assoc [ ("path", `String "lib/foo.ml") ];
                  };
              ];
            usage = { input_tokens = 10; output_tokens = 4 };
          } );
      ( "Model_response",
        Event.Model_response
          { action = Model_action.Final_answer { answer = "Done" } } );
      ("Tool_call", Event.Tool_call (Tool_call.read_file "lib/foo.ml"));
      ( "Tool_result_message",
        Event.Tool_result_message
          { id = "call-123"; result = Tool_result.Success { output = "42" } } );
      ("Tool_result", Event.Tool_result (Tool_result.Success { output = "42" }));
      ( "Context_compacted",
        Event.Context_compacted
          { summary = "Earlier findings"; recent = [ Llm.user "continue" ] } );
      ( "Plan_updated",
        Event.Plan_updated
          {
            items =
              [
                { Event.status = Event.Todo; text = "inspect code" };
                { Event.status = Event.Doing; text = "implement command" };
                { Event.status = Event.Done; text = "run tests" };
              ];
          } );
      ( "Graph_event",
        Event.Graph_event
          (Graph_event.Node_started
             { node_id = "graph"; kind = Graph_event.Sequence }) );
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
      ("Tool_call: missing name", `Assoc [ ("path", `String "foo.ml") ]);
      ("Tool_call: wrong name shape", `Assoc [ ("name", `Int 42) ]);
      ( "Tool_call: wrong shape",
        `List [ `Assoc [ ("name", `String "read_file") ] ] );
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
          Alcotest.test_case "graph_event" `Quick test_graph_event_roundtrips;
          Alcotest.test_case "model_action" `Quick test_model_action_roundtrips;
          Alcotest.test_case "llm" `Quick test_llm_roundtrips;
          Alcotest.test_case "event" `Quick test_event_roundtrips;
        ] );
      ( "invalid_json",
        [ Alcotest.test_case "invalid_json" `Quick test_invalid_json ] );
    ]
