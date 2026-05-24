open! Base
open Fp_agent

let parse = Model_client.parse_action

let test_parse_tool_call () =
  match
    parse {|{"action":"tool_call","tool":"read_file","args":{"path":"a.ml"}}|}
  with
  | Ok (Model_action.Tool_call (Tool_call.Read_file { path })) ->
      Alcotest.(check string) "path" "a.ml" path
  | Ok _ -> Alcotest.fail "wrong action variant"
  | Error e -> Alcotest.failf "unexpected error: %s" e

let test_parse_edit_wire_names () =
  match
    parse
      {|{"action":"tool_call","tool":"edit_file","args":{"path":"a.ml","old":"x","new":"y"}}|}
  with
  | Ok
      (Model_action.Tool_call (Tool_call.Edit_file { path; old_text; new_text }))
    ->
      Alcotest.(check string) "path" "a.ml" path;
      Alcotest.(check string) "old" "x" old_text;
      Alcotest.(check string) "new" "y" new_text
  | _ -> Alcotest.fail "expected edit_file"

let test_parse_final_answer () =
  match
    parse {|{"action":"final_answer","summary":"done","details":"more"}|}
  with
  | Ok (Model_action.Final_answer { answer }) ->
      Alcotest.(check bool)
        "answer combines summary+details" true
        (String.is_substring answer ~substring:"done"
        && String.is_substring answer ~substring:"more")
  | _ -> Alcotest.fail "expected final_answer"

let test_parse_with_fences () =
  let content =
    "```json\n{\"action\":\"final_answer\",\"summary\":\"ok\"}\n```"
  in
  match parse content with
  | Ok (Model_action.Final_answer { answer }) ->
      Alcotest.(check string) "answer" "ok" answer
  | _ -> Alcotest.fail "expected final_answer through fences"

let test_parse_invalid_json () =
  Alcotest.(check bool)
    "invalid json errors" true
    (Result.is_error (parse "not json at all"))

let test_parse_unknown_action () =
  Alcotest.(check bool)
    "unknown action errors" true
    (Result.is_error (parse {|{"action":"explode"}|}))

let test_mock_client () =
  let client =
    Model_client.create_mock ~send:(fun _messages ->
        Lwt.return (Ok (Model_action.Final_answer { answer = "mocked" })))
  in
  let result =
    Lwt_main.run (Model_client.send client ~messages:[ Message.user "hi" ])
  in
  match result with
  | Ok (Model_action.Final_answer { answer }) ->
      Alcotest.(check string) "mock answer" "mocked" answer
  | _ -> Alcotest.fail "mock did not return final answer"

let test_config_requires_key () =
  Unix.putenv "OPENAI_API_KEY" "";
  Alcotest.(check bool)
    "missing key errors" true
    (Result.is_error (Config.load ()));
  Unix.putenv "OPENAI_API_KEY" "sk-test";
  Unix.putenv "MODEL_NAME" "my-model";
  match Config.load () with
  | Ok cfg ->
      Alcotest.(check string) "model from env" "my-model" cfg.model;
      Alcotest.(check string) "api key" "sk-test" cfg.api_key
  | Error e -> Alcotest.failf "expected ok config: %s" e

let () =
  Alcotest.run "model_client"
    [
      ( "parse",
        [
          Alcotest.test_case "tool_call" `Quick test_parse_tool_call;
          Alcotest.test_case "edit_wire_names" `Quick test_parse_edit_wire_names;
          Alcotest.test_case "final_answer" `Quick test_parse_final_answer;
          Alcotest.test_case "fences" `Quick test_parse_with_fences;
          Alcotest.test_case "invalid_json" `Quick test_parse_invalid_json;
          Alcotest.test_case "unknown_action" `Quick test_parse_unknown_action;
        ] );
      ("client", [ Alcotest.test_case "mock" `Quick test_mock_client ]);
      ( "config",
        [ Alcotest.test_case "requires_key" `Quick test_config_requires_key ] );
    ]
