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

let test_parse_flat_tool () =
  (* action holds the tool name directly, args at top level *)
  match
    parse {|{"action":"write_file","path":"a.ml","content":"let x = 1"}|}
  with
  | Ok (Model_action.Tool_call (Tool_call.Write_file { path; content })) ->
      Alcotest.(check string) "path" "a.ml" path;
      Alcotest.(check string) "content" "let x = 1" content
  | _ -> Alcotest.fail "expected flat write_file"

let test_parse_bare_tool_field () =
  match parse {|{"tool":"list_files","args":{"path":"lib"}}|} with
  | Ok (Model_action.Tool_call (Tool_call.List_files { path })) ->
      Alcotest.(check string) "path" "lib" path
  | _ -> Alcotest.fail "expected list_files from bare tool field"

let test_parse_array_wrapped () =
  match parse {|[{"action":"read_file","path":"a.ml"}]|} with
  | Ok (Model_action.Tool_call (Tool_call.Read_file { path })) ->
      Alcotest.(check string) "path" "a.ml" path
  | _ -> Alcotest.fail "expected read_file from array-wrapped action"

let test_parse_non_object_errors () =
  (* a bare array of non-objects must not crash; it returns an error *)
  Alcotest.(check bool)
    "non-object json errors cleanly" true
    (Result.is_error (parse {|[1,2,3]|}))

let test_parse_search () =
  match
    parse {|{"action":"tool_call","tool":"search","args":{"query":"needle"}}|}
  with
  | Ok (Model_action.Tool_call (Tool_call.Search { query; path = None })) ->
      Alcotest.(check string) "query" "needle" query
  | _ -> Alcotest.fail "expected search tool call"

let test_parse_flat_new_tools () =
  (match parse {|{"action":"make_dir","path":"tmp/new"}|} with
  | Ok (Model_action.Tool_call (Tool_call.Make_dir { path })) ->
      Alcotest.(check string) "make_dir path" "tmp/new" path
  | _ -> Alcotest.fail "expected flat make_dir");
  match
    parse
      {|{"action":"multi_edit","edits":[{"path":"a.ml","old":"x","new":"y"}]}|}
  with
  | Ok (Model_action.Tool_call (Tool_call.Multi_edit { edits = [ edit ] })) ->
      Alcotest.(check string) "multi_edit path" "a.ml" edit.path;
      Alcotest.(check string) "multi_edit old" "x" edit.old_text;
      Alcotest.(check string) "multi_edit new" "y" edit.new_text
  | _ -> Alcotest.fail "expected flat multi_edit"

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

let test_config_providers () =
  Unix.putenv "KIMI_API_KEY" "kimi-secret";
  Unix.putenv "DEEPSEEK_API_KEY" "ds-secret";
  (match Config.load () with
  | Ok cfg ->
      Alcotest.(check string)
        "default model is kimi" "kimi-for-coding" cfg.model;
      Alcotest.(check bool)
        "kimi base" true
        (String.is_substring cfg.api_base ~substring:"api.kimi.com");
      Alcotest.(check bool)
        "kimi uses anthropic protocol" true
        (match cfg.protocol with Provider.Anthropic -> true | _ -> false);
      Alcotest.(check string) "kimi key" "kimi-secret" cfg.api_key
  | Error e -> Alcotest.failf "kimi load: %s" e);
  (match Config.load ~provider:"deepseek" () with
  | Ok cfg ->
      Alcotest.(check string) "deepseek model" "deepseek-v4-flash" cfg.model;
      Alcotest.(check string) "deepseek key" "ds-secret" cfg.api_key
  | Error e -> Alcotest.failf "deepseek load: %s" e);
  (match Config.load ~model:"custom-model" () with
  | Ok cfg -> Alcotest.(check string) "model override" "custom-model" cfg.model
  | Error e -> Alcotest.failf "override load: %s" e);
  Alcotest.(check bool)
    "unknown provider errors" true
    (Result.is_error (Config.load ~provider:"nope" ()));
  Unix.putenv "KIMI_API_KEY" "";
  Alcotest.(check bool)
    "missing key errors" true
    (Result.is_error (Config.load ()))

let () =
  Alcotest.run "model_client"
    [
      ( "parse",
        [
          Alcotest.test_case "tool_call" `Quick test_parse_tool_call;
          Alcotest.test_case "edit_wire_names" `Quick test_parse_edit_wire_names;
          Alcotest.test_case "flat_tool" `Quick test_parse_flat_tool;
          Alcotest.test_case "bare_tool_field" `Quick test_parse_bare_tool_field;
          Alcotest.test_case "search" `Quick test_parse_search;
          Alcotest.test_case "flat_new_tools" `Quick test_parse_flat_new_tools;
          Alcotest.test_case "array_wrapped" `Quick test_parse_array_wrapped;
          Alcotest.test_case "non_object" `Quick test_parse_non_object_errors;
          Alcotest.test_case "final_answer" `Quick test_parse_final_answer;
          Alcotest.test_case "fences" `Quick test_parse_with_fences;
          Alcotest.test_case "invalid_json" `Quick test_parse_invalid_json;
          Alcotest.test_case "unknown_action" `Quick test_parse_unknown_action;
        ] );
      ("client", [ Alcotest.test_case "mock" `Quick test_mock_client ]);
      ("config", [ Alcotest.test_case "providers" `Quick test_config_providers ]);
    ]
