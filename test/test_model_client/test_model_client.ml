open! Base
open Fp_agent

let parse = Model_client.parse_action

let check_tool tc ~name =
  Alcotest.(check string) "tool name" name tc.Tool_call.name

let check_arg tc key expected =
  Alcotest.(check (option string))
    key (Some expected)
    (Tool_call.arg_string tc key)

let test_parse_tool_call () =
  match
    parse {|{"action":"tool_call","tool":"read_file","args":{"path":"a.ml"}}|}
  with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"read_file";
      check_arg tc "path" "a.ml"
  | Ok _ -> Alcotest.fail "wrong action variant"
  | Error e -> Alcotest.failf "unexpected error: %s" e

let test_parse_edit_wire_names () =
  match
    parse
      {|{"action":"tool_call","tool":"edit_file","args":{"path":"a.ml","old":"x","new":"y"}}|}
  with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"edit_file";
      check_arg tc "path" "a.ml";
      check_arg tc "old" "x";
      check_arg tc "new" "y"
  | _ -> Alcotest.fail "expected edit_file"

let test_parse_flat_tool () =
  (* action holds the tool name directly, args at top level *)
  match
    parse {|{"action":"write_file","path":"a.ml","content":"let x = 1"}|}
  with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"write_file";
      check_arg tc "path" "a.ml";
      check_arg tc "content" "let x = 1"
  | _ -> Alcotest.fail "expected flat write_file"

let test_parse_bare_tool_field () =
  match parse {|{"tool":"list_files","args":{"path":"lib"}}|} with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"list_files";
      check_arg tc "path" "lib"
  | _ -> Alcotest.fail "expected list_files from bare tool field"

let test_parse_array_wrapped () =
  match parse {|[{"action":"read_file","path":"a.ml"}]|} with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"read_file";
      check_arg tc "path" "a.ml"
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
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"search";
      check_arg tc "query" "needle";
      Alcotest.(check (option string))
        "path" None
        (Tool_call.arg_string tc "path")
  | _ -> Alcotest.fail "expected search tool call"

let test_parse_flat_new_tools () =
  (match parse {|{"action":"make_dir","path":"tmp/new"}|} with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"make_dir";
      check_arg tc "path" "tmp/new"
  | _ -> Alcotest.fail "expected flat make_dir");
  match
    parse
      {|{"action":"multi_edit","edits":[{"path":"a.ml","old":"x","new":"y"}]}|}
  with
  | Ok (Model_action.Tool_call tc) -> (
      check_tool tc ~name:"multi_edit";
      match Tool_call.arg tc "edits" with
      | `List [ edit ] ->
          let edit_arg key =
            match Yojson.Safe.Util.member key edit with
            | `String s -> Some s
            | _ -> None
          in
          Alcotest.(check (option string))
            "multi_edit path" (Some "a.ml") (edit_arg "path");
          Alcotest.(check (option string))
            "multi_edit old" (Some "x") (edit_arg "old");
          Alcotest.(check (option string))
            "multi_edit new" (Some "y") (edit_arg "new")
      | _ -> Alcotest.fail "expected one multi_edit item")
  | _ -> Alcotest.fail "expected flat multi_edit"

let test_parse_update_plan () =
  match
    parse
      {|{"action":"tool_call","tool":"update_plan","args":{"plan":[{"step":"inspect code","status":"pending"},{"step":"run tests","status":"in_progress"}],"explanation":"starting"}}|}
  with
  | Ok (Model_action.Tool_call tc) -> (
      check_tool tc ~name:"update_plan";
      match Event.plan_items_of_json tc.args with
      | Error e -> Alcotest.failf "plan args did not parse: %s" e
      | Ok [ first; second ] ->
          Alcotest.(check string) "first step" "inspect code" first.text;
          Alcotest.(check string)
            "first status" "todo"
            (Event.plan_status_to_string first.status);
          Alcotest.(check string) "second step" "run tests" second.text;
          Alcotest.(check string)
            "second status" "doing"
            (Event.plan_status_to_string second.status)
      | Ok items ->
          Alcotest.failf "unexpected plan item count: %d" (List.length items))
  | Ok _ -> Alcotest.fail "expected update_plan tool call"
  | Error e -> Alcotest.failf "unexpected update_plan parse error: %s" e

let test_parse_tool_calls_batch () =
  match
    parse
      {|{"action":"tool_calls","calls":[{"tool":"read_file","args":{"path":"a.ml"}},{"tool":"list_files","args":{"path":"lib"}}]}|}
  with
  | Ok (Model_action.Tool_calls [ read; list ]) ->
      check_tool read ~name:"read_file";
      check_arg read "path" "a.ml";
      check_tool list ~name:"list_files";
      check_arg list "path" "lib"
  | Ok _ -> Alcotest.fail "expected two tool calls"
  | Error e -> Alcotest.failf "unexpected batch parse error: %s" e

let test_parse_array_tool_batch () =
  match
    parse
      {|[{"action":"read_file","path":"a.ml"},{"action":"search","query":"needle"}]|}
  with
  | Ok (Model_action.Tool_calls [ read; search ]) ->
      check_tool read ~name:"read_file";
      check_arg read "path" "a.ml";
      check_tool search ~name:"search";
      check_arg search "query" "needle"
  | Ok _ -> Alcotest.fail "expected array batch"
  | Error e -> Alcotest.failf "unexpected array batch parse error: %s" e

let test_parse_anthropic_tool_use () =
  match
    parse {|{"type":"tool_use","name":"read_file","input":{"path":"a.ml"}}|}
  with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"read_file";
      check_arg tc "path" "a.ml"
  | Ok _ -> Alcotest.fail "expected anthropic tool_use"
  | Error e -> Alcotest.failf "unexpected anthropic parse error: %s" e

let test_parse_anthropic_tool_use_batch () =
  match
    parse
      {|[{"type":"tool_use","name":"read_file","input":{"path":"a.ml"}},{"type":"tool_use","name":"list_files","input":{"path":"lib"}}]|}
  with
  | Ok (Model_action.Tool_calls [ read; list ]) ->
      check_tool read ~name:"read_file";
      check_arg read "path" "a.ml";
      check_tool list ~name:"list_files";
      check_arg list "path" "lib"
  | Ok _ -> Alcotest.fail "expected anthropic tool_use batch"
  | Error e -> Alcotest.failf "unexpected anthropic batch parse error: %s" e

let test_parse_mixed_content_tool_use_batch () =
  match
    parse
      {|[{"type":"text","text":"I'll inspect files."},{"type":"tool_use","name":"read_file","input":{"path":"README.md"}}]|}
  with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"read_file";
      check_arg tc "path" "README.md"
  | Ok _ -> Alcotest.fail "expected mixed content to select tool_use"
  | Error e -> Alcotest.failf "unexpected mixed content parse error: %s" e

let test_parse_text_only_tool_calls_as_final () =
  match
    parse
      {|{"action":"tool_calls","calls":[{"type":"text","text":"Review complete."}]}|}
  with
  | Ok (Model_action.Final_answer { answer }) ->
      Alcotest.(check string) "text final" "Review complete." answer
  | Ok _ -> Alcotest.fail "expected text-only tool_calls to become final answer"
  | Error e -> Alcotest.failf "unexpected text-only tool_calls error: %s" e

let test_parse_name_args_tool_calls () =
  match
    parse
      {|{"action":"tool_calls","calls":[{"name":"list_files","args":{"path":"lib/ui"}}]}|}
  with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"list_files";
      check_arg tc "path" "lib/ui"
  | Ok _ -> Alcotest.fail "expected name+args tool call"
  | Error e -> Alcotest.failf "unexpected name+args parse error: %s" e

let test_parse_ppx_variant_tool_call () =
  match
    parse {|["Tool_call",{"name":"read_file","args":{"path":"README.md"}}]|}
  with
  | Ok (Model_action.Tool_call tc) ->
      check_tool tc ~name:"read_file";
      check_arg tc "path" "README.md"
  | Ok _ -> Alcotest.fail "expected ppx variant tool call"
  | Error e -> Alcotest.failf "unexpected ppx variant parse error: %s" e

let test_parse_single_string_array_as_final () =
  match parse {|["Review complete."]|} with
  | Ok (Model_action.Final_answer { answer }) ->
      Alcotest.(check string) "answer" "Review complete." answer
  | Ok _ -> Alcotest.fail "expected single string array final answer"
  | Error e -> Alcotest.failf "unexpected single string array error: %s" e

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
    Model_client.create_mock ~send:(fun _turns ->
        Lwt.return (Ok ([ Llm.Text "mocked" ], Llm.zero_usage)))
  in
  let result =
    Lwt_main.run
      (Model_client.send client ~system:"sys" ~turns:[ Llm.user "hi" ])
  in
  match result with
  | Ok ([ Llm.Text answer ], _) ->
      Alcotest.(check string) "mock answer" "mocked" answer
  | _ -> Alcotest.fail "mock did not return final answer"

let test_streaming_tool_ids () =
  let openai_payloads =
    [
      {|{"choices":[{"delta":{"tool_calls":[{"index":0,"id":"call_openai","function":{"name":"read_file","arguments":"{\"path\""}}]}}]}|};
      {|{"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":":\"README.md\"}"}}]}}]}|};
      {|{"usage":{"prompt_tokens":11,"completion_tokens":5},"choices":[]}|};
    ]
  in
  (match Model_client.openai_complete_for_test openai_payloads with
  | Ok ([ Llm.Tool_use { id; name; input } ], usage) ->
      Alcotest.(check string) "openai id" "call_openai" id;
      Alcotest.(check string) "openai name" "read_file" name;
      Alcotest.(check (option string))
        "openai path" (Some "README.md")
        (match Yojson.Safe.Util.member "path" input with
        | `String s -> Some s
        | _ -> None);
      Alcotest.(check int) "openai input tokens" 11 usage.input_tokens;
      Alcotest.(check int) "openai output tokens" 5 usage.output_tokens
  | Ok _ -> Alcotest.fail "expected one openai tool_use"
  | Error e -> Alcotest.failf "openai complete failed: %s" e);
  let anthropic_payloads =
    [
      {|{"type":"message_start","message":{"usage":{"input_tokens":7,"output_tokens":1}}}|};
      {|{"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"call_anthropic","name":"list_files"}}|};
      {|{"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\"path\":\"lib\"}"}}|};
      {|{"type":"content_block_stop","index":0}|};
      {|{"type":"message_delta","usage":{"output_tokens":3}}|};
    ]
  in
  match Model_client.anthropic_complete_for_test anthropic_payloads with
  | Ok ([ Llm.Tool_use { id; name; input } ], usage) ->
      Alcotest.(check string) "anthropic id" "call_anthropic" id;
      Alcotest.(check string) "anthropic name" "list_files" name;
      Alcotest.(check (option string))
        "anthropic path" (Some "lib")
        (match Yojson.Safe.Util.member "path" input with
        | `String s -> Some s
        | _ -> None);
      Alcotest.(check int) "anthropic input tokens" 7 usage.input_tokens;
      Alcotest.(check int) "anthropic output tokens" 3 usage.output_tokens
  | Ok _ -> Alcotest.fail "expected one anthropic tool_use"
  | Error e -> Alcotest.failf "anthropic complete failed: %s" e

let test_streaming_preserves_reasoning_blocks () =
  let openai_payloads =
    [
      {|{"choices":[{"delta":{"reasoning_content":"inspect first"}}]}|};
      {|{"choices":[{"delta":{"tool_calls":[{"index":0,"id":"call_openai","function":{"name":"read_file","arguments":"{\"path\":\"README.md\"}"}}]}}]}|};
    ]
  in
  (match Model_client.openai_complete_for_test openai_payloads with
  | Ok
      ( [
          Llm.Thinking { text = reasoning; signature };
          Llm.Tool_use { id; name; input };
        ],
        _ ) ->
      Alcotest.(check string) "openai reasoning" "inspect first" reasoning;
      Alcotest.(check string) "openai reasoning signature" "" signature;
      Alcotest.(check string) "openai tool id" "call_openai" id;
      Alcotest.(check string) "openai tool name" "read_file" name;
      Alcotest.(check (option string))
        "openai tool path" (Some "README.md")
        (match Yojson.Safe.Util.member "path" input with
        | `String s -> Some s
        | _ -> None)
  | Ok blocks ->
      Alcotest.failf "unexpected openai blocks: %s"
        (Yojson.Safe.to_string
           (`List (List.map (fst blocks) ~f:Llm.content_to_json)))
  | Error e -> Alcotest.failf "openai complete failed: %s" e);
  let anthropic_payloads =
    [
      {|{"type":"content_block_start","index":0,"content_block":{"type":"thinking"}}|};
      {|{"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"inspect"}}|};
      {|{"type":"content_block_delta","index":0,"delta":{"type":"signature_delta","signature":"sig-123"}}|};
      {|{"type":"content_block_stop","index":0}|};
      {|{"type":"content_block_start","index":1,"content_block":{"type":"text"}}|};
      {|{"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"done"}}|};
      {|{"type":"content_block_stop","index":1}|};
    ]
  in
  match Model_client.anthropic_complete_for_test anthropic_payloads with
  | Ok ([ Llm.Thinking { text = reasoning; signature }; Llm.Text answer ], _) ->
      Alcotest.(check string) "anthropic reasoning" "inspect" reasoning;
      Alcotest.(check string) "anthropic signature" "sig-123" signature;
      Alcotest.(check string) "anthropic answer" "done" answer
  | Ok blocks ->
      Alcotest.failf "unexpected anthropic blocks: %s"
        (Yojson.Safe.to_string
           (`List (List.map (fst blocks) ~f:Llm.content_to_json)))
  | Error e -> Alcotest.failf "anthropic complete failed: %s" e

let test_request_preserves_tool_result_ids () =
  let config =
    {
      Config.provider = "test";
      api_key = "k";
      api_base = "http://localhost/v1";
      model = "m";
      models = [ "m" ];
      protocol = Provider.Openai;
      compat = Config.default_compat;
      max_tokens = None;
      max_steps = 1;
      workspace_root = ".";
    }
  in
  let turns =
    [
      Llm.assistant
        [
          Llm.Thinking { text = "look first"; signature = "" };
          Llm.Tool_use
            {
              id = "call_keep";
              name = "read_file";
              input = `Assoc [ ("path", `String "README.md") ];
            };
        ];
      {
        Llm.role = Llm.User;
        content = [ Llm.Tool_result { id = "call_keep"; content = "ok" } ];
      };
    ]
  in
  let body = Model_client.request_body_for_test ~config ~system:"sys" ~turns in
  let open Yojson.Safe.Util in
  let messages = body |> member "messages" |> to_list in
  let assistant = List.nth_exn messages 1 in
  let tool_msg = List.nth_exn messages 2 in
  let tool_call = assistant |> member "tool_calls" |> to_list |> List.hd_exn in
  Alcotest.(check string)
    "assistant tool id" "call_keep"
    (tool_call |> member "id" |> to_string);
  Alcotest.(check string)
    "reasoning content" "look first"
    (assistant |> member "reasoning_content" |> to_string);
  Alcotest.(check string)
    "tool result id" "call_keep"
    (tool_msg |> member "tool_call_id" |> to_string)

let test_request_can_disable_tools () =
  let config protocol =
    {
      Config.provider = "test";
      api_key = "k";
      api_base = "http://localhost/v1";
      model = "m";
      models = [ "m" ];
      protocol;
      compat = Config.default_compat;
      max_tokens = None;
      max_steps = 1;
      workspace_root = ".";
    }
  in
  let body protocol =
    Model_client.request_body_with_options_for_test ~tools_enabled:false
      ~config:(config protocol) ~system:"sys"
      ~turns:[ Llm.user "summarize" ]
  in
  let open Yojson.Safe.Util in
  let openai_body = body Provider.Openai in
  Alcotest.(check bool)
    "openai tools omitted" true
    (match member "tools" openai_body with `Null -> true | _ -> false);
  Alcotest.(check bool)
    "openai tool_choice omitted" true
    (match member "tool_choice" openai_body with `Null -> true | _ -> false);
  let anthropic_body = body Provider.Anthropic in
  Alcotest.(check bool)
    "anthropic tools omitted" true
    (match member "tools" anthropic_body with `Null -> true | _ -> false)

let test_config_providers () =
  Unix.putenv "KIMI_API_KEY" "kimi-secret";
  Unix.putenv "DEEPSEEK_API_KEY" "ds-secret";
  Unix.putenv "LOCAL_API_KEY" "";
  Unix.putenv "LOCAL_MODELS" "";
  Unix.putenv "FP_AGENT_CONFIG" "";
  (match Config.load () with
  | Ok cfg ->
      Alcotest.(check string)
        "default model is kimi" "kimi-for-coding" cfg.model;
      Alcotest.(check string) "default provider" "kimi" cfg.provider;
      Alcotest.(check bool)
        "kimi base" true
        (String.is_substring cfg.api_base ~substring:"api.kimi.com");
      Alcotest.(check bool)
        "kimi uses anthropic protocol" true
        (match cfg.protocol with Provider.Anthropic -> true | _ -> false);
      Alcotest.(check string) "kimi key" "kimi-secret" cfg.api_key;
      let headers =
        Model_client.request_headers_for_test ~config:cfg ~system:"sys"
          ~turns:[ Llm.user "hi" ]
      in
      let header name =
        List.Assoc.find headers name ~equal:(fun a b ->
            String.equal (String.lowercase a) (String.lowercase b))
      in
      Alcotest.(check (option string))
        "kimi x-api-key" (Some "kimi-secret") (header "x-api-key");
      Alcotest.(check (option string))
        "kimi user-agent" (Some "KimiCLI/1.5") (header "user-agent")
  | Error e -> Alcotest.failf "kimi load: %s" e);
  (match Config.load ~provider:"deepseek" () with
  | Ok cfg ->
      Alcotest.(check string) "deepseek model" "deepseek-v4-flash" cfg.model;
      Alcotest.(check string) "deepseek key" "ds-secret" cfg.api_key
  | Error e -> Alcotest.failf "deepseek load: %s" e);
  (match Config.load ~provider:"local" ~model:"qwen-local" () with
  | Ok cfg ->
      Alcotest.(check string) "local provider" "local" cfg.provider;
      Alcotest.(check string) "local model" "qwen-local" cfg.model;
      Alcotest.(check string) "local key optional" "" cfg.api_key;
      Alcotest.(check bool)
        "local uses openai protocol" true
        (match cfg.protocol with Provider.Openai -> true | _ -> false)
  | Error e -> Alcotest.failf "local load: %s" e);
  Unix.putenv "LOCAL_MODELS" "qwen-local,llama3";
  let dir = Stdlib.Filename.temp_dir "fp_agent_provider" "" in
  let config_path = Stdlib.Filename.concat dir "providers.json" in
  Stdlib.Out_channel.with_open_bin config_path (fun oc ->
      Stdlib.Out_channel.output_string oc
        {|
{
  "local-llm": {
    "baseUrl": "http://101.132.142.56:18080/v1",
    "api": "openai-completions",
    "apiKey": "dummy",
    "compat": {
      "supportsDeveloperRole": false,
      "supportsUsageInStreaming": false,
      "maxTokensField": "max_tokens"
    },
    "models": [
      { "id": "qwen36-rtx", "name": "qwen36-rtx", "maxTokens": 8192 },
      { "id": "qwen-coder" }
    ]
  }
}
|});
  Unix.putenv "FP_AGENT_CONFIG" config_path;
  (match Config.load ~provider:"local-llm" () with
  | Ok cfg ->
      Alcotest.(check string) "custom provider" "local-llm" cfg.provider;
      Alcotest.(check string)
        "custom base" "http://101.132.142.56:18080/v1" cfg.api_base;
      Alcotest.(check string) "custom key" "dummy" cfg.api_key;
      Alcotest.(check string) "first custom model" "qwen36-rtx" cfg.model;
      Alcotest.(check (option int))
        "custom max tokens" (Some 8192) cfg.max_tokens;
      Alcotest.(check bool)
        "custom disables usage streaming" false
        cfg.compat.supports_usage_in_streaming;
      Alcotest.(check (option string))
        "custom max token field" (Some "max_tokens") cfg.compat.max_tokens_field;
      Alcotest.(check (list string))
        "custom models"
        [ "qwen36-rtx"; "qwen-coder" ]
        cfg.models;
      let body =
        Model_client.request_body_for_test ~config:cfg ~system:"sys"
          ~turns:[ Llm.user "hi" ]
      in
      let member = Yojson.Safe.Util.member in
      Alcotest.(check bool)
        "custom omits stream usage option" true
        (match member "stream_options" body with `Null -> true | _ -> false);
      Alcotest.(check (option int))
        "custom request max_tokens" (Some 8192)
        (match member "max_tokens" body with `Int n -> Some n | _ -> None)
  | Error e -> Alcotest.failf "custom provider load: %s" e);
  let catalog = Config.available_providers () in
  let find_provider name =
    List.find catalog ~f:(fun (entry : Config.provider_catalog_entry) ->
        String.equal entry.provider_name name)
  in
  (match find_provider "deepseek" with
  | Some entry ->
      Alcotest.(check bool)
        "deepseek catalog has pro" true
        (List.mem entry.provider_models "deepseek-v4-pro" ~equal:String.equal)
  | None -> Alcotest.fail "deepseek missing from catalog");
  (match find_provider "local" with
  | Some entry ->
      Alcotest.(check (list string))
        "local catalog includes LOCAL_MODELS"
        [ "local-model"; "qwen-local"; "llama3" ]
        entry.provider_models
  | None -> Alcotest.fail "local missing from catalog");
  (match find_provider "local-llm" with
  | Some entry ->
      Alcotest.(check string)
        "custom catalog base" "http://101.132.142.56:18080/v1"
        entry.provider_api_base;
      Alcotest.(check (list string))
        "custom catalog models"
        [ "qwen36-rtx"; "qwen-coder" ]
        entry.provider_models
  | None -> Alcotest.fail "custom provider missing from catalog");
  (match
     Config.load ~provider:"local-llm" ~model:"qwen-coder"
       ~api_base:"http://127.0.0.1:18080/v1" ()
   with
  | Ok cfg ->
      Alcotest.(check string) "custom model override" "qwen-coder" cfg.model;
      Alcotest.(check string)
        "custom base override" "http://127.0.0.1:18080/v1" cfg.api_base
  | Error e -> Alcotest.failf "custom override load: %s" e);
  (try Unix.unlink config_path with Unix.Unix_error _ -> ());
  (try Unix.rmdir dir with Unix.Unix_error _ -> ());
  Unix.putenv "FP_AGENT_CONFIG" "";
  Unix.putenv "LOCAL_MODELS" "";
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

let test_write_custom_provider_config () =
  let dir = Stdlib.Filename.temp_dir "fp_agent_provider_write" "" in
  let config_path = Stdlib.Filename.concat dir "providers.json" in
  Unix.putenv "FP_AGENT_CONFIG" config_path;
  let compat =
    Config.
      {
        supports_developer_role = false;
        supports_reasoning_effort = false;
        supports_usage_in_streaming = false;
        max_tokens_field = Some "max_tokens";
      }
  in
  (match
     Config.write_custom_provider ~name:"local-added"
       ~api_base:"http://127.0.0.1:18080/v1" ~api_key:"dummy" ~compat
       ~max_tokens:2048
       ~models:[ "qwen-added"; "qwen-added-fast" ]
       ()
   with
  | Error e -> Alcotest.failf "write provider: %s" e
  | Ok path ->
      Alcotest.(check string) "write path" config_path path;
      Alcotest.(check bool)
        "config file exists" true
        (Stdlib.Sys.file_exists config_path));
  (match
     Config.write_custom_provider ~name:"local-added"
       ~api_base:"http://127.0.0.1:18080/v1" ~models:[ "qwen-added" ] ()
   with
  | Ok _ -> Alcotest.fail "duplicate provider write unexpectedly succeeded"
  | Error e ->
      Alcotest.(check bool)
        "duplicate asks for replace" true
        (String.is_substring e ~substring:"pass --replace"));
  (match Config.load ~provider:"local-added" () with
  | Error e -> Alcotest.failf "load written provider: %s" e
  | Ok cfg ->
      Alcotest.(check string) "written provider" "local-added" cfg.provider;
      Alcotest.(check string)
        "written base" "http://127.0.0.1:18080/v1" cfg.api_base;
      Alcotest.(check (list string))
        "written models"
        [ "qwen-added"; "qwen-added-fast" ]
        cfg.models;
      Alcotest.(check string) "written default model" "qwen-added" cfg.model;
      Alcotest.(check (option int))
        "written max tokens" (Some 2048) cfg.max_tokens;
      Alcotest.(check bool)
        "written local compat" false cfg.compat.supports_usage_in_streaming);
  (match
     Config.write_custom_provider ~replace:true ~name:"local-added"
       ~api_base:"http://127.0.0.1:18080/v1" ~models:[ "qwen-replaced" ] ()
   with
  | Error e -> Alcotest.failf "replace provider: %s" e
  | Ok _ -> ());
  (match Config.load ~provider:"local-added" () with
  | Error e -> Alcotest.failf "load replaced provider: %s" e
  | Ok cfg ->
      Alcotest.(check (list string))
        "replaced models" [ "qwen-replaced" ] cfg.models);
  (try Unix.unlink config_path with Unix.Unix_error _ -> ());
  (try Unix.rmdir dir with Unix.Unix_error _ -> ());
  Unix.putenv "FP_AGENT_CONFIG" ""

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
          Alcotest.test_case "update_plan" `Quick test_parse_update_plan;
          Alcotest.test_case "tool_calls_batch" `Quick
            test_parse_tool_calls_batch;
          Alcotest.test_case "array_tool_batch" `Quick
            test_parse_array_tool_batch;
          Alcotest.test_case "anthropic_tool_use" `Quick
            test_parse_anthropic_tool_use;
          Alcotest.test_case "anthropic_tool_use_batch" `Quick
            test_parse_anthropic_tool_use_batch;
          Alcotest.test_case "mixed_content_tool_use_batch" `Quick
            test_parse_mixed_content_tool_use_batch;
          Alcotest.test_case "text_only_tool_calls_as_final" `Quick
            test_parse_text_only_tool_calls_as_final;
          Alcotest.test_case "name_args_tool_calls" `Quick
            test_parse_name_args_tool_calls;
          Alcotest.test_case "ppx_variant_tool_call" `Quick
            test_parse_ppx_variant_tool_call;
          Alcotest.test_case "single_string_array_as_final" `Quick
            test_parse_single_string_array_as_final;
          Alcotest.test_case "array_wrapped" `Quick test_parse_array_wrapped;
          Alcotest.test_case "non_object" `Quick test_parse_non_object_errors;
          Alcotest.test_case "final_answer" `Quick test_parse_final_answer;
          Alcotest.test_case "fences" `Quick test_parse_with_fences;
          Alcotest.test_case "invalid_json" `Quick test_parse_invalid_json;
          Alcotest.test_case "unknown_action" `Quick test_parse_unknown_action;
        ] );
      ("client", [ Alcotest.test_case "mock" `Quick test_mock_client ]);
      ( "provider_blocks",
        [
          Alcotest.test_case "streaming_tool_ids" `Quick test_streaming_tool_ids;
          Alcotest.test_case "streaming_reasoning_blocks" `Quick
            test_streaming_preserves_reasoning_blocks;
          Alcotest.test_case "request_preserves_tool_result_ids" `Quick
            test_request_preserves_tool_result_ids;
          Alcotest.test_case "request_can_disable_tools" `Quick
            test_request_can_disable_tools;
        ] );
      ( "config",
        [
          Alcotest.test_case "providers" `Quick test_config_providers;
          Alcotest.test_case "write_provider" `Quick
            test_write_custom_provider_config;
        ] );
    ]
