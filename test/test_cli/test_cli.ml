open! Base

type run_result = { code : int; stdout : string; stderr : string }

let rec find_root dir =
  if Stdlib.Sys.file_exists (Stdlib.Filename.concat dir "dune-project") then dir
  else
    let parent = Stdlib.Filename.dirname dir in
    if String.equal parent dir then failwith "could not locate project root";
    find_root parent

let project_root () = find_root (Stdlib.Sys.getcwd ())

let fp_agent_bin () =
  match Stdlib.Sys.getenv_opt "FP_AGENT_BIN" with
  | Some p when not (String.is_empty p) -> p
  | _ ->
      Stdlib.Filename.concat (project_root ())
        (Stdlib.Filename.concat "_build"
           (Stdlib.Filename.concat "default"
              (Stdlib.Filename.concat "bin" "main.exe")))

let quote = Stdlib.Filename.quote

let env_with overrides =
  let drop name =
    List.exists overrides ~f:(fun (k, _) -> String.equal k name)
  in
  let inherited =
    Array.to_list (Unix.environment ())
    |> List.filter ~f:(fun kv ->
        match String.lsplit2 kv ~on:'=' with
        | Some (name, _) -> not (drop name)
        | None -> true)
  in
  Array.of_list
    (inherited
    @ List.map overrides ~f:(fun (k, v) -> Printf.sprintf "%s=%s" k v))

let run ?(stdin = "") ?(env = []) args =
  let command = String.concat ~sep:" " (List.map args ~f:quote) in
  let stdout_ic, stdin_oc, stderr_ic =
    Unix.open_process_full command (env_with env)
  in
  Stdlib.Out_channel.output_string stdin_oc stdin;
  Stdlib.Out_channel.close stdin_oc;
  let stdout = Stdlib.In_channel.input_all stdout_ic in
  let stderr = Stdlib.In_channel.input_all stderr_ic in
  let status = Unix.close_process_full (stdout_ic, stdin_oc, stderr_ic) in
  let code =
    match status with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n -> 128 + n
    | Unix.WSTOPPED n -> 128 + n
  in
  { code; stdout; stderr }

let tmp_dir prefix =
  let file = Stdlib.Filename.temp_file prefix "" in
  Stdlib.Sys.remove file;
  Unix.mkdir file 0o755;
  file

let mkdir_p path =
  if not (Stdlib.Sys.file_exists path) then Unix.mkdir path 0o755

let write_file path content =
  Stdlib.Out_channel.with_open_bin path (fun oc ->
      Stdlib.Out_channel.output_string oc content)

let assert_success label result =
  Alcotest.(check int) (label ^ " exit") 0 result.code

let assert_failure label result =
  Alcotest.(check bool) (label ^ " failed") true (not (Int.equal result.code 0))

let assert_contains label haystack needle =
  Alcotest.(check bool)
    label true
    (String.is_substring haystack ~substring:needle)

let isolated_env root =
  let home = Stdlib.Filename.concat root "home" in
  let workspace = Stdlib.Filename.concat root "workspace" in
  let sessions = Stdlib.Filename.concat workspace ".ocaml-agent" in
  mkdir_p home;
  mkdir_p workspace;
  mkdir_p sessions;
  [
    ("HOME", home);
    ("WORKSPACE_ROOT", workspace);
    ("KIMI_API_KEY", "dummy");
    ("FP_AGENT_PLUGIN_HOME", Stdlib.Filename.concat root "plugins-home");
  ]

let test_plugin_lifecycle_cli () =
  let root = tmp_dir "fp-agent-cli-plugin-" in
  let plugin_dir = Stdlib.Filename.concat root "my-plugin" in
  let home = Stdlib.Filename.concat root "installed" in
  let bin = fp_agent_bin () in
  let env = isolated_env root @ [ ("FP_AGENT_PLUGIN_HOME", home) ] in
  let created = run ~env [ bin; "--new-plugin"; plugin_dir ] in
  assert_success "new plugin" created;
  assert_contains "new plugin output" created.stdout "created plugin scaffold";
  Alcotest.(check bool)
    "manifest created" true
    (Stdlib.Sys.file_exists
       (Stdlib.Filename.concat plugin_dir "fp-agent-plugin.json"));
  let checked = run ~env [ bin; "--check-plugin"; plugin_dir ] in
  assert_success "check plugin" checked;
  assert_contains "check output" checked.stdout "plugin manifest ok";
  assert_contains "check tool output" checked.stdout "hello_world";
  let installed = run ~env [ bin; "--install-plugin"; plugin_dir ] in
  assert_success "install plugin" installed;
  assert_contains "install output" installed.stdout "installed plugin:";
  Alcotest.(check bool)
    "installed manifest" true
    (Stdlib.Sys.file_exists
       (Stdlib.Filename.concat home
          (Stdlib.Filename.concat "local.my-plugin" "fp-agent-plugin.json")))

let test_plugin_tool_debug_cli () =
  let root = tmp_dir "fp-agent-cli-plugin-run-" in
  let plugin_dir = Stdlib.Filename.concat root "my-plugin" in
  let bin = fp_agent_bin () in
  let env = isolated_env root in
  assert_success "new plugin" (run ~env [ bin; "--new-plugin"; plugin_dir ]);
  let ok =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        plugin_dir;
        "--plugin-tool";
        "hello_world";
        "--plugin-args";
        {|{"message":"hi"}|};
      ]
  in
  assert_success "run plugin tool" ok;
  assert_contains "plugin output prefix" ok.stdout "hello from fp-agent plugin:";
  assert_contains "plugin output args" ok.stdout {|"message":"hi"|};
  let bad_json =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        plugin_dir;
        "--plugin-tool";
        "hello_world";
        "--plugin-args";
        "{";
      ]
  in
  assert_failure "bad plugin JSON" bad_json;
  assert_contains "bad json stderr" bad_json.stderr "invalid plugin args JSON";
  let missing =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        plugin_dir;
        "--plugin-tool";
        "missing_tool";
        "--plugin-args";
        "{}";
      ]
  in
  assert_failure "missing plugin tool" missing;
  assert_contains "missing tool stderr" missing.stderr "unknown plugin tool"

let test_repl_lists_dynamic_plugin_tools () =
  let root = tmp_dir "fp-agent-cli-repl-plugin-" in
  let plugin_dir = Stdlib.Filename.concat root "my-plugin" in
  let bin = fp_agent_bin () in
  let env = isolated_env root @ [ ("FP_AGENT_PLUGIN_PATH", plugin_dir) ] in
  assert_success "new plugin" (run ~env [ bin; "--new-plugin"; plugin_dir ]);
  let repl = run ~env ~stdin:"/plugins\n/tools\n/exit\n" [ bin ] in
  assert_success "repl plugin commands" repl;
  assert_contains "plugins lists scaffold" repl.stdout "local.my-plugin";
  assert_contains "tools lists plugin tool" repl.stdout "hello_world";
  assert_contains "tools marks plugin" repl.stdout "plugin local.my-plugin"

let provider_config =
  {|{
  "local-llm": {
    "baseUrl": "http://101.132.142.56:18080/v1",
    "api": "openai-completions",
    "apiKey": "dummy",
    "compat": {
      "supportsDeveloperRole": false,
      "supportsReasoningEffort": false,
      "supportsUsageInStreaming": false,
      "maxTokensField": "max_tokens"
    },
    "models": [
      {
        "id": "qwen36-rtx",
        "name": "qwen36-rtx",
        "reasoning": false,
        "input": ["text"],
        "contextWindow": 131072,
        "maxTokens": 8192,
        "cost": {
          "input": 0,
          "output": 0,
          "cacheRead": 0,
          "cacheWrite": 0
        }
      }
    ]
  }
}
|}

let test_repl_lists_and_switches_custom_provider_models () =
  let root = tmp_dir "fp-agent-cli-models-" in
  let config_path = Stdlib.Filename.concat root "providers.json" in
  write_file config_path provider_config;
  let env = isolated_env root @ [ ("FP_AGENT_CONFIG", config_path) ] in
  let repl =
    run ~env ~stdin:"/models\n/provider local-llm qwen36-rtx\n/model\n/exit\n"
      [ fp_agent_bin () ]
  in
  assert_success "repl model commands" repl;
  assert_contains "models include deepseek" repl.stdout "deepseek";
  assert_contains "models include custom provider" repl.stdout "local-llm";
  assert_contains "models include custom model" repl.stdout "qwen36-rtx";
  assert_contains "provider switched" repl.stdout "provider: local-llm";
  assert_contains "model switched" repl.stdout "model: qwen36-rtx"

let test_repl_inspects_session_events () =
  let root = tmp_dir "fp-agent-cli-inspect-" in
  let env = isolated_env root in
  let workspace =
    match List.Assoc.find env "WORKSPACE_ROOT" ~equal:String.equal with
    | Some path -> path
    | None -> Alcotest.fail "missing workspace env"
  in
  let sessions_root =
    Stdlib.Filename.concat workspace
      (Stdlib.Filename.concat ".ocaml-agent" "sessions")
  in
  mkdir_p sessions_root;
  let session_dir = Stdlib.Filename.concat sessions_root "inspect-session" in
  mkdir_p session_dir;
  write_file
    (Stdlib.Filename.concat session_dir "events.jsonl")
    {|{"schema_version":1,"ts":"2026-05-24T00:00:00Z","event":["Tool_call",{"name":"search","args":{"query":"Plugin","path":"lib"}}]}
|};
  let repl =
    run ~env
      ~stdin:"/log\n/inspect 0\n/inspect\n/inspect 3\n/inspect nope\n/exit\n"
      [ fp_agent_bin (); "--resume"; session_dir ]
  in
  assert_success "repl inspect command" repl;
  assert_contains "log includes event" repl.stdout "tool_call search";
  assert_contains "inspect prints index" repl.stdout "event 0";
  assert_contains "inspect kind" repl.stdout "kind: tool_call";
  assert_contains "inspect tool" repl.stdout "tool: search";
  assert_contains "inspect args" repl.stdout "\"query\": \"Plugin\"";
  assert_contains "inspect json" repl.stdout "JSON";
  assert_contains "inspect range" repl.stdout "no event at index 3 (0..0)";
  assert_contains "inspect usage" repl.stdout "usage: /inspect [event-index]"

let test_tui_confirm_conflict_fails_before_config () =
  let root = tmp_dir "fp-agent-cli-tui-" in
  let env = isolated_env root in
  let result =
    run ~env [ fp_agent_bin (); "--confirm"; "--tui"; "touch a file" ]
  in
  assert_failure "confirm tui conflict" result;
  assert_contains "conflict stderr" result.stderr
    "--confirm cannot be combined with --tui"

let () =
  Alcotest.run "cli"
    [
      ( "cli",
        [
          Alcotest.test_case "plugin lifecycle" `Quick test_plugin_lifecycle_cli;
          Alcotest.test_case "plugin tool debug" `Quick
            test_plugin_tool_debug_cli;
          Alcotest.test_case "repl plugin tools" `Quick
            test_repl_lists_dynamic_plugin_tools;
          Alcotest.test_case "custom provider models" `Quick
            test_repl_lists_and_switches_custom_provider_models;
          Alcotest.test_case "repl inspect events" `Quick
            test_repl_inspects_session_events;
          Alcotest.test_case "tui confirm conflict" `Quick
            test_tui_confirm_conflict_fails_before_config;
        ] );
    ]
