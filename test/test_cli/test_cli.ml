open! Base
open Fp_agent

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

let assert_not_contains label haystack needle =
  Alcotest.(check bool)
    label false
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
  Alcotest.(check bool)
    "readme created" true
    (Stdlib.Sys.file_exists (Stdlib.Filename.concat plugin_dir "README.md"));
  Alcotest.(check bool)
    "sample args created" true
    (Stdlib.Sys.file_exists
       (Stdlib.Filename.concat plugin_dir
          (Stdlib.Filename.concat "examples" "hello.args.json")));
  let checked = run ~env [ bin; "--check-plugin"; plugin_dir ] in
  assert_success "check plugin" checked;
  assert_contains "check output" checked.stdout "plugin manifest ok";
  assert_contains "check tool output" checked.stdout "hello_world";
  let check_conflict_dir = Stdlib.Filename.concat root "conflict-check" in
  mkdir_p check_conflict_dir;
  write_file
    (Stdlib.Filename.concat check_conflict_dir "fp-agent-plugin.json")
    {|{
  "id": "com.example.check_conflict",
  "tools": [
    {
      "name": "read_file",
      "kind": "read",
      "description": "Conflicts with a built-in tool",
      "command": "sh echo.sh"
    }
  ]
}|};
  write_file (Stdlib.Filename.concat check_conflict_dir "echo.sh") "cat\n";
  let checked_conflict =
    run ~env [ bin; "--check-plugin"; check_conflict_dir ]
  in
  assert_failure "check plugin conflict" checked_conflict;
  assert_contains "check conflict stderr" checked_conflict.stderr
    "plugin tool name conflict";
  assert_contains "check conflict owner" checked_conflict.stderr "built-in tool";
  let empty = run ~env [ bin; "--list-plugins" ] in
  assert_success "list plugins before install" empty;
  assert_contains "empty plugin list" empty.stdout "(no installed plugins)";
  let installed = run ~env [ bin; "--install-plugin"; plugin_dir ] in
  assert_success "install plugin" installed;
  assert_contains "install output" installed.stdout "installed plugin:";
  let installed_dir = Stdlib.Filename.concat home "local.my-plugin" in
  Alcotest.(check bool)
    "installed manifest" true
    (Stdlib.Sys.file_exists
       (Stdlib.Filename.concat installed_dir "fp-agent-plugin.json"));
  Alcotest.(check bool)
    "installed readme" true
    (Stdlib.Sys.file_exists (Stdlib.Filename.concat installed_dir "README.md"));
  let duplicate = run ~env [ bin; "--install-plugin"; plugin_dir ] in
  assert_failure "install duplicate plugin" duplicate;
  assert_contains "duplicate install stderr" duplicate.stderr
    "plugin already installed";
  write_file
    (Stdlib.Filename.concat plugin_dir "hello.sh")
    "#!/bin/sh\nprintf 'replacement plugin: '\ncat\n";
  let replaced =
    run ~env [ bin; "--install-plugin"; plugin_dir; "--replace-plugin" ]
  in
  assert_success "replace plugin install" replaced;
  assert_contains "replace install output" replaced.stdout "installed plugin:";
  let checked_replace =
    run ~env [ bin; "--check-plugin"; plugin_dir; "--replace-plugin" ]
  in
  assert_success "check plugin replacement" checked_replace;
  assert_contains "check replacement output" checked_replace.stdout
    "plugin manifest ok";
  let replaced_run =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        installed_dir;
        "--plugin-tool";
        "hello_world";
        "--plugin-args";
        {|{"message":"replace"}|};
      ]
  in
  assert_success "run replaced plugin" replaced_run;
  assert_contains "replaced plugin output" replaced_run.stdout
    "replacement plugin:";
  let replace_without_install = run ~env [ bin; "--replace-plugin" ] in
  assert_failure "replace without install" replace_without_install;
  assert_contains "replace without install stderr"
    replace_without_install.stderr
    "--replace-plugin requires --install-plugin DIR or --check-plugin DIR";
  let invalid_installed = Stdlib.Filename.concat home "invalid-plugin" in
  mkdir_p invalid_installed;
  write_file
    (Stdlib.Filename.concat invalid_installed "fp-agent-plugin.json")
    {|{"id":"com.example.invalid","tools":[]}|};
  let conflict_installed = Stdlib.Filename.concat home "conflict-plugin" in
  mkdir_p conflict_installed;
  write_file
    (Stdlib.Filename.concat conflict_installed "fp-agent-plugin.json")
    {|{
  "id": "com.example.installed_conflict",
  "tools": [
    {
      "name": "read_file",
      "kind": "read",
      "description": "Conflicts with a built-in tool",
      "command": "sh echo.sh"
    }
  ]
}|};
  write_file (Stdlib.Filename.concat conflict_installed "echo.sh") "cat\n";
  let listed = run ~env [ bin; "--list-plugins" ] in
  assert_success "list plugins after install" listed;
  assert_contains "list plugin id" listed.stdout "local.my-plugin";
  assert_contains "list plugin tool" listed.stdout "hello_world";
  assert_contains "list invalid plugin section" listed.stdout
    "Invalid installed plugins:";
  assert_contains "list invalid plugin error" listed.stdout "at least one tool";
  assert_contains "list conflict section" listed.stdout "Plugin tool conflicts:";
  assert_contains "list conflict owner" listed.stdout
    "read_file from com.example.installed_conflict skipped; already provided \
     by built-in tool";
  let removed = run ~env [ bin; "--remove-plugin"; "local.my-plugin" ] in
  assert_success "remove plugin" removed;
  assert_contains "remove output" removed.stdout "removed plugin:";
  Alcotest.(check bool)
    "removed manifest" false
    (Stdlib.Sys.file_exists
       (Stdlib.Filename.concat home
          (Stdlib.Filename.concat "local.my-plugin" "fp-agent-plugin.json")));
  let missing = run ~env [ bin; "--remove-plugin"; "local.my-plugin" ] in
  assert_failure "remove missing plugin" missing;
  assert_contains "remove missing stderr" missing.stderr
    "plugin is not installed"

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
  let args_file =
    Stdlib.Filename.concat plugin_dir
      (Stdlib.Filename.concat "examples" "hello.args.json")
  in
  let ok_file =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        plugin_dir;
        "--plugin-tool";
        "hello_world";
        "--plugin-args-file";
        args_file;
      ]
  in
  assert_success "run plugin tool args file" ok_file;
  assert_contains "plugin args file output" ok_file.stdout {|"message":"hi"|};
  let both_args =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        plugin_dir;
        "--plugin-tool";
        "hello_world";
        "--plugin-args";
        {|{"message":"hi"}|};
        "--plugin-args-file";
        args_file;
      ]
  in
  assert_failure "both plugin arg sources" both_args;
  assert_contains "both args stderr" both_args.stderr
    "use only one of --plugin-args or --plugin-args-file";
  let missing_args_file =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        plugin_dir;
        "--plugin-tool";
        "hello_world";
        "--plugin-args-file";
        Stdlib.Filename.concat plugin_dir "missing.args.json";
      ]
  in
  assert_failure "missing plugin args file" missing_args_file;
  assert_contains "missing args file stderr" missing_args_file.stderr
    "cannot read plugin args file";
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
  let bad_args =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        plugin_dir;
        "--plugin-tool";
        "hello_world";
        "--plugin-args";
        "{}";
      ]
  in
  assert_failure "bad plugin args" bad_args;
  assert_contains "bad args stderr" bad_args.stderr "schema validation failed";
  assert_contains "bad args detail" bad_args.stderr
    "missing required field 'message'";
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
  let bad_plugin_dir = Stdlib.Filename.concat root "bad-plugin" in
  let conflict_plugin_dir = Stdlib.Filename.concat root "conflict-plugin" in
  let bin = fp_agent_bin () in
  let env =
    isolated_env root
    @ [
        ( "FP_AGENT_PLUGIN_PATH",
          String.concat ~sep:":"
            [ plugin_dir; bad_plugin_dir; conflict_plugin_dir ] );
      ]
  in
  assert_success "new plugin" (run ~env [ bin; "--new-plugin"; plugin_dir ]);
  mkdir_p bad_plugin_dir;
  write_file
    (Stdlib.Filename.concat bad_plugin_dir "fp-agent-plugin.json")
    {|{"id":"com.example.bad","tools":[]}|};
  mkdir_p conflict_plugin_dir;
  write_file
    (Stdlib.Filename.concat conflict_plugin_dir "fp-agent-plugin.json")
    {|{
  "id": "com.example.conflict",
  "tools": [
    {
      "name": "read_file",
      "kind": "read",
      "description": "Conflicts with a built-in tool",
      "command": "sh echo.sh"
    }
  ]
}|};
  write_file (Stdlib.Filename.concat conflict_plugin_dir "echo.sh") "cat\n";
  let repl =
    run ~env
      ~stdin:
        "/plugins\n\
         /plugin local.my-plugin\n\
         /plugin hello_world\n\
         /plugin missing\n\
         /tool read_file\n\
         /tool hello_world\n\
         /tool missing\n\
         /tools\n\
         /exit\n"
      [ bin ]
  in
  assert_success "repl plugin commands" repl;
  assert_contains "plugins lists scaffold" repl.stdout "local.my-plugin";
  assert_contains "plugins report invalid section" repl.stdout
    "Invalid plugins:";
  assert_contains "plugins report invalid error" repl.stdout "at least one tool";
  assert_contains "plugins report conflict section" repl.stdout
    "Plugin tool conflicts:";
  assert_contains "plugins report conflict owner" repl.stdout
    "read_file from com.example.conflict skipped; already provided by built-in \
     tool";
  assert_contains "plugin detail id" repl.stdout "id: local.my-plugin";
  assert_contains "plugin detail tool" repl.stdout "- hello_world";
  assert_contains "plugin detail command" repl.stdout "command: sh hello.sh";
  assert_contains "plugin detail schema" repl.stdout "input_schema:";
  assert_contains "plugin missing" repl.stdout
    "no plugin or tool matching: missing";
  assert_contains "builtin tool detail" repl.stdout "name: read_file";
  assert_contains "plugin tool detail" repl.stdout "name: hello_world";
  assert_contains "tool detail schema" repl.stdout "input_schema:";
  assert_contains "tool missing" repl.stdout "no tool matching: missing";
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
  let log = Event_log.create ~session_dir in
  Event_log.append log
    (Event.Tool_call
       (Tool_call.make ~name:"search"
          ~args:
            (`Assoc [ ("query", `String "Plugin"); ("path", `String "lib") ])));
  Event_log.append log
    (Event.Assistant_message
       {
         content = [ Llm.Text "done" ];
         usage = { input_tokens = 31; output_tokens = 9 };
       });
  Event_log.close log;
  let repl =
    run ~env
      ~stdin:
        "/log\n\
         /usage\n\
         /inspect 0\n\
         /inspect\n\
         /inspect 3\n\
         /inspect nope\n\
         /retry\n\
         /new\n\
         /log\n\
         /exit\n"
      [ fp_agent_bin (); "--resume"; session_dir ]
  in
  assert_success "repl inspect command" repl;
  assert_contains "log includes event" repl.stdout "tool_call search";
  assert_contains "inspect prints index" repl.stdout "event 0";
  assert_contains "inspect kind" repl.stdout "kind: tool_call";
  assert_contains "inspect tool" repl.stdout "tool: search";
  assert_contains "inspect args" repl.stdout "\"query\": \"Plugin\"";
  assert_contains "inspect json" repl.stdout "JSON";
  assert_contains "usage input" repl.stdout "input_tokens: 31";
  assert_contains "usage total" repl.stdout "total_tokens: 40";
  assert_contains "inspect range" repl.stdout "no event at index 3 (0..1)";
  assert_contains "inspect usage" repl.stdout "usage: /inspect [event-index]";
  assert_contains "retry without user task" repl.stdout
    "no previous user task to retry";
  assert_contains "new session switched" repl.stdout "new session:";
  assert_contains "new session has empty log" repl.stdout "(no events yet)"

let test_tui_confirm_reaches_config_for_oneshot () =
  let root = tmp_dir "fp-agent-cli-tui-" in
  let env = isolated_env root in
  let result =
    run ~env
      [
        fp_agent_bin ();
        "--provider";
        "missing-provider";
        "--confirm";
        "--tui";
        "touch a file";
      ]
  in
  assert_failure "confirm tui config error" result;
  assert_contains "config stderr" result.stderr
    "config error: unknown provider: missing-provider";
  assert_not_contains "no tui confirm conflict" result.stderr
    "--confirm cannot be combined with --tui"

let test_tui_confirm_reaches_config_for_repl () =
  let root = tmp_dir "fp-agent-cli-tui-repl-" in
  let env = isolated_env root in
  let result =
    run ~env
      [
        fp_agent_bin (); "--provider"; "missing-provider"; "--confirm"; "--tui";
      ]
  in
  assert_failure "confirm tui repl config error" result;
  assert_contains "config stderr" result.stderr
    "config error: unknown provider: missing-provider";
  assert_not_contains "no tui repl confirm conflict" result.stderr
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
          Alcotest.test_case "tui confirm config" `Quick
            test_tui_confirm_reaches_config_for_oneshot;
          Alcotest.test_case "tui repl confirm config" `Quick
            test_tui_confirm_reaches_config_for_repl;
        ] );
    ]
