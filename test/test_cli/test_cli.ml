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
  let sdk = run ~env [ bin; "--plugin-sdk" ] in
  assert_success "plugin sdk" sdk;
  assert_contains "plugin sdk manifest" sdk.stdout
    "manifest_file: fp-agent-plugin.json";
  assert_contains "plugin sdk template" sdk.stdout
    "python (aliases: python3, py)";
  assert_contains "plugin sdk workflow" sdk.stdout
    "/plugin-dev --replace my-plugin";
  let created = run ~env [ bin; "--new-plugin"; plugin_dir ] in
  assert_success "new plugin" created;
  assert_contains "new plugin output" created.stdout "created plugin scaffold";
  let plugin_args_file =
    Stdlib.Filename.concat plugin_dir
      (Stdlib.Filename.concat "examples" "hello_world.args.json")
  in
  assert_contains "new plugin run hint" created.stdout
    ("next: /plugin-run " ^ plugin_dir ^ " hello_world @" ^ plugin_args_file);
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
          (Stdlib.Filename.concat "examples" "hello_world.args.json")));
  let smoke_cases_dir =
    Stdlib.Filename.concat plugin_dir
      (Stdlib.Filename.concat "examples" "hello_world")
  in
  mkdir_p smoke_cases_dir;
  write_file
    (Stdlib.Filename.concat smoke_cases_dir "01-case.json")
    {|{"message":"from-case"}|};
  let checked = run ~env [ bin; "--check-plugin"; plugin_dir ] in
  assert_success "check plugin" checked;
  assert_contains "check output" checked.stdout "plugin manifest ok";
  assert_contains "check tool output" checked.stdout "hello_world";
  let smoked = run ~env [ bin; "--smoke-plugin"; plugin_dir ] in
  assert_success "smoke plugin" smoked;
  assert_contains "smoke output" smoked.stdout "smoke ok: hello_world";
  assert_contains "smoke tool output" smoked.stdout
    "hello from fp-agent plugin:";
  assert_contains "smoke case file" smoked.stdout
    "examples/hello_world/01-case.json";
  assert_contains "smoke case output" smoked.stdout "from-case";
  let missing_smoke_dir = Stdlib.Filename.concat root "missing-smoke" in
  assert_success "new plugin missing smoke"
    (run ~env [ bin; "--new-plugin"; missing_smoke_dir ]);
  let missing_args =
    Stdlib.Filename.concat missing_smoke_dir
      (Stdlib.Filename.concat "examples" "hello_world.args.json")
  in
  Stdlib.Sys.remove missing_args;
  let missing_smoke = run ~env [ bin; "--smoke-plugin"; missing_smoke_dir ] in
  assert_failure "smoke missing args" missing_smoke;
  assert_contains "smoke missing args stderr" missing_smoke.stderr
    "missing smoke args for tool hello_world";
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
  assert_contains "install plugin id hint" installed.stdout
    "plugin id: local.my-plugin";
  let installed_args_file =
    Stdlib.Filename.concat installed_dir
      (Stdlib.Filename.concat "examples" "hello_world.args.json")
  in
  assert_contains "install run hint" installed.stdout
    ("next: /plugin-run " ^ installed_dir ^ " hello_world @"
   ^ installed_args_file);
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
    "--replace-plugin requires --install-plugin DIR or --check-plugin DIR or \
     --smoke-plugin DIR or --dev-plugin DIR";
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
  let doctor = run ~env [ bin; "--doctor-plugins" ] in
  assert_success "doctor plugins" doctor;
  assert_contains "doctor header" doctor.stdout "Plugin diagnostics";
  assert_contains "doctor install home" doctor.stdout "install_home:";
  assert_contains "doctor search roots" doctor.stdout "search_roots:";
  assert_contains "doctor valid count" doctor.stdout "valid_plugins: 2";
  assert_contains "doctor invalid count" doctor.stdout "invalid_plugins: 1";
  assert_contains "doctor conflict count" doctor.stdout "tool_conflicts: 1";
  assert_contains "doctor next command" doctor.stdout
    "next: /plugin-dev --replace <dir>";
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

let test_repl_installs_and_removes_plugin () =
  let root = tmp_dir "fp-agent-cli-repl-install-" in
  let plugin_dir = Stdlib.Filename.concat root "my-plugin" in
  let home = Stdlib.Filename.concat root "installed" in
  let bin = fp_agent_bin () in
  let env =
    isolated_env root
    @ [ ("FP_AGENT_PLUGIN_PATH", ""); ("FP_AGENT_PLUGIN_HOME", home) ]
  in
  let repl =
    run ~env
      ~stdin:
        (String.concat ~sep:"\n"
           [
             "/plugin-new --id local.repl-plugin --tool-name repl_echo "
             ^ plugin_dir;
             "/plugin-check " ^ plugin_dir;
             "/plugin-run " ^ plugin_dir ^ {| repl_echo {"message":"from-run"}|};
             "/plugin-run " ^ plugin_dir ^ " repl_echo @" ^ plugin_dir
             ^ "/examples/repl_echo.args.json";
             "/plugin-install " ^ plugin_dir;
             "/plugins";
             "/plugin-doctor";
             "/tool repl_echo";
             "/plugin-smoke --replace " ^ plugin_dir;
             "/plugin-remove local.repl-plugin";
             "/plugin-dev --replace " ^ plugin_dir;
             "/tool repl_echo";
             "/plugin-remove local.repl-plugin";
             "/tool repl_echo";
             "/plugins";
             "/exit";
             "";
           ])
      [ bin ]
  in
  assert_success "repl plugin install commands" repl;
  assert_contains "new output" repl.stdout "created plugin scaffold:";
  assert_contains "new install hint" repl.stdout
    ("next: /plugin-install --replace " ^ plugin_dir);
  assert_contains "check output" repl.stdout "plugin manifest ok:";
  assert_contains "plugin run output" repl.stdout "plugin run ok: repl_echo";
  assert_contains "plugin run inline args" repl.stdout "from-run";
  assert_contains "plugin run file args" repl.stdout {|"message":"hi"|};
  assert_contains "install output" repl.stdout "installed plugin:";
  assert_contains "install reload output" repl.stdout "tools reloaded";
  assert_contains "install count output" repl.stdout
    "tools reloaded; tooling: 1 plugin(s)";
  assert_contains "install plugin id hint" repl.stdout
    "plugin id: local.repl-plugin";
  assert_contains "install tool list hint" repl.stdout "tools: repl_echo";
  assert_contains "install plugin inspect hint" repl.stdout
    "next: /plugin local.repl-plugin";
  assert_contains "install tool inspect hint" repl.stdout
    "next: /tool repl_echo";
  assert_contains "plugin listed after install" repl.stdout "local.repl-plugin";
  assert_contains "plugin doctor output" repl.stdout "Plugin diagnostics";
  assert_contains "plugin doctor next command" repl.stdout
    "next: /plugin-check <dir>";
  assert_contains "tool available after install" repl.stdout "name: repl_echo";
  assert_contains "smoke output" repl.stdout "smoke ok: repl_echo";
  assert_contains "dev check output" repl.stdout
    "plugin dev check ok: local.repl-plugin";
  assert_contains "dev smoke output" repl.stdout "plugin dev smoke ok:";
  assert_contains "dev install output" repl.stdout "installed plugin:";
  assert_contains "remove output" repl.stdout "removed plugin:";
  assert_contains "remove count output" repl.stdout
    "tools reloaded; tooling: 0 plugin(s)";
  assert_contains "remove plugins hint" repl.stdout "next: /plugins";
  assert_contains "tool gone after remove" repl.stdout
    "no tool matching: repl_echo";
  assert_contains "plugins empty after remove" repl.stdout
    "(no plugins discovered)"

let test_new_plugin_custom_id_cli () =
  let root = tmp_dir "fp-agent-cli-plugin-id-" in
  let plugin_dir = Stdlib.Filename.concat root "named-plugin" in
  let home = Stdlib.Filename.concat root "installed" in
  let bin = fp_agent_bin () in
  let env = isolated_env root @ [ ("FP_AGENT_PLUGIN_HOME", home) ] in
  let created =
    run ~env
      [
        bin;
        "--new-plugin";
        plugin_dir;
        "--plugin-id";
        "com.example.named_plugin";
        "--plugin-tool-name";
        "named_echo";
        "--plugin-kind";
        "exec";
      ]
  in
  assert_success "new plugin custom id" created;
  Alcotest.(check bool)
    "custom tool args created" true
    (Stdlib.Sys.file_exists
       (Stdlib.Filename.concat plugin_dir
          (Stdlib.Filename.concat "examples" "named_echo.args.json")));
  let checked = run ~env [ bin; "--check-plugin"; plugin_dir ] in
  assert_success "check custom id plugin" checked;
  assert_contains "custom id in check output" checked.stdout
    "com.example.named_plugin";
  assert_contains "custom tool in check output" checked.stdout "named_echo";
  assert_contains "custom kind in check output" checked.stdout "exec";
  let smoked = run ~env [ bin; "--smoke-plugin"; plugin_dir ] in
  assert_success "smoke custom tool plugin" smoked;
  assert_contains "custom tool smoke output" smoked.stdout
    "smoke ok: named_echo";
  let installed = run ~env [ bin; "--dev-plugin"; plugin_dir ] in
  assert_success "dev custom id plugin" installed;
  assert_contains "dev custom check output" installed.stdout
    "plugin dev check ok: com.example.named_plugin";
  assert_contains "dev custom smoke output" installed.stdout
    "plugin dev smoke ok:";
  assert_contains "dev custom tool hint" installed.stdout
    "next: /tool named_echo";
  let installed_named_dir =
    Stdlib.Filename.concat home "com.example.named_plugin"
  in
  let installed_named_args =
    Stdlib.Filename.concat installed_named_dir
      (Stdlib.Filename.concat "examples" "named_echo.args.json")
  in
  assert_contains "dev custom plugin run hint" installed.stdout
    ("next: /plugin-run " ^ installed_named_dir ^ " named_echo @"
   ^ installed_named_args);
  Alcotest.(check bool)
    "installed under custom id" true
    (Stdlib.Sys.file_exists
       (Stdlib.Filename.concat home
          (Stdlib.Filename.concat "com.example.named_plugin"
             "fp-agent-plugin.json")));
  let invalid =
    run ~env
      [
        bin;
        "--new-plugin";
        Stdlib.Filename.concat root "bad";
        "--plugin-id";
        "bad id";
      ]
  in
  assert_failure "new plugin invalid custom id" invalid;
  assert_contains "invalid custom id stderr" invalid.stderr "plugin id";
  let invalid_tool =
    run ~env
      [
        bin;
        "--new-plugin";
        Stdlib.Filename.concat root "bad-tool";
        "--plugin-tool-name";
        "bad tool";
      ]
  in
  assert_failure "new plugin invalid custom tool" invalid_tool;
  assert_contains "invalid custom tool stderr" invalid_tool.stderr "tool name";
  let stray = run ~env [ bin; "--plugin-id"; "com.example.only" ] in
  assert_failure "plugin id without new plugin" stray;
  assert_contains "stray plugin id stderr" stray.stderr
    "--plugin-id requires --new-plugin DIR";
  let stray_tool = run ~env [ bin; "--plugin-tool-name"; "named_echo" ] in
  assert_failure "plugin tool name without new plugin" stray_tool;
  assert_contains "stray plugin tool name stderr" stray_tool.stderr
    "--plugin-tool-name requires --new-plugin DIR";
  let stray_kind = run ~env [ bin; "--plugin-kind"; "exec" ] in
  assert_failure "plugin kind without new plugin" stray_kind;
  assert_contains "stray plugin kind stderr" stray_kind.stderr
    "--plugin-kind requires --new-plugin DIR";
  let python_dir = Stdlib.Filename.concat root "python-plugin" in
  let python_created =
    run ~env
      [
        bin;
        "--new-plugin";
        python_dir;
        "--plugin-tool-name";
        "python_echo";
        "--plugin-template";
        "python";
      ]
  in
  assert_success "new plugin python template" python_created;
  Alcotest.(check bool)
    "python scaffold created main.py" true
    (Stdlib.Sys.file_exists (Stdlib.Filename.concat python_dir "main.py"));
  Alcotest.(check bool)
    "python scaffold created sdk" true
    (Stdlib.Sys.file_exists
       (Stdlib.Filename.concat python_dir "fp_agent_sdk.py"));
  let python_manifest =
    Stdlib.In_channel.with_open_bin
      (Stdlib.Filename.concat python_dir "fp-agent-plugin.json")
      Stdlib.In_channel.input_all
  in
  assert_contains "python manifest command" python_manifest "python3 main.py";
  let checked_python = run ~env [ bin; "--check-plugin"; python_dir ] in
  assert_success "check python scaffold" checked_python;
  let stray_template = run ~env [ bin; "--plugin-template"; "python" ] in
  assert_failure "plugin template without new plugin" stray_template;
  assert_contains "stray plugin template stderr" stray_template.stderr
    "--plugin-template requires --new-plugin DIR";
  let invalid_template =
    run ~env
      [
        bin;
        "--new-plugin";
        Stdlib.Filename.concat root "bad-template";
        "--plugin-template";
        "ruby";
      ]
  in
  assert_failure "new plugin invalid template" invalid_template;
  assert_contains "invalid template stderr" invalid_template.stderr
    "unknown plugin template";
  let invalid_kind =
    run ~env
      [
        bin;
        "--new-plugin";
        Stdlib.Filename.concat root "bad-kind";
        "--plugin-kind";
        "network";
      ]
  in
  assert_failure "new plugin invalid custom kind" invalid_kind;
  assert_contains "invalid custom kind stderr" invalid_kind.stderr
    "unknown tool kind: network"

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
      (Stdlib.Filename.concat "examples" "hello_world.args.json")
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
  let enum_plugin_dir = Stdlib.Filename.concat root "enum-plugin" in
  mkdir_p enum_plugin_dir;
  write_file
    (Stdlib.Filename.concat enum_plugin_dir "fp-agent-plugin.json")
    {|{
  "id": "com.example.enum",
  "tools": [
    {
      "name": "enum_echo",
      "kind": "read",
      "description": "Requires an enum mode",
      "command": "sh echo.sh",
      "input_schema": {
        "type": "object",
        "properties": {
          "mode": { "type": "string", "enum": ["fast", "safe"] }
        },
        "required": ["mode"]
      }
    }
  ]
}|};
  write_file (Stdlib.Filename.concat enum_plugin_dir "echo.sh") "cat\n";
  let enum_bad =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        enum_plugin_dir;
        "--plugin-tool";
        "enum_echo";
        "--plugin-args";
        {|{"mode":"slow"}|};
      ]
  in
  assert_failure "bad plugin enum args" enum_bad;
  assert_contains "bad enum stderr" enum_bad.stderr
    "field 'mode' expected one of: \"fast\", \"safe\"";
  let enum_ok =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        enum_plugin_dir;
        "--plugin-tool";
        "enum_echo";
        "--plugin-args";
        {|{"mode":"safe"}|};
      ]
  in
  assert_success "good plugin enum args" enum_ok;
  assert_contains "good enum output" enum_ok.stdout {|"mode":"safe"|};
  let strict_plugin_dir = Stdlib.Filename.concat root "strict-plugin" in
  mkdir_p strict_plugin_dir;
  write_file
    (Stdlib.Filename.concat strict_plugin_dir "fp-agent-plugin.json")
    {|{
  "id": "com.example.strict",
  "tools": [
    {
      "name": "strict_echo",
      "kind": "read",
      "description": "Rejects undeclared args",
      "command": "sh echo.sh",
      "input_schema": {
        "type": "object",
        "properties": {
          "message": { "type": "string" }
        },
        "required": ["message"],
        "additionalProperties": false
      }
    }
  ]
}|};
  write_file (Stdlib.Filename.concat strict_plugin_dir "echo.sh") "cat\n";
  let strict_bad =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        strict_plugin_dir;
        "--plugin-tool";
        "strict_echo";
        "--plugin-args";
        {|{"message":"hi","extra":"ignored"}|};
      ]
  in
  assert_failure "bad plugin additional args" strict_bad;
  assert_contains "bad additional stderr" strict_bad.stderr
    "unexpected field 'extra'";
  let strict_ok =
    run ~env
      [
        bin;
        "--run-plugin-tool";
        strict_plugin_dir;
        "--plugin-tool";
        "strict_echo";
        "--plugin-args";
        {|{"message":"hi"}|};
      ]
  in
  assert_success "good plugin additional args" strict_ok;
  assert_contains "good additional output" strict_ok.stdout {|"message":"hi"|};
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
        (String.concat ~sep:"\n"
           [
             "/plugins";
             "/plugin-doctor";
             "/plugin local.my-plugin";
             "/plugin hello_world";
             "/plugin-smoke --replace " ^ plugin_dir;
             "/plugin missing";
             "/tool read_file";
             "/tool hello_world";
             "/tool missing";
             "/tools";
             "/exit";
             "";
           ])
      [ bin ]
  in
  assert_success "repl plugin commands" repl;
  assert_contains "plugins lists scaffold" repl.stdout "local.my-plugin";
  assert_contains "plugin doctor output" repl.stdout "Plugin diagnostics";
  assert_contains "plugin doctor conflict count" repl.stdout "tool_conflicts: 1";
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
  assert_contains "plugin smoke output" repl.stdout "smoke ok: hello_world";
  assert_contains "plugin smoke tool output" repl.stdout
    "hello from fp-agent plugin:";
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
      },
      {
        "id": "qwen36-rtx-fast",
        "name": "qwen36-rtx-fast",
        "reasoning": false,
        "input": ["text"],
        "contextWindow": 65536,
        "maxTokens": 4096,
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
    run ~env
      ~stdin:
        "/providers\n\
         /models\n\
         /model qwen36-rtx\n\
         /model-next\n\
         /model\n\
         /model-cycle\n\
         /model\n\
         /exit\n"
      [ fp_agent_bin () ]
  in
  assert_success "repl model commands" repl;
  assert_contains "providers header" repl.stdout "Providers:";
  assert_contains "providers include protocol" repl.stdout "protocol: openai";
  assert_contains "providers include custom auth" repl.stdout
    "auth: custom config (api key hidden)";
  assert_contains "providers include custom base" repl.stdout
    "api_base: http://101.132.142.56:18080/v1";
  assert_contains "models include deepseek" repl.stdout "deepseek";
  assert_contains "models include custom provider" repl.stdout "local-llm";
  assert_contains "models include custom model" repl.stdout "qwen36-rtx";
  assert_contains "models include second custom model" repl.stdout
    "qwen36-rtx-fast";
  assert_contains "model command switched provider" repl.stdout
    "provider: local-llm";
  assert_contains "model command selected model" repl.stdout "model: qwen36-rtx";
  assert_contains "model-next switched" repl.stdout "model: qwen36-rtx-fast"

let test_cli_adds_custom_provider_profile () =
  let root = tmp_dir "fp-agent-cli-provider-add-" in
  let config_path = Stdlib.Filename.concat root "providers.json" in
  let env = isolated_env root @ [ ("FP_AGENT_CONFIG", config_path) ] in
  let bin = fp_agent_bin () in
  let added =
    run ~env
      [
        bin;
        "--add-provider";
        "local-added";
        "--provider-base";
        "http://127.0.0.1:18080/v1";
        "--provider-model";
        "qwen-added,qwen-added-fast";
        "--provider-api-key";
        "dummy";
        "--provider-local-compat";
        "--provider-max-tokens";
        "4096";
      ]
  in
  assert_success "add provider profile" added;
  assert_contains "add provider stdout" added.stdout
    "provider saved: local-added";
  assert_contains "add provider next" added.stdout
    "next: /provider local-added qwen-added";
  Alcotest.(check bool)
    "provider config created" true
    (Stdlib.Sys.file_exists config_path);
  let duplicate =
    run ~env
      [
        bin;
        "--add-provider";
        "local-added";
        "--provider-base";
        "http://127.0.0.1:18080/v1";
        "--provider-model";
        "qwen-added";
      ]
  in
  assert_failure "add provider duplicate" duplicate;
  assert_contains "duplicate stderr" duplicate.stderr "pass --replace";
  let repl =
    run ~env ~stdin:"/models\n/model qwen-added\n/model-next\n/model\n/exit\n"
      [ bin ]
  in
  assert_success "repl uses added provider" repl;
  assert_contains "models includes added provider" repl.stdout "local-added";
  assert_contains "models includes added model" repl.stdout "qwen-added";
  assert_contains "model switched to added provider" repl.stdout
    "provider: local-added";
  assert_contains "model-next uses added model list" repl.stdout
    "model: qwen-added-fast"

let test_repl_shows_project_instructions () =
  let root = tmp_dir "fp-agent-cli-instructions-" in
  let env = isolated_env root in
  let workspace =
    match List.Assoc.find env "WORKSPACE_ROOT" ~equal:String.equal with
    | Some path -> path
    | None -> Alcotest.fail "missing workspace env"
  in
  write_file
    (Stdlib.Filename.concat workspace "RTK.md")
    "Prefer repo-specific test evidence.\n";
  write_file
    (Stdlib.Filename.concat workspace "AGENTS.md")
    "Follow workspace conventions.\n@RTK.md\n";
  let repl =
    run ~env ~stdin:"/instructions\n/status\n/exit\n" [ fp_agent_bin () ]
  in
  assert_success "repl instructions command" repl;
  assert_contains "instructions header" repl.stdout
    "Project instructions loaded";
  assert_contains "instructions include agents" repl.stdout "--- AGENTS.md ---";
  assert_contains "instructions include include" repl.stdout "--- RTK.md ---";
  assert_contains "instructions include content" repl.stdout
    "Prefer repo-specific test evidence.";
  assert_contains "status shows instructions" repl.stdout
    "project_instructions: loaded"

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
         /plan\n\
         /plan-set todo inspect code; doing implement plan; done write tests\n\
         /plan\n\
         /log\n\
         /usage\n\
         /status\n\
         /inspect 0\n\
         /inspect\n\
         /inspect 3\n\
         /inspect nope\n\
         /plan-add todo verify status\n\
         /plan-update 2 done implement plan\n\
         /plan\n\
         /status\n\
         /handoff\n\
         /plan-clear\n\
         /plan\n\
         /log\n\
         /retry\n\
         /new\n\
         /log\n\
         /exit\n"
      [ fp_agent_bin (); "--resume"; session_dir ]
  in
  assert_success "repl inspect command" repl;
  assert_contains "log includes event" repl.stdout "tool_call search";
  assert_contains "plan initially empty" repl.stdout "(no session plan)";
  assert_contains "plan update" repl.stdout "plan updated: 3 item(s)";
  assert_contains "plan item" repl.stdout "2. [doing] implement plan";
  assert_contains "log includes plan event" repl.stdout "plan: 1/3 done";
  assert_contains "plan add update" repl.stdout "plan updated: 4 item(s)";
  assert_contains "plan update marks done" repl.stdout
    "2. [done] implement plan";
  assert_contains "plan add item" repl.stdout "4. [todo] verify status";
  assert_contains "status updated plan" repl.stdout "plan: 2/4 done";
  assert_contains "handoff header" repl.stdout "Session handoff";
  assert_contains "handoff resume" repl.stdout
    "resume: dune exec -- fp-agent --resume";
  assert_contains "handoff last task" repl.stdout "last_user_task: (none)";
  assert_contains "handoff recent events" repl.stdout "Recent events:";
  assert_contains "plan clear" repl.stdout "plan updated: 0 item(s)";
  assert_contains "plan empty after clear" repl.stdout "(plan is empty)";
  assert_contains "inspect prints index" repl.stdout "event 0";
  assert_contains "inspect kind" repl.stdout "kind: tool_call";
  assert_contains "inspect tool" repl.stdout "tool: search";
  assert_contains "inspect args" repl.stdout "\"query\": \"Plugin\"";
  assert_contains "inspect json" repl.stdout "JSON";
  assert_contains "usage input" repl.stdout "input_tokens: 31";
  assert_contains "usage total" repl.stdout "total_tokens: 40";
  assert_contains "status session" repl.stdout "session: inspect-session";
  assert_contains "status events" repl.stdout "events: 3";
  assert_contains "status tokens" repl.stdout
    "tokens: input 31 output 9 total 40";
  assert_contains "status tools" repl.stdout "tools:";
  assert_contains "inspect range" repl.stdout "no event at index 3 (0..2)";
  assert_contains "inspect usage" repl.stdout "usage: /inspect [event-index]";
  assert_contains "retry without user task" repl.stdout
    "no previous user task to retry";
  assert_contains "new session switched" repl.stdout "new session:";
  assert_contains "new session has empty log" repl.stdout "(no events yet)"

let test_repl_compacts_session_events () =
  let root = tmp_dir "fp-agent-cli-compact-" in
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
  let session_dir = Stdlib.Filename.concat sessions_root "compact-session" in
  mkdir_p session_dir;
  let log = Event_log.create ~session_dir in
  List.iter (List.range 0 8) ~f:(fun i ->
      Event_log.append log
        (Event.User_message { content = Printf.sprintf "task-%d" i });
      Event_log.append log
        (Event.Assistant_message
           {
             content = [ Llm.Text (Printf.sprintf "answer-%d" i) ];
             usage = Llm.zero_usage;
           }));
  Event_log.close log;
  let repl =
    run ~env ~stdin:"/compact\n/log\n/exit\n"
      [ fp_agent_bin (); "--resume"; session_dir ]
  in
  assert_success "repl compact command" repl;
  assert_contains "compact reports summary" repl.stdout
    "compacted older history into";
  assert_contains "log includes compaction" repl.stdout "context compacted"

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
          Alcotest.test_case "repl plugin install" `Quick
            test_repl_installs_and_removes_plugin;
          Alcotest.test_case "plugin custom id" `Quick
            test_new_plugin_custom_id_cli;
          Alcotest.test_case "plugin tool debug" `Quick
            test_plugin_tool_debug_cli;
          Alcotest.test_case "repl plugin tools" `Quick
            test_repl_lists_dynamic_plugin_tools;
          Alcotest.test_case "custom provider models" `Quick
            test_repl_lists_and_switches_custom_provider_models;
          Alcotest.test_case "add provider profile" `Quick
            test_cli_adds_custom_provider_profile;
          Alcotest.test_case "repl instructions" `Quick
            test_repl_shows_project_instructions;
          Alcotest.test_case "repl inspect events" `Quick
            test_repl_inspects_session_events;
          Alcotest.test_case "repl compact events" `Quick
            test_repl_compacts_session_events;
          Alcotest.test_case "tui confirm config" `Quick
            test_tui_confirm_reaches_config_for_oneshot;
          Alcotest.test_case "tui repl confirm config" `Quick
            test_tui_confirm_reaches_config_for_repl;
        ] );
    ]
