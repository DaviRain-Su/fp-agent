open! Base
open Fp_agent

let rm_rf path =
  ignore
    (Shell.run
       ~command:(Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote path))
       ~timeout_sec:10
      : (Shell.result, string) Result.t)

let mkdir_p path =
  ignore
    (Shell.run
       ~command:(Printf.sprintf "mkdir -p %s" (Stdlib.Filename.quote path))
       ~timeout_sec:10
      : (Shell.result, string) Result.t)

let write path content =
  mkdir_p (Stdlib.Filename.dirname path);
  Stdlib.Out_channel.with_open_bin path (fun oc ->
      Stdlib.Out_channel.output_string oc content)

let with_temp_dir prefix f =
  let root = Stdlib.Filename.temp_dir prefix "" in
  Exn.protect ~f:(fun () -> f root) ~finally:(fun () -> rm_rf root)

let write_plugin ?(version = "0.1.0") ?permissions ?script dir ~id ~tool_name
    ~kind =
  let permissions =
    Option.value_map permissions ~default:"" ~f:(fun json ->
        ",\n      \"permissions\": " ^ json)
  in
  write
    (Stdlib.Filename.concat dir Plugin.manifest_file)
    (Printf.sprintf
       {|
{
  "id": "%s",
  "name": "Test Plugin",
  "version": "%s",
  "sdk_version": 1,
  "tools": [
    {
      "name": "%s",
      "kind": "%s",
      "description": "Echoes its JSON input",
      "command": "sh echo.sh"%s,
      "input_schema": {
        "type": "object",
        "properties": { "message": { "type": "string" } },
        "required": ["message"]
      }
    }
  ]
}
|}
       id version tool_name kind permissions);
  write
    (Stdlib.Filename.concat dir "echo.sh")
    (Option.value script
       ~default:
         "printf 'tool=%s workspace=%s input=' \"$FP_AGENT_TOOL_NAME\" \
          \"$FP_AGENT_WORKSPACE\"\n\
          cat\n")

let workspace root =
  match Workspace.create ~root with
  | Ok ws -> ws
  | Error e -> Alcotest.failf "workspace: %s" e

let test_config root =
  {
    Config.provider = "test";
    api_key = "k";
    api_base = "http://localhost";
    model = "m";
    models = [];
    protocol = Provider.Openai;
    compat = Config.default_compat;
    max_tokens = None;
    max_steps = 10;
    workspace_root = root;
  }

let test_manifest_plugin_executes () =
  with_temp_dir "fp_agent_plugin_exec" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      mkdir_p plugin_dir;
      write_plugin plugin_dir ~id:"com.example.exec" ~tool_name:"plugin_echo"
        ~kind:"read";
      Unix.putenv "FP_AGENT_PLUGIN_PATH" plugin_dir;
      Unix.putenv "FP_AGENT_PLUGIN_HOME" (Stdlib.Filename.concat root "home");
      Tool_loader.register_all ();
      match Tool.find "plugin_echo" with
      | None -> Alcotest.fail "plugin tool was not registered"
      | Some tool -> (
          Alcotest.(check string)
            "kind" "read"
            (match tool.kind with
            | Read -> "read"
            | Write -> "write"
            | Exec -> "exec");
          Alcotest.(check bool)
            "schema registered" true
            (Option.is_some tool.input_schema);
          let result =
            Tool_runner.run ~workspace:(workspace root)
              ~tool_call:
                (Tool_call.make ~name:"plugin_echo"
                   ~args:(`Assoc [ ("message", `String "hello") ]))
              ()
          in
          match result with
          | Tool_result.Success { output } ->
              Alcotest.(check bool)
                "output includes tool name" true
                (String.is_substring output ~substring:"tool=plugin_echo");
              Alcotest.(check bool)
                "output includes JSON input" true
                (String.is_substring output ~substring:{|"message":"hello"|})
          | Tool_result.Error { message } ->
              Alcotest.failf "plugin failed: %s" message))

let test_plugin_write_policy_uses_path_bounds () =
  with_temp_dir "fp_agent_plugin_policy" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      mkdir_p plugin_dir;
      mkdir_p (Stdlib.Filename.concat root ".git");
      write_plugin plugin_dir ~id:"com.example.policy"
        ~tool_name:"plugin_write_guard" ~kind:"write";
      Unix.putenv "FP_AGENT_PLUGIN_PATH" plugin_dir;
      Unix.putenv "FP_AGENT_PLUGIN_HOME" (Stdlib.Filename.concat root "home");
      Tool_loader.register_all ();
      let denied =
        Policy.check ~workspace:(workspace root)
          ~tool_call:
            (Tool_call.make ~name:"plugin_write_guard"
               ~args:
                 (`Assoc
                    [
                      ("path", `String ".git/config"); ("message", `String "x");
                    ]))
          ()
      in
      Alcotest.(check bool)
        "plugin write to .git denied" true
        (match denied with Permission.Deny _ -> true | _ -> false))

let test_install_plugin_copies_to_home () =
  with_temp_dir "fp_agent_plugin_install" (fun root ->
      let src = Stdlib.Filename.concat root "src" in
      let home = Stdlib.Filename.concat root "home" in
      mkdir_p src;
      write_plugin src ~id:"com.example.install"
        ~tool_name:"plugin_installed_echo" ~kind:"read";
      Unix.putenv "FP_AGENT_PLUGIN_PATH" "";
      Unix.putenv "FP_AGENT_PLUGIN_HOME" home;
      match Plugin.install src with
      | Error e -> Alcotest.failf "install failed: %s" e
      | Ok dst -> (
          Alcotest.(check string)
            "installed path"
            (Stdlib.Filename.concat home "com.example.install")
            dst;
          Alcotest.(check bool)
            "manifest copied" true
            (Stdlib.Sys.file_exists
               (Stdlib.Filename.concat dst Plugin.manifest_file));
          let manifests = Plugin.manifests () in
          Alcotest.(check bool)
            "installed manifest discovered" true
            (List.exists manifests ~f:(fun (m : Plugin.manifest) ->
                 String.equal m.id "com.example.install"));
          let installed = Plugin.installed_manifests () in
          Alcotest.(check bool)
            "installed manifest listed" true
            (List.exists installed ~f:(fun (m : Plugin.manifest) ->
                 String.equal m.id "com.example.install"));
          match Plugin.remove "com.example.install" with
          | Error e -> Alcotest.failf "remove failed: %s" e
          | Ok removed ->
              Alcotest.(check string) "removed path" dst removed;
              Alcotest.(check bool)
                "installed dir removed" false
                (Stdlib.Sys.file_exists dst);
              Alcotest.(check bool)
                "missing remove rejected" true
                (Result.is_error (Plugin.remove "com.example.install"))))

let test_install_plugin_can_replace_existing () =
  with_temp_dir "fp_agent_plugin_replace" (fun root ->
      let src_v1 = Stdlib.Filename.concat root "src-v1" in
      let bad_src = Stdlib.Filename.concat root "bad-src" in
      let src_v2 = Stdlib.Filename.concat root "src-v2" in
      let home = Stdlib.Filename.concat root "home" in
      mkdir_p src_v1;
      mkdir_p bad_src;
      mkdir_p src_v2;
      Unix.putenv "FP_AGENT_PLUGIN_PATH" "";
      Unix.putenv "FP_AGENT_PLUGIN_HOME" home;
      write_plugin src_v1 ~id:"com.example.replace"
        ~tool_name:"plugin_replace_echo" ~kind:"read"
        ~script:"printf 'version=v1 input='; cat\n";
      write (Stdlib.Filename.concat src_v1 "old-only.txt") "stale";
      write
        (Stdlib.Filename.concat bad_src Plugin.manifest_file)
        {|{
  "id": "com.example.replace",
  "tools": []
}
|};
      write_plugin src_v2 ~id:"com.example.replace" ~version:"0.2.0"
        ~tool_name:"plugin_replace_echo" ~kind:"read"
        ~script:"printf 'version=v2 input='; cat\n";
      match Plugin.install src_v1 with
      | Error e -> Alcotest.failf "initial install failed: %s" e
      | Ok dst -> (
          Alcotest.(check bool)
            "second install rejected" true
            (Result.is_error (Plugin.install src_v2));
          Alcotest.(check bool)
            "bad replace rejected" true
            (Result.is_error (Plugin.install ~replace:true bad_src));
          Alcotest.(check bool)
            "bad replace preserved old files" true
            (Stdlib.Sys.file_exists (Stdlib.Filename.concat dst "old-only.txt"));
          (match Plugin.check dst with
          | Error e -> Alcotest.failf "check old install failed: %s" e
          | Ok manifest ->
              Alcotest.(check string)
                "old install still present" "0.1.0" manifest.version);
          match Plugin.install ~replace:true src_v2 with
          | Error e -> Alcotest.failf "replace install failed: %s" e
          | Ok replaced -> (
              Alcotest.(check string) "replace path" dst replaced;
              Alcotest.(check bool)
                "stale file removed" false
                (Stdlib.Sys.file_exists
                   (Stdlib.Filename.concat dst "old-only.txt"));
              match Plugin.check dst with
              | Error e -> Alcotest.failf "check replaced failed: %s" e
              | Ok manifest -> (
                  Alcotest.(check string)
                    "replaced version" "0.2.0" manifest.version;
                  match
                    Plugin.run_tool ~dir:dst ~tool_name:"plugin_replace_echo"
                      ~workspace:(workspace root)
                      ~args:(`Assoc [ ("message", `String "hello") ])
                  with
                  | Error e -> Alcotest.failf "run replaced failed: %s" e
                  | Ok (Tool_result.Error { message }) ->
                      Alcotest.failf "replaced tool error: %s" message
                  | Ok (Tool_result.Success { output }) ->
                      Alcotest.(check bool)
                        "replaced script ran" true
                        (String.is_substring output ~substring:"version=v2")))))

let test_discover_reports_invalid_manifests () =
  with_temp_dir "fp_agent_plugin_discover" (fun root ->
      let plugin_root = Stdlib.Filename.concat root "plugins" in
      let valid = Stdlib.Filename.concat plugin_root "valid" in
      let invalid = Stdlib.Filename.concat plugin_root "invalid" in
      mkdir_p valid;
      mkdir_p invalid;
      Unix.putenv "FP_AGENT_PLUGIN_PATH" plugin_root;
      Unix.putenv "FP_AGENT_PLUGIN_HOME" (Stdlib.Filename.concat root "home");
      write_plugin valid ~id:"com.example.discover"
        ~tool_name:"plugin_discover_echo" ~kind:"read";
      write
        (Stdlib.Filename.concat invalid Plugin.manifest_file)
        {|{
  "id": "com.example.invalid",
  "tools": []
}
|};
      let discovery = Plugin.discover () in
      Alcotest.(check int)
        "valid manifest count" 1
        (List.length discovery.manifests);
      Alcotest.(check string)
        "valid manifest id" "com.example.discover"
        (List.hd_exn discovery.manifests).id;
      Alcotest.(check int)
        "invalid manifest count" 1
        (List.length discovery.errors);
      let error = List.hd_exn discovery.errors in
      Alcotest.(check string) "invalid manifest dir" invalid error.dir;
      Alcotest.(check bool)
        "invalid manifest message" true
        (String.is_substring error.message ~substring:"at least one tool");
      Alcotest.(check int)
        "legacy manifest helper ignores invalid" 1
        (List.length (Plugin.manifests ())))

let test_tool_conflicts_report_collisions () =
  with_temp_dir "fp_agent_plugin_conflicts" (fun root ->
      let plugin_root = Stdlib.Filename.concat root "plugins" in
      let builtin_conflict = Stdlib.Filename.concat plugin_root "builtin" in
      let first = Stdlib.Filename.concat plugin_root "first" in
      let second = Stdlib.Filename.concat plugin_root "second" in
      mkdir_p builtin_conflict;
      mkdir_p first;
      mkdir_p second;
      Unix.putenv "FP_AGENT_PLUGIN_PATH" plugin_root;
      Unix.putenv "FP_AGENT_PLUGIN_HOME" (Stdlib.Filename.concat root "home");
      write_plugin builtin_conflict ~id:"com.example.builtin_conflict"
        ~tool_name:"read_file" ~kind:"read";
      write_plugin first ~id:"com.example.first" ~tool_name:"shared_echo"
        ~kind:"read";
      write_plugin second ~id:"com.example.second" ~tool_name:"shared_echo"
        ~kind:"read";
      let conflicts = Plugin.tool_conflicts () in
      Alcotest.(check int) "conflict count" 2 (List.length conflicts);
      let joined =
        conflicts
        |> List.map ~f:(fun (conflict : Plugin.tool_conflict) ->
            String.concat ~sep:"|"
              [
                conflict.plugin_id; conflict.tool_name; conflict.existing_owner;
              ])
        |> String.concat ~sep:"\n"
      in
      Alcotest.(check bool)
        "reports builtin conflict" true
        (String.is_substring joined
           ~substring:"com.example.builtin_conflict|read_file|built-in tool");
      Alcotest.(check bool)
        "reports plugin conflict" true
        (String.is_substring joined
           ~substring:"com.example.second|shared_echo|plugin com.example.first"))

let test_check_rejects_candidate_tool_conflicts () =
  with_temp_dir "fp_agent_plugin_check_conflicts" (fun root ->
      let existing = Stdlib.Filename.concat root "existing" in
      let builtin_conflict = Stdlib.Filename.concat root "builtin-conflict" in
      let plugin_conflict = Stdlib.Filename.concat root "plugin-conflict" in
      let replacement_installed =
        Stdlib.Filename.concat root
          (Stdlib.Filename.concat "home" "com.example.replace_check")
      in
      let replacement_candidate =
        Stdlib.Filename.concat root "replacement-candidate"
      in
      mkdir_p existing;
      mkdir_p builtin_conflict;
      mkdir_p plugin_conflict;
      mkdir_p replacement_installed;
      mkdir_p replacement_candidate;
      Unix.putenv "FP_AGENT_PLUGIN_PATH" existing;
      Unix.putenv "FP_AGENT_PLUGIN_HOME" (Stdlib.Filename.concat root "home");
      write_plugin existing ~id:"com.example.existing"
        ~tool_name:"existing_echo" ~kind:"read";
      write_plugin builtin_conflict ~id:"com.example.builtin_check"
        ~tool_name:"read_file" ~kind:"read";
      write_plugin plugin_conflict ~id:"com.example.plugin_check"
        ~tool_name:"existing_echo" ~kind:"read";
      write_plugin replacement_installed ~id:"com.example.replace_check"
        ~tool_name:"replace_echo" ~kind:"read";
      write_plugin replacement_candidate ~id:"com.example.replace_check"
        ~tool_name:"replace_echo" ~kind:"read";
      let assert_conflict label result substring =
        match result with
        | Ok _ -> Alcotest.failf "%s unexpectedly succeeded" label
        | Error e ->
            Alcotest.(check bool)
              (label ^ " conflict") true
              (String.is_substring e ~substring:"plugin tool name conflict");
            Alcotest.(check bool)
              (label ^ " detail") true
              (String.is_substring e ~substring)
      in
      assert_conflict "builtin check"
        (Plugin.check builtin_conflict)
        "built-in tool";
      assert_conflict "plugin check"
        (Plugin.check plugin_conflict)
        "plugin com.example.existing";
      assert_conflict "builtin install"
        (Plugin.install builtin_conflict)
        "built-in tool";
      (match Plugin.check replacement_candidate with
      | Ok _ ->
          Alcotest.fail "replacement check should conflict without replace"
      | Error e ->
          Alcotest.(check bool)
            "replacement conflict" true
            (String.is_substring e ~substring:"plugin com.example.replace_check"));
      match Plugin.check ~replace:true replacement_candidate with
      | Error e -> Alcotest.failf "replace check failed: %s" e
      | Ok manifest ->
          Alcotest.(check string)
            "replace check id" "com.example.replace_check" manifest.id)

let test_run_tool_for_plugin_development () =
  with_temp_dir "fp_agent_plugin_run_tool" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      mkdir_p plugin_dir;
      write_plugin plugin_dir ~id:"com.example.run_tool"
        ~tool_name:"plugin_dev_echo" ~kind:"read";
      match
        Plugin.run_tool ~dir:plugin_dir ~tool_name:"plugin_dev_echo"
          ~workspace:(workspace root)
          ~args:(`Assoc [ ("message", `String "hello") ])
      with
      | Error e -> Alcotest.failf "run_tool failed: %s" e
      | Ok (Tool_result.Error { message }) ->
          Alcotest.failf "plugin returned error: %s" message
      | Ok (Tool_result.Success { output }) ->
          Alcotest.(check bool)
            "output includes tool" true
            (String.is_substring output ~substring:"tool=plugin_dev_echo");
          Alcotest.(check bool)
            "output includes args" true
            (String.is_substring output ~substring:{|"message":"hello"|}))

let test_smoke_runs_example_args () =
  with_temp_dir "fp_agent_plugin_smoke" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      let cases_dir =
        Stdlib.Filename.concat plugin_dir
          (Stdlib.Filename.concat "examples" "plugin_smoke_echo")
      in
      mkdir_p plugin_dir;
      mkdir_p cases_dir;
      write_plugin plugin_dir ~id:"com.example.smoke"
        ~tool_name:"plugin_smoke_echo" ~kind:"read";
      write
        (Stdlib.Filename.concat plugin_dir
           (Stdlib.Filename.concat "examples" "plugin_smoke_echo.args.json"))
        {|{"message":"from-smoke"}|};
      write
        (Stdlib.Filename.concat cases_dir "01-safe.json")
        {|{"message":"from-case-1"}|};
      write
        (Stdlib.Filename.concat cases_dir "02-edge.json")
        {|{"message":"from-case-2"}|};
      match Plugin.smoke ~workspace:(workspace root) plugin_dir with
      | Error e -> Alcotest.failf "smoke failed: %s" e
      | Ok [ default; case_1; case_2 ] ->
          Alcotest.(check string)
            "default smoke tool" "plugin_smoke_echo" default.tool_name;
          Alcotest.(check bool)
            "default smoke args file" true
            (String.is_suffix default.args_file
               ~suffix:"examples/plugin_smoke_echo.args.json");
          Alcotest.(check bool)
            "default smoke output" true
            (String.is_substring default.output ~substring:"from-smoke");
          Alcotest.(check bool)
            "case 1 args file" true
            (String.is_suffix case_1.args_file
               ~suffix:"examples/plugin_smoke_echo/01-safe.json");
          Alcotest.(check bool)
            "case 1 output" true
            (String.is_substring case_1.output ~substring:"from-case-1");
          Alcotest.(check bool)
            "case 2 args file" true
            (String.is_suffix case_2.args_file
               ~suffix:"examples/plugin_smoke_echo/02-edge.json");
          Alcotest.(check bool)
            "case 2 output" true
            (String.is_substring case_2.output ~substring:"from-case-2")
      | Ok results ->
          Alcotest.failf "expected three smoke results, got %d"
            (List.length results))

let test_tool_loader_refreshes_removed_plugins () =
  with_temp_dir "fp_agent_plugin_reload" (fun root ->
      let src = Stdlib.Filename.concat root "src" in
      let home = Stdlib.Filename.concat root "home" in
      mkdir_p src;
      Unix.putenv "FP_AGENT_PLUGIN_PATH" "";
      Unix.putenv "FP_AGENT_PLUGIN_HOME" home;
      write_plugin src ~id:"com.example.reload" ~tool_name:"plugin_reload_echo"
        ~kind:"read";
      (match Plugin.install src with
      | Error e -> Alcotest.failf "install failed: %s" e
      | Ok _ -> ());
      let counts = Tool_loader.refresh_counts () in
      Alcotest.(check int) "installed plugin count" 1 counts.plugins;
      Alcotest.(check bool)
        "installed plugin registered" true
        (Option.is_some (Tool.find "plugin_reload_echo"));
      (match Plugin.remove "com.example.reload" with
      | Error e -> Alcotest.failf "remove failed: %s" e
      | Ok _ -> ());
      let counts = Tool_loader.refresh_counts () in
      Alcotest.(check int) "removed plugin count" 0 counts.plugins;
      Alcotest.(check bool)
        "removed plugin unregistered" true
        (Option.is_none (Tool.find "plugin_reload_echo")))

let test_plugin_runtime_environment () =
  with_temp_dir "fp_agent_plugin_env" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      mkdir_p plugin_dir;
      write_plugin plugin_dir ~id:"com.example.env" ~tool_name:"plugin_env"
        ~kind:"exec"
        ~permissions:{|{"workspace":"read","network":false,"env":["FOO"]}|}
        ~script:
          "printf 'id=%s name=%s version=%s sdk=%s tool=%s kind=%s perms=%s \
           args_file_exists=' \"$FP_AGENT_PLUGIN_ID\" \
           \"$FP_AGENT_PLUGIN_NAME\" \"$FP_AGENT_PLUGIN_VERSION\" \
           \"$FP_AGENT_PLUGIN_SDK_VERSION\" \"$FP_AGENT_TOOL_NAME\" \
           \"$FP_AGENT_TOOL_KIND\" \"$FP_AGENT_TOOL_PERMISSIONS\"\n\
           if [ -f \"$FP_AGENT_ARGS_FILE\" ]; then printf yes; else printf no; \
           fi\n\
           printf ' stdin='\n\
           cat\n\
           printf ' args_file='\n\
           cat \"$FP_AGENT_ARGS_FILE\"\n";
      match
        Plugin.run_tool ~dir:plugin_dir ~tool_name:"plugin_env"
          ~workspace:(workspace root)
          ~args:(`Assoc [ ("message", `String "hello") ])
      with
      | Error e -> Alcotest.failf "run_tool failed: %s" e
      | Ok (Tool_result.Error { message }) ->
          Alcotest.failf "plugin returned error: %s" message
      | Ok (Tool_result.Success { output }) ->
          Alcotest.(check bool)
            "output includes plugin id" true
            (String.is_substring output ~substring:"id=com.example.env");
          Alcotest.(check bool)
            "output includes plugin name" true
            (String.is_substring output ~substring:"name=Test Plugin");
          Alcotest.(check bool)
            "output includes sdk version" true
            (String.is_substring output ~substring:"sdk=1");
          Alcotest.(check bool)
            "output includes tool kind" true
            (String.is_substring output ~substring:"kind=exec");
          Alcotest.(check bool)
            "output includes tool permissions" true
            (String.is_substring output
               ~substring:
                 {|perms={"workspace":"read","network":false,"env":["FOO"]}|});
          Alcotest.(check bool)
            "args file exists" true
            (String.is_substring output ~substring:"args_file_exists=yes");
          Alcotest.(check bool)
            "stdin includes args" true
            (String.is_substring output ~substring:{|stdin={"message":"hello"}|});
          Alcotest.(check bool)
            "args file includes args" true
            (String.is_substring output
               ~substring:{|args_file={"message":"hello"}|}))

let test_run_tool_validates_input_schema () =
  with_temp_dir "fp_agent_plugin_schema_validation" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      let marker = Stdlib.Filename.concat plugin_dir "ran" in
      mkdir_p plugin_dir;
      write_plugin plugin_dir ~id:"com.example.schema_validation"
        ~tool_name:"plugin_schema_guard" ~kind:"read";
      write
        (Stdlib.Filename.concat plugin_dir "echo.sh")
        "touch ran\nprintf 'should not run'\n";
      let run args =
        Plugin.run_tool ~dir:plugin_dir ~tool_name:"plugin_schema_guard"
          ~workspace:(workspace root) ~args
      in
      let assert_schema_error label args substring =
        match run args with
        | Error e -> Alcotest.failf "%s load error: %s" label e
        | Ok (Tool_result.Success { output }) ->
            Alcotest.failf "%s unexpectedly succeeded: %s" label output
        | Ok (Tool_result.Error { message }) ->
            Alcotest.(check bool)
              (label ^ " prefix") true
              (String.is_substring message ~substring:"schema validation failed");
            Alcotest.(check bool)
              (label ^ " detail") true
              (String.is_substring message ~substring)
      in
      assert_schema_error "missing required" (`Assoc [])
        "missing required field 'message'";
      assert_schema_error "wrong type"
        (`Assoc [ ("message", `Int 1) ])
        "field 'message' expected string";
      assert_schema_error "root type" (`String "hello") "args expected object";
      Alcotest.(check bool)
        "plugin command was not executed" false
        (Stdlib.Sys.file_exists marker))

let test_run_tool_validates_input_schema_enum () =
  with_temp_dir "fp_agent_plugin_schema_enum" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      let marker = Stdlib.Filename.concat plugin_dir "ran" in
      mkdir_p plugin_dir;
      write
        (Stdlib.Filename.concat plugin_dir Plugin.manifest_file)
        {|{
  "id": "com.example.schema_enum",
  "tools": [
    {
      "name": "plugin_schema_enum",
      "kind": "read",
      "description": "Requires an enum value",
      "command": "sh echo.sh",
      "input_schema": {
        "type": "object",
        "properties": {
          "mode": {
            "type": "string",
            "enum": ["fast", "safe"]
          }
        },
        "required": ["mode"]
      }
    }
  ]
}
|};
      write
        (Stdlib.Filename.concat plugin_dir "echo.sh")
        "touch ran\nprintf 'mode ok: '\ncat\n";
      let run args =
        Plugin.run_tool ~dir:plugin_dir ~tool_name:"plugin_schema_enum"
          ~workspace:(workspace root) ~args
      in
      (match run (`Assoc [ ("mode", `String "slow") ]) with
      | Error e -> Alcotest.failf "enum load error: %s" e
      | Ok (Tool_result.Success { output }) ->
          Alcotest.failf "enum unexpectedly succeeded: %s" output
      | Ok (Tool_result.Error { message }) ->
          Alcotest.(check bool)
            "enum prefix" true
            (String.is_substring message ~substring:"schema validation failed");
          Alcotest.(check bool)
            "enum detail" true
            (String.is_substring message
               ~substring:"field 'mode' expected one of: \"fast\", \"safe\""));
      Alcotest.(check bool)
        "invalid enum did not execute command" false
        (Stdlib.Sys.file_exists marker);
      match run (`Assoc [ ("mode", `String "safe") ]) with
      | Error e -> Alcotest.failf "enum valid load error: %s" e
      | Ok (Tool_result.Error { message }) ->
          Alcotest.failf "enum valid tool error: %s" message
      | Ok (Tool_result.Success { output }) ->
          Alcotest.(check bool)
            "enum valid output" true
            (String.is_substring output ~substring:{|"mode":"safe"|}))

let test_run_tool_rejects_additional_properties () =
  with_temp_dir "fp_agent_plugin_schema_additional" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      let marker = Stdlib.Filename.concat plugin_dir "ran" in
      mkdir_p plugin_dir;
      write
        (Stdlib.Filename.concat plugin_dir Plugin.manifest_file)
        {|{
  "id": "com.example.schema_additional",
  "tools": [
    {
      "name": "plugin_schema_additional",
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
}
|};
      write
        (Stdlib.Filename.concat plugin_dir "echo.sh")
        "touch ran\nprintf 'args ok: '\ncat\n";
      let run args =
        Plugin.run_tool ~dir:plugin_dir ~tool_name:"plugin_schema_additional"
          ~workspace:(workspace root) ~args
      in
      (match
         run
           (`Assoc
              [ ("message", `String "hello"); ("extra", `String "ignored") ])
       with
      | Error e -> Alcotest.failf "additional load error: %s" e
      | Ok (Tool_result.Success { output }) ->
          Alcotest.failf "additional unexpectedly succeeded: %s" output
      | Ok (Tool_result.Error { message }) ->
          Alcotest.(check bool)
            "additional prefix" true
            (String.is_substring message ~substring:"schema validation failed");
          Alcotest.(check bool)
            "additional detail" true
            (String.is_substring message ~substring:"unexpected field 'extra'"));
      Alcotest.(check bool)
        "unexpected arg did not execute command" false
        (Stdlib.Sys.file_exists marker);
      match run (`Assoc [ ("message", `String "hello") ]) with
      | Error e -> Alcotest.failf "additional valid load error: %s" e
      | Ok (Tool_result.Error { message }) ->
          Alcotest.failf "additional valid tool error: %s" message
      | Ok (Tool_result.Success { output }) ->
          Alcotest.(check bool)
            "additional valid output" true
            (String.is_substring output ~substring:{|"message":"hello"|}))

let test_run_tool_validates_additional_properties_schema () =
  with_temp_dir "fp_agent_plugin_schema_additional_schema" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      mkdir_p plugin_dir;
      write
        (Stdlib.Filename.concat plugin_dir Plugin.manifest_file)
        {|{
  "id": "com.example.schema_additional_schema",
  "tools": [
    {
      "name": "plugin_schema_additional_schema",
      "kind": "read",
      "description": "Validates undeclared args with one schema",
      "command": "sh echo.sh",
      "input_schema": {
        "type": "object",
        "properties": {
          "message": { "type": "string" }
        },
        "required": ["message"],
        "additionalProperties": { "type": "integer" }
      }
    }
  ]
}
|};
      write (Stdlib.Filename.concat plugin_dir "echo.sh") "cat\n";
      let run args =
        Plugin.run_tool ~dir:plugin_dir
          ~tool_name:"plugin_schema_additional_schema"
          ~workspace:(workspace root) ~args
      in
      (match
         run
           (`Assoc [ ("message", `String "hello"); ("count", `String "wrong") ])
       with
      | Error e -> Alcotest.failf "additional schema load error: %s" e
      | Ok (Tool_result.Success { output }) ->
          Alcotest.failf "additional schema unexpectedly succeeded: %s" output
      | Ok (Tool_result.Error { message }) ->
          Alcotest.(check bool)
            "additional schema detail" true
            (String.is_substring message
               ~substring:"field 'count' expected integer"));
      match
        run (`Assoc [ ("message", `String "hello"); ("count", `Int 2) ])
      with
      | Error e -> Alcotest.failf "additional schema valid load error: %s" e
      | Ok (Tool_result.Error { message }) ->
          Alcotest.failf "additional schema valid tool error: %s" message
      | Ok (Tool_result.Success { output }) ->
          Alcotest.(check bool)
            "additional schema valid output" true
            (String.is_substring output ~substring:{|"count":2|}))

let test_run_tool_ignores_unsupported_schema_shape () =
  with_temp_dir "fp_agent_plugin_schema_shape" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      mkdir_p plugin_dir;
      write
        (Stdlib.Filename.concat plugin_dir Plugin.manifest_file)
        {|{
  "id": "com.example.schema_shape",
  "tools": [
    {
      "name": "plugin_schema_shape",
      "kind": "read",
      "description": "Uses an unsupported schema shape",
      "command": "sh echo.sh",
      "input_schema": true
    }
  ]
}
|};
      write (Stdlib.Filename.concat plugin_dir "echo.sh") "cat\n";
      match
        Plugin.run_tool ~dir:plugin_dir ~tool_name:"plugin_schema_shape"
          ~workspace:(workspace root) ~args:(`Assoc [])
      with
      | Error e -> Alcotest.failf "run_tool failed: %s" e
      | Ok (Tool_result.Error { message }) ->
          Alcotest.failf "plugin returned error: %s" message
      | Ok (Tool_result.Success { output }) ->
          Alcotest.(check string) "unsupported schema ignored" "{}" output)

let test_run_tool_reports_unknown_tool () =
  with_temp_dir "fp_agent_plugin_unknown_tool" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      mkdir_p plugin_dir;
      write_plugin plugin_dir ~id:"com.example.unknown_tool"
        ~tool_name:"plugin_known" ~kind:"read";
      match
        Plugin.run_tool ~dir:plugin_dir ~tool_name:"plugin_missing"
          ~workspace:(workspace root) ~args:(`Assoc [])
      with
      | Ok _ -> Alcotest.fail "expected unknown tool error"
      | Error e ->
          Alcotest.(check bool)
            "unknown tool message" true
            (String.is_substring e ~substring:"unknown plugin tool"))

let test_scaffold_creates_valid_plugin () =
  with_temp_dir "fp_agent_plugin_scaffold" (fun root ->
      let dir = Stdlib.Filename.concat root "starter" in
      match
        Plugin.scaffold ~id:"com.example.scaffold" ~tool_name:"scaffold_echo"
          ~kind:"write" dir
      with
      | Error e -> Alcotest.failf "scaffold failed: %s" e
      | Ok created -> (
          Alcotest.(check string) "created dir" dir created;
          Alcotest.(check bool)
            "manifest exists" true
            (Stdlib.Sys.file_exists
               (Stdlib.Filename.concat dir Plugin.manifest_file));
          Alcotest.(check bool)
            "readme exists" true
            (Stdlib.Sys.file_exists (Stdlib.Filename.concat dir "README.md"));
          Alcotest.(check bool)
            "sample args exists" true
            (Stdlib.Sys.file_exists
               (Stdlib.Filename.concat dir
                  (Stdlib.Filename.concat "examples" "scaffold_echo.args.json")));
          let readme =
            Stdlib.In_channel.with_open_bin
              (Stdlib.Filename.concat dir "README.md")
              Stdlib.In_channel.input_all
          in
          Alcotest.(check bool)
            "readme documents args file env" true
            (String.is_substring readme ~substring:"FP_AGENT_ARGS_FILE");
          Alcotest.(check bool)
            "readme documents permissions env" true
            (String.is_substring readme ~substring:"FP_AGENT_TOOL_PERMISSIONS");
          Alcotest.(check bool)
            "readme documents tool kind" true
            (String.is_substring readme ~substring:"Initial tool kind: `write`");
          Alcotest.(check bool)
            "readme documents tool permissions" true
            (String.is_substring readme
               ~substring:"Initial permissions: `workspace=write`");
          Alcotest.(check bool)
            "readme documents interactive check" true
            (String.is_substring readme ~substring:"/plugin-check .");
          Alcotest.(check bool)
            "readme documents interactive smoke" true
            (String.is_substring readme ~substring:"/plugin-smoke .");
          Alcotest.(check bool)
            "readme documents plugin dev" true
            (String.is_substring readme ~substring:"/plugin-dev --replace .");
          Alcotest.(check bool)
            "readme documents plugin run" true
            (String.is_substring readme ~substring:"/plugin-run . scaffold_echo");
          Alcotest.(check bool)
            "readme documents cli plugin dev" true
            (String.is_substring readme
               ~substring:"--dev-plugin . --replace-plugin");
          Alcotest.(check bool)
            "readme documents multi-case smoke" true
            (String.is_substring readme ~substring:"examples/scaffold_echo/");
          Alcotest.(check bool)
            "readme documents replace install" true
            (String.is_substring readme ~substring:"/plugin-install --replace .");
          Alcotest.(check bool)
            "readme documents cli replace install" true
            (String.is_substring readme
               ~substring:"--install-plugin . --replace-plugin");
          Alcotest.(check bool)
            "readme documents remove" true
            (String.is_substring readme
               ~substring:"/plugin-remove com.example.scaffold");
          match Plugin.check dir with
          | Error e -> Alcotest.failf "scaffold check failed: %s" e
          | Ok manifest ->
              Alcotest.(check string)
                "scaffold id" "com.example.scaffold" manifest.id;
              Alcotest.(check int)
                "scaffold sdk version" Plugin.supported_sdk_version
                manifest.sdk_version;
              let tool = Option.value_exn (List.hd manifest.tools) in
              Alcotest.(check string)
                "scaffold tool" "scaffold_echo" tool.tool_name;
              Alcotest.(check bool)
                "scaffold kind" true
                (Poly.equal tool.tool_kind Tool.Write);
              Alcotest.(check string)
                "scaffold permissions" "workspace=write"
                (Plugin.permissions_label tool.tool_permissions);
              Alcotest.(check int) "one tool" 1 (List.length manifest.tools)))

let test_check_rejects_invalid_manifest () =
  with_temp_dir "fp_agent_plugin_bad" (fun root ->
      let check_error name json substring =
        let dir = Stdlib.Filename.concat root name in
        mkdir_p dir;
        write (Stdlib.Filename.concat dir Plugin.manifest_file) json;
        match Plugin.check dir with
        | Ok _ -> Alcotest.failf "expected invalid manifest: %s" name
        | Error e ->
            Alcotest.(check bool)
              (name ^ " error") true
              (String.is_substring e ~substring)
      in
      check_error "bad-id" {|{"id":"bad id","tools":[]}|} "plugin id";
      check_error "dotdot-id" {|{"id":"..","tools":[]}|} "plugin id cannot be";
      check_error "empty-tools" {|{"id":"com.example.empty","tools":[]}|}
        "at least one tool";
      check_error "unsupported-sdk"
        {|{
  "id":"com.example.unsupported",
  "sdk_version":999,
  "tools":[
    {"name":"echo","kind":"read","description":"Echo","command":"sh echo.sh"}
  ]
}|}
        "unsupported sdk_version";
      check_error "bad-sdk"
        {|{
  "id":"com.example.bad_sdk",
  "sdk_version":0,
  "tools":[
    {"name":"echo","kind":"read","description":"Echo","command":"sh echo.sh"}
  ]
}|}
        "sdk_version must be positive";
      check_error "non-integer-sdk"
        {|{
  "id":"com.example.bad_sdk_type",
  "sdk_version":"newest",
  "tools":[
    {"name":"echo","kind":"read","description":"Echo","command":"sh echo.sh"}
  ]
}|}
        "sdk_version must be an integer";
      check_error "duplicate-tool"
        {|{
  "id":"com.example.duplicate",
  "tools":[
    {"name":"echo","kind":"read","description":"Echo","command":"sh echo.sh"},
    {"name":"echo","kind":"read","description":"Echo","command":"sh echo.sh"}
  ]
}|}
        "duplicate tool name: echo";
      check_error "bad-timeout"
        {|{
  "id":"com.example.timeout",
  "tools":[
    {
      "name":"echo",
      "kind":"read",
      "description":"Echo",
      "command":"sh echo.sh",
      "timeout":0
    }
  ]
}|}
        "timeout must be positive";
      check_error "bad-permissions"
        {|{
  "id":"com.example.permissions",
  "tools":[
    {
      "name":"echo",
      "kind":"read",
      "description":"Echo",
      "command":"sh echo.sh",
      "permissions":["workspace", 1]
    }
  ]
}|}
        "permissions must contain only strings";
      check_error "empty-command"
        {|{
  "id":"com.example.command",
  "tools":[
    {"name":"echo","kind":"read","description":"Echo","command":" "}
  ]
}|}
        "missing string field 'command'")

let test_plugin_schema_reaches_native_tool_request () =
  with_temp_dir "fp_agent_plugin_schema" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      mkdir_p plugin_dir;
      write_plugin plugin_dir ~id:"com.example.schema"
        ~tool_name:"plugin_schema_echo" ~kind:"read";
      Unix.putenv "FP_AGENT_PLUGIN_PATH" plugin_dir;
      Unix.putenv "FP_AGENT_PLUGIN_HOME" (Stdlib.Filename.concat root "home");
      let body =
        Model_client.request_body_with_options_for_test ~tools_enabled:true
          ~config:(test_config root) ~system:"sys" ~turns:[]
      in
      let open Yojson.Safe.Util in
      let tools = body |> member "tools" |> to_list in
      let plugin_tool =
        List.find tools ~f:(fun tool ->
            String.equal
              (tool |> member "function" |> member "name" |> to_string)
              "plugin_schema_echo")
      in
      match plugin_tool with
      | None -> Alcotest.fail "plugin tool missing from native request"
      | Some tool ->
          Alcotest.(check string)
            "schema property" "string"
            (tool |> member "function" |> member "parameters"
           |> member "properties" |> member "message" |> member "type"
           |> to_string))

let () =
  Alcotest.run "plugin"
    [
      ( "plugins",
        [
          Alcotest.test_case "manifest_plugin_executes" `Quick
            test_manifest_plugin_executes;
          Alcotest.test_case "plugin_write_policy" `Quick
            test_plugin_write_policy_uses_path_bounds;
          Alcotest.test_case "install_plugin" `Quick
            test_install_plugin_copies_to_home;
          Alcotest.test_case "install_replace_plugin" `Quick
            test_install_plugin_can_replace_existing;
          Alcotest.test_case "discover_invalid_plugin" `Quick
            test_discover_reports_invalid_manifests;
          Alcotest.test_case "tool_conflicts" `Quick
            test_tool_conflicts_report_collisions;
          Alcotest.test_case "check_tool_conflicts" `Quick
            test_check_rejects_candidate_tool_conflicts;
          Alcotest.test_case "run_tool" `Quick
            test_run_tool_for_plugin_development;
          Alcotest.test_case "smoke" `Quick test_smoke_runs_example_args;
          Alcotest.test_case "tool_loader_reload" `Quick
            test_tool_loader_refreshes_removed_plugins;
          Alcotest.test_case "runtime_environment" `Quick
            test_plugin_runtime_environment;
          Alcotest.test_case "run_tool_schema_validation" `Quick
            test_run_tool_validates_input_schema;
          Alcotest.test_case "run_tool_schema_enum" `Quick
            test_run_tool_validates_input_schema_enum;
          Alcotest.test_case "run_tool_schema_additional_properties" `Quick
            test_run_tool_rejects_additional_properties;
          Alcotest.test_case "run_tool_schema_additional_properties_schema"
            `Quick test_run_tool_validates_additional_properties_schema;
          Alcotest.test_case "run_tool_unsupported_schema_shape" `Quick
            test_run_tool_ignores_unsupported_schema_shape;
          Alcotest.test_case "run_tool_unknown" `Quick
            test_run_tool_reports_unknown_tool;
          Alcotest.test_case "scaffold_plugin" `Quick
            test_scaffold_creates_valid_plugin;
          Alcotest.test_case "check_invalid_plugin" `Quick
            test_check_rejects_invalid_manifest;
          Alcotest.test_case "plugin_schema_request" `Quick
            test_plugin_schema_reaches_native_tool_request;
        ] );
    ]
