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

let write_plugin dir ~id ~tool_name ~kind =
  write
    (Stdlib.Filename.concat dir Plugin.manifest_file)
    (Printf.sprintf
       {|
{
  "id": "%s",
  "name": "Test Plugin",
  "version": "0.1.0",
  "sdk_version": 1,
  "tools": [
    {
      "name": "%s",
      "kind": "%s",
      "description": "Echoes its JSON input",
      "command": "sh echo.sh",
      "input_schema": {
        "type": "object",
        "properties": { "message": { "type": "string" } },
        "required": ["message"]
      }
    }
  ]
}
|}
       id tool_name kind);
  write
    (Stdlib.Filename.concat dir "echo.sh")
    "printf 'tool=%s workspace=%s input=' \"$FP_AGENT_TOOL_NAME\" \
     \"$FP_AGENT_WORKSPACE\"\n\
     cat\n"

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
      match Plugin.scaffold ~id:"com.example.scaffold" dir with
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
                  (Stdlib.Filename.concat "examples" "hello.args.json")));
          match Plugin.check dir with
          | Error e -> Alcotest.failf "scaffold check failed: %s" e
          | Ok manifest ->
              Alcotest.(check string)
                "scaffold id" "com.example.scaffold" manifest.id;
              Alcotest.(check int)
                "scaffold sdk version" Plugin.supported_sdk_version
                manifest.sdk_version;
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
          Alcotest.test_case "run_tool" `Quick
            test_run_tool_for_plugin_development;
          Alcotest.test_case "run_tool_schema_validation" `Quick
            test_run_tool_validates_input_schema;
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
