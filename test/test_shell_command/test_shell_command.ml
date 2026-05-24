open! Base
open Fp_agent

let require_task name expected = function
  | Shell_command.Task actual -> Alcotest.(check string) name expected actual
  | _ -> Alcotest.failf "%s: expected task" name

let require_command name expected_id expected_args = function
  | Shell_command.Command (actual_id, actual_args) ->
      Alcotest.(check bool)
        (name ^ " id") true
        (Poly.equal expected_id actual_id);
      Alcotest.(check string) (name ^ " args") expected_args actual_args
  | _ -> Alcotest.failf "%s: expected command" name

let require_acceptance name expected = function
  | Shell_command.Execute line ->
      Alcotest.(check (pair string string)) name expected ("execute", line)
  | Shell_command.Draft draft ->
      Alcotest.(check (pair string string)) name expected ("draft", draft)

let test_parse () =
  Alcotest.(check bool)
    "empty" true
    (match Shell_command.parse "  " with
    | Shell_command.Empty -> true
    | _ -> false);
  require_task "task" "fix tests" (Shell_command.parse "  fix tests  ");
  require_command "model" Shell_command.Model "qwen36-rtx"
    (Shell_command.parse "/model qwen36-rtx");
  require_command "models" Shell_command.Models ""
    (Shell_command.parse "/models");
  require_command "new session" Shell_command.NewSession ""
    (Shell_command.parse "/new");
  require_command "usage" Shell_command.Usage "" (Shell_command.parse "/usage");
  require_command "status" Shell_command.Status ""
    (Shell_command.parse "/status");
  require_command "instructions" Shell_command.Instructions ""
    (Shell_command.parse "/instructions");
  require_command "compact" Shell_command.Compact ""
    (Shell_command.parse "/compact");
  require_command "retry" Shell_command.Retry "" (Shell_command.parse "/retry");
  require_command "provider" Shell_command.Provider
    "local-llm qwen36-rtx http://127.0.0.1:8000/v1"
    (Shell_command.parse
       "/provider local-llm qwen36-rtx http://127.0.0.1:8000/v1");
  require_command "plugin check" Shell_command.PluginCheck "./my-plugin"
    (Shell_command.parse "/plugin-check ./my-plugin");
  require_command "plugin install" Shell_command.PluginInstall
    "--replace ./my-plugin"
    (Shell_command.parse "/plugin-install --replace ./my-plugin");
  require_command "plugin remove alias" Shell_command.PluginRemove "local.foo"
    (Shell_command.parse "/plugin-uninstall local.foo");
  require_command "plugin smoke" Shell_command.PluginSmoke
    "--replace ./my-plugin"
    (Shell_command.parse "/plugin-smoke --replace ./my-plugin");
  require_command "quit alias" Shell_command.Exit ""
    (Shell_command.parse "/quit");
  Alcotest.(check bool)
    "unknown" true
    (match Shell_command.parse "/missing" with
    | Shell_command.Unknown "/missing" -> true
    | _ -> false)

let test_metadata () =
  let palette =
    List.map Shell_command.palette_entries ~f:(fun entry -> entry.command)
  in
  Alcotest.(check bool)
    "palette has tools" true
    (List.mem palette "/tools" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has provider" true
    (List.mem palette "/provider <name> [model] [api-base]" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has usage" true
    (List.mem palette "/usage" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has status" true
    (List.mem palette "/status" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plugin check" true
    (List.mem palette "/plugin-check [--replace] <dir>" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plugin install" true
    (List.mem palette "/plugin-install [--replace] <dir>" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plugin remove" true
    (List.mem palette "/plugin-remove <id>" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plugin smoke" true
    (List.mem palette "/plugin-smoke [--replace] <dir>" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has instructions" true
    (List.mem palette "/instructions" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has compact" true
    (List.mem palette "/compact" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has new session" true
    (List.mem palette "/new" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has retry" true
    (List.mem palette "/retry" ~equal:String.equal);
  let help = Shell_command.help_text () in
  Alcotest.(check bool)
    "help has alias" true
    (String.is_substring help ~substring:"/exit, /quit");
  Alcotest.(check bool)
    "help has task fallback" true
    (String.is_substring help ~substring:"Anything else is sent to the agent")

let test_acceptance () =
  let entry command =
    Option.value_exn
      (List.find Shell_command.palette_entries ~f:(fun entry ->
           String.equal entry.command command))
  in
  require_acceptance "tools execute" ("execute", "/tools")
    (Shell_command.accept (entry "/tools"));
  require_acceptance "model execute" ("execute", "/model")
    (Shell_command.accept (entry "/model [id]"));
  require_acceptance "usage execute" ("execute", "/usage")
    (Shell_command.accept (entry "/usage"));
  require_acceptance "status execute" ("execute", "/status")
    (Shell_command.accept (entry "/status"));
  require_acceptance "instructions execute"
    ("execute", "/instructions")
    (Shell_command.accept (entry "/instructions"));
  require_acceptance "compact draft" ("draft", "/compact")
    (Shell_command.accept (entry "/compact"));
  require_acceptance "tool draft" ("draft", "/tool ")
    (Shell_command.accept (entry "/tool <name>"));
  require_acceptance "provider draft" ("draft", "/provider ")
    (Shell_command.accept (entry "/provider <name> [model] [api-base]"));
  require_acceptance "plugin check draft"
    ("draft", "/plugin-check ")
    (Shell_command.accept (entry "/plugin-check [--replace] <dir>"));
  require_acceptance "plugin install draft"
    ("draft", "/plugin-install ")
    (Shell_command.accept (entry "/plugin-install [--replace] <dir>"));
  require_acceptance "plugin remove draft"
    ("draft", "/plugin-remove ")
    (Shell_command.accept (entry "/plugin-remove <id>"));
  require_acceptance "plugin smoke draft"
    ("draft", "/plugin-smoke ")
    (Shell_command.accept (entry "/plugin-smoke [--replace] <dir>"));
  require_acceptance "new session draft" ("draft", "/new")
    (Shell_command.accept (entry "/new"));
  require_acceptance "retry draft" ("draft", "/retry")
    (Shell_command.accept (entry "/retry"));
  require_acceptance "undo draft" ("draft", "/undo")
    (Shell_command.accept (entry "/undo"))

let () =
  Alcotest.run "shell_command"
    [
      ( "commands",
        [
          Alcotest.test_case "parse" `Quick test_parse;
          Alcotest.test_case "metadata" `Quick test_metadata;
          Alcotest.test_case "acceptance" `Quick test_acceptance;
        ] );
    ]
