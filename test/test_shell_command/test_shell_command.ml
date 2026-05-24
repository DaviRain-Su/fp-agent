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
  require_command "model next" Shell_command.ModelNext ""
    (Shell_command.parse "/model-next");
  require_command "model cycle alias" Shell_command.ModelNext ""
    (Shell_command.parse "/model-cycle");
  require_command "models" Shell_command.Models ""
    (Shell_command.parse "/models");
  require_command "providers" Shell_command.Providers ""
    (Shell_command.parse "/providers");
  require_command "providers alias" Shell_command.Providers ""
    (Shell_command.parse "/provider-list");
  require_command "new session" Shell_command.NewSession ""
    (Shell_command.parse "/new");
  require_command "usage" Shell_command.Usage "" (Shell_command.parse "/usage");
  require_command "status" Shell_command.Status ""
    (Shell_command.parse "/status");
  require_command "context" Shell_command.Context ""
    (Shell_command.parse "/context");
  require_command "context alias" Shell_command.Context ""
    (Shell_command.parse "/ctx");
  require_command "handoff" Shell_command.Handoff ""
    (Shell_command.parse "/handoff");
  require_command "instructions" Shell_command.Instructions ""
    (Shell_command.parse "/instructions");
  require_command "compact" Shell_command.Compact ""
    (Shell_command.parse "/compact");
  require_command "review" Shell_command.Review "security risks"
    (Shell_command.parse "/review security risks");
  require_command "retry" Shell_command.Retry "" (Shell_command.parse "/retry");
  require_command "plan" Shell_command.Plan "" (Shell_command.parse "/plan");
  require_command "plan set" Shell_command.PlanSet
    "todo inspect code; doing implement plan; done write tests"
    (Shell_command.parse
       "/plan-set todo inspect code; doing implement plan; done write tests");
  require_command "plan add" Shell_command.PlanAdd "todo run tests"
    (Shell_command.parse "/plan-add todo run tests");
  require_command "plan update" Shell_command.PlanUpdate "2 done write tests"
    (Shell_command.parse "/plan-update 2 done write tests");
  require_command "plan clear" Shell_command.PlanClear ""
    (Shell_command.parse "/plan-clear");
  require_command "provider" Shell_command.Provider
    "local-llm qwen36-rtx http://127.0.0.1:8000/v1"
    (Shell_command.parse
       "/provider local-llm qwen36-rtx http://127.0.0.1:8000/v1");
  require_command "provider add" Shell_command.ProviderAdd
    "local-llm http://127.0.0.1:8000/v1 qwen36-rtx --api-key dummy \
     --local-compat"
    (Shell_command.parse
       "/provider-add local-llm http://127.0.0.1:8000/v1 qwen36-rtx --api-key \
        dummy --local-compat");
  require_command "plugin new" Shell_command.PluginNew
    "--id local.foo --tool-name foo --kind exec --template python ./my-plugin"
    (Shell_command.parse
       "/plugin-new --id local.foo --tool-name foo --kind exec --template \
        python ./my-plugin");
  require_command "plugin dev" Shell_command.PluginDev "--replace ./my-plugin"
    (Shell_command.parse "/plugin-dev --replace ./my-plugin");
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
  require_command "plugin run" Shell_command.PluginRun
    {|./my-plugin hello_world {"message":"hi"}|}
    (Shell_command.parse
       {|/plugin-run ./my-plugin hello_world {"message":"hi"}|});
  require_command "plugin doctor" Shell_command.PluginDoctor ""
    (Shell_command.parse "/plugin-doctor");
  require_command "plugin doctor alias" Shell_command.PluginDoctor ""
    (Shell_command.parse "/plugins-doctor");
  require_command "plugin sdk" Shell_command.PluginSdk ""
    (Shell_command.parse "/plugin-sdk");
  require_command "plugin sdk alias" Shell_command.PluginSdk ""
    (Shell_command.parse "/plugin-templates");
  require_command "quit alias" Shell_command.Exit ""
    (Shell_command.parse "/quit");
  Alcotest.(check bool)
    "unknown" true
    (match Shell_command.parse "/missing" with
    | Shell_command.Unknown "/missing" -> true
    | _ -> false)

let test_metadata () =
  let entry command =
    Option.value_exn
      (List.find Shell_command.palette_entries ~f:(fun entry ->
           String.equal entry.command command))
  in
  let palette =
    List.map Shell_command.palette_entries ~f:(fun entry -> entry.command)
  in
  Alcotest.(check bool)
    "palette has tools" true
    (List.mem palette "/tools" ~equal:String.equal);
  Alcotest.(check string) "tools group" "Tools" (entry "/tools").group;
  Alcotest.(check bool)
    "palette has provider" true
    (List.mem palette "/provider <name> [model] [api-base]" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has providers" true
    (List.mem palette "/providers" ~equal:String.equal);
  Alcotest.(check string) "providers group" "Models" (entry "/providers").group;
  Alcotest.(check string)
    "provider group" "Models"
    (entry "/provider <name> [model] [api-base]").group;
  Alcotest.(check bool)
    "palette has provider add" true
    (List.mem palette
       "/provider-add <name> <base-url> <model[,model...]> [--api-key KEY] \
        [--local-compat]"
       ~equal:String.equal);
  Alcotest.(check string)
    "provider add group" "Models"
    (entry
       "/provider-add <name> <base-url> <model[,model...]> [--api-key KEY] \
        [--local-compat]")
      .group;
  Alcotest.(check bool)
    "palette has model next" true
    (List.mem palette "/model-next" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has usage" true
    (List.mem palette "/usage" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plan" true
    (List.mem palette "/plan" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plan set" true
    (List.mem palette "/plan-set <status item; ...>" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plan add" true
    (List.mem palette "/plan-add <status> <item>" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plan update" true
    (List.mem palette "/plan-update <number> <status> [item]"
       ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plan clear" true
    (List.mem palette "/plan-clear" ~equal:String.equal);
  Alcotest.(check string) "plan group" "Context" (entry "/plan").group;
  Alcotest.(check bool)
    "palette has status" true
    (List.mem palette "/status" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has context" true
    (List.mem palette "/context" ~equal:String.equal);
  Alcotest.(check string) "context group" "Context" (entry "/context").group;
  Alcotest.(check bool)
    "palette has handoff" true
    (List.mem palette "/handoff" ~equal:String.equal);
  Alcotest.(check string) "handoff group" "Context" (entry "/handoff").group;
  Alcotest.(check bool)
    "palette has plugin new" true
    (List.mem palette
       "/plugin-new [--id ID] [--tool-name NAME] [--kind KIND] [--template \
        NAME] <dir>"
       ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plugin check" true
    (List.mem palette "/plugin-check [--replace] <dir>" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plugin dev" true
    (List.mem palette "/plugin-dev [--replace] <dir>" ~equal:String.equal);
  Alcotest.(check string)
    "plugin dev group" "Plugins" (entry "/plugin-dev [--replace] <dir>").group;
  Alcotest.(check bool)
    "palette has plugin install" true
    (List.mem palette "/plugin-install [--replace] <dir>" ~equal:String.equal);
  Alcotest.(check string)
    "plugin install group" "Plugins"
    (entry "/plugin-install [--replace] <dir>").group;
  Alcotest.(check bool)
    "palette has plugin remove" true
    (List.mem palette "/plugin-remove <id>" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plugin smoke" true
    (List.mem palette "/plugin-smoke [--replace] <dir>" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has plugin run" true
    (List.mem palette "/plugin-run <dir> <tool> <json|@file>"
       ~equal:String.equal);
  Alcotest.(check string)
    "plugin run group" "Plugins"
    (entry "/plugin-run <dir> <tool> <json|@file>").group;
  Alcotest.(check bool)
    "palette has plugin doctor" true
    (List.mem palette "/plugin-doctor" ~equal:String.equal);
  Alcotest.(check string)
    "plugin doctor group" "Plugins" (entry "/plugin-doctor").group;
  Alcotest.(check bool)
    "palette has plugin sdk" true
    (List.mem palette "/plugin-sdk" ~equal:String.equal);
  Alcotest.(check string)
    "plugin sdk group" "Plugins" (entry "/plugin-sdk").group;
  Alcotest.(check bool)
    "palette has instructions" true
    (List.mem palette "/instructions" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has compact" true
    (List.mem palette "/compact" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has review" true
    (List.mem palette "/review [focus]" ~equal:String.equal);
  Alcotest.(check string)
    "review group" "Run Control" (entry "/review [focus]").group;
  Alcotest.(check bool)
    "palette has new session" true
    (List.mem palette "/new" ~equal:String.equal);
  Alcotest.(check bool)
    "palette has retry" true
    (List.mem palette "/retry" ~equal:String.equal);
  Alcotest.(check string) "retry group" "Run Control" (entry "/retry").group;
  let help = Shell_command.help_text () in
  Alcotest.(check bool)
    "help has plugin group" true
    (String.is_substring help ~substring:"Plugins:");
  Alcotest.(check bool)
    "help has run control group" true
    (String.is_substring help ~substring:"Run Control:");
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
  require_acceptance "model next execute" ("execute", "/model-next")
    (Shell_command.accept (entry "/model-next"));
  require_acceptance "providers execute" ("execute", "/providers")
    (Shell_command.accept (entry "/providers"));
  require_acceptance "usage execute" ("execute", "/usage")
    (Shell_command.accept (entry "/usage"));
  require_acceptance "plan execute" ("execute", "/plan")
    (Shell_command.accept (entry "/plan"));
  require_acceptance "status execute" ("execute", "/status")
    (Shell_command.accept (entry "/status"));
  require_acceptance "context execute" ("execute", "/context")
    (Shell_command.accept (entry "/context"));
  require_acceptance "handoff execute" ("execute", "/handoff")
    (Shell_command.accept (entry "/handoff"));
  require_acceptance "instructions execute"
    ("execute", "/instructions")
    (Shell_command.accept (entry "/instructions"));
  require_acceptance "compact draft" ("draft", "/compact")
    (Shell_command.accept (entry "/compact"));
  require_acceptance "tool draft" ("draft", "/tool ")
    (Shell_command.accept (entry "/tool <name>"));
  require_acceptance "provider draft" ("draft", "/provider ")
    (Shell_command.accept (entry "/provider <name> [model] [api-base]"));
  require_acceptance "provider add draft"
    ("draft", "/provider-add ")
    (Shell_command.accept
       (entry
          "/provider-add <name> <base-url> <model[,model...]> [--api-key KEY] \
           [--local-compat]"));
  require_acceptance "plan set draft" ("draft", "/plan-set ")
    (Shell_command.accept (entry "/plan-set <status item; ...>"));
  require_acceptance "plan add draft"
    ("draft", "/plan-add todo ")
    (Shell_command.accept (entry "/plan-add <status> <item>"));
  require_acceptance "plan update draft" ("draft", "/plan-update ")
    (Shell_command.accept (entry "/plan-update <number> <status> [item]"));
  require_acceptance "plan clear draft" ("draft", "/plan-clear")
    (Shell_command.accept (entry "/plan-clear"));
  require_acceptance "plugin new draft" ("draft", "/plugin-new ")
    (Shell_command.accept
       (entry
          "/plugin-new [--id ID] [--tool-name NAME] [--kind KIND] [--template \
           NAME] <dir>"));
  require_acceptance "plugin dev draft" ("draft", "/plugin-dev ")
    (Shell_command.accept (entry "/plugin-dev [--replace] <dir>"));
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
  require_acceptance "plugin run draft" ("draft", "/plugin-run ")
    (Shell_command.accept (entry "/plugin-run <dir> <tool> <json|@file>"));
  require_acceptance "plugin doctor execute"
    ("execute", "/plugin-doctor")
    (Shell_command.accept (entry "/plugin-doctor"));
  require_acceptance "plugin sdk execute" ("execute", "/plugin-sdk")
    (Shell_command.accept (entry "/plugin-sdk"));
  require_acceptance "new session draft" ("draft", "/new")
    (Shell_command.accept (entry "/new"));
  require_acceptance "review draft" ("draft", "/review ")
    (Shell_command.accept (entry "/review [focus]"));
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
