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
  require_command "provider" Shell_command.Provider
    "local-llm qwen36-rtx http://127.0.0.1:8000/v1"
    (Shell_command.parse
       "/provider local-llm qwen36-rtx http://127.0.0.1:8000/v1");
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
  let help = Shell_command.help_text () in
  Alcotest.(check bool)
    "help has alias" true
    (String.is_substring help ~substring:"/exit, /quit");
  Alcotest.(check bool)
    "help has task fallback" true
    (String.is_substring help ~substring:"Anything else is sent to the agent")

let () =
  Alcotest.run "shell_command"
    [
      ( "commands",
        [
          Alcotest.test_case "parse" `Quick test_parse;
          Alcotest.test_case "metadata" `Quick test_metadata;
        ] );
    ]
