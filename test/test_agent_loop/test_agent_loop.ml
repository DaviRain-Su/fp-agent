open! Base
open Fp_agent

let with_env f =
  let root = Stdlib.Filename.temp_dir "fp_agent_loop" "" in
  let config =
    {
      Config.provider = "test";
      Config.api_key = "test-key";
      api_base = "http://localhost";
      model = "test";
      models = [];
      protocol = Provider.Openai;
      compat = Config.default_compat;
      max_tokens = None;
      max_steps = 10;
      workspace_root = root;
    }
  in
  let workspace =
    match Workspace.create ~root with
    | Ok ws -> ws
    | Error e -> Alcotest.failf "workspace: %s" e
  in
  let session_dir = Session.create ~base_dir:root in
  let event_log = Event_log.create ~session_dir in
  Exn.protect
    ~f:(fun () -> f config workspace event_log session_dir)
    ~finally:(fun () ->
      Event_log.close event_log;
      ignore
        (Shell.run ~command:(Printf.sprintf "rm -rf %s" root) ~timeout_sec:10
          : (Shell.result, string) Result.t))

let content_of_action = function
  | Model_action.Final_answer { answer } -> [ Llm.Text answer ]
  | Tool_call tc ->
      [ Llm.Tool_use { id = "call-0"; name = tc.name; input = tc.args } ]
  | Tool_calls calls ->
      List.mapi calls ~f:(fun i (tc : Tool_call.t) ->
          Llm.Tool_use
            { id = Printf.sprintf "call-%d" i; name = tc.name; input = tc.args })

let response action = (content_of_action action, Llm.zero_usage)

let write_file path content =
  Stdlib.Out_channel.with_open_bin path (fun oc ->
      Stdlib.Out_channel.output_string oc content)

let shell_ok label command =
  match Shell.run ~command ~timeout_sec:30 with
  | Ok { exit_code = 0; _ } -> ()
  | Ok { exit_code; stdout; stderr } ->
      Alcotest.failf "%s failed (exit %d): stdout=%s stderr=%s" label exit_code
        stdout stderr
  | Error e -> Alcotest.failf "%s failed: %s" label e

(* A mock that returns a scripted sequence of responses, one per call. *)
let scripted actions =
  let remaining = ref (List.map actions ~f:response) in
  Model_client.create_mock ~send:(fun _turns ->
      match !remaining with
      | a :: tl ->
          remaining := tl;
          Lwt.return (Ok a)
      | [] ->
          Lwt.return
            (Ok (response (Model_action.Final_answer { answer = "exhausted" }))))

let run config workspace event_log client task =
  Lwt_main.run
    (Agent_loop.run ~config ~model_client:client ~event_log ~workspace ~task ())

let write_then_final () =
  scripted
    [
      Model_action.Tool_call
        (Tool_call.write_file ~path:"out.txt" ~content:"hi");
      Model_action.Final_answer { answer = "done" };
    ]

let run_with_approval config workspace event_log client task ~approve =
  let policy = { Policy.approve_commands = true; approve_writes = true } in
  let on_approval _ _ = Lwt.return approve in
  Lwt_main.run
    (Agent_loop.run ~policy ~on_approval ~config ~model_client:client ~event_log
       ~workspace ~task ())

let test_approval_denied () =
  with_env (fun config workspace event_log _ ->
      let outcome =
        run_with_approval config workspace event_log (write_then_final ())
          "write" ~approve:false
      in
      Alcotest.(check string)
        "still completes" "completed"
        (Agent_loop.status_to_string outcome.status);
      let file = Stdlib.Filename.concat (Workspace.root workspace) "out.txt" in
      Alcotest.(check bool)
        "file not written when denied" false
        (Stdlib.Sys.file_exists file))

let test_approval_granted () =
  with_env (fun config workspace event_log _ ->
      let _ =
        run_with_approval config workspace event_log (write_then_final ())
          "write" ~approve:true
      in
      let file = Stdlib.Filename.concat (Workspace.root workspace) "out.txt" in
      Alcotest.(check bool)
        "file written when approved" true
        (Stdlib.Sys.file_exists file))

let test_tool_then_final () =
  with_env (fun config workspace event_log session_dir ->
      let client =
        scripted
          [
            Model_action.Tool_call
              (Tool_call.write_file ~path:"out.txt" ~content:"hi there");
            Model_action.Final_answer { answer = "done" };
          ]
      in
      let outcome = run config workspace event_log client "make a file" in
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      Alcotest.(check int) "two steps" 2 outcome.steps;
      Alcotest.(check string) "summary" "done" outcome.summary;
      let file_path =
        Stdlib.Filename.concat (Workspace.root workspace) "out.txt"
      in
      Alcotest.(check bool)
        "file written" true
        (Stdlib.Sys.file_exists file_path);
      let log_path = Stdlib.Filename.concat session_dir "events.jsonl" in
      let contents =
        Stdlib.In_channel.with_open_bin log_path Stdlib.In_channel.input_all
      in
      Alcotest.(check bool)
        "log has tool result" true
        (String.is_substring contents ~substring:"Tool_result"))

let test_parallel_tool_batch_then_final () =
  with_env (fun config workspace event_log session_dir ->
      let client =
        scripted
          [
            Model_action.Tool_calls
              [
                Tool_call.write_file ~path:"a.txt" ~content:"one";
                Tool_call.write_file ~path:"b.txt" ~content:"two";
              ];
            Model_action.Final_answer { answer = "done" };
          ]
      in
      let outcome = run config workspace event_log client "make two files" in
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      Alcotest.(check int) "two model turns" 2 outcome.steps;
      Alcotest.(check bool)
        "a written" true
        (Stdlib.Sys.file_exists
           (Stdlib.Filename.concat (Workspace.root workspace) "a.txt"));
      Alcotest.(check bool)
        "b written" true
        (Stdlib.Sys.file_exists
           (Stdlib.Filename.concat (Workspace.root workspace) "b.txt"));
      let log_path = Stdlib.Filename.concat session_dir "events.jsonl" in
      let contents =
        Stdlib.In_channel.with_open_bin log_path Stdlib.In_channel.input_all
      in
      let count substring =
        String.substr_index_all contents ~may_overlap:false ~pattern:substring
        |> List.length
      in
      Alcotest.(check int) "two tool call events" 2 (count "\"Tool_call\"");
      Alcotest.(check int)
        "two tool result events" 2
        (count "\"Tool_result_message\""))

let test_update_plan_tool_emits_plan_event () =
  with_env (fun config workspace event_log session_dir ->
      let client =
        scripted
          [
            Model_action.Tool_call
              (Tool_call.update_plan
                 [
                   ("pending", "inspect implementation");
                   ("in_progress", "add regression tests");
                   ("completed", "summarize result");
                 ]);
            Model_action.Final_answer { answer = "done" };
          ]
      in
      let outcome = run config workspace event_log client "work in stages" in
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      match Journal.read ~session_dir with
      | Error e -> Alcotest.failf "read event log: %s" e
      | Ok events -> (
          let plan =
            List.find_map events ~f:(function
              | Event.Plan_updated { items } -> Some items
              | _ -> None)
          in
          match plan with
          | None -> Alcotest.fail "expected Plan_updated event"
          | Some [ first; second; third ] ->
              Alcotest.(check string)
                "first plan text" "inspect implementation" first.text;
              Alcotest.(check string)
                "first plan status" "todo"
                (Event.plan_status_to_string first.status);
              Alcotest.(check string)
                "second plan status" "doing"
                (Event.plan_status_to_string second.status);
              Alcotest.(check string)
                "third plan status" "done"
                (Event.plan_status_to_string third.status);
              Alcotest.(check bool)
                "tool result is logged" true
                (List.exists events ~f:(function
                  | Event.Tool_result_message
                      { result = Tool_result.Success { output }; _ } ->
                      String.is_substring output
                        ~substring:"plan updated: 3 item(s)"
                  | _ -> false))
          | Some items ->
              Alcotest.failf "unexpected plan length: %d" (List.length items)))

let test_workspace_snapshot_emitted_after_task () =
  with_env (fun config workspace event_log session_dir ->
      let root = Workspace.root workspace in
      shell_ok "git init"
        (Printf.sprintf "git -C %s init --quiet" (Stdlib.Filename.quote root));
      let client =
        scripted
          [
            Model_action.Tool_call
              (Tool_call.write_file ~path:"out.txt" ~content:"hi");
            Model_action.Final_answer { answer = "done" };
          ]
      in
      let outcome = run config workspace event_log client "make a file" in
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      match Journal.read ~session_dir with
      | Error e -> Alcotest.failf "read event log: %s" e
      | Ok events -> (
          match
            List.find events ~f:(function
              | Event.Workspace_snapshot _ -> true
              | _ -> false)
          with
          | Some (Event.Workspace_snapshot { is_git; status; diff_stat = _ }) ->
              Alcotest.(check bool) "snapshot is git" true is_git;
              Alcotest.(check bool)
                "status has output file" true
                (List.exists status ~f:(fun line ->
                     String.is_substring line ~substring:"out.txt"));
              Alcotest.(check bool)
                "status excludes session logs" false
                (List.exists status ~f:(fun line ->
                     String.is_substring line ~substring:".ocaml-agent"));
              Alcotest.(check bool)
                "display summarizes workspace" true
                (List.exists events ~f:(fun event ->
                     match Event.to_display event with
                     | Some line ->
                         String.is_substring line ~substring:"workspace:"
                     | None -> false))
          | Some _ | None -> Alcotest.fail "expected Workspace_snapshot event"))

let test_history_truncation_keeps_tool_exchange_atomic () =
  let big_result =
    String.make (Agent_loop.max_history_chars_for_test - 100) 'x'
  in
  let old_tool_use =
    Llm.assistant
      [
        Llm.Tool_use
          {
            id = "old-call";
            name = "read_file";
            input = `Assoc [ ("path", `String "big.txt") ];
          };
      ]
  in
  let old_tool_result =
    {
      Llm.role = Llm.User;
      content = [ Llm.Tool_result { id = "old-call"; content = big_result } ];
    }
  in
  let newest = Llm.user "newest task" in
  let truncated =
    Agent_loop.truncate_history_for_test
      [ Llm.user "old task"; old_tool_use; old_tool_result; newest ]
  in
  Alcotest.(check int)
    "oversized older tool exchange is dropped as a unit" 1
    (List.length truncated);
  match truncated with
  | [ { role = Llm.User; content = [ Llm.Text "newest task" ] } ] -> ()
  | turns ->
      Alcotest.failf "unexpected truncated history: %s"
        (Yojson.Safe.to_string (`List (List.map turns ~f:Llm.turn_to_json)))

let test_auto_compaction_preserves_summary () =
  with_env (fun config workspace event_log session_dir ->
      let chunk =
        String.make
          ((Agent_loop.compact_threshold_chars_for_test / 8) + 200)
          'x'
      in
      let initial_history =
        List.init 8 ~f:(fun i -> Llm.user (Printf.sprintf "old-%d %s" i chunk))
      in
      let saw_summary = ref false in
      let client =
        Model_client.create_mock_with_options ~send:(fun ~tools_enabled turns ->
            Alcotest.(check bool)
              "normal model send keeps tools enabled" true tools_enabled;
            saw_summary :=
              List.exists turns ~f:(fun (turn : Llm.turn) ->
                  List.exists turn.content ~f:(function
                    | Llm.Text text ->
                        String.is_substring text
                          ~substring:"[Earlier conversation summary]"
                    | _ -> false));
            Lwt.return (Ok ([ Llm.Text "done" ], Llm.zero_usage)))
      in
      let outcome =
        Lwt_main.run
          (Agent_loop.run ~initial_history ~config ~model_client:client
             ~event_log ~workspace ~task:"continue" ())
      in
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      Alcotest.(check bool) "summary sent to model" true !saw_summary;
      let log_path = Stdlib.Filename.concat session_dir "events.jsonl" in
      let contents =
        Stdlib.In_channel.with_open_bin log_path Stdlib.In_channel.input_all
      in
      Alcotest.(check bool)
        "log has compaction event" true
        (String.is_substring contents ~substring:"Context_compacted"))

let test_manual_compaction_event_preserves_recent_turns () =
  let turns = List.init 8 ~f:(fun i -> Llm.user (Printf.sprintf "turn-%d" i)) in
  match Agent_loop.compact_event_of_turns turns with
  | None -> Alcotest.fail "expected compaction event"
  | Some (Event.Context_compacted { summary; recent }) -> (
      Alcotest.(check bool)
        "summary has older turn" true
        (String.is_substring summary ~substring:"turn-0");
      Alcotest.(check bool)
        "summary omits newest turn" false
        (String.is_substring summary ~substring:"turn-7");
      Alcotest.(check int) "keeps recent chunks" 6 (List.length recent);
      match List.last recent with
      | Some { Llm.role = Llm.User; content = [ Llm.Text "turn-7" ] } -> ()
      | _ -> Alcotest.fail "newest turn not kept in recent history")
  | Some event ->
      Alcotest.failf "unexpected event: %s"
        (Yojson.Safe.to_string (Event.to_yojson event))

let test_code_review_task_adds_system_guidance_without_rewriting_user_event () =
  with_env (fun config workspace event_log session_dir ->
      let captured_system = ref "" in
      let captured_turns = ref [] in
      let client =
        Model_client.create_mock_with_request
          ~send:(fun ~system ~tools_enabled turns ->
            Alcotest.(check bool) "tools enabled" true tools_enabled;
            captured_system := system;
            captured_turns := turns;
            Lwt.return (Ok ([ Llm.Text "reviewed" ], Llm.zero_usage)))
      in
      let outcome =
        run config workspace event_log client "code review this change"
      in
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      Alcotest.(check bool)
        "system has review guidance" true
        (String.is_substring !captured_system ~substring:"Code review mode");
      Alcotest.(check bool)
        "system asks for diff stat" true
        (String.is_substring !captured_system ~substring:"git diff --stat");
      (match !captured_turns with
      | [
       { Llm.role = Llm.User; content = [ Llm.Text content ] };
       { Llm.role = Llm.User; content = [ Llm.Text preflight ] };
      ] ->
          Alcotest.(check string)
            "model sees original user task" "code review this change" content;
          Alcotest.(check bool)
            "model sees review preflight" true
            (String.is_substring preflight ~substring:"[Code review preflight]")
      | turns ->
          Alcotest.failf "unexpected turns: %s"
            (Yojson.Safe.to_string (`List (List.map turns ~f:Llm.turn_to_json))));
      let log_path = Stdlib.Filename.concat session_dir "events.jsonl" in
      let contents =
        Stdlib.In_channel.with_open_bin log_path Stdlib.In_channel.input_all
      in
      Alcotest.(check bool)
        "log has original task" true
        (String.is_substring contents ~substring:"code review this change");
      Alcotest.(check bool)
        "log omits review guidance" false
        (String.is_substring contents ~substring:"Code review mode"))

let test_regular_task_uses_base_system_prompt () =
  with_env (fun config workspace event_log _ ->
      let captured_system = ref "" in
      let client =
        Model_client.create_mock_with_request
          ~send:(fun ~system ~tools_enabled:_ _turns ->
            captured_system := system;
            Lwt.return (Ok ([ Llm.Text "done" ], Llm.zero_usage)))
      in
      let outcome = run config workspace event_log client "add a feature" in
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      Alcotest.(check bool)
        "regular task has no review guidance" false
        (String.is_substring !captured_system ~substring:"Code review mode"))

let test_project_instructions_are_added_to_system_prompt () =
  with_env (fun config workspace event_log session_dir ->
      let root = Workspace.root workspace in
      write_file
        (Stdlib.Filename.concat root "RTK.md")
        "Prefer repo-specific test evidence.\n";
      write_file
        (Stdlib.Filename.concat root "AGENTS.md")
        "Follow workspace conventions.\n@RTK.md\n";
      let captured_system = ref "" in
      let client =
        Model_client.create_mock_with_request
          ~send:(fun ~system ~tools_enabled:_ _turns ->
            captured_system := system;
            Lwt.return (Ok ([ Llm.Text "done" ], Llm.zero_usage)))
      in
      let outcome = run config workspace event_log client "add a feature" in
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      Alcotest.(check bool)
        "system has project instruction header" true
        (String.is_substring !captured_system
           ~substring:"Project instructions loaded");
      Alcotest.(check bool)
        "system has agents file" true
        (String.is_substring !captured_system ~substring:"--- AGENTS.md ---");
      Alcotest.(check bool)
        "system has included file" true
        (String.is_substring !captured_system ~substring:"--- RTK.md ---");
      Alcotest.(check bool)
        "system has included content" true
        (String.is_substring !captured_system
           ~substring:"Prefer repo-specific test evidence.");
      let log_path = Stdlib.Filename.concat session_dir "events.jsonl" in
      let contents =
        Stdlib.In_channel.with_open_bin log_path Stdlib.In_channel.input_all
      in
      Alcotest.(check bool)
        "instructions stay out of event log" false
        (String.is_substring contents ~substring:"Project instructions loaded"))

let test_max_steps () =
  with_env (fun config workspace event_log _ ->
      let config = { config with Config.max_steps = 3 } in
      let client =
        Model_client.create_mock ~send:(fun _ ->
            Lwt.return
              (Ok (response (Model_action.Tool_call (Tool_call.list_files ".")))))
      in
      let outcome = run config workspace event_log client "loop forever" in
      Alcotest.(check string)
        "status max_steps" "max_steps_reached"
        (Agent_loop.status_to_string outcome.status))

let test_max_steps_requests_final_answer_without_tools () =
  with_env (fun config workspace event_log _ ->
      let config = { config with Config.max_steps = 1 } in
      let saw_finalization = ref false in
      let saw_review_nudge = ref false in
      let client =
        Model_client.create_mock_with_options ~send:(fun ~tools_enabled turns ->
            if tools_enabled then
              Lwt.return
                (Ok
                   (response
                      (Model_action.Tool_call (Tool_call.list_files "."))))
            else (
              saw_finalization := true;
              saw_review_nudge :=
                List.exists turns ~f:(fun (turn : Llm.turn) ->
                    List.exists turn.content ~f:(function
                      | Llm.Text text ->
                          String.is_substring text
                            ~substring:"Review budget exhausted"
                      | _ -> false));
              Lwt.return (Ok ([ Llm.Text "review summary" ], Llm.zero_usage))))
      in
      let outcome = run config workspace event_log client "review" in
      Alcotest.(check string)
        "status max_steps" "max_steps_reached"
        (Agent_loop.status_to_string outcome.status);
      Alcotest.(check string) "summary" "review summary" outcome.summary;
      Alcotest.(check bool)
        "tools disabled for finalization" true !saw_finalization;
      Alcotest.(check bool) "uses review nudge" true !saw_review_nudge)

let test_review_task_injects_preflight () =
  with_env (fun config workspace event_log _ ->
      let saw_preflight = ref false in
      let client =
        Model_client.create_mock ~send:(fun turns ->
            saw_preflight :=
              List.exists turns ~f:(fun (turn : Llm.turn) ->
                  List.exists turn.content ~f:(function
                    | Llm.Text text ->
                        String.is_substring text
                          ~substring:"[Code review preflight]"
                    | _ -> false));
            Lwt.return
              (Ok
                 ( [ Llm.Text "Findings\nNo findings.\n\nTests run: not run." ],
                   Llm.zero_usage )))
      in
      let outcome =
        run config workspace event_log client "code review for this project"
      in
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      Alcotest.(check bool) "preflight sent to model" true !saw_preflight)

let test_review_rejects_onboarding_answer () =
  with_env (fun config workspace event_log _ ->
      let calls = ref 0 in
      let client =
        Model_client.create_mock ~send:(fun _turns ->
            Int.incr calls;
            if !calls = 1 then
              Lwt.return
                (Ok
                   ( [
                       Llm.Text
                         "## Codebase Onboarding Document\n\n\
                          ### 1. High-Level Architecture\n\
                          ...";
                     ],
                     Llm.zero_usage ))
            else
              Lwt.return
                (Ok
                   ( [
                       Llm.Text
                         "Findings\n\
                          No findings.\n\n\
                          Tests run: not run.\n\n\
                          Residual risks: none.";
                     ],
                     Llm.zero_usage )))
      in
      let outcome =
        run config workspace event_log client "code review for this project"
      in
      Alcotest.(check int) "bad answer retried" 2 !calls;
      Alcotest.(check string)
        "status completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      Alcotest.(check bool)
        "final summary is review" true
        (String.is_substring outcome.summary ~substring:"Findings"))

let test_model_error () =
  with_env (fun config workspace event_log _ ->
      let client =
        Model_client.create_mock ~send:(fun _ -> Lwt.return (Error "boom"))
      in
      let outcome = run config workspace event_log client "do something" in
      Alcotest.(check string)
        "status failed" "failed"
        (Agent_loop.status_to_string outcome.status))

let () =
  Alcotest.run "agent_loop"
    [
      ( "loop",
        [
          Alcotest.test_case "tool_then_final" `Quick test_tool_then_final;
          Alcotest.test_case "parallel_tool_batch" `Quick
            test_parallel_tool_batch_then_final;
          Alcotest.test_case "update_plan_tool" `Quick
            test_update_plan_tool_emits_plan_event;
          Alcotest.test_case "workspace_snapshot" `Quick
            test_workspace_snapshot_emitted_after_task;
          Alcotest.test_case "history_truncation_atomic" `Quick
            test_history_truncation_keeps_tool_exchange_atomic;
          Alcotest.test_case "auto_compaction" `Quick
            test_auto_compaction_preserves_summary;
          Alcotest.test_case "manual_compaction_event" `Quick
            test_manual_compaction_event_preserves_recent_turns;
          Alcotest.test_case "code_review_guidance" `Quick
            test_code_review_task_adds_system_guidance_without_rewriting_user_event;
          Alcotest.test_case "regular_task_system" `Quick
            test_regular_task_uses_base_system_prompt;
          Alcotest.test_case "project_instructions" `Quick
            test_project_instructions_are_added_to_system_prompt;
          Alcotest.test_case "max_steps" `Quick test_max_steps;
          Alcotest.test_case "max_steps_finalizes" `Quick
            test_max_steps_requests_final_answer_without_tools;
          Alcotest.test_case "review_preflight" `Quick
            test_review_task_injects_preflight;
          Alcotest.test_case "review_rejects_onboarding" `Quick
            test_review_rejects_onboarding_answer;
          Alcotest.test_case "model_error" `Quick test_model_error;
          Alcotest.test_case "approval_denied" `Quick test_approval_denied;
          Alcotest.test_case "approval_granted" `Quick test_approval_granted;
        ] );
    ]
