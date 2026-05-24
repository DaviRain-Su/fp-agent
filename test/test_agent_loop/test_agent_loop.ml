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

(* A mock that returns a scripted sequence of actions, one per call. *)
let scripted actions =
  let remaining = ref actions in
  Model_client.create_mock ~send:(fun _messages ->
      match !remaining with
      | a :: tl ->
          remaining := tl;
          Lwt.return (Ok a)
      | [] ->
          Lwt.return (Ok (Model_action.Final_answer { answer = "exhausted" })))

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
      Alcotest.(check int) "two tool result events" 2 (count "\"Tool_result\""))

let test_max_steps () =
  with_env (fun config workspace event_log _ ->
      let config = { config with Config.max_steps = 3 } in
      let client =
        Model_client.create_mock ~send:(fun _ ->
            Lwt.return (Ok (Model_action.Tool_call (Tool_call.list_files "."))))
      in
      let outcome = run config workspace event_log client "loop forever" in
      Alcotest.(check string)
        "status max_steps" "max_steps_reached"
        (Agent_loop.status_to_string outcome.status))

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
          Alcotest.test_case "max_steps" `Quick test_max_steps;
          Alcotest.test_case "model_error" `Quick test_model_error;
          Alcotest.test_case "approval_denied" `Quick test_approval_denied;
          Alcotest.test_case "approval_granted" `Quick test_approval_granted;
        ] );
    ]
