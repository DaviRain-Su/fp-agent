open! Base
open Fp_agent

(* End-to-end: a scripted model edits an existing README, then finishes. We
   assert the file changed, the outcome is success, and the event log records
   the full ordered trace. *)

let read_file path =
  Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all

let test_edit_readme_e2e () =
  let root = Stdlib.Filename.temp_dir "fp_agent_e2e" "" in
  Exn.protect
    ~finally:(fun () ->
      ignore
        (Shell.run ~command:(Printf.sprintf "rm -rf %s" root) ~timeout_sec:10
          : (Shell.result, string) Result.t))
    ~f:(fun () ->
      let readme = Stdlib.Filename.concat root "README.md" in
      Stdlib.Out_channel.with_open_bin readme (fun oc ->
          Stdlib.Out_channel.output_string oc "# Project\n\nTODO: write docs\n");
      let config =
        {
          Config.api_key = "k";
          api_base = "http://localhost";
          model = "m";
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
      let actions =
        ref
          [
            Model_action.Tool_call
              (Tool_call.Edit_file
                 {
                   path = "README.md";
                   old_text = "TODO: write docs";
                   new_text = "This project is documented.";
                 });
            Model_action.Final_answer { answer = "Updated the README." };
          ]
      in
      let client =
        Model_client.create_mock ~send:(fun _ ->
            match !actions with
            | a :: tl ->
                actions := tl;
                Lwt.return (Ok a)
            | [] -> Alcotest.fail "model called more times than scripted")
      in
      let outcome =
        Lwt_main.run
          (Agent_loop.run ~config ~model_client:client ~event_log ~workspace
             ~task:"document the project")
      in
      Event_log.close event_log;
      Alcotest.(check string)
        "completed" "completed"
        (Agent_loop.status_to_string outcome.status);
      let contents = read_file readme in
      Alcotest.(check bool)
        "readme updated" true
        (String.is_substring contents ~substring:"This project is documented."
        && not (String.is_substring contents ~substring:"TODO"));
      let log = read_file (Stdlib.Filename.concat session_dir "events.jsonl") in
      let lines =
        String.split_lines log
        |> List.filter ~f:(fun l -> not (String.is_empty l))
      in
      (* user_message, transitions, model_response, tool_call, tool_result, ... *)
      Alcotest.(check bool) "log non-trivial" true (List.length lines >= 6);
      let idx substring =
        List.findi lines ~f:(fun _ l -> String.is_substring l ~substring)
        |> Option.map ~f:fst
      in
      match (idx "Tool_call", idx "Tool_result", idx "User_message") with
      | Some tc, Some tr, Some um ->
          Alcotest.(check bool) "user_message first" true (um < tc);
          Alcotest.(check bool) "tool_call before result" true (tc < tr)
      | _ -> Alcotest.fail "expected ordered events in log")

let () =
  Alcotest.run "e2e"
    [
      ("e2e", [ Alcotest.test_case "edit_readme" `Quick test_edit_readme_e2e ]);
    ]
