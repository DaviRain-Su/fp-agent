open! Base
open Fp_agent

let with_workspace f =
  let root = Stdlib.Filename.temp_dir "fp_agent_run" "" in
  Unix.mkdir (Stdlib.Filename.concat root ".git") 0o755;
  let ws =
    match Workspace.create ~root with
    | Ok ws -> ws
    | Error msg -> Alcotest.failf "workspace create failed: %s" msg
  in
  Exn.protect
    ~f:(fun () -> f ws root)
    ~finally:(fun () ->
      ignore
        (Shell.run ~command:(Printf.sprintf "rm -rf %s" root) ~timeout_sec:10
          : (Shell.result, string) Result.t))

let run ws tc = Tool_runner.run ~workspace:ws ~tool_call:tc ()

let output = function
  | Tool_result.Success { output } -> output
  | Tool_result.Error { message } -> "ERROR:" ^ message

let is_success = function Tool_result.Success _ -> true | _ -> false
let is_error = function Tool_result.Error _ -> true | _ -> false

let test_write_then_read () =
  with_workspace (fun ws _ ->
      let w =
        run ws
          (Tool_call.Write_file { path = "a/b.txt"; content = "hello world" })
      in
      Alcotest.(check bool) "write ok" true (is_success w);
      let r = run ws (Tool_call.Read_file { path = "a/b.txt" }) in
      Alcotest.(check string) "read back" "hello world" (output r))

let test_edit () =
  with_workspace (fun ws _ ->
      ignore
        (run ws
           (Tool_call.Write_file { path = "f.txt"; content = "let x = 42" }));
      let e =
        run ws
          (Tool_call.Edit_file
             { path = "f.txt"; old_text = "42"; new_text = "43" })
      in
      Alcotest.(check bool) "edit ok" true (is_success e);
      let r = run ws (Tool_call.Read_file { path = "f.txt" }) in
      Alcotest.(check string) "edited content" "let x = 43" (output r))

let test_edit_no_match () =
  with_workspace (fun ws _ ->
      ignore (run ws (Tool_call.Write_file { path = "f.txt"; content = "abc" }));
      let e =
        run ws
          (Tool_call.Edit_file
             { path = "f.txt"; old_text = "zzz"; new_text = "q" })
      in
      Alcotest.(check bool) "no-match is error" true (is_error e))

let test_list_files () =
  with_workspace (fun ws _ ->
      ignore (run ws (Tool_call.Write_file { path = "one.txt"; content = "1" }));
      ignore (run ws (Tool_call.Write_file { path = "two.txt"; content = "2" }));
      let l = run ws (Tool_call.List_files { path = "." }) in
      Alcotest.(check bool)
        "lists files" true
        (String.is_substring (output l) ~substring:"one.txt"
        && String.is_substring (output l) ~substring:"two.txt"))

let test_search () =
  with_workspace (fun ws _ ->
      ignore
        (run ws
           (Tool_call.Write_file
              { path = "a/foo.ml"; content = "let needle = 1\nlet x = 2" }));
      ignore
        (run ws
           (Tool_call.Write_file { path = "b/bar.ml"; content = "let y = 3" }));
      let r = run ws (Tool_call.Search { query = "needle"; path = None }) in
      Alcotest.(check bool)
        "finds match with location" true
        (String.is_substring (output r) ~substring:"a/foo.ml"
        && String.is_substring (output r) ~substring:"needle");
      let none =
        run ws (Tool_call.Search { query = "zzzznope"; path = None })
      in
      Alcotest.(check bool)
        "reports no matches" true
        (String.is_substring (output none) ~substring:"no matches"))

let test_make_dir () =
  with_workspace (fun ws root ->
      let r = run ws (Tool_call.Make_dir { path = "x/y/z" }) in
      Alcotest.(check bool) "make_dir ok" true (is_success r);
      Alcotest.(check bool)
        "dir exists" true
        (Stdlib.Sys.is_directory (Stdlib.Filename.concat root "x/y/z"));
      let denied = run ws (Tool_call.Make_dir { path = "../escape" }) in
      Alcotest.(check bool) "escape denied" true (is_error denied))

let test_apply_patch () =
  with_workspace (fun ws _ ->
      ignore
        (run ws (Tool_call.Write_file { path = "p.txt"; content = "alpha\n" }));
      let patch = "--- a/p.txt\n+++ b/p.txt\n@@ -1 +1 @@\n-alpha\n+beta\n" in
      let r = run ws (Tool_call.Apply_patch { patch }) in
      Alcotest.(check bool) "patch applied" true (is_success r);
      let read = run ws (Tool_call.Read_file { path = "p.txt" }) in
      Alcotest.(check bool)
        "content patched" true
        (String.is_substring (output read) ~substring:"beta"
        && not (String.is_substring (output read) ~substring:"alpha")))

let test_multi_edit () =
  with_workspace (fun ws _ ->
      ignore (run ws (Tool_call.Write_file { path = "a.txt"; content = "one" }));
      ignore (run ws (Tool_call.Write_file { path = "b.txt"; content = "two" }));
      let r =
        run ws
          (Tool_call.Multi_edit
             {
               edits =
                 [
                   { path = "a.txt"; old_text = "one"; new_text = "1" };
                   { path = "b.txt"; old_text = "two"; new_text = "2" };
                 ];
             })
      in
      Alcotest.(check bool) "multi_edit ok" true (is_success r);
      Alcotest.(check string)
        "a edited" "1"
        (output (run ws (Tool_call.Read_file { path = "a.txt" })));
      Alcotest.(check string)
        "b edited" "2"
        (output (run ws (Tool_call.Read_file { path = "b.txt" }))))

let test_multi_edit_atomic () =
  with_workspace (fun ws _ ->
      ignore (run ws (Tool_call.Write_file { path = "a.txt"; content = "one" }));
      let r =
        run ws
          (Tool_call.Multi_edit
             {
               edits =
                 [
                   { path = "a.txt"; old_text = "one"; new_text = "1" };
                   { path = "a.txt"; old_text = "ZZZ"; new_text = "x" };
                 ];
             })
      in
      Alcotest.(check bool) "fails on bad edit" true (is_error r);
      (* first edit must not have been written (atomic) *)
      Alcotest.(check string)
        "unchanged on abort" "one"
        (output (run ws (Tool_call.Read_file { path = "a.txt" }))))

let test_run_command () =
  with_workspace (fun ws _ ->
      let r =
        run ws (Tool_call.Run_command { command = "echo hi"; cwd = None })
      in
      Alcotest.(check bool)
        "command output" true
        (String.is_substring (output r) ~substring:"hi"
        && String.is_substring (output r) ~substring:"exit_code=0"))

let test_policy_denied () =
  with_workspace (fun ws _ ->
      let d =
        run ws (Tool_call.Write_file { path = ".git/x"; content = "y" })
      in
      Alcotest.(check bool) ".git write denied" true (is_error d);
      let c =
        run ws (Tool_call.Run_command { command = "rm -rf /"; cwd = None })
      in
      Alcotest.(check bool) "dangerous denied" true (is_error c))

let test_event_log () =
  with_workspace (fun _ root ->
      let session_dir = Session.create ~base_dir:root in
      let log = Event_log.create ~session_dir in
      Event_log.append log (Event.User_message { content = "do it" });
      Event_log.append log
        (Event.Tool_call (Tool_call.Read_file { path = "f.txt" }));
      Event_log.append log
        (Event.Tool_result (Tool_result.Success { output = "ok" }));
      Event_log.close log;
      let path = Stdlib.Filename.concat session_dir "events.jsonl" in
      let contents =
        Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all
      in
      let lines =
        String.split_lines contents
        |> List.filter ~f:(fun l -> not (String.is_empty l))
      in
      Alcotest.(check int) "three events logged" 3 (List.length lines);
      List.iter lines ~f:(fun line ->
          match Yojson.Safe.from_string line with
          | `Assoc fields ->
              Alcotest.(check bool)
                "has schema_version" true
                (List.Assoc.mem fields "schema_version" ~equal:String.equal);
              Alcotest.(check bool)
                "has event" true
                (List.Assoc.mem fields "event" ~equal:String.equal)
          | _ -> Alcotest.fail "line is not a JSON object");
      Alcotest.(check bool)
        "no api key leaked" false
        (String.is_substring (String.lowercase contents) ~substring:"api_key"))

let () =
  Alcotest.run "tool_runner"
    [
      ( "tools",
        [
          Alcotest.test_case "write_then_read" `Quick test_write_then_read;
          Alcotest.test_case "edit" `Quick test_edit;
          Alcotest.test_case "edit_no_match" `Quick test_edit_no_match;
          Alcotest.test_case "list_files" `Quick test_list_files;
          Alcotest.test_case "search" `Quick test_search;
          Alcotest.test_case "make_dir" `Quick test_make_dir;
          Alcotest.test_case "apply_patch" `Quick test_apply_patch;
          Alcotest.test_case "multi_edit" `Quick test_multi_edit;
          Alcotest.test_case "multi_edit_atomic" `Quick test_multi_edit_atomic;
          Alcotest.test_case "run_command" `Quick test_run_command;
          Alcotest.test_case "policy_denied" `Quick test_policy_denied;
        ] );
      ("event_log", [ Alcotest.test_case "event_log" `Quick test_event_log ]);
    ]
