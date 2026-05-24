open! Base
open Fp_agent

let rm_rf path =
  ignore
    (Shell.run
       ~command:(Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote path))
       ~timeout_sec:10
      : (Shell.result, string) Result.t)

let write path content =
  let dir = Stdlib.Filename.dirname path in
  if not (Stdlib.Sys.file_exists dir) then Unix.mkdir dir 0o755;
  Stdlib.Out_channel.with_open_bin path (fun oc ->
      Stdlib.Out_channel.output_string oc content)

let read path = Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all

let run_git root args =
  match
    Shell.run
      ~command:(Printf.sprintf "git -C %s %s" (Stdlib.Filename.quote root) args)
      ~timeout_sec:30
  with
  | Ok { exit_code = 0; _ } -> ()
  | Ok { exit_code; stderr; stdout } ->
      Alcotest.failf "git %s failed (%d): %s%s" args exit_code stderr stdout
  | Error e -> Alcotest.failf "git %s failed: %s" args e

let with_git_repo f =
  let root = Stdlib.Filename.temp_dir "fp_agent_git_snapshot" "" in
  Exn.protect
    ~finally:(fun () -> rm_rf root)
    ~f:(fun () ->
      run_git root "init";
      run_git root "config user.email test@example.invalid";
      run_git root "config user.name fp-agent-test";
      write (Stdlib.Filename.concat root "tracked.txt") "base\n";
      run_git root "add tracked.txt";
      run_git root "commit -m initial";
      f root)

let test_checkpoint_and_undo_restore_last_turn () =
  with_git_repo (fun root ->
      let tracked = Stdlib.Filename.concat root "tracked.txt" in
      let new_file = Stdlib.Filename.concat root "new.txt" in
      let agent_dir = Stdlib.Filename.concat root ".ocaml-agent" in
      Unix.mkdir agent_dir 0o755;
      let agent_log = Stdlib.Filename.concat agent_dir "events.jsonl" in
      write tracked "before task\n";
      write agent_log "before\n";
      let snapshots = Git_snapshot.create ~root in
      Git_snapshot.checkpoint snapshots;
      write tracked "after task\n";
      write new_file "created by task\n";
      write agent_log "after\n";
      let result = Git_snapshot.undo snapshots in
      Alcotest.(check (list string))
        "undo status"
        [ "reverted the last turn's changes" ]
        result;
      Alcotest.(check string) "tracked restored" "before task\n" (read tracked);
      Alcotest.(check bool)
        "new file removed" false
        (Stdlib.Sys.file_exists new_file);
      Alcotest.(check string) "agent log preserved" "after\n" (read agent_log);
      Alcotest.(check (list string))
        "second undo empty" [ "nothing to undo" ]
        (Git_snapshot.undo snapshots))

let test_non_git_undo_is_empty () =
  let root = Stdlib.Filename.temp_dir "fp_agent_git_snapshot_nongit" "" in
  Exn.protect
    ~finally:(fun () -> rm_rf root)
    ~f:(fun () ->
      let snapshots = Git_snapshot.create ~root in
      Git_snapshot.checkpoint snapshots;
      Alcotest.(check (list string))
        "nothing captured" [ "nothing to undo" ]
        (Git_snapshot.undo snapshots))

let () =
  Alcotest.run "git_snapshot"
    [
      ( "undo",
        [
          Alcotest.test_case "checkpoint_and_undo_restore_last_turn" `Quick
            test_checkpoint_and_undo_restore_last_turn;
          Alcotest.test_case "non_git_undo_is_empty" `Quick
            test_non_git_undo_is_empty;
        ] );
    ]
