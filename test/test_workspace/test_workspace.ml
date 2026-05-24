open! Base
open Fp_agent

let with_temp_workspace f =
  let root = Stdlib.Filename.temp_dir "fp_agent_ws" "" in
  Unix.mkdir (Stdlib.Filename.concat root ".git") 0o755;
  let ws =
    match Workspace.create ~root with
    | Ok ws -> ws
    | Error msg -> Alcotest.failf "workspace create failed: %s" msg
  in
  Exn.protect
    ~f:(fun () -> f ws)
    ~finally:(fun () ->
      ignore
        (Shell.run ~command:(Printf.sprintf "rm -rf %s" root) ~timeout_sec:10
          : (Shell.result, string) Result.t))

let is_ok = function Ok _ -> true | Error _ -> false
let is_error = function Ok _ -> false | Error _ -> true

let test_resolve_within () =
  with_temp_workspace (fun ws ->
      Alcotest.(check bool)
        "relative path resolves" true
        (is_ok (Workspace.resolve_path ws "foo.ml"));
      Alcotest.(check bool)
        "nested . and .. that stay inside resolve" true
        (is_ok (Workspace.resolve_path ws "sub/../foo.ml")))

let test_reject_escape () =
  with_temp_workspace (fun ws ->
      Alcotest.(check bool)
        "../ escape rejected" true
        (is_error (Workspace.resolve_path ws "../etc/passwd"));
      Alcotest.(check bool)
        "deep escape rejected" true
        (is_error (Workspace.resolve_path ws "a/b/../../../etc/passwd")))

let test_reject_git_write () =
  with_temp_workspace (fun ws ->
      Alcotest.(check bool)
        ".git write rejected" true
        (is_error (Workspace.validate_write_path ws ".git/config"));
      Alcotest.(check bool)
        "normal write allowed" true
        (is_ok (Workspace.validate_write_path ws "lib/foo.ml")))

let test_missing_root () =
  Alcotest.(check bool)
    "missing root rejected" true
    (is_error (Workspace.create ~root:"/nonexistent/path/xyz"))

let test_shell_stdout () =
  match Shell.run ~command:"echo hello" ~timeout_sec:10 with
  | Ok r ->
      Alcotest.(check int) "exit 0" 0 r.exit_code;
      Alcotest.(check bool)
        "stdout has hello" true
        (String.is_substring r.stdout ~substring:"hello")
  | Error msg -> Alcotest.failf "unexpected error: %s" msg

let test_shell_exit_and_stderr () =
  match Shell.run ~command:"echo oops >&2; exit 3" ~timeout_sec:10 with
  | Ok r ->
      Alcotest.(check int) "exit 3" 3 r.exit_code;
      Alcotest.(check bool)
        "stderr has oops" true
        (String.is_substring r.stderr ~substring:"oops")
  | Error msg -> Alcotest.failf "unexpected error: %s" msg

let test_shell_timeout () =
  Alcotest.(check bool)
    "long command times out" true
    (is_error (Shell.run ~command:"sleep 5" ~timeout_sec:1))

let test_shell_scrubs_secret_env () =
  Unix.putenv "KIMI_API_KEY" "super-secret";
  Unix.putenv "FP_AGENT_TEST_VISIBLE" "visible";
  match
    Shell.run
      ~command:"printf '%s:%s' \"$KIMI_API_KEY\" \"$FP_AGENT_TEST_VISIBLE\""
      ~timeout_sec:10
  with
  | Ok r ->
      Unix.putenv "KIMI_API_KEY" "";
      Unix.putenv "FP_AGENT_TEST_VISIBLE" "";
      Alcotest.(check string) "secret scrubbed" ":visible" r.stdout
  | Error msg ->
      Unix.putenv "KIMI_API_KEY" "";
      Unix.putenv "FP_AGENT_TEST_VISIBLE" "";
      Alcotest.failf "unexpected error: %s" msg

let () =
  Alcotest.run "workspace_shell"
    [
      ( "workspace",
        [
          Alcotest.test_case "resolve_within" `Quick test_resolve_within;
          Alcotest.test_case "reject_escape" `Quick test_reject_escape;
          Alcotest.test_case "reject_git_write" `Quick test_reject_git_write;
          Alcotest.test_case "missing_root" `Quick test_missing_root;
        ] );
      ( "shell",
        [
          Alcotest.test_case "stdout" `Quick test_shell_stdout;
          Alcotest.test_case "exit_and_stderr" `Quick test_shell_exit_and_stderr;
          Alcotest.test_case "timeout" `Quick test_shell_timeout;
          Alcotest.test_case "scrubs_secret_env" `Quick
            test_shell_scrubs_secret_env;
        ] );
    ]
