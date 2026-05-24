open! Base

type result = { stdout : string; stderr : string; exit_code : int }

let read_and_remove file =
  let contents =
    Stdlib.In_channel.with_open_bin file Stdlib.In_channel.input_all
  in
  (try Unix.unlink file with Unix.Unix_error _ -> ());
  contents

(* Run [command] through [/bin/sh -c] with output captured to temp files.
   Polls the child so the call can enforce [timeout_sec] and kill a runaway
   process. *)
let run ~command ~timeout_sec =
  let out_file = Stdlib.Filename.temp_file "fp_agent_out" ".txt" in
  let err_file = Stdlib.Filename.temp_file "fp_agent_err" ".txt" in
  let fd_out = Unix.openfile out_file [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let fd_err = Unix.openfile err_file [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let pid =
    Unix.create_process "/bin/sh"
      [| "/bin/sh"; "-c"; command |]
      Unix.stdin fd_out fd_err
  in
  Unix.close fd_out;
  Unix.close fd_err;
  let deadline = Unix.gettimeofday () +. Float.of_int timeout_sec in
  let rec wait () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Float.( > ) (Unix.gettimeofday ()) deadline then (
          (try Unix.kill pid Stdlib.Sys.sigkill with Unix.Unix_error _ -> ());
          ignore (Unix.waitpid [] pid);
          `Timeout)
        else (
          (try ignore (Unix.select [] [] [] 0.02 : _ * _ * _)
           with Unix.Unix_error (Unix.EINTR, _, _) -> ());
          wait ())
    | _, status -> `Done status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  let outcome = wait () in
  let stdout = read_and_remove out_file in
  let stderr = read_and_remove err_file in
  match outcome with
  | `Timeout ->
      Error
        (Printf.sprintf "command timed out after %ds: %s" timeout_sec command)
  | `Done (Unix.WEXITED code) -> Ok { stdout; stderr; exit_code = code }
  | `Done (Unix.WSIGNALED s) -> Ok { stdout; stderr; exit_code = 128 + s }
  | `Done (Unix.WSTOPPED s) -> Ok { stdout; stderr; exit_code = 128 + s }
