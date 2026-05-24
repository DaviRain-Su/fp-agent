type result = { stdout : string; stderr : string; exit_code : int }
(** Captured result of running a shell command. *)

val run : command:string -> timeout_sec:int -> (result, string) Stdlib.result
(** [run ~command ~timeout_sec] runs [command] via [/bin/sh -c], capturing
    stdout/stderr/exit code. A non-zero exit is returned as [Ok] (the caller
    decides what to do with it). [Error] is reserved for timeouts and harness
    failures. The process is killed if it exceeds [timeout_sec]. *)
