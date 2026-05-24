(** Reading a session's event log (the envelopes written by {!Event_log}). *)

val read : session_dir:string -> (Event.t list, string) result
(** Decoded events in order. *)

val read_lines : session_dir:string -> (string list, string) result
(** Raw non-empty lines, in order (used to copy a prefix when forking). *)
