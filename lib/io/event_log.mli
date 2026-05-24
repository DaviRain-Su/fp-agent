(** Append-only JSONL event log. Each line is a versioned envelope wrapping an
    {!Event.t}. API keys are never part of events, so they never reach disk. *)

type t

val create : session_dir:string -> t

val append : t -> Event.t -> unit
(** Append one event as a single JSON line and flush. *)

val close : t -> unit
