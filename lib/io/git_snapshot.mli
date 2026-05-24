type t

val create : root:string -> t
(** Create a per-process undo stack for [root]. Non-git roots are supported but
    cannot create restore snapshots. *)

val checkpoint : t -> unit
(** Capture the current git worktree state, excluding fp-agent session logs, so
    a later [undo] can restore it. No-op outside git repositories. *)

val undo : t -> string list
(** Restore the most recent checkpoint and return human-readable status lines.
*)
