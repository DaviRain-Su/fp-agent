(** Read-only command execution for the fullscreen TUI command palette. *)

type context = {
  provider : string;
  model : string;
  api_base : string;
  workspace_root : string;
  sessions_root : string;
  session_dir : string;
  events : Event.t list;
  selected_event_index : int option;
}

val run : context -> string -> string list option
(** Execute a safe command-palette or submitted slash command and return lines
    to append to the TUI timeline. Returns [None] for commands that are
    unsupported in the TUI or require side effects beyond display. *)

val last_user_message : Event.t list -> string option
(** Return the latest non-empty user task from an event log, if any. *)
