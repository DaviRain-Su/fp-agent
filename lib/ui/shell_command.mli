type id =
  | Help
  | Tools
  | Tool
  | Plugins
  | Plugin
  | Sessions
  | Tree
  | Resume
  | Model
  | Models
  | Provider
  | Log
  | Inspect
  | Fork
  | Diff
  | Undo
  | Exit

type entry = { command : string; description : string }

type parse_result =
  | Empty
  | Task of string
  | Command of id * string
  | Unknown of string

val palette_entries : entry list
(** Commands shown in the TUI command palette. *)

val help_text : unit -> string
(** Render REPL help from the same command metadata. *)

val parse : string -> parse_result
(** Parse a REPL/TUI command line. Non-command input is returned as [Task]. *)
