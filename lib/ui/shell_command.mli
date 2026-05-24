type id =
  | Help
  | Tools
  | Tool
  | Plugins
  | Plugin
  | PluginNew
  | PluginDev
  | PluginCheck
  | PluginInstall
  | PluginPackage
  | PluginRemove
  | PluginSmoke
  | PluginRun
  | PluginDoctor
  | PluginSdk
  | Sessions
  | Tree
  | NewSession
  | Resume
  | Model
  | ModelNext
  | Models
  | Providers
  | Provider
  | ProviderAdd
  | Log
  | Inspect
  | Plan
  | PlanSet
  | PlanAdd
  | PlanUpdate
  | PlanClear
  | Usage
  | Status
  | Context
  | Handoff
  | Instructions
  | Compact
  | Fork
  | Diff
  | Review
  | Retry
  | Undo
  | Exit

type entry = { command : string; description : string; group : string }
type acceptance = Execute of string | Draft of string

type parse_result =
  | Empty
  | Task of string
  | Command of id * string
  | Unknown of string

val palette_entries : entry list
(** Commands shown in the TUI command palette. *)

val help_text : unit -> string
(** Render REPL help from the same command metadata. *)

val accept : entry -> acceptance
(** Resolve what accepting a palette entry should do. Safe no-arg commands can
    execute directly; commands needing user input return a draft prefix. *)

val parse : string -> parse_result
(** Parse a REPL/TUI command line. Non-command input is returned as [Task]. *)
