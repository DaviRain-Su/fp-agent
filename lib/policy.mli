val check : workspace:Workspace.t -> tool_call:Tool_call.t -> Permission.t
(** [check ~workspace ~tool_call] returns the permission decision for a tool
    call: reads/lists and writes are validated against the workspace bounds, and
    shell commands are screened against a dangerous-command deny-list. *)

type t = { approve_commands : bool; approve_writes : bool }
(** Approval policy: which already-safe tool calls additionally require human
    confirmation before running. *)

val default : t
(** No approval required (fully autonomous). *)

val approval_reason : t -> Tool_call.t -> string option
(** [approval_reason t tool_call] is [Some reason] when [tool_call] needs human
    approval under policy [t], otherwise [None]. *)
