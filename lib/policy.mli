val check : workspace:Workspace.t -> tool_call:Tool_call.t -> Permission.t
(** [check ~workspace ~tool_call] returns the permission decision for a tool
    call: reads/lists and writes are validated against the workspace bounds, and
    shell commands are screened against a dangerous-command deny-list. *)
