val run : workspace:Workspace.t -> tool_call:Tool_call.t -> Tool_result.t
(** [run ~workspace ~tool_call] checks the call against {!Policy} and, if
    allowed, executes it. Policy denials and execution failures are returned as
    {!Tool_result.Error}; a non-zero command exit is still a
    {!Tool_result.Success} carrying the exit code and captured output. *)
