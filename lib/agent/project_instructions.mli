val load : Workspace.t -> string option
(** Load workspace-local instruction files for the model system prompt. Supports
    AGENTS.md, CLAUDE.md, .fp-agent/instructions.md, and whole-line relative
    include references inside those files. Includes are constrained to the
    workspace. *)
