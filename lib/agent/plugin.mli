type plugin_tool = {
  tool_name : string;
  tool_kind : Tool.kind;
  tool_description : string;
  tool_command : string;
  tool_input_schema : Yojson.Safe.t option;
  tool_timeout_sec : int;
}

type manifest = {
  id : string;
  name : string;
  version : string;
  dir : string;
  tools : plugin_tool list;
}

val manifest_file : string

val load_manifest : string -> (manifest, string) result
(** Load and validate [fp-agent-plugin.json] from a plugin directory. *)

val manifests : unit -> manifest list
(** Discover plugin manifests from [FP_AGENT_PLUGIN_PATH], [.fp-agent/plugins],
    and the install home. *)

val register_all : unit -> unit
(** Register all discovered plugin tools. Built-in tools keep precedence when a
    plugin declares the same tool name. *)

val install : string -> (string, string) result
(** Install a plugin directory into the user plugin home and return the
    installed path. *)

val check : string -> (manifest, string) result
(** Validate a plugin directory and return its parsed manifest. *)

val scaffold : ?id:string -> string -> (string, string) result
(** Create a starter plugin directory and return its path. *)

val run_tool :
  dir:string ->
  tool_name:string ->
  workspace:Workspace.t ->
  args:Yojson.Safe.t ->
  (Tool_result.t, string) result
(** Load [dir], run [tool_name] with [args], and return the tool result. This is
    the local developer-facing runner used by the CLI debug command. *)

val install_home : unit -> string option
(** Directory used by [install]. Controlled by [FP_AGENT_PLUGIN_HOME], falling
    back to [~/.local/share/fp-agent/plugins]. *)
