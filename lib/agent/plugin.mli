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
  sdk_version : int;
  dir : string;
  tools : plugin_tool list;
}

type load_error = { dir : string; message : string }
type discovery = { manifests : manifest list; errors : load_error list }

type tool_conflict = {
  dir : string;
  plugin_id : string;
  tool_name : string;
  existing_owner : string;
}

type smoke_result = { tool_name : string; args_file : string; output : string }

val manifest_file : string
val supported_sdk_version : int

val load_manifest : string -> (manifest, string) result
(** Load and validate [fp-agent-plugin.json] from a plugin directory. *)

val discover : unit -> discovery
(** Discover plugin manifests and return both valid manifests and invalid
    manifest diagnostics. *)

val manifests : unit -> manifest list
(** Discover plugin manifests from [FP_AGENT_PLUGIN_PATH], [.fp-agent/plugins],
    and the install home. *)

val tool_conflicts : unit -> tool_conflict list
(** Return plugin tools that cannot be registered because their name is already
    owned by a built-in tool or by an earlier discovered plugin. *)

val installed_tool_conflicts : unit -> tool_conflict list
(** Return registration conflicts among installed plugin manifests. *)

val register_all : unit -> unit
(** Register all discovered plugin tools. Built-in tools keep precedence when a
    plugin declares the same tool name. *)

val install : ?replace:bool -> string -> (string, string) result
(** Install a plugin directory into the user plugin home and return the
    installed path. When [replace] is [true], an existing installed plugin with
    the same id is replaced after the new plugin has been validated and staged.
*)

val installed_manifests : unit -> manifest list
(** Load valid manifests installed directly under the plugin home. *)

val installed_discovery : unit -> discovery
(** Load installed plugin manifests and diagnostics from the plugin home. *)

val remove : string -> (string, string) result
(** Remove an installed plugin by id from the plugin home and return the removed
    directory path. *)

val check : ?replace:bool -> string -> (manifest, string) result
(** Validate a plugin directory and return its parsed manifest. This also
    rejects tool names that would be shadowed by built-in tools or existing
    discovered plugins. When [replace] is [true], an existing installed plugin
    with the same id is ignored for conflict checks. *)

val scaffold :
  ?id:string -> ?tool_name:string -> string -> (string, string) result
(** Create a starter plugin directory and return its path. *)

val run_tool :
  dir:string ->
  tool_name:string ->
  workspace:Workspace.t ->
  args:Yojson.Safe.t ->
  (Tool_result.t, string) result
(** Load [dir], run [tool_name] with [args], and return the tool result. This is
    the local developer-facing runner used by the CLI debug command. *)

val smoke :
  ?replace:bool ->
  workspace:Workspace.t ->
  string ->
  (smoke_result list, string) result
(** Validate a plugin directory and run each tool with
    [examples/<tool>.args.json]. *)

val install_home : unit -> string option
(** Directory used by [install]. Controlled by [FP_AGENT_PLUGIN_HOME], falling
    back to [~/.local/share/fp-agent/plugins]. *)
