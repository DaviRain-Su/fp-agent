type provider_compat = {
  supports_developer_role : bool;
  supports_reasoning_effort : bool;
  supports_usage_in_streaming : bool;
  max_tokens_field : string option;
}

type t = {
  provider : string;
  api_key : string;
  api_base : string;
  model : string;
  models : string list;
  protocol : Provider.protocol;
  compat : provider_compat;
  max_tokens : int option;
  max_steps : int;
  workspace_root : string;
}

type provider_catalog_entry = {
  provider_name : string;
  provider_api_base : string;
  provider_models : string list;
  provider_protocol : Provider.protocol;
}

val load :
  ?provider:string ->
  ?api_base:string ->
  ?model:string ->
  unit ->
  (t, string) result
(** Load configuration for the selected provider (default {!Provider.default},
    or the [PROVIDER] env var, or the [?provider] override). Built-in providers
    read their API key env var and fail fast if it is missing, except for
    providers that do not require a key, such as [local]. Unknown provider names
    are resolved from [FP_AGENT_CONFIG] or the default provider config files.
    [api_base] and [model] fall back to the [API_BASE] / [MODEL_NAME] env vars
    and then the provider defaults or configured model list. [MAX_STEPS] and
    [WORKSPACE_ROOT] are read from the environment. *)

val available_providers : unit -> provider_catalog_entry list
(** Return the built-in provider/model catalog plus custom providers found in
    [FP_AGENT_CONFIG] or the default provider config files. API keys are not
    required for catalog listing. *)

val provider_config_path : ?path:string -> unit -> string
(** Return the custom provider config path that write operations should use.
    Explicit [path] wins, then [FP_AGENT_CONFIG], then
    [.fp-agent/providers.json]. *)

val write_custom_provider :
  ?path:string ->
  ?api:string ->
  ?api_key:string ->
  ?compat:provider_compat ->
  ?max_tokens:int ->
  ?replace:bool ->
  name:string ->
  api_base:string ->
  models:string list ->
  unit ->
  (string, string) result
(** Add or replace a custom provider profile in the selected provider config
    file and return the file path written. *)

val default_compat : provider_compat
(** Conservative OpenAI-compatible defaults used by tests and providers without
    explicit compatibility metadata. *)
