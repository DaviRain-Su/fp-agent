type t = {
  api_key : string;
  api_base : string;
  model : string;
  protocol : Provider.protocol;
  max_steps : int;
  workspace_root : string;
}

val load :
  ?provider:string ->
  ?api_base:string ->
  ?model:string ->
  unit ->
  (t, string) result
(** Load configuration for the selected provider (default {!Provider.default},
    or the [PROVIDER] env var, or the [?provider] override). Reads the
    provider's API key env var and fails fast if it is missing. [api_base] and
    [model] fall back to the [API_BASE] / [MODEL_NAME] env vars and then the
    provider defaults. [MAX_STEPS] and [WORKSPACE_ROOT] are read from the
    environment. *)
