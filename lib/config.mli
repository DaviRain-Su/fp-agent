type t = {
  api_key : string;
  api_base : string;
  model : string;
  max_steps : int;
  workspace_root : string;
}

val load : unit -> (t, string) result
(** Load configuration from environment variables. Fails fast if
    [OPENAI_API_KEY] is missing. Other settings fall back to defaults:
    [OPENAI_API_BASE], [MODEL_NAME], [MAX_STEPS], [WORKSPACE_ROOT]. *)
