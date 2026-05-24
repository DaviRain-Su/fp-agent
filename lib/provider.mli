(** Supported model providers. All expose an OpenAI-compatible chat completion
    endpoint, so only the key, base URL, and default model differ. *)

type t = Kimi | Zhipu | Deepseek

val all : t list
val to_string : t -> string

val of_string : string -> t option
(** Parse a provider name (case-insensitive, accepts a few aliases). *)

val default : t
(** The provider used when none is specified. *)

val key_env : t -> string
(** Environment variable holding this provider's API key. *)

val default_api_base : t -> string
(** Default OpenAI-compatible base URL (no trailing slash). *)

val default_model : t -> string
(** Default model id for this provider. *)
