(** Client for an OpenAI-compatible chat completion endpoint. The client is a
    record of one function so tests and the agent loop can inject a mock. *)

type t

val create : config:Config.t -> t
(** Real HTTP client backed by [cohttp-lwt-unix]. *)

val create_mock :
  send:(Llm.turn list -> (Llm.content list * Llm.usage, string) result Lwt.t) ->
  t
(** Mock client driven by a caller-supplied [send] function. *)

val send :
  ?on_delta:(string -> unit) ->
  system:string ->
  t ->
  turns:Llm.turn list ->
  (Llm.content list * Llm.usage, string) result Lwt.t
(** Send the conversation and parse the next model action. [on_delta] is called
    with streamed assistant text chunks when the provider emits text deltas
    before the final parsed action is available. *)

val system_prompt : string
(** The system prompt describing the tools and the JSON output contract. *)

val parse_action : string -> (Model_action.t, string) result
(** Parse a raw model message (per the JSON contract) into an action. Exposed
    for testing; tolerates accidental markdown code fences. *)

val request_body_for_test :
  config:Config.t -> system:string -> turns:Llm.turn list -> Yojson.Safe.t
(** Build the provider request JSON body without sending it. Exposed for
    request-shape tests. *)

val request_headers_for_test :
  config:Config.t ->
  system:string ->
  turns:Llm.turn list ->
  (string * string) list

val openai_complete_for_test :
  string list -> (Llm.content list * Llm.usage, string) result

val anthropic_complete_for_test :
  string list -> (Llm.content list * Llm.usage, string) result
