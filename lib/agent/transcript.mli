val of_session : session_dir:string -> (Llm.turn list, string) result
(** Reconstruct the model-visible conversation history from a session's
    [events.jsonl], for resuming a previous run. *)

val messages_of_session : session_dir:string -> (Message.t list, string) result
(** Lossy text view of {!of_session}. *)
