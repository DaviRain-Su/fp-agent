(** Agent state defined as a fold over the event log (event sourcing): the log
    is the source of truth and [replay] reconstructs the state from it. *)

type t = { turns : Llm.turn list; agent_state : Agent_state.t; steps : int }

val empty : t

val reduce : t -> Event.t -> t
(** Apply one event to the state. *)

val replay : Event.t list -> t
(** Reconstruct the state by reducing all events in order. *)

val messages : t -> Message.t list
(** Lossy text view of [turns], kept for transcript compatibility. *)

val turns : t -> Llm.turn list
val agent_state : t -> Agent_state.t
val steps : t -> int
