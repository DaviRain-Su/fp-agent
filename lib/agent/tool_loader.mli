type counts = { plugins : int; tools : int }

val register_all : unit -> unit
(** Register built-in tools and discovered plugin tools. *)

val refresh_counts : unit -> counts
(** Register built-in tools and discovered plugin tools, then return the current
    valid plugin and registered tool counts. *)
