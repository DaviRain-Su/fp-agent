type t = { role : string; content : string }
(** A chat message exchanged with the model. *)

val system : string -> t
val user : string -> t
val assistant : string -> t
