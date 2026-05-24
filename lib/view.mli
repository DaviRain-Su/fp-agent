(** Pure helpers behind the TUI rendering (no terminal dependency). *)

val window : rows:int -> string list -> string list
(** [window ~rows lines] returns the most recent [rows] lines, or all of them if
    there are fewer; [[]] when [rows <= 0]. *)

val classify : string -> [ `Ok | `Err | `Action | `Plain ]
(** Classify a display line by its leading icon so the renderer can color it. *)
