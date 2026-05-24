(** Pure helpers behind the TUI rendering (no terminal dependency). *)

val window : rows:int -> string list -> string list
(** [window ~rows lines] returns the most recent [rows] lines, or all of them if
    there are fewer; [[]] when [rows <= 0]. *)

val display_lines : string -> string list
(** Split display text into terminal lines. Empty text produces no lines. *)

val classify : string -> [ `Ok | `Err | `Action | `Plain ]
(** Classify a display line by its leading icon so the renderer can color it. *)
