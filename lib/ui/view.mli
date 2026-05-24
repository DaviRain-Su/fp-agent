(** Pure helpers behind the TUI rendering (no terminal dependency). *)

val window : rows:int -> string list -> string list
(** [window ~rows lines] returns the most recent [rows] lines, or all of them if
    there are fewer; [[]] when [rows <= 0]. *)

val display_lines : string -> string list
(** Split display text into terminal lines. Empty text produces no lines. *)

val wrap_line : cols:int -> string -> string list
(** Wrap one display line to [cols] columns. Empty lines are preserved; no lines
    are returned when [cols <= 0]. *)

val viewport : rows:int -> cols:int -> string list -> string list
(** Wrap lines to [cols] columns, then return the most recent [rows] visible
    rows. *)

val classify : string -> [ `Ok | `Err | `Action | `Plain ]
(** Classify a display line by its leading icon so the renderer can color it. *)
