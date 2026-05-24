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

val truncate : cols:int -> string -> string
(** Truncate display text to [cols] columns. *)

val pad_right : cols:int -> string -> string
(** Truncate, then right-pad display text to exactly [cols] bytes when possible.
*)

type panes = { timeline_cols : int; inspector_cols : int }

val split_panes : width:int -> panes option
(** Return a two-pane layout for wide terminals. Narrow terminals use the
    single-pane timeline. *)

type status = {
  provider : string;
  model : string;
  session : string;
  phase : string option;
  events : int;
  plugins : int;
  tools : int;
}

val status_line : status -> string
(** Render the compact status strip. *)

val inspector_lines : status -> last_event:string -> string list
(** Render the right-side run inspector as plain lines. *)

val event_kind : Event.t -> string
(** Stable event type label for the inspector. *)

val event_summary : Event.t -> string
(** One-line event summary for timelines and inspector headers. *)

val event_inspector_lines : Event.t -> string list
(** Render event details, including important tool/policy fields and a JSON
    preview, as plain inspector lines. *)

val classify : string -> [ `Ok | `Err | `Action | `Plain ]
(** Classify a display line by its leading icon so the renderer can color it. *)
