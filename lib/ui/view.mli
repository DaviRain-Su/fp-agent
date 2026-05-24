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

val inspector_lines :
  ?focus_label:string -> status -> last_event:string -> string list
(** Render the right-side run inspector as plain lines. *)

type event_selection =
  | Follow_latest
  | Pinned of int
      (** Which event the TUI inspector should show. [Follow_latest] tracks new
          events; [Pinned i] keeps inspecting a historical event. *)

val selection_index : event_count:int -> event_selection -> int option
(** Resolve a selection to a concrete event index, if any events exist. *)

val selection_label : event_count:int -> event_selection -> string
(** Human-readable selection status for inspector labels and footers. *)

val move_selection :
  event_count:int -> delta:int -> event_selection -> event_selection
(** Move the selected event by [delta], clamped to the valid event range. Moving
    to the newest event resumes [Follow_latest]. *)

val select_event : event_count:int -> index:int -> event_selection
(** Select a concrete event index, clamped to the valid event range. Selecting
    the newest event resumes [Follow_latest]. *)

type command_entry = { command : string; description : string }

val command_palette_entries : command_entry list
(** Built-in command palette entries shown in the TUI. *)

type palette_state =
  | Palette_closed
  | Palette_open of int
      (** TUI command palette visibility plus selected command index. *)

val palette_index : command_count:int -> palette_state -> int option
(** Resolve the selected command index when the palette is open. *)

val palette_label : command_count:int -> palette_state -> string
(** Human-readable command palette status. *)

val toggle_palette : command_count:int -> palette_state -> palette_state
(** Open the palette at the first entry, or close it if already open. *)

val move_palette :
  command_count:int -> delta:int -> palette_state -> palette_state
(** Move the selected command by [delta], clamped to the command range. *)

val command_palette_lines : selected:int -> command_entry list -> string list
(** Render the command palette as plain lines. *)

type prompt_editor = { text : string; cursor : int }
(** Multiline prompt draft plus byte cursor position. Kept pure so a fullscreen
    TUI can test input editing without a real terminal. *)

val prompt_empty : prompt_editor
(** Empty prompt editor. *)

val prompt_make : ?cursor:int -> string -> prompt_editor
(** Build an editor with cursor clamped inside the text. Defaults to end. *)

val prompt_insert_text : string -> prompt_editor -> prompt_editor
(** Insert text at the cursor and move the cursor after the inserted text. *)

val prompt_newline : prompt_editor -> prompt_editor
(** Insert a newline at the cursor. *)

val prompt_backspace : prompt_editor -> prompt_editor
(** Delete the byte before the cursor, if any. *)

val prompt_delete : prompt_editor -> prompt_editor
(** Delete the byte at the cursor, if any. *)

val prompt_move : delta:int -> prompt_editor -> prompt_editor
(** Move the cursor by [delta] bytes, clamped to the draft range. *)

val prompt_home : prompt_editor -> prompt_editor
(** Move the cursor to the beginning of the draft. *)

val prompt_end : prompt_editor -> prompt_editor
(** Move the cursor to the end of the draft. *)

val prompt_is_empty : prompt_editor -> bool
(** True when the draft contains only whitespace. *)

val prompt_editor_lines : prompt_editor -> string list
(** Render a compact multiline prompt editor with a visible cursor. *)

val event_kind : Event.t -> string
(** Stable event type label for the inspector. *)

val event_summary : Event.t -> string
(** One-line event summary for timelines and inspector headers. *)

val event_inspector_lines : Event.t -> string list
(** Render event details, including important tool/policy fields and a JSON
    preview, as plain inspector lines. *)

val plugin_inspector_lines : Plugin.manifest -> string list
(** Render plugin manifest and tool details as plain inspector lines. *)

val tool_inspector_lines : Tool.t -> string list
(** Render registered tool details and its input schema as plain inspector
    lines. *)

val classify : string -> [ `Ok | `Err | `Action | `Plain ]
(** Classify a display line by its leading icon so the renderer can color it. *)
