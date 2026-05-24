(** Pure state machine for the fullscreen interactive shell. This module keeps
    terminal input behavior testable outside Notty. *)

type t = {
  draft : View.prompt_editor;
  selection : View.event_selection;
  palette : View.palette_state;
  event_count : int;
  command_count : int;
}

type action =
  | Insert_text of string
  | Newline
  | Backspace
  | Delete
  | Move_cursor of int
  | Prompt_home
  | Prompt_end
  | Submit_prompt
  | Toggle_palette
  | Close_palette
  | Move_palette of int
  | Palette_home
  | Palette_end
  | Move_event of int
  | Event_home
  | Event_end

type result = { state : t; submitted : string option }

val create : ?command_count:int -> unit -> t
(** Initial shell state. [command_count] defaults to the built-in command
    palette size. *)

val set_event_count : int -> t -> t
(** Update event count and clamp any pinned event selection into range. *)

val selected_event_index : t -> int option
(** Currently inspected event index, if events exist. *)

val selected_command_index : t -> int option
(** Currently highlighted palette command, if the palette is open. *)

val palette_open : t -> bool
(** True when the command palette is open. *)

val selection_label : t -> string
(** Human-readable event selection label. *)

val palette_label : t -> string
(** Human-readable palette selection label. *)

val handle : t -> action -> result
(** Apply one input action. [Submit_prompt] returns the submitted prompt and
    clears the draft when the draft is not empty. *)
