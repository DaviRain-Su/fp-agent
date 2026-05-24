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
  | Accept_palette
  | Move_palette of int
  | Palette_home
  | Palette_end
  | Move_event of int
  | Event_home
  | Event_end

type input =
  | Text of string
  | Enter
  | Ctrl_enter
  | Shift_enter
  | Backspace_key
  | Delete_key
  | Left
  | Right
  | Up
  | Down
  | Page_up
  | Page_down
  | Home
  | End
  | Escape
  | Slash
  | Question
  | Mouse_scroll_up
  | Mouse_scroll_down
  | Unknown

type result = {
  state : t;
  submitted : string option;
  accepted_command : View.command_entry option;
}

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
    clears the draft when the draft is not empty. [Accept_palette] closes the
    palette and returns the highlighted command when one is selected. *)

val action_of_input : page_size:int -> t -> input -> action option
(** Translate abstract terminal input into a shell action. Palette navigation
    has priority while the palette is open; otherwise text edits the prompt and
    navigation keys inspect events, with Home/End targeting the draft when one
    is active. *)

val handle_input : page_size:int -> t -> input -> result
(** Translate and apply one abstract terminal input. Unknown or context-invalid
    input leaves the state unchanged. *)
