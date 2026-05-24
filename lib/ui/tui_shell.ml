open! Base

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
  dispatched_command : string option;
}

let create ?command_count () =
  let command_count =
    Option.value command_count
      ~default:(List.length View.command_palette_entries)
  in
  {
    draft = View.prompt_empty;
    selection = View.Follow_latest;
    palette = View.Palette_closed;
    event_count = 0;
    command_count = Int.max 0 command_count;
  }

let selected_event_index t =
  View.selection_index ~event_count:t.event_count t.selection

let selected_command_index t =
  View.palette_index ~command_count:t.command_count t.palette

let palette_open t = Option.is_some (selected_command_index t)

let normalize_selection t =
  match selected_event_index t with
  | None -> View.Follow_latest
  | Some index -> View.select_event ~event_count:t.event_count ~index

let set_event_count event_count t =
  let t = { t with event_count = Int.max 0 event_count } in
  { t with selection = normalize_selection t }

let selection_label t =
  View.selection_label ~event_count:t.event_count t.selection

let palette_label t =
  View.palette_label ~command_count:t.command_count t.palette

let no_submit state =
  {
    state;
    submitted = None;
    accepted_command = None;
    dispatched_command = None;
  }

let page_delta page_size = Int.max 1 page_size
let draft_has_text t = not (String.is_empty t.draft.text)
let command_at index = List.nth View.command_palette_entries index

let handle_prompt t = function
  | Insert_text text ->
      no_submit { t with draft = View.prompt_insert_text text t.draft }
  | Newline -> no_submit { t with draft = View.prompt_newline t.draft }
  | Backspace -> no_submit { t with draft = View.prompt_backspace t.draft }
  | Delete -> no_submit { t with draft = View.prompt_delete t.draft }
  | Move_cursor delta ->
      no_submit { t with draft = View.prompt_move ~delta t.draft }
  | Prompt_home -> no_submit { t with draft = View.prompt_home t.draft }
  | Prompt_end -> no_submit { t with draft = View.prompt_end t.draft }
  | Submit_prompt ->
      if View.prompt_is_empty t.draft then no_submit t
      else
        {
          state = { t with draft = View.prompt_empty };
          submitted = Some t.draft.text;
          accepted_command = None;
          dispatched_command = None;
        }
  | _ -> no_submit t

let handle t action =
  match action with
  | Toggle_palette ->
      no_submit
        {
          t with
          palette = View.toggle_palette ~command_count:t.command_count t.palette;
        }
  | Close_palette -> no_submit { t with palette = View.Palette_closed }
  | Accept_palette -> (
      match Option.bind (selected_command_index t) ~f:command_at with
      | None -> no_submit { t with palette = View.Palette_closed }
      | Some command -> (
          match Shell_command.accept command with
          | Shell_command.Execute line ->
              {
                state = { t with palette = View.Palette_closed };
                submitted = None;
                accepted_command = Some command;
                dispatched_command = Some line;
              }
          | Shell_command.Draft draft ->
              {
                state =
                  {
                    t with
                    palette = View.Palette_closed;
                    draft = View.prompt_make draft;
                  };
                submitted = None;
                accepted_command = Some command;
                dispatched_command = None;
              }))
  | Move_palette delta ->
      no_submit
        {
          t with
          palette =
            View.move_palette ~command_count:t.command_count ~delta t.palette;
        }
  | Palette_home ->
      no_submit
        {
          t with
          palette =
            (if t.command_count <= 0 then View.Palette_closed
             else View.Palette_open 0);
        }
  | Palette_end ->
      no_submit
        {
          t with
          palette =
            (if t.command_count <= 0 then View.Palette_closed
             else View.Palette_open (t.command_count - 1));
        }
  | Move_event delta ->
      no_submit
        {
          t with
          selection =
            View.move_selection ~event_count:t.event_count ~delta t.selection;
        }
  | Event_home ->
      no_submit
        {
          t with
          selection = View.select_event ~event_count:t.event_count ~index:0;
        }
  | Event_end -> no_submit { t with selection = View.Follow_latest }
  | Insert_text _ | Newline | Backspace | Delete | Move_cursor _ | Prompt_home
  | Prompt_end | Submit_prompt ->
      handle_prompt t action

let action_of_input ~page_size t input =
  let page_size = page_delta page_size in
  if palette_open t then
    match input with
    | Escape -> Some Close_palette
    | Enter | Ctrl_enter -> Some Accept_palette
    | Slash | Question -> Some Toggle_palette
    | Up | Mouse_scroll_up -> Some (Move_palette (-1))
    | Down | Mouse_scroll_down -> Some (Move_palette 1)
    | Page_up -> Some (Move_palette (-page_size))
    | Page_down -> Some (Move_palette page_size)
    | Home -> Some Palette_home
    | End -> Some Palette_end
    | Text _ | Shift_enter | Backspace_key | Delete_key | Left | Right | Unknown
      ->
        None
  else
    match input with
    | Slash | Question -> Some Toggle_palette
    | Text text ->
        if String.is_empty text then None else Some (Insert_text text)
    | Enter | Shift_enter -> Some Newline
    | Ctrl_enter -> Some Submit_prompt
    | Backspace_key -> Some Backspace
    | Delete_key -> Some Delete
    | Left -> Some (Move_cursor (-1))
    | Right -> Some (Move_cursor 1)
    | Home when draft_has_text t -> Some Prompt_home
    | End when draft_has_text t -> Some Prompt_end
    | Up | Mouse_scroll_up -> Some (Move_event (-1))
    | Down | Mouse_scroll_down -> Some (Move_event 1)
    | Page_up -> Some (Move_event (-page_size))
    | Page_down -> Some (Move_event page_size)
    | Home -> Some Event_home
    | End -> Some Event_end
    | Escape | Unknown -> None

let handle_input ~page_size t input =
  match action_of_input ~page_size t input with
  | None -> no_submit t
  | Some action -> handle t action
