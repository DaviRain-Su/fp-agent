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
  | Move_palette of int
  | Palette_home
  | Palette_end
  | Move_event of int
  | Event_home
  | Event_end

type result = { state : t; submitted : string option }

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

let no_submit state = { state; submitted = None }

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
