open! Base

type t = {
  draft : View.prompt_editor;
  history : string list;
  history_index : int option;
  history_stash : View.prompt_editor option;
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
  | Insert_palette_text of string
  | Palette_backspace
  | Palette_clear_query
  | Move_palette of int
  | Palette_home
  | Palette_end
  | History_previous
  | History_next
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
  | Ctrl_up
  | Ctrl_down
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

type approval_decision = Approve | Deny

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
    history = [];
    history_index = None;
    history_stash = None;
    selection = View.Follow_latest;
    palette = View.Palette_closed;
    event_count = 0;
    command_count = Int.max 0 command_count;
  }

let selected_event_index t =
  View.selection_index ~event_count:t.event_count t.selection

let command_entries t = List.take View.command_palette_entries t.command_count

let visible_command_entries t =
  let query = Option.value (View.palette_query t.palette) ~default:"" in
  View.filter_command_palette_entries ~query (command_entries t)

let visible_command_count t = List.length (visible_command_entries t)

let selected_command_index t =
  View.palette_index ~command_count:(visible_command_count t) t.palette

let palette_open t = View.palette_is_open t.palette
let palette_query t = View.palette_query t.palette

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
  View.palette_label ~command_count:(visible_command_count t) t.palette

let no_submit state =
  {
    state;
    submitted = None;
    accepted_command = None;
    dispatched_command = None;
  }

let page_delta page_size = Int.max 1 page_size
let draft_has_text t = not (String.is_empty t.draft.text)
let command_at t index = List.nth (visible_command_entries t) index

let clear_history_browse t =
  { t with history_index = None; history_stash = None }

let push_history prompt history =
  match List.last history with
  | Some last when String.equal last prompt -> history
  | _ -> history @ [ prompt ]

let history_entry t index =
  Option.value_map (List.nth t.history index) ~default:t.draft
    ~f:View.prompt_make

let history_previous t =
  match t.history with
  | [] -> t
  | _ ->
      let last_index = List.length t.history - 1 in
      let index =
        match t.history_index with
        | None -> last_index
        | Some current -> Int.max 0 (current - 1)
      in
      {
        t with
        draft = history_entry t index;
        history_index = Some index;
        history_stash = Some (Option.value t.history_stash ~default:t.draft);
      }

let history_next t =
  match t.history_index with
  | None -> t
  | Some current when current + 1 < List.length t.history ->
      let index = current + 1 in
      { t with draft = history_entry t index; history_index = Some index }
  | Some _ ->
      {
        t with
        draft = Option.value t.history_stash ~default:View.prompt_empty;
        history_index = None;
        history_stash = None;
      }

let set_palette_index t index =
  let count = visible_command_count t in
  if count <= 0 then t
  else
    let current = Option.value (selected_command_index t) ~default:0 in
    {
      t with
      palette =
        View.move_palette ~command_count:count ~delta:(index - current)
          t.palette;
    }

let update_palette_query t query =
  let visible_count =
    View.filter_command_palette_entries ~query (command_entries t)
    |> List.length
  in
  {
    t with
    palette =
      View.set_palette_query ~command_count:visible_count ~query t.palette;
  }

let handle_prompt t = function
  | Insert_text text ->
      let t = clear_history_browse t in
      no_submit { t with draft = View.prompt_insert_text text t.draft }
  | Newline ->
      let t = clear_history_browse t in
      no_submit { t with draft = View.prompt_newline t.draft }
  | Backspace ->
      let t = clear_history_browse t in
      no_submit { t with draft = View.prompt_backspace t.draft }
  | Delete ->
      let t = clear_history_browse t in
      no_submit { t with draft = View.prompt_delete t.draft }
  | Move_cursor delta ->
      no_submit { t with draft = View.prompt_move ~delta t.draft }
  | Prompt_home -> no_submit { t with draft = View.prompt_home t.draft }
  | Prompt_end -> no_submit { t with draft = View.prompt_end t.draft }
  | Submit_prompt ->
      if View.prompt_is_empty t.draft then no_submit t
      else
        let prompt = t.draft.text in
        {
          state =
            {
              (clear_history_browse t) with
              draft = View.prompt_empty;
              history = push_history prompt t.history;
            };
          submitted = Some prompt;
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
      match Option.bind (selected_command_index t) ~f:(command_at t) with
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
                    history_index = None;
                    history_stash = None;
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
            View.move_palette ~command_count:(visible_command_count t) ~delta
              t.palette;
        }
  | Palette_home ->
      no_submit { t with palette = (set_palette_index t 0).palette }
  | Palette_end ->
      no_submit
        {
          t with
          palette = (set_palette_index t (visible_command_count t - 1)).palette;
        }
  | Insert_palette_text text ->
      let query = Option.value (palette_query t) ~default:"" ^ text in
      no_submit (update_palette_query t query)
  | Palette_backspace ->
      let query = Option.value (palette_query t) ~default:"" in
      let query =
        if String.is_empty query then query
        else String.prefix query (String.length query - 1)
      in
      no_submit (update_palette_query t query)
  | Palette_clear_query -> no_submit (update_palette_query t "")
  | History_previous -> no_submit (history_previous t)
  | History_next -> no_submit (history_next t)
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
    | Text text -> Some (Insert_palette_text text)
    | Backspace_key -> Some Palette_backspace
    | Delete_key -> Some Palette_clear_query
    | Ctrl_up -> Some (Move_palette (-1))
    | Ctrl_down -> Some (Move_palette 1)
    | Shift_enter | Left | Right | Unknown -> None
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
    | Ctrl_up -> Some History_previous
    | Ctrl_down -> Some History_next
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

let approval_decision_of_input = function
  | Text text -> (
      match String.lowercase (String.strip text) with
      | "y" | "yes" -> Some Approve
      | "n" | "no" -> Some Deny
      | _ -> None)
  | Enter | Ctrl_enter | Escape -> Some Deny
  | _ -> None

let feedback_lines result =
  match
    (result.submitted, result.dispatched_command, result.accepted_command)
  with
  | Some prompt, _, _ ->
      [ "[tui] prompt submitted: " ^ View.truncate ~cols:80 prompt ]
  | None, Some command, _ -> [ "[tui] accepted command: " ^ command ]
  | None, None, Some command ->
      [
        "[tui] draft command: " ^ command.command;
        "[tui] draft text: " ^ View.truncate ~cols:80 result.state.draft.text;
      ]
  | None, None, None -> []
