open! Base

(* Pure helpers behind the TUI, kept out of the notty-dependent rendering so
   they can be unit-tested. *)

(* The most recent [rows] lines (all of them if there are fewer). *)
let window ~rows lines =
  if rows <= 0 then []
  else
    let n = List.length lines in
    if n <= rows then lines else List.drop lines (n - rows)

let display_lines text =
  if String.is_empty text then [] else String.split_lines text

let wrap_line ~cols line =
  if cols <= 0 then []
  else if String.is_empty line then [ "" ]
  else
    let rec loop acc start =
      if start >= String.length line then List.rev acc
      else
        let len = Int.min cols (String.length line - start) in
        loop (String.sub line ~pos:start ~len :: acc) (start + len)
    in
    loop [] 0

let viewport ~rows ~cols lines =
  lines |> List.concat_map ~f:(wrap_line ~cols) |> window ~rows

let truncate ~cols s =
  if cols <= 0 then ""
  else if String.length s <= cols then s
  else if cols <= 1 then "…"
  else String.prefix s (cols - 1) ^ "…"

let pad_right ~cols s =
  let s = truncate ~cols s in
  let padding = cols - String.length s in
  if padding <= 0 then s else s ^ String.make padding ' '

type panes = { timeline_cols : int; inspector_cols : int }

let split_panes ~width =
  if width < 96 then None
  else
    let inspector_cols = Int.min 42 (Int.max 28 (width / 3)) in
    let timeline_cols = width - inspector_cols - 3 in
    if timeline_cols < 40 then None else Some { timeline_cols; inspector_cols }

type status = {
  provider : string;
  model : string;
  session : string;
  phase : string option;
  events : int;
  plugins : int;
  tools : int;
}

let status_line s =
  let phase = Option.value s.phase ~default:"idle" in
  Printf.sprintf "%s/%s | %s | %s | events %d | plugins %d | tools %d"
    s.provider s.model s.session phase s.events s.plugins s.tools

let inspector_lines ?(focus_label = "Last event") s ~last_event =
  [
    "Inspector";
    "provider: " ^ s.provider;
    "model: " ^ s.model;
    "session: " ^ s.session;
    "phase: " ^ Option.value s.phase ~default:"idle";
    Printf.sprintf "events: %d" s.events;
    Printf.sprintf "plugins: %d" s.plugins;
    Printf.sprintf "tools: %d" s.tools;
    "";
    focus_label;
    last_event;
  ]

type event_selection = Follow_latest | Pinned of int

let latest_index event_count =
  if event_count <= 0 then None else Some (event_count - 1)

let clamp ~lo ~hi n = Int.max lo (Int.min hi n)

let normalize_selection ~event_count selection =
  match latest_index event_count with
  | None -> Follow_latest
  | Some latest -> (
      match selection with
      | Follow_latest -> Follow_latest
      | Pinned index ->
          let index = clamp ~lo:0 ~hi:latest index in
          if index = latest then Follow_latest else Pinned index)

let selection_index ~event_count selection =
  match normalize_selection ~event_count selection with
  | Follow_latest -> latest_index event_count
  | Pinned index -> Some index

let selection_label ~event_count selection =
  match selection_index ~event_count selection with
  | None -> "no events"
  | Some index -> (
      match normalize_selection ~event_count selection with
      | Follow_latest -> Printf.sprintf "latest (%d/%d)" (index + 1) event_count
      | Pinned _ -> Printf.sprintf "event %d/%d" (index + 1) event_count)

let select_event ~event_count ~index =
  normalize_selection ~event_count (Pinned index)

let move_selection ~event_count ~delta selection =
  match selection_index ~event_count selection with
  | None -> Follow_latest
  | Some index -> select_event ~event_count ~index:(index + delta)

type command_entry = Shell_command.entry = {
  command : string;
  description : string;
}

let command_palette_entries = Shell_command.palette_entries

let filter_command_palette_entries ~query entries =
  let terms =
    query |> String.strip |> String.lowercase |> String.split ~on:' '
    |> List.filter ~f:(fun term -> not (String.is_empty term))
  in
  if List.is_empty terms then entries
  else
    List.filter entries ~f:(fun entry ->
        let haystack =
          String.lowercase (entry.command ^ " " ^ entry.description)
        in
        List.for_all terms ~f:(fun term ->
            String.is_substring haystack ~substring:term))

type palette_state =
  | Palette_closed
  | Palette_open of { index : int; query : string }

let normalize_palette ~command_count state =
  match state with
  | Palette_closed -> Palette_closed
  | Palette_open { index; query } ->
      let index =
        if command_count <= 0 then 0
        else clamp ~lo:0 ~hi:(command_count - 1) index
      in
      Palette_open { index; query }

let palette_is_open = function
  | Palette_closed -> false
  | Palette_open _ -> true

let palette_query = function
  | Palette_closed -> None
  | Palette_open p -> Some p.query

let palette_index ~command_count state =
  match normalize_palette ~command_count state with
  | Palette_closed -> None
  | Palette_open { index; _ } -> if command_count <= 0 then None else Some index

let palette_label ~command_count state =
  match normalize_palette ~command_count state with
  | Palette_closed -> "palette closed"
  | Palette_open { index; query } ->
      let suffix =
        if String.is_empty query then "" else Printf.sprintf " for %S" query
      in
      if command_count <= 0 then "no commands" ^ suffix
      else Printf.sprintf "command %d/%d%s" (index + 1) command_count suffix

let toggle_palette ~command_count state =
  match normalize_palette ~command_count state with
  | Palette_closed ->
      if command_count <= 0 then Palette_closed
      else Palette_open { index = 0; query = "" }
  | Palette_open _ -> Palette_closed

let move_palette ~command_count ~delta state =
  match normalize_palette ~command_count state with
  | Palette_closed -> state
  | Palette_open { index; query } ->
      normalize_palette ~command_count
        (Palette_open { index = index + delta; query })

let set_palette_query ~command_count ~query state =
  match state with
  | Palette_closed -> Palette_closed
  | Palette_open { index; _ } ->
      normalize_palette ~command_count (Palette_open { index; query })

let command_palette_lines ?(query = "") ~selected entries =
  let header =
    if String.is_empty query then "Command Palette"
    else Printf.sprintf "Command Palette: %s" query
  in
  [ header; "type to filter, Enter accept, Esc close"; "" ]
  @
  if List.is_empty entries then [ "  no matching commands" ]
  else
    List.mapi entries ~f:(fun index entry ->
        let marker =
          match selected with
          | Some selected when index = selected -> "> "
          | _ -> "  "
        in
        Printf.sprintf "%s%-24s %s" marker entry.command entry.description)

type prompt_editor = { text : string; cursor : int }

let prompt_make ?cursor text =
  let cursor = Option.value cursor ~default:(String.length text) in
  { text; cursor = clamp ~lo:0 ~hi:(String.length text) cursor }

let prompt_empty = prompt_make ""

let prompt_insert_text inserted editor =
  let editor = prompt_make ~cursor:editor.cursor editor.text in
  {
    text =
      String.prefix editor.text editor.cursor
      ^ inserted
      ^ String.drop_prefix editor.text editor.cursor;
    cursor = editor.cursor + String.length inserted;
  }

let prompt_newline editor = prompt_insert_text "\n" editor

let prompt_backspace editor =
  let editor = prompt_make ~cursor:editor.cursor editor.text in
  if editor.cursor = 0 then editor
  else
    {
      text =
        String.prefix editor.text (editor.cursor - 1)
        ^ String.drop_prefix editor.text editor.cursor;
      cursor = editor.cursor - 1;
    }

let prompt_delete editor =
  let editor = prompt_make ~cursor:editor.cursor editor.text in
  if editor.cursor >= String.length editor.text then editor
  else
    {
      text =
        String.prefix editor.text editor.cursor
        ^ String.drop_prefix editor.text (editor.cursor + 1);
      cursor = editor.cursor;
    }

let prompt_move ~delta editor =
  prompt_make ~cursor:(editor.cursor + delta) editor.text

let prompt_home editor = prompt_make ~cursor:0 editor.text
let prompt_end editor = prompt_make editor.text
let prompt_is_empty editor = String.is_empty (String.strip editor.text)

let prompt_editor_lines editor =
  let editor = prompt_make ~cursor:editor.cursor editor.text in
  let visible =
    String.prefix editor.text editor.cursor
    ^ "|"
    ^ String.drop_prefix editor.text editor.cursor
  in
  let lines =
    match display_lines visible with [] -> [ "|" ] | lines -> lines
  in
  [ "Prompt"; "Ctrl+Enter submit, Shift+Enter newline"; "" ]
  @ List.map lines ~f:(fun line -> "> " ^ line)

let flat s = String.substr_replace_all s ~pattern:"\n" ~with_:" "

let event_kind (e : Event.t) =
  match e with
  | User_message _ -> "user_message"
  | Model_delta _ -> "model_delta"
  | Assistant_message _ -> "assistant_message"
  | Model_response _ -> "model_response"
  | Policy_decision _ -> "policy_decision"
  | Tool_call _ -> "tool_call"
  | Tool_result_message _ -> "tool_result_message"
  | Tool_result _ -> "tool_result"
  | Context_compacted _ -> "context_compacted"
  | Graph_event _ -> "graph_event"
  | State_transition _ -> "state_transition"

let event_summary (e : Event.t) =
  match Event.to_display e with
  | Some line -> line
  | None -> (
      match e with
      | User_message { content } -> "user: " ^ truncate ~cols:80 (flat content)
      | Model_delta _ -> "model: streaming"
      | Assistant_message { content; _ } -> (
          match Llm.tool_uses content with
          | [] -> "assistant: final answer"
          | calls ->
              Printf.sprintf "assistant: %d tool call%s" (List.length calls)
                (if List.length calls = 1 then "" else "s"))
      | Model_response { action = Tool_call tc } ->
          "model: " ^ Event.describe_tool tc
      | Model_response { action = Tool_calls calls } ->
          Printf.sprintf "model: %d tool calls" (List.length calls)
      | Model_response { action = Final_answer _ } -> "model: final answer"
      | Policy_decision { permission; _ } ->
          "policy: " ^ Permission.to_string permission
      | State_transition { to_state; _ } ->
          "state: " ^ Agent_state.to_string to_state
      | Tool_call _ | Tool_result_message _ | Tool_result _
      | Context_compacted _ | Graph_event _ ->
          "event")

let json_preview_lines json = Yojson.Safe.pretty_to_string json |> display_lines

let tool_call_lines (tc : Tool_call.t) =
  [ "tool: " ^ tc.name; "args:" ] @ json_preview_lines tc.args

let result_lines = function
  | Tool_result.Success { output } -> [ "ok: true"; "output: " ^ flat output ]
  | Tool_result.Error { message } -> [ "ok: false"; "error: " ^ flat message ]

let event_detail_lines (e : Event.t) =
  match e with
  | Tool_call tc -> tool_call_lines tc
  | Model_response { action = Tool_call tc } -> tool_call_lines tc
  | Model_response { action = Tool_calls calls } ->
      [ Printf.sprintf "tools: %d" (List.length calls) ]
      @ List.concat_mapi calls ~f:(fun i tc ->
          ("#" ^ Int.to_string (i + 1)) :: tool_call_lines tc)
  | Tool_result result -> result_lines result
  | Tool_result_message { id; result } ->
      ("tool_use_id: " ^ id) :: result_lines result
  | Policy_decision { tool_call; permission } ->
      [
        "permission: " ^ Permission.to_string permission;
        "tool: " ^ tool_call.name;
        "args:";
      ]
      @ json_preview_lines tool_call.args
  | Assistant_message { usage; content } ->
      [
        Printf.sprintf "usage: %d in / %d out" usage.input_tokens
          usage.output_tokens;
        Printf.sprintf "content blocks: %d" (List.length content);
      ]
  | Context_compacted { summary; recent } ->
      [
        "summary: " ^ flat summary;
        Printf.sprintf "recent turns: %d" (List.length recent);
      ]
  | State_transition { from_state; to_state } ->
      [
        "from: " ^ Agent_state.to_string from_state;
        "to: " ^ Agent_state.to_string to_state;
      ]
  | Graph_event event -> [ "graph: " ^ Graph_event.describe event ]
  | User_message { content } -> [ "content: " ^ flat content ]
  | Model_delta { content } -> [ "delta: " ^ flat content ]
  | Model_response { action = Final_answer { answer } } ->
      [ "final: " ^ flat answer ]

let event_inspector_lines e =
  [
    "Event";
    "kind: " ^ event_kind e;
    "summary: " ^ event_summary e;
    "";
    "Details";
  ]
  @ event_detail_lines e
  @ ("" :: "JSON" :: json_preview_lines (Event.to_yojson e))

let tool_kind_label = function
  | Tool.Read -> "read"
  | Tool.Write -> "write"
  | Tool.Exec -> "exec"

let tool_inspector_lines (tool : Tool.t) =
  [
    "Tool";
    "name: " ^ tool.name;
    "kind: " ^ tool_kind_label tool.kind;
    "description: " ^ tool.description;
  ]
  @
  match tool.input_schema with
  | None -> [ "input_schema: <none>" ]
  | Some schema ->
      "input_schema:" :: List.map (json_preview_lines schema) ~f:(( ^ ) "  ")

let plugin_tool_lines (tool : Plugin.plugin_tool) =
  [
    "- " ^ tool.tool_name;
    "  kind: " ^ tool_kind_label tool.tool_kind;
    "  description: " ^ tool.tool_description;
    "  command: " ^ tool.tool_command;
    Printf.sprintf "  timeout: %ds" tool.tool_timeout_sec;
  ]
  @
  match tool.tool_input_schema with
  | None -> [ "  input_schema: <none>" ]
  | Some schema ->
      "  input_schema:"
      :: List.map (json_preview_lines schema) ~f:(( ^ ) "    ")

let plugin_inspector_lines (plugin : Plugin.manifest) =
  [
    "Plugin";
    "id: " ^ plugin.id;
    "name: " ^ plugin.name;
    "version: " ^ plugin.version;
    "dir: " ^ plugin.dir;
    Printf.sprintf "tools: %d" (List.length plugin.tools);
    "";
    "Tools";
  ]
  @ List.concat_map plugin.tools ~f:plugin_tool_lines

(* Classify a display line so the renderer can pick a color. Mirrors the icons
   produced by {!Event.to_display}. *)
let classify s : [ `Ok | `Err | `Action | `Plain ] =
  let t = String.lstrip s in
  if String.is_prefix t ~prefix:"✓" then `Ok
  else if String.is_prefix t ~prefix:"✗" then `Err
  else if String.is_prefix s ~prefix:"→" then `Action
  else `Plain
