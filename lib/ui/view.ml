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

let inspector_lines s ~last_event =
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
    "Last event";
    last_event;
  ]

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

(* Classify a display line so the renderer can pick a color. Mirrors the icons
   produced by {!Event.to_display}. *)
let classify s : [ `Ok | `Err | `Action | `Plain ] =
  let t = String.lstrip s in
  if String.is_prefix t ~prefix:"✓" then `Ok
  else if String.is_prefix t ~prefix:"✗" then `Err
  else if String.is_prefix s ~prefix:"→" then `Action
  else `Plain
