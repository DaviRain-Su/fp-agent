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

(* Classify a display line so the renderer can pick a color. Mirrors the icons
   produced by {!Event.to_display}. *)
let classify s : [ `Ok | `Err | `Action | `Plain ] =
  let t = String.lstrip s in
  if String.is_prefix t ~prefix:"✓" then `Ok
  else if String.is_prefix t ~prefix:"✗" then `Err
  else if String.is_prefix s ~prefix:"→" then `Action
  else `Plain
