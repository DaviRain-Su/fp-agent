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

(* Classify a display line so the renderer can pick a color. Mirrors the icons
   produced by {!Event.to_display}. *)
let classify s : [ `Ok | `Err | `Action | `Plain ] =
  let t = String.lstrip s in
  if String.is_prefix t ~prefix:"✓" then `Ok
  else if String.is_prefix t ~prefix:"✗" then `Err
  else if String.is_prefix s ~prefix:"→" then `Action
  else `Plain
