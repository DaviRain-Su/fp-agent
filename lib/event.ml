open! Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t =
  | User_message of { content : string }
  | Model_response of { action : Model_action.t }
  | Policy_decision of { tool_call : Tool_call.t; permission : Permission.t }
  | Tool_call of Tool_call.t
  | Tool_result of Tool_result.t
  | State_transition of { from_state : Agent_state.t; to_state : Agent_state.t }
[@@deriving yojson_of, of_yojson]

let to_yojson = yojson_of_t

let of_yojson json =
  match t_of_yojson json with
  | t -> Ok t
  | exception exn -> Error (Exn.to_string exn)

let describe_tool (tc : Tool_call.t) =
  match tc with
  | Read_file { path } -> "read_file " ^ path
  | Write_file { path; _ } -> "write_file " ^ path
  | Edit_file { path; _ } -> "edit_file " ^ path
  | Run_command { command; _ } -> "run_command " ^ command
  | List_files { path } -> "list_files " ^ path
  | Search { query; _ } -> "search " ^ query
  | Make_dir { path } -> "make_dir " ^ path
  | Apply_patch _ -> "apply_patch"
  | Multi_edit { edits } ->
      Printf.sprintf "multi_edit (%d edits)" (List.length edits)

let first_line s =
  let line =
    match String.lsplit2 s ~on:'\n' with Some (h, _) -> h | None -> s
  in
  if String.length line > 120 then String.prefix line 120 ^ "…" else line

(* A concise one-line rendering for live display; [None] means "do not show".
   The full record is always in the event log regardless. *)
let to_display (t : t) =
  match t with
  | Tool_call tc -> Some ("→ " ^ describe_tool tc)
  | Tool_result (Success { output }) -> Some ("  ✓ " ^ first_line output)
  | Tool_result (Error { message }) -> Some ("  ✗ " ^ first_line message)
  | Policy_decision { permission = Permission.Deny reason; _ } ->
      Some ("  ✗ policy denied: " ^ reason)
  | Policy_decision { permission = Permission.Ask_user reason; _ } ->
      Some ("  ? needs approval: " ^ reason)
  | User_message _ | Model_response _ | Policy_decision _ | State_transition _
    ->
      None
