open! Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type plan_status = Todo | Doing | Done [@@deriving yojson_of, of_yojson]

type plan_item = { status : plan_status; text : string }
[@@deriving yojson_of, of_yojson]

type t =
  | User_message of { content : string }
  | Model_delta of { content : string }
  | Assistant_message of { content : Llm.content list; usage : Llm.usage }
  | Model_response of { action : Model_action.t }
  | Policy_decision of { tool_call : Tool_call.t; permission : Permission.t }
  | Tool_call of Tool_call.t
  | Tool_result_message of { id : string; result : Tool_result.t }
  | Tool_result of Tool_result.t
  | Context_compacted of { summary : string; recent : Llm.turn list }
  | Plan_updated of { items : plan_item list }
  | Graph_event of Graph_event.t
  | State_transition of { from_state : Agent_state.t; to_state : Agent_state.t }
[@@deriving yojson_of, of_yojson]

let to_yojson = yojson_of_t

let of_yojson json =
  match t_of_yojson json with
  | t -> Ok t
  | exception exn -> Error (Exn.to_string exn)

let describe_tool (tc : Tool_call.t) =
  let detail =
    List.find_map [ "path"; "command"; "query" ] ~f:(fun key ->
        Tool_call.arg_string tc key)
  in
  match detail with
  | Some d -> tc.Tool_call.name ^ " " ^ d
  | None -> tc.Tool_call.name

let first_line s =
  let line =
    match String.lsplit2 s ~on:'\n' with Some (h, _) -> h | None -> s
  in
  if String.length line > 120 then String.prefix line 120 ^ "…" else line

let plan_status_to_string = function
  | Todo -> "todo"
  | Doing -> "doing"
  | Done -> "done"

let plan_status_of_string s =
  match String.lowercase (String.strip s) with
  | "todo" | "pending" | "open" -> Some Todo
  | "doing" | "in-progress" | "in_progress" | "active" -> Some Doing
  | "done" | "complete" | "completed" -> Some Done
  | _ -> None

let plan_item_line (item : plan_item) =
  Printf.sprintf "[%s] %s" (plan_status_to_string item.status) item.text

let plan_counts items =
  let total = List.length items in
  let done_count =
    List.count items ~f:(fun item -> Poly.equal item.status Done)
  in
  (done_count, total)

(* A concise one-line rendering for live display; [None] means "do not show".
   The full record is always in the event log regardless. *)
let to_display (t : t) =
  match t with
  | Tool_call tc -> Some ("→ " ^ describe_tool tc)
  | Tool_result_message { result = Success { output }; _ } ->
      Some ("  ✓ " ^ first_line output)
  | Tool_result_message { result = Error { message }; _ } ->
      Some ("  ✗ " ^ first_line message)
  | Tool_result (Success { output }) -> Some ("  ✓ " ^ first_line output)
  | Tool_result (Error { message }) -> Some ("  ✗ " ^ first_line message)
  | Context_compacted { summary; _ } ->
      Some ("  ↻ compacted context: " ^ first_line summary)
  | Plan_updated { items } ->
      let done_count, total = plan_counts items in
      Some (Printf.sprintf "plan: %d/%d done" done_count total)
  | Graph_event event -> Some ("graph: " ^ Graph_event.describe event)
  | Policy_decision { permission = Permission.Deny reason; _ } ->
      Some ("  ✗ policy denied: " ^ reason)
  | Policy_decision { permission = Permission.Ask_user reason; _ } ->
      Some ("  ? needs approval: " ^ reason)
  | User_message _ | Model_delta _ | Assistant_message _ | Model_response _
  | Policy_decision _ | State_transition _ ->
      None
