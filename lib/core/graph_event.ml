open! Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type node_kind = Agent | Tool | Parallel | Sequence | Router
[@@deriving yojson_of, of_yojson]

type t =
  | Node_started of { node_id : string; kind : node_kind }
  | Node_completed of {
      node_id : string;
      kind : node_kind;
      output : string option;
    }
  | Node_failed of { node_id : string; kind : node_kind; error : string }
  | Edge_selected of {
      node_id : string;
      label : string;
      target_node_id : string;
    }
[@@deriving yojson_of, of_yojson]

let to_yojson = yojson_of_t

let of_yojson json =
  match t_of_yojson json with
  | t -> Ok t
  | exception exn -> Error (Exn.to_string exn)

let node_kind_to_string = function
  | Agent -> "agent"
  | Tool -> "tool"
  | Parallel -> "parallel"
  | Sequence -> "sequence"
  | Router -> "router"

let describe = function
  | Node_started { node_id; kind } ->
      Printf.sprintf "%s %s started" (node_kind_to_string kind) node_id
  | Node_completed { node_id; kind; output = None } ->
      Printf.sprintf "%s %s completed" (node_kind_to_string kind) node_id
  | Node_completed { node_id; kind; output = Some output } ->
      Printf.sprintf "%s %s completed: %s" (node_kind_to_string kind) node_id
        output
  | Node_failed { node_id; kind; error } ->
      Printf.sprintf "%s %s failed: %s" (node_kind_to_string kind) node_id error
  | Edge_selected { node_id; label; target_node_id } ->
      Printf.sprintf "router %s selected %s -> %s" node_id label target_node_id
