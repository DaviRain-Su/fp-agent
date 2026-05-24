type node_kind = Agent | Tool | Parallel | Sequence | Router

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

val yojson_of_node_kind : node_kind -> Yojson.Safe.t
val node_kind_of_yojson : Yojson.Safe.t -> node_kind
val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
val node_kind_to_string : node_kind -> string
val describe : t -> string
