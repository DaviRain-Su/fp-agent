type output = {
  node_id : string;
  output : string option;
  children : output list;
}

type node =
  | Agent of { id : string; run : unit -> (string, string) Result.t Lwt.t }
  | Tool of { id : string; tool_call : Tool_call.t }
  | Parallel of { id : string; children : node list }
  | Sequence of { id : string; children : node list }
  | Router of {
      id : string;
      choose : unit -> (string, string) Result.t Lwt.t;
      routes : (string * node) list;
    }

val node_id : node -> string
val node_kind : node -> Graph_event.node_kind

val run :
  ?on_event:(Event.t -> unit) ->
  ?yolo:bool ->
  workspace:Workspace.t ->
  node ->
  (output, string) result Lwt.t
(** Execute a graph node. Graph lifecycle events are emitted through [on_event].
    Parallel children are executed concurrently; returned child outputs preserve
    graph order rather than completion order. *)
