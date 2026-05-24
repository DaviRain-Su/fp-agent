open! Base

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

let node_id = function
  | Agent { id; _ }
  | Tool { id; _ }
  | Parallel { id; _ }
  | Sequence { id; _ }
  | Router { id; _ } ->
      id

let node_kind = function
  | Agent _ -> Graph_event.Agent
  | Tool _ -> Graph_event.Tool
  | Parallel _ -> Graph_event.Parallel
  | Sequence _ -> Graph_event.Sequence
  | Router _ -> Graph_event.Router

let emit on_event event = on_event (Event.Graph_event event)
let make_output ?output ?(children = []) node_id = { node_id; output; children }

let tool_result_to_result = function
  | Tool_result.Success { output } -> Ok output
  | Tool_result.Error { message } -> Error message

let first_error results =
  List.find_map results ~f:(function Error e -> Some e | Ok _ -> None)

let rec run ?(on_event = fun _ -> ()) ?(yolo = false) ~workspace node =
  let id = node_id node in
  let kind = node_kind node in
  let complete ?output ?children () =
    emit on_event (Graph_event.Node_completed { node_id = id; kind; output });
    Lwt.return (Ok (make_output ?output ?children id))
  in
  let fail error =
    emit on_event (Graph_event.Node_failed { node_id = id; kind; error });
    Lwt.return (Error error)
  in
  emit on_event (Graph_event.Node_started { node_id = id; kind });
  match node with
  | Agent { run; _ } ->
      Lwt.bind (run ()) (function
        | Ok output -> complete ~output ()
        | Error error -> fail error)
  | Tool { tool_call; _ } ->
      Lwt.bind (Tool_runner.run_lwt ~yolo ~workspace ~tool_call ())
        (fun result ->
          match tool_result_to_result result with
          | Ok output -> complete ~output ()
          | Error error -> fail error)
  | Sequence { children; _ } ->
      let rec loop acc = function
        | [] -> complete ~children:(List.rev acc) ()
        | child :: rest ->
            Lwt.bind (run ~on_event ~yolo ~workspace child) (function
              | Ok output -> loop (output :: acc) rest
              | Error error -> fail error)
      in
      loop [] children
  | Parallel { children; _ } ->
      Lwt.bind
        (Lwt.all (List.map children ~f:(run ~on_event ~yolo ~workspace)))
        (fun results ->
          match first_error results with
          | Some error -> fail error
          | None ->
              let children =
                List.filter_map results ~f:(function
                  | Ok output -> Some output
                  | Error _ -> None)
              in
              complete ~children ())
  | Router { choose; routes; _ } ->
      Lwt.bind (choose ()) (function
        | Error error -> fail error
        | Ok label -> (
            match List.Assoc.find routes label ~equal:String.equal with
            | None -> fail ("router route not found: " ^ label)
            | Some child ->
                emit on_event
                  (Graph_event.Edge_selected
                     { node_id = id; label; target_node_id = node_id child });
                Lwt.bind (run ~on_event ~yolo ~workspace child) (function
                  | Error error -> fail error
                  | Ok child_output -> complete ~children:[ child_output ] ())))
