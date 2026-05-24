open! Base
open Fp_agent

let with_workspace f =
  let root = Stdlib.Filename.temp_dir "fp_agent_graph" "" in
  Unix.mkdir (Stdlib.Filename.concat root ".git") 0o755;
  let workspace =
    match Workspace.create ~root with
    | Ok ws -> ws
    | Error msg -> Alcotest.failf "workspace create failed: %s" msg
  in
  Exn.protect
    ~f:(fun () -> f workspace root)
    ~finally:(fun () ->
      ignore
        (Shell.run ~command:(Printf.sprintf "rm -rf %s" root) ~timeout_sec:10
          : (Shell.result, string) Result.t))

let run_graph workspace node =
  let events = ref [] in
  let result =
    Lwt_main.run
      (Graph_runtime.run
         ~on_event:(fun e -> events := e :: !events)
         ~workspace node)
  in
  (result, List.rev !events)

let read path = Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all

let graph_events events =
  List.filter_map events ~f:(function
    | Event.Graph_event event -> Some event
    | _ -> None)

let test_tool_node_logs_graph_events () =
  with_workspace (fun workspace root ->
      let node =
        Graph_runtime.Tool
          {
            id = "write";
            tool_call = Tool_call.write_file ~path:"out.txt" ~content:"hello";
          }
      in
      let result, events = run_graph workspace node in
      (match result with
      | Ok output ->
          Alcotest.(check string) "node id" "write" output.node_id;
          Alcotest.(check (option string))
            "tool output" (Some "wrote 5 bytes to out.txt") output.output
      | Error e -> Alcotest.failf "graph failed: %s" e);
      Alcotest.(check string)
        "file content" "hello"
        (read (Stdlib.Filename.concat root "out.txt"));
      match graph_events events with
      | [
       Graph_event.Node_started { node_id = "write"; kind = Tool };
       Node_completed { node_id = "write"; kind = Tool; _ };
      ] ->
          ()
      | _ -> Alcotest.fail "expected tool start/complete graph events")

let test_sequence_runs_in_order () =
  with_workspace (fun workspace root ->
      let node =
        Graph_runtime.Sequence
          {
            id = "seq";
            children =
              [
                Tool
                  {
                    id = "a";
                    tool_call = Tool_call.write_file ~path:"a.txt" ~content:"a";
                  };
                Tool
                  {
                    id = "b";
                    tool_call = Tool_call.write_file ~path:"b.txt" ~content:"b";
                  };
              ];
          }
      in
      let result, _events = run_graph workspace node in
      (match result with
      | Ok output ->
          Alcotest.(check int) "two children" 2 (List.length output.children)
      | Error e -> Alcotest.failf "sequence failed: %s" e);
      Alcotest.(check string)
        "a" "a"
        (read (Stdlib.Filename.concat root "a.txt"));
      Alcotest.(check string)
        "b" "b"
        (read (Stdlib.Filename.concat root "b.txt")))

let test_parallel_preserves_child_order () =
  with_workspace (fun workspace root ->
      let node =
        Graph_runtime.Parallel
          {
            id = "par";
            children =
              [
                Agent
                  {
                    id = "slow";
                    run =
                      (fun () ->
                        Lwt.bind (Lwt_unix.sleep 0.05) (fun () ->
                            Lwt.return (Ok "slow")));
                  };
                Tool
                  {
                    id = "fast";
                    tool_call =
                      Tool_call.write_file ~path:"fast.txt" ~content:"fast";
                  };
              ];
          }
      in
      let result, _events = run_graph workspace node in
      (match result with
      | Ok output ->
          Alcotest.(check (list string))
            "child order" [ "slow"; "fast" ]
            (List.map output.children ~f:(fun child -> child.node_id))
      | Error e -> Alcotest.failf "parallel failed: %s" e);
      Alcotest.(check string)
        "fast file" "fast"
        (read (Stdlib.Filename.concat root "fast.txt")))

let test_router_selects_route () =
  with_workspace (fun workspace root ->
      let node =
        Graph_runtime.Router
          {
            id = "router";
            choose = (fun () -> Lwt.return (Ok "write"));
            routes =
              [
                ( "write",
                  Tool
                    {
                      id = "selected";
                      tool_call =
                        Tool_call.write_file ~path:"selected.txt" ~content:"ok";
                    } );
              ];
          }
      in
      let result, events = run_graph workspace node in
      (match result with
      | Ok output ->
          Alcotest.(check int) "one child" 1 (List.length output.children)
      | Error e -> Alcotest.failf "router failed: %s" e);
      Alcotest.(check string)
        "selected file" "ok"
        (read (Stdlib.Filename.concat root "selected.txt"));
      Alcotest.(check bool)
        "edge selected logged" true
        (List.exists (graph_events events) ~f:(function
          | Graph_event.Edge_selected
              {
                node_id = "router";
                label = "write";
                target_node_id = "selected";
              } ->
              true
          | _ -> false)))

let test_failure_logs_graph_event () =
  with_workspace (fun workspace _root ->
      let node =
        Graph_runtime.Tool
          {
            id = "bad";
            tool_call = Tool_call.write_file ~path:".git/config" ~content:"x";
          }
      in
      let result, events = run_graph workspace node in
      (match result with
      | Ok _ -> Alcotest.fail "expected graph failure"
      | Error e ->
          Alcotest.(check bool)
            "failure mentions policy" true
            (String.is_substring e ~substring:"policy denied"));
      Alcotest.(check bool)
        "node failed logged" true
        (List.exists (graph_events events) ~f:(function
          | Graph_event.Node_failed { node_id = "bad"; kind = Tool; _ } -> true
          | _ -> false)))

let () =
  Alcotest.run "graph_runtime"
    [
      ( "runtime",
        [
          Alcotest.test_case "tool_node_logs" `Quick
            test_tool_node_logs_graph_events;
          Alcotest.test_case "sequence" `Quick test_sequence_runs_in_order;
          Alcotest.test_case "parallel_order" `Quick
            test_parallel_preserves_child_order;
          Alcotest.test_case "router" `Quick test_router_selects_route;
          Alcotest.test_case "failure_event" `Quick
            test_failure_logs_graph_event;
        ] );
    ]
