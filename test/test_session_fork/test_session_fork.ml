open! Base
open Fp_agent

let test_fork () =
  let base = Stdlib.Filename.temp_dir "fp_agent_fork" "" in
  Exn.protect
    ~finally:(fun () ->
      ignore
        (Shell.run ~command:(Printf.sprintf "rm -rf %s" base) ~timeout_sec:10
          : (Shell.result, string) Result.t))
    ~f:(fun () ->
      let parent = Session.create ~base_dir:base in
      let log = Event_log.create ~session_dir:parent in
      Event_log.append log (Event.User_message { content = "task one" });
      Event_log.append log
        (Event.Model_response
           { action = Model_action.Final_answer { answer = "done one" } });
      Event_log.append log (Event.User_message { content = "task two" });
      Event_log.close log;
      (* root has no parent *)
      Alcotest.(check bool)
        "root meta has no parent" true
        (match Session.read_meta parent with None, None -> true | _ -> false);
      (* fork at the first event only *)
      let child =
        match
          Session.fork ~base_dir:base ~parent_session_dir:parent ~at:(Some 1)
        with
        | Ok c -> c
        | Error e -> Alcotest.failf "fork: %s" e
      in
      let child_events =
        match Journal.read ~session_dir:child with
        | Ok e -> e
        | Error e -> Alcotest.failf "read: %s" e
      in
      Alcotest.(check int)
        "child has the 1-event prefix" 1 (List.length child_events);
      let parent_name, forked_at = Session.read_meta child in
      Alcotest.(check bool)
        "child records parent" true
        (Option.equal String.equal parent_name
           (Some (Stdlib.Filename.basename parent)));
      Alcotest.(check (option int)) "forked_at = 1" (Some 1) forked_at;
      (* replaying the child's prefix yields the earlier state (one user msg) *)
      let st = Session_state.replay child_events in
      Alcotest.(check int)
        "child state: one message" 1
        (List.length (Session_state.messages st));
      (* fork with None copies all events *)
      let full =
        match
          Session.fork ~base_dir:base ~parent_session_dir:parent ~at:None
        with
        | Ok c -> c
        | Error e -> Alcotest.failf "fork: %s" e
      in
      let n =
        match Journal.read ~session_dir:full with
        | Ok e -> List.length e
        | Error _ -> 0
      in
      Alcotest.(check int) "full fork copies all 3 events" 3 n)

let () =
  Alcotest.run "session_fork"
    [ ("fork", [ Alcotest.test_case "fork" `Quick test_fork ]) ]
