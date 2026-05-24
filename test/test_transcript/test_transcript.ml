open! Base
open Fp_agent

let test_reconstruct () =
  let root = Stdlib.Filename.temp_dir "fp_agent_tr" "" in
  Exn.protect
    ~finally:(fun () ->
      ignore
        (Shell.run ~command:(Printf.sprintf "rm -rf %s" root) ~timeout_sec:10
          : (Shell.result, string) Result.t))
    ~f:(fun () ->
      let session_dir = Session.create ~base_dir:root in
      let log = Event_log.create ~session_dir in
      Event_log.append log (Event.User_message { content = "do the thing" });
      Event_log.append log
        (Event.Model_response
           { action = Model_action.Tool_call (Tool_call.read_file "f") });
      (* Tool_call and Policy_decision are not part of the model transcript *)
      Event_log.append log (Event.Tool_call (Tool_call.read_file "f"));
      Event_log.append log
        (Event.Tool_result (Tool_result.Success { output = "contents" }));
      Event_log.close log;
      match Transcript.of_session ~session_dir with
      | Error e -> Alcotest.failf "reconstruct failed: %s" e
      | Ok messages ->
          Alcotest.(check int)
            "three transcript messages" 3 (List.length messages);
          let roles = List.map messages ~f:(fun m -> m.Message.role) in
          Alcotest.(check (list string))
            "roles in order"
            [ "user"; "assistant"; "user" ]
            roles;
          let first = List.hd_exn messages in
          Alcotest.(check string) "task preserved" "do the thing" first.content;
          let last = List.last_exn messages in
          Alcotest.(check bool)
            "observation reconstructed" true
            (String.is_substring last.content ~substring:"contents"))

let () =
  Alcotest.run "transcript"
    [ ("resume", [ Alcotest.test_case "reconstruct" `Quick test_reconstruct ]) ]
