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
      match Transcript.messages_of_session ~session_dir with
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

let test_reconstruct_normalized_turns () =
  let root = Stdlib.Filename.temp_dir "fp_agent_tr_norm" "" in
  Exn.protect
    ~finally:(fun () ->
      ignore
        (Shell.run ~command:(Printf.sprintf "rm -rf %s" root) ~timeout_sec:10
          : (Shell.result, string) Result.t))
    ~f:(fun () ->
      let session_dir = Session.create ~base_dir:root in
      let log = Event_log.create ~session_dir in
      Event_log.append log (Event.User_message { content = "inspect f" });
      Event_log.append log
        (Event.Assistant_message
           {
             content =
               [
                 Llm.Tool_use
                   {
                     id = "provider-call-9";
                     name = "read_file";
                     input = `Assoc [ ("path", `String "f") ];
                   };
               ];
             usage = { input_tokens = 7; output_tokens = 4 };
           });
      Event_log.append log (Event.Tool_call (Tool_call.read_file "f"));
      Event_log.append log
        (Event.Tool_result_message
           {
             id = "provider-call-9";
             result = Tool_result.Success { output = "contents" };
           });
      Event_log.append log
        (Event.Assistant_message
           {
             content = [ Llm.Text "done" ];
             usage = { input_tokens = 12; output_tokens = 2 };
           });
      Event_log.close log;
      match Transcript.of_session ~session_dir with
      | Error e -> Alcotest.failf "reconstruct failed: %s" e
      | Ok turns -> (
          Alcotest.(check int) "four transcript turns" 4 (List.length turns);
          match turns with
          | [
           { role = Llm.User; content = [ Llm.Text "inspect f" ] };
           {
             role = Llm.Assistant;
             content = [ Llm.Tool_use { id = requested_id; name; input } ];
           };
           {
             role = Llm.User;
             content = [ Llm.Tool_result { id = result_id; content } ];
           };
           { role = Llm.Assistant; content = [ Llm.Text "done" ] };
          ] ->
              Alcotest.(check string)
                "assistant tool id" "provider-call-9" requested_id;
              Alcotest.(check string) "tool result id" requested_id result_id;
              Alcotest.(check string) "tool name" "read_file" name;
              Alcotest.(check (testable Yojson.Safe.pp Yojson.Safe.equal))
                "tool input"
                (`Assoc [ ("path", `String "f") ])
                input;
              Alcotest.(check bool)
                "observation content" true
                (String.is_substring content ~substring:"contents")
          | turns ->
              Alcotest.failf "unexpected turns: %s"
                (Yojson.Safe.to_string
                   (`List (List.map turns ~f:Llm.turn_to_json)))))

let () =
  Alcotest.run "transcript"
    [
      ( "resume",
        [
          Alcotest.test_case "reconstruct" `Quick test_reconstruct;
          Alcotest.test_case "reconstruct_normalized_turns" `Quick
            test_reconstruct_normalized_turns;
        ] );
    ]
