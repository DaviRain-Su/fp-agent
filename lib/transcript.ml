open! Base

(* Rebuild the model-visible conversation from a session's event log, so a run
   can be resumed with prior context. Only the events that shaped the
   conversation are replayed: the user task, the model's actions, and the tool
   observations fed back to it. *)
let message_of_event (event : Event.t) =
  match event with
  | User_message { content } -> Some (Message.user content)
  | Model_response { action } ->
      Some
        (Message.assistant
           (Yojson.Safe.to_string (Model_action.to_yojson action)))
  | Tool_result result ->
      Some (Message.user (Tool_result.to_observation result))
  | Tool_call _ | Policy_decision _ | State_transition _ -> None

let of_session ~session_dir =
  let path = Stdlib.Filename.concat session_dir "events.jsonl" in
  match Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all with
  | exception Sys_error msg -> Error msg
  | contents ->
      let lines =
        String.split_lines contents
        |> List.filter ~f:(fun l -> not (String.is_empty (String.strip l)))
      in
      let messages =
        List.filter_map lines ~f:(fun line ->
            match Yojson.Safe.from_string line with
            | exception _ -> None
            | envelope -> (
                match
                  Event.of_yojson (Yojson.Safe.Util.member "event" envelope)
                with
                | Ok event -> message_of_event event
                | Error _ -> None))
      in
      Ok messages
