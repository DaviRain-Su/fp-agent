open! Base

(* Read an event log. Lines are the versioned envelopes written by Event_log;
   we return the decoded {!Event.t} payloads in order, and also expose the raw
   lines (used by forking, which copies a prefix verbatim). *)

let read_lines ~session_dir =
  let path = Stdlib.Filename.concat session_dir "events.jsonl" in
  match Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all with
  | exception Sys_error msg -> Error msg
  | contents ->
      Ok
        (String.split_lines contents
        |> List.filter ~f:(fun l -> not (String.is_empty (String.strip l))))

let event_of_line line =
  match Yojson.Safe.from_string line with
  | exception _ -> None
  | envelope -> (
      match Event.of_yojson (Yojson.Safe.Util.member "event" envelope) with
      | Ok event -> Some event
      | Error _ -> None)

let read ~session_dir =
  Result.map (read_lines ~session_dir) ~f:(List.filter_map ~f:event_of_line)
