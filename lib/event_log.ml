open! Base

type t = { oc : Stdlib.out_channel }

(* Bumped if the on-disk record envelope changes; lets future tooling replay
   old logs. The wrapped [Event.t] payload has its own ppx-derived shape. *)
let schema_version = 1

let create ~session_dir =
  let path = Stdlib.Filename.concat session_dir "events.jsonl" in
  let oc =
    Stdlib.open_out_gen [ Stdlib.Open_append; Stdlib.Open_creat ] 0o644 path
  in
  { oc }

let iso_now () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let append t event =
  let record =
    `Assoc
      [
        ("schema_version", `Int schema_version);
        ("ts", `String (iso_now ()));
        ("event", Event.to_yojson event);
      ]
  in
  Stdlib.output_string t.oc (Yojson.Safe.to_string record);
  Stdlib.output_char t.oc '\n';
  Stdlib.flush t.oc

let close t = Stdlib.close_out t.oc
