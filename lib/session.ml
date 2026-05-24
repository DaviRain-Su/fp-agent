open! Base

let timestamp_for_dir () =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02d-%02d-%02d-%02d" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let short_id () =
  let chars = "0123456789abcdef" in
  String.init 6 ~f:(fun _ ->
      String.get chars (Random.int (String.length chars)))

let rec mkdir_p dir =
  if not (Stdlib.Sys.file_exists dir) then (
    mkdir_p (Stdlib.Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let meta_path dir = Stdlib.Filename.concat dir "meta.json"

(* meta.json records the session's place in the fork tree: its parent (by
   directory name) and the event index it was forked at, if any. *)
let write_meta dir ~parent ~forked_at =
  let json =
    `Assoc
      [
        ("parent", match parent with Some p -> `String p | None -> `Null);
        ("forked_at", match forked_at with Some n -> `Int n | None -> `Null);
        ("created", `String (timestamp_for_dir ()));
      ]
  in
  Stdlib.Out_channel.with_open_bin (meta_path dir) (fun oc ->
      Stdlib.Out_channel.output_string oc (Yojson.Safe.to_string json))

let read_meta dir =
  match
    Stdlib.In_channel.with_open_bin (meta_path dir) Stdlib.In_channel.input_all
  with
  | exception _ -> (None, None)
  | contents -> (
      match Yojson.Safe.from_string contents with
      | exception _ -> (None, None)
      | json ->
          let parent =
            match Yojson.Safe.Util.member "parent" json with
            | `String s -> Some s
            | _ -> None
          in
          let forked_at =
            match Yojson.Safe.Util.member "forked_at" json with
            | `Int n -> Some n
            | _ -> None
          in
          (parent, forked_at))

let create ~base_dir =
  Random.self_init ();
  let dir =
    List.fold_left
      [ ".ocaml-agent"; "sessions"; timestamp_for_dir () ^ "-" ^ short_id () ]
      ~init:base_dir ~f:Stdlib.Filename.concat
  in
  mkdir_p dir;
  write_meta dir ~parent:None ~forked_at:None;
  dir

(* Fork a session: create a child whose event log is the parent's first [at]
   events (all of them when [at] is None), recording the parent and fork point
   in the child's meta. Continuing the child reconstructs state from that
   prefix, leaving the parent untouched. *)
let fork ~base_dir ~parent_session_dir ~at =
  match Journal.read_lines ~session_dir:parent_session_dir with
  | Error e -> Error e
  | Ok lines ->
      let total = List.length lines in
      let n =
        match at with None -> total | Some k -> Int.max 0 (Int.min k total)
      in
      let kept = List.take lines n in
      let child = create ~base_dir in
      Stdlib.Out_channel.with_open_bin
        (Stdlib.Filename.concat child "events.jsonl") (fun oc ->
          List.iter kept ~f:(fun line ->
              Stdlib.Out_channel.output_string oc line;
              Stdlib.Out_channel.output_char oc '\n'));
      write_meta child
        ~parent:(Some (Stdlib.Filename.basename parent_session_dir))
        ~forked_at:(Some n);
      Ok child
