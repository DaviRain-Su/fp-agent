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

let create ~base_dir =
  Random.self_init ();
  let dir =
    List.fold_left
      [ ".ocaml-agent"; "sessions"; timestamp_for_dir () ^ "-" ^ short_id () ]
      ~init:base_dir ~f:Stdlib.Filename.concat
  in
  mkdir_p dir;
  dir
