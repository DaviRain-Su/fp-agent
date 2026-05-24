open! Base

let candidates = [ "AGENTS.md"; "CLAUDE.md"; ".fp-agent/instructions.md" ]
let max_include_depth = 8
let max_total_chars = 32_000

let is_regular_file path =
  match (Unix.stat path).st_kind with
  | Unix.S_REG -> true
  | _ -> false
  | exception _ -> false

let read_file path =
  try Ok (Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all)
  with exn -> Error (Exn.to_string exn)

let relative_label workspace absolute =
  let root = Workspace.root workspace in
  if String.equal absolute root then "."
  else
    match String.chop_prefix absolute ~prefix:(root ^ "/") with
    | Some rel -> rel
    | None -> absolute

let include_target line =
  let line = String.strip line in
  if not (String.is_prefix line ~prefix:"@") then None
  else
    let path = String.drop_prefix line 1 |> String.strip in
    if String.is_empty path then None
    else if
      String.exists path ~f:(fun c ->
          Char.equal c ' ' || Char.equal c '\t' || Char.equal c '\n'
          || Char.equal c '\r')
    then None
    else Some path

let resolve_include workspace ~base_dir path =
  let path =
    if Stdlib.Filename.is_relative path then
      Stdlib.Filename.concat base_dir path
    else path
  in
  Workspace.resolve_path workspace path

let bound text =
  if String.length text <= max_total_chars then text
  else
    String.prefix text max_total_chars
    ^ Printf.sprintf "\n\n[project instructions truncated: omitted %d chars]"
        (String.length text - max_total_chars)

let load workspace =
  let rec render_file ~depth ~visited path =
    if depth > max_include_depth then
      [
        Printf.sprintf "--- %s ---" (relative_label workspace path);
        "[include skipped: maximum include depth reached]";
      ]
    else if Set.mem visited path then
      [
        Printf.sprintf "--- %s ---" (relative_label workspace path);
        "[include skipped: cycle detected]";
      ]
    else
      let label = relative_label workspace path in
      match read_file path with
      | Error e -> [ Printf.sprintf "[include skipped: %s: %s]" label e ]
      | Ok content ->
          let visited = Set.add visited path in
          let base_dir = Stdlib.Filename.dirname path in
          let lines =
            String.split_lines content
            |> List.concat_map ~f:(fun line ->
                match include_target line with
                | None -> [ line ]
                | Some target -> (
                    match resolve_include workspace ~base_dir target with
                    | Error e -> [ Printf.sprintf "[include skipped: %s]" e ]
                    | Ok include_path when is_regular_file include_path ->
                        render_file ~depth:(depth + 1) ~visited include_path
                    | Ok include_path ->
                        [
                          Printf.sprintf "[include skipped: %s is not a file]"
                            (relative_label workspace include_path);
                        ]))
          in
          Printf.sprintf "--- %s ---" label :: lines
  in
  let root = Workspace.root workspace in
  let visited = Set.empty (module String) in
  let blocks =
    List.filter_map candidates ~f:(fun rel ->
        match Workspace.resolve_path workspace rel with
        | Error _ -> None
        | Ok path when is_regular_file path ->
            Some (String.concat ~sep:"\n" (render_file ~depth:0 ~visited path))
        | Ok _ -> None)
  in
  match blocks with
  | [] -> None
  | blocks ->
      Some
        (bound
           (Printf.sprintf
              "Project instructions loaded from %s. Follow these instructions \
               in addition to the built-in system rules.\n\n\
               %s"
              root
              (String.concat blocks ~sep:"\n\n")))
