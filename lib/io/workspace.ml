open! Base

type t = { root : string }

let root t = t.root

(* Lexically normalize an absolute path: drop "." segments and resolve ".."
   against earlier segments without touching the filesystem. This is enough to
   reject "../" escapes even when the target file does not exist yet. *)
let normalize_abs path =
  let segments = String.split path ~on:'/' in
  let stack =
    List.fold segments ~init:[] ~f:(fun acc seg ->
        match seg with
        | "" | "." -> acc
        | ".." -> ( match acc with [] -> [] | _ :: rest -> rest)
        | s -> s :: acc)
  in
  "/" ^ String.concat ~sep:"/" (List.rev stack)

let create ~root =
  match Unix.realpath root with
  | abs ->
      if Poly.equal (Unix.stat abs).st_kind Unix.S_DIR then Ok { root = abs }
      else Error ("workspace root is not a directory: " ^ root)
  | exception Unix.Unix_error (_, _, _) ->
      Error ("workspace root does not exist: " ^ root)

let resolve_path t path =
  let absolute =
    if Stdlib.Filename.is_relative path then Stdlib.Filename.concat t.root path
    else path
  in
  let normalized = normalize_abs absolute in
  if
    String.equal normalized t.root
    || String.is_prefix normalized ~prefix:(t.root ^ "/")
  then Ok normalized
  else Error ("path escapes workspace: " ^ path)

(* The portion of [absolute] relative to the workspace root, as path segments. *)
let relative_segments t absolute =
  let rel =
    if String.equal absolute t.root then ""
    else String.chop_prefix_exn absolute ~prefix:(t.root ^ "/")
  in
  String.split rel ~on:'/' |> List.filter ~f:(fun s -> not (String.is_empty s))

let validate_write_path t path =
  match resolve_path t path with
  | Error _ as e -> e
  | Ok absolute ->
      let segments = relative_segments t absolute in
      if List.mem segments ".git" ~equal:String.equal then
        Error "refusing to modify the .git directory"
      else Ok absolute
