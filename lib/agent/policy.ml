open! Base

(* Collapse runs of whitespace to single spaces and lowercase, so deny-list
   matching is robust to formatting. *)
let normalize command =
  command |> String.lowercase
  |> String.split_on_chars ~on:[ ' '; '\t'; '\n'; '\r' ]
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> String.concat ~sep:" "

(* Each rule maps the normalized command to an optional denial reason. These
   are intentionally conservative blunt-instrument checks, not a full sandbox. *)
let dangerous_command_reason command =
  let c = normalize command in
  let has substring = String.is_substring c ~substring in
  let rules =
    [
      ( (fun () -> has "rm -rf /" || has "rm -fr /" || has "rm -r -f /"),
        "recursive force-remove targeting the filesystem root" );
      ( (fun () -> has "rm -rf ~" || has "rm -fr ~"),
        "recursive force-remove targeting the home directory" );
      ((fun () -> has ":(){"), "shell fork bomb");
      ((fun () -> has "mkfs"), "filesystem format command");
      ( (fun () -> has "of=/dev/" || has "> /dev/sd" || has "> /dev/disk"),
        "raw write to a block device" );
      ( (fun () ->
          (has "curl" || has "wget")
          && (has "| sh" || has "| bash" || has "|sh" || has "|bash")),
        "piping a remote download straight into a shell" );
      ((fun () -> has "chmod -r 777 /"), "recursive world-writable on root");
    ]
  in
  List.find_map rules ~f:(fun (pred, reason) ->
      if pred () then Some reason else None)

let trim_patch_metadata path =
  let path = String.strip path in
  match String.lsplit2 path ~on:'\t' with
  | Some (path, _) -> String.strip path
  | None -> path

let strip_diff_prefix path =
  let path = trim_patch_metadata path in
  if String.is_prefix path ~prefix:"a/" || String.is_prefix path ~prefix:"b/"
  then String.drop_prefix path 2
  else path

let patch_line_path line =
  match String.split line ~on:' ' with
  | [ "diff"; "--git"; a; b ] -> [ strip_diff_prefix a; strip_diff_prefix b ]
  | "+++" :: rest
  | "---" :: rest
  | "rename" :: "from" :: rest
  | "rename" :: "to" :: rest
  | "copy" :: "from" :: rest
  | "copy" :: "to" :: rest ->
      [ strip_diff_prefix (String.concat ~sep:" " rest) ]
  | _ -> []

let validate_patch_paths workspace patch =
  let paths =
    String.split_lines patch
    |> List.concat_map ~f:patch_line_path
    |> List.filter ~f:(fun path ->
        not (String.is_empty path || String.equal path "/dev/null"))
  in
  match paths with
  | [] -> Permission.Deny "patch does not declare any file paths"
  | _ -> (
      match
        List.find_map paths ~f:(fun path ->
            match Workspace.validate_write_path workspace path with
            | Ok _ -> None
            | Error reason -> Some reason)
      with
      | Some reason -> Permission.Deny reason
      | None -> Permission.Allow)

let check ?(yolo = false) ~workspace ~tool_call () =
  match (tool_call : Tool_call.t) with
  | Read_file { path } | List_files { path } | Search { path = Some path; _ }
    -> (
      match Workspace.resolve_path workspace path with
      | Ok _ -> Permission.Allow
      | Error reason -> Permission.Deny reason)
  | Search { path = None; _ } -> Permission.Allow
  | Write_file { path; _ } | Edit_file { path; _ } | Make_dir { path } -> (
      match Workspace.validate_write_path workspace path with
      | Ok _ -> Permission.Allow
      | Error reason -> Permission.Deny reason)
  | Run_command { command; _ } -> (
      match dangerous_command_reason command with
      | Some reason -> if yolo then Permission.Allow else Permission.Deny reason
      | None -> Permission.Allow)
  | Apply_patch { patch } -> validate_patch_paths workspace patch
  | Multi_edit { edits } -> (
      match
        List.find_map edits ~f:(fun e ->
            match Workspace.validate_write_path workspace e.Tool_call.path with
            | Error reason -> Some reason
            | Ok _ -> None)
      with
      | Some reason -> Permission.Deny reason
      | None -> Permission.Allow)

(* Approval policy: which already-safe tool calls additionally require explicit
   human confirmation. Independent of the deny-list above. *)
type t = { approve_commands : bool; approve_writes : bool }

let default = { approve_commands = false; approve_writes = false }

let approval_reason t (tool_call : Tool_call.t) =
  match tool_call with
  | Run_command _ when t.approve_commands ->
      Some "shell command requires approval"
  | (Write_file _ | Edit_file _ | Make_dir _ | Apply_patch _ | Multi_edit _)
    when t.approve_writes ->
      Some "file modification requires approval"
  | _ -> None
