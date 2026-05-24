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
  (* git apply itself rejects paths that escape the tree. *)
  | Apply_patch _ -> Permission.Allow

(* Approval policy: which already-safe tool calls additionally require explicit
   human confirmation. Independent of the deny-list above. *)
type t = { approve_commands : bool; approve_writes : bool }

let default = { approve_commands = false; approve_writes = false }

let approval_reason t (tool_call : Tool_call.t) =
  match tool_call with
  | Run_command _ when t.approve_commands ->
      Some "shell command requires approval"
  | (Write_file _ | Edit_file _ | Make_dir _ | Apply_patch _)
    when t.approve_writes ->
      Some "file modification requires approval"
  | _ -> None
