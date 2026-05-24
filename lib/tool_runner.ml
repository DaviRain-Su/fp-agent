open! Base

let max_output_bytes = 32 * 1024
let default_timeout_sec = 60

let truncate s =
  if String.length s <= max_output_bytes then s
  else String.prefix s max_output_bytes ^ "\n…[truncated]"

let is_binary s = String.mem s '\000'

let rec mkdir_p dir =
  if not (Stdlib.Sys.file_exists dir) then (
    mkdir_p (Stdlib.Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let read_file abs =
  try Ok (Stdlib.In_channel.with_open_bin abs Stdlib.In_channel.input_all)
  with exn -> Error (Exn.to_string exn)

let write_file abs content =
  try
    mkdir_p (Stdlib.Filename.dirname abs);
    Stdlib.Out_channel.with_open_bin abs (fun oc ->
        Stdlib.Out_channel.output_string oc content);
    Ok (String.length content)
  with exn -> Error (Exn.to_string exn)

let err message = Tool_result.Error { message }

let exec_read_file ws path =
  match Workspace.resolve_path ws path with
  | Error e -> err e
  | Ok abs -> (
      match read_file abs with
      | Error e -> err e
      | Ok content ->
          if is_binary content then err ("refusing to read binary file: " ^ path)
          else Tool_result.Success { output = truncate content })

let exec_write_file ws path content =
  match Workspace.validate_write_path ws path with
  | Error e -> err e
  | Ok abs -> (
      match write_file abs content with
      | Error e -> err e
      | Ok n ->
          Tool_result.Success
            { output = Printf.sprintf "wrote %d bytes to %s" n path })

let exec_edit_file ws path old_text new_text =
  match Workspace.validate_write_path ws path with
  | Error e -> err e
  | Ok abs -> (
      match read_file abs with
      | Error e -> err e
      | Ok content -> (
          if not (String.is_substring content ~substring:old_text) then
            err ("old_text not found in " ^ path)
          else
            let edited =
              String.substr_replace_first content ~pattern:old_text
                ~with_:new_text
            in
            match write_file abs edited with
            | Error e -> err e
            | Ok _ -> Tool_result.Success { output = "edited " ^ path }))

let exec_list_files ws path =
  match Workspace.resolve_path ws path with
  | Error e -> err e
  | Ok abs -> (
      try
        let entries = Stdlib.Sys.readdir abs in
        Array.sort entries ~compare:String.compare;
        Tool_result.Success
          { output = truncate (String.concat_array entries ~sep:"\n") }
      with exn -> err (Exn.to_string exn))

let exec_run_command ws command cwd =
  let dir_result =
    match cwd with
    | None -> Ok (Workspace.root ws)
    | Some c -> Workspace.resolve_path ws c
  in
  match dir_result with
  | Error e -> err e
  | Ok dir -> (
      let full =
        Printf.sprintf "cd %s && %s" (Stdlib.Filename.quote dir) command
      in
      match Shell.run ~command:full ~timeout_sec:default_timeout_sec with
      | Error e -> err e
      | Ok { stdout; stderr; exit_code } ->
          let output =
            Printf.sprintf
              "exit_code=%d\n--- stdout ---\n%s\n--- stderr ---\n%s" exit_code
              (truncate stdout) (truncate stderr)
          in
          Tool_result.Success { output })

let execute ws (tool_call : Tool_call.t) =
  match tool_call with
  | Read_file { path } -> exec_read_file ws path
  | Write_file { path; content } -> exec_write_file ws path content
  | Edit_file { path; old_text; new_text } ->
      exec_edit_file ws path old_text new_text
  | List_files { path } -> exec_list_files ws path
  | Run_command { command; cwd } -> exec_run_command ws command cwd

let run ~workspace ~tool_call =
  match Policy.check ~workspace ~tool_call with
  | Permission.Deny reason -> err ("policy denied: " ^ reason)
  | Permission.Ask_user reason ->
      err ("requires user approval (not supported in MVP): " ^ reason)
  | Permission.Allow -> execute workspace tool_call
