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

let max_search_matches = 200
let max_search_file_bytes = 1_000_000

(* Recursively visit regular files under [dir], skipping dotfiles/dirs (incl.
   .git). Per-directory readdir failures are ignored. *)
let rec walk_files dir ~f =
  match Stdlib.Sys.readdir dir with
  | exception Sys_error _ -> ()
  | entries ->
      Array.iter entries ~f:(fun name ->
          if String.is_prefix name ~prefix:"." then ()
          else
            let p = Stdlib.Filename.concat dir name in
            if Stdlib.Sys.is_directory p then walk_files p ~f else f p)

let exec_search ws query path =
  match Workspace.resolve_path ws (Option.value path ~default:".") with
  | Error e -> err e
  | Ok abs ->
      let root = Workspace.root ws in
      let rel file =
        Option.value
          (String.chop_prefix file ~prefix:(root ^ "/"))
          ~default:file
      in
      let matches = ref [] and count = ref 0 in
      let scan_file file =
        if !count < max_search_matches then
          match read_file file with
          | Error _ -> ()
          | Ok content ->
              if
                (not (is_binary content))
                && String.length content <= max_search_file_bytes
              then
                List.iteri (String.split_lines content) ~f:(fun i line ->
                    if
                      !count < max_search_matches
                      && String.is_substring line ~substring:query
                    then (
                      matches :=
                        Printf.sprintf "%s:%d:%s" (rel file) (i + 1)
                          (String.strip line)
                        :: !matches;
                      Int.incr count))
      in
      if Stdlib.Sys.is_directory abs then walk_files abs ~f:scan_file
      else scan_file abs;
      let out = String.concat ~sep:"\n" (List.rev !matches) in
      Tool_result.Success
        {
          output =
            (if String.is_empty out then "(no matches)" else truncate out);
        }

let exec_make_dir ws path =
  match Workspace.validate_write_path ws path with
  | Error e -> err e
  | Ok abs -> (
      try
        mkdir_p abs;
        Tool_result.Success { output = "created directory " ^ path }
      with exn -> err (Exn.to_string exn))

(* Apply a unified diff with `git apply`, which itself refuses paths that
   escape the tree, run with the workspace as its working directory. *)
let exec_apply_patch ws patch =
  let root = Workspace.root ws in
  let tmp = Stdlib.Filename.temp_file "fp_agent_patch" ".diff" in
  let cleanup () = try Unix.unlink tmp with Unix.Unix_error _ -> () in
  match write_file tmp patch with
  | Error e ->
      cleanup ();
      err e
  | Ok _ -> (
      let cmd =
        Printf.sprintf "git -C %s apply --whitespace=nowarn %s"
          (Stdlib.Filename.quote root)
          (Stdlib.Filename.quote tmp)
      in
      let result = Shell.run ~command:cmd ~timeout_sec:default_timeout_sec in
      cleanup ();
      match result with
      | Error e -> err e
      | Ok { exit_code = 0; _ } ->
          Tool_result.Success { output = "patch applied" }
      | Ok { stderr; exit_code; _ } ->
          err
            (Printf.sprintf "git apply failed (exit %d): %s" exit_code
               (truncate stderr)))

(* Apply several edits atomically: compute all results in memory (so multiple
   edits to the same file compose), and only then write each file. Any failure
   aborts before anything is written. *)
let exec_multi_edit ws (edits : Tool_call.edit list) =
  let tbl = Hashtbl.create (module String) in
  let load abs =
    match Hashtbl.find tbl abs with
    | Some c -> Ok c
    | None -> (
        match read_file abs with
        | Ok c ->
            Hashtbl.set tbl ~key:abs ~data:c;
            Ok c
        | Error e -> Error e)
  in
  let rec apply i = function
    | [] -> Ok ()
    | { Tool_call.path; old_text; new_text } :: rest -> (
        match Workspace.validate_write_path ws path with
        | Error e -> Error (Printf.sprintf "edit %d: %s" i e)
        | Ok abs -> (
            match load abs with
            | Error e -> Error (Printf.sprintf "edit %d: %s" i e)
            | Ok content ->
                if not (String.is_substring content ~substring:old_text) then
                  Error
                    (Printf.sprintf "edit %d: old_text not found in %s" i path)
                else (
                  Hashtbl.set tbl ~key:abs
                    ~data:
                      (String.substr_replace_first content ~pattern:old_text
                         ~with_:new_text);
                  apply (i + 1) rest)))
  in
  match apply 1 edits with
  | Error e -> err e
  | Ok () -> (
      let written =
        Hashtbl.fold tbl ~init:(Ok 0) ~f:(fun ~key:abs ~data acc ->
            match acc with
            | Error _ as e -> e
            | Ok n -> (
                match write_file abs data with
                | Ok _ -> Ok (n + 1)
                | Error e -> Error e))
      in
      match written with
      | Ok files ->
          Tool_result.Success
            {
              output =
                Printf.sprintf "applied %d edit(s) across %d file(s)"
                  (List.length edits) files;
            }
      | Error e -> err e)

let execute ws (tool_call : Tool_call.t) =
  match tool_call with
  | Read_file { path } -> exec_read_file ws path
  | Write_file { path; content } -> exec_write_file ws path content
  | Edit_file { path; old_text; new_text } ->
      exec_edit_file ws path old_text new_text
  | List_files { path } -> exec_list_files ws path
  | Run_command { command; cwd } -> exec_run_command ws command cwd
  | Search { query; path } -> exec_search ws query path
  | Make_dir { path } -> exec_make_dir ws path
  | Apply_patch { patch } -> exec_apply_patch ws patch
  | Multi_edit { edits } -> exec_multi_edit ws edits

let run ?(yolo = false) ~workspace ~tool_call () =
  match Policy.check ~yolo ~workspace ~tool_call () with
  | Permission.Deny reason -> err ("policy denied: " ^ reason)
  | Permission.Ask_user reason ->
      err ("requires user approval (not supported in MVP): " ^ reason)
  | Permission.Allow -> execute workspace tool_call
