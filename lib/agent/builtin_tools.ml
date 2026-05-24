open! Base

(* The built-in tools, each defined as a Tool.t (descriptor + check + run) and
   registered into the tool registry. Args arrive as JSON and are parsed here. *)

let max_output_bytes = 32 * 1024
let default_timeout_sec = 60
let max_search_matches = 200
let max_search_file_bytes = 1_000_000

let truncate s =
  if String.length s <= max_output_bytes then s
  else String.prefix s max_output_bytes ^ "\n…[truncated]"

let is_binary s = String.mem s '\000'
let err message = Tool_result.Error { message }

let props names =
  `Assoc
    (List.map names ~f:(fun name ->
         (name, `Assoc [ ("type", `String "string") ])))

let object_schema ?(required = []) properties =
  `Assoc
    [
      ("type", `String "object");
      ("properties", properties);
      ("required", `List (List.map required ~f:(fun s -> `String s)));
      ("additionalProperties", `Bool true);
    ]

let plan_item_schema =
  object_schema ~required:[ "status"; "step" ]
    (`Assoc
       [
         ("status", `Assoc [ ("type", `String "string") ]);
         ("step", `Assoc [ ("type", `String "string") ]);
       ])

let plan_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "plan",
              `Assoc [ ("type", `String "array"); ("items", plan_item_schema) ]
            );
            ("explanation", `Assoc [ ("type", `String "string") ]);
          ] );
      ("required", `List [ `String "plan" ]);
      ("additionalProperties", `Bool true);
    ]

let schema_for = function
  | "read_file" | "list_files" | "make_dir" ->
      object_schema ~required:[ "path" ] (props [ "path" ])
  | "write_file" ->
      object_schema ~required:[ "path"; "content" ]
        (props [ "path"; "content" ])
  | "edit_file" ->
      object_schema ~required:[ "path"; "old"; "new" ]
        (props [ "path"; "old"; "new" ])
  | "run_command" ->
      object_schema ~required:[ "command" ] (props [ "command"; "cwd" ])
  | "search" -> object_schema ~required:[ "query" ] (props [ "query"; "path" ])
  | "apply_patch" -> object_schema ~required:[ "patch" ] (props [ "patch" ])
  | "multi_edit" ->
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "edits",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "items",
                        object_schema ~required:[ "path"; "old"; "new" ]
                          (props [ "path"; "old"; "new" ]) );
                    ] );
              ] );
          ("required", `List [ `String "edits" ]);
        ]
  | "update_plan" -> plan_schema
  | _ -> object_schema (`Assoc [])

let tool ~name ~kind ~description ~check ~run =
  {
    Tool.name;
    kind;
    description;
    approval_reason = None;
    input_schema = Some (schema_for name);
    check;
    run;
  }

(* arg helpers *)
let str obj key =
  match Yojson.Safe.Util.member key obj with `String s -> Some s | _ -> None

let req obj key =
  match str obj key with
  | Some s -> Ok s
  | None -> Error (Printf.sprintf "missing string arg '%s'" key)

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

(* --- dangerous-command deny-list --- *)
let normalize command =
  command |> String.lowercase
  |> String.split_on_chars ~on:[ ' '; '\t'; '\n'; '\r' ]
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> String.concat ~sep:" "

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

(* --- patch path extraction (so apply_patch stays inside the workspace) --- *)
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

let patch_paths patch =
  String.split_lines patch
  |> List.concat_map ~f:patch_line_path
  |> List.filter ~f:(fun path ->
      not (String.is_empty path || String.equal path "/dev/null"))

let validate_patch_paths workspace patch =
  match patch_paths patch with
  | [] -> Permission.Deny "patch does not declare any file paths"
  | paths -> (
      match
        List.find_map paths ~f:(fun path ->
            match Workspace.validate_write_path workspace path with
            | Ok _ -> None
            | Error reason -> Some reason)
      with
      | Some reason -> Permission.Deny reason
      | None -> Permission.Allow)

(* --- per-tool checks --- *)
let resolve_check ws args =
  match str args "path" with
  | None -> Permission.Allow
  | Some path -> (
      match Workspace.resolve_path ws path with
      | Ok _ -> Permission.Allow
      | Error reason -> Permission.Deny reason)

let write_check ws args =
  match str args "path" with
  | None -> Permission.Allow
  | Some path -> (
      match Workspace.validate_write_path ws path with
      | Ok _ -> Permission.Allow
      | Error reason -> Permission.Deny reason)

(* --- multi_edit parsing --- *)
let parse_edits args =
  match Yojson.Safe.Util.member "edits" args with
  | `List items ->
      List.fold items ~init:(Ok []) ~f:(fun acc j ->
          match acc with
          | Error _ as e -> e
          | Ok xs -> (
              match (str j "path", str j "old", str j "new") with
              | Some p, Some o, Some n -> Ok ((p, o, n) :: xs)
              | _ -> Error "each edit needs string 'path', 'old', 'new'"))
      |> Result.map ~f:List.rev
  | _ -> Error "multi_edit requires an 'edits' array"

(* --- runners --- *)
let read_file_run ws args =
  match req args "path" with
  | Error e -> err e
  | Ok path -> (
      match Workspace.resolve_path ws path with
      | Error e -> err e
      | Ok abs -> (
          match read_file abs with
          | Error e -> err e
          | Ok content ->
              if is_binary content then
                err ("refusing to read binary file: " ^ path)
              else Tool_result.Success { output = truncate content }))

let write_file_run ws args =
  match (req args "path", req args "content") with
  | Error e, _ | _, Error e -> err e
  | Ok path, Ok content -> (
      match Workspace.validate_write_path ws path with
      | Error e -> err e
      | Ok abs -> (
          match write_file abs content with
          | Error e -> err e
          | Ok n ->
              Tool_result.Success
                { output = Printf.sprintf "wrote %d bytes to %s" n path }))

let edit_file_run ws args =
  match (req args "path", req args "old", req args "new") with
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> err e
  | Ok path, Ok old_text, Ok new_text -> (
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
                | Ok _ -> Tool_result.Success { output = "edited " ^ path })))

let list_files_run ws args =
  match req args "path" with
  | Error e -> err e
  | Ok path -> (
      match Workspace.resolve_path ws path with
      | Error e -> err e
      | Ok abs -> (
          try
            let entries = Stdlib.Sys.readdir abs in
            Array.sort entries ~compare:String.compare;
            Tool_result.Success
              { output = truncate (String.concat_array entries ~sep:"\n") }
          with exn -> err (Exn.to_string exn)))

let run_command_run ws args =
  match req args "command" with
  | Error e -> err e
  | Ok command -> (
      let dir_result =
        match str args "cwd" with
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
              Tool_result.Success
                {
                  output =
                    Printf.sprintf
                      "exit_code=%d\n--- stdout ---\n%s\n--- stderr ---\n%s"
                      exit_code (truncate stdout) (truncate stderr);
                }))

let rec walk_files dir ~f =
  match Stdlib.Sys.readdir dir with
  | exception Sys_error _ -> ()
  | entries ->
      Array.iter entries ~f:(fun name ->
          if String.is_prefix name ~prefix:"." then ()
          else
            let p = Stdlib.Filename.concat dir name in
            if Stdlib.Sys.is_directory p then walk_files p ~f else f p)

let search_run ws args =
  match req args "query" with
  | Error e -> err e
  | Ok query -> (
      let path = Option.value (str args "path") ~default:"." in
      match Workspace.resolve_path ws path with
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
            })

let make_dir_run ws args =
  match req args "path" with
  | Error e -> err e
  | Ok path -> (
      match Workspace.validate_write_path ws path with
      | Error e -> err e
      | Ok abs -> (
          try
            mkdir_p abs;
            Tool_result.Success { output = "created directory " ^ path }
          with exn -> err (Exn.to_string exn)))

let apply_patch_run ws args =
  match req args "patch" with
  | Error e -> err e
  | Ok patch -> (
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
          let result =
            Shell.run ~command:cmd ~timeout_sec:default_timeout_sec
          in
          cleanup ();
          match result with
          | Error e -> err e
          | Ok { exit_code = 0; _ } ->
              Tool_result.Success { output = "patch applied" }
          | Ok { stderr; exit_code; _ } ->
              err
                (Printf.sprintf "git apply failed (exit %d): %s" exit_code
                   (truncate stderr))))

let multi_edit_run ws args =
  match parse_edits args with
  | Error e -> err e
  | Ok edits -> (
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
        | (path, old_text, new_text) :: rest -> (
            match Workspace.validate_write_path ws path with
            | Error e -> Error (Printf.sprintf "edit %d: %s" i e)
            | Ok abs -> (
                match load abs with
                | Error e -> Error (Printf.sprintf "edit %d: %s" i e)
                | Ok content ->
                    if not (String.is_substring content ~substring:old_text)
                    then
                      Error
                        (Printf.sprintf "edit %d: old_text not found in %s" i
                           path)
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
          | Error e -> err e))

let update_plan_run _ws args =
  match Event.plan_items_of_json args with
  | Error e -> err e
  | Ok items ->
      Tool_result.Success
        {
          output = Printf.sprintf "plan updated: %d item(s)" (List.length items);
        }

(* --- descriptors --- *)
let tools : Tool.t list =
  [
    tool ~name:"read_file" ~kind:Read ~description:{|{"path": string}|}
      ~check:resolve_check ~run:read_file_run;
    tool ~name:"list_files" ~kind:Read ~description:{|{"path": string}|}
      ~check:resolve_check ~run:list_files_run;
    tool ~name:"search" ~kind:Read
      ~description:
        {|{"query": string, "path": string (optional)} (substring search)|}
      ~check:resolve_check ~run:search_run;
    tool ~name:"write_file" ~kind:Write
      ~description:{|{"path": string, "content": string}|} ~check:write_check
      ~run:write_file_run;
    tool ~name:"edit_file" ~kind:Write
      ~description:
        {|{"path": string, "old": string, "new": string} (replaces first exact occurrence)|}
      ~check:write_check ~run:edit_file_run;
    tool ~name:"make_dir" ~kind:Write ~description:{|{"path": string}|}
      ~check:write_check ~run:make_dir_run;
    tool ~name:"apply_patch" ~kind:Write
      ~description:{|{"patch": string} (unified diff via git apply)|}
      ~check:(fun ws args ->
        validate_patch_paths ws (Option.value (str args "patch") ~default:""))
      ~run:apply_patch_run;
    tool ~name:"multi_edit" ~kind:Write
      ~description:
        {|{"edits": [{"path","old","new"}, ...]} (applied atomically)|}
      ~check:(fun ws args ->
        match parse_edits args with
        | Error reason -> Permission.Deny reason
        | Ok edits -> (
            match
              List.find_map edits ~f:(fun (path, _, _) ->
                  match Workspace.validate_write_path ws path with
                  | Ok _ -> None
                  | Error reason -> Some reason)
            with
            | Some reason -> Permission.Deny reason
            | None -> Permission.Allow))
      ~run:multi_edit_run;
    tool ~name:"update_plan" ~kind:Read
      ~description:
        {|{"plan": [{"step": string, "status": "todo"|"doing"|"done"}], "explanation": string (optional)}|}
      ~check:(fun _ws args ->
        match Event.plan_items_of_json args with
        | Ok _ -> Permission.Allow
        | Error reason -> Permission.Deny reason)
      ~run:update_plan_run;
    tool ~name:"run_command" ~kind:Exec
      ~description:{|{"command": string, "cwd": string (optional)}|}
      ~check:(fun _ws args ->
        match str args "command" with
        | None -> Permission.Allow
        | Some command -> (
            match dangerous_command_reason command with
            | Some reason -> Permission.Deny reason
            | None -> Permission.Allow))
      ~run:run_command_run;
  ]

let register_all () = List.iter tools ~f:Tool.register
