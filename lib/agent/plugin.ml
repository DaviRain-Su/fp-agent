open! Base

type plugin_tool = {
  tool_name : string;
  tool_kind : Tool.kind;
  tool_description : string;
  tool_command : string;
  tool_input_schema : Yojson.Safe.t option;
  tool_timeout_sec : int;
}

type manifest = {
  id : string;
  name : string;
  version : string;
  sdk_version : int;
  dir : string;
  tools : plugin_tool list;
}

let manifest_file = "fp-agent-plugin.json"
let supported_sdk_version = 1
let default_timeout_sec = 60
let max_output_bytes = 32 * 1024

let getenv_nonempty name =
  match Stdlib.Sys.getenv_opt name with
  | Some "" | None -> None
  | Some s -> Some s

let truncate s =
  if String.length s <= max_output_bytes then s
  else String.prefix s max_output_bytes ^ "\n...[truncated]"

let json_member obj names =
  List.find_map names ~f:(fun name ->
      match Yojson.Safe.Util.member name obj with `Null -> None | v -> Some v)

let json_string obj names =
  match json_member obj names with Some (`String s) -> Some s | _ -> None

let json_int obj names =
  match json_member obj names with Some (`Int i) -> Some i | _ -> None

let schema_member name = function
  | `Assoc _ as schema -> Yojson.Safe.Util.member name schema
  | _ -> `Null

let schema_types schema =
  match schema_member "type" schema with
  | `String s -> [ s ]
  | `List values ->
      List.filter_map values ~f:(function `String s -> Some s | _ -> None)
  | _ -> []

let type_matches expected (value : Yojson.Safe.t) =
  match (expected, value) with
  | "string", `String _ -> true
  | "number", (`Int _ | `Intlit _ | `Float _) -> true
  | "integer", (`Int _ | `Intlit _) -> true
  | "boolean", `Bool _ -> true
  | "object", `Assoc _ -> true
  | "array", `List _ -> true
  | "null", `Null -> true
  | _ -> false

let schema_path_label = function
  | "$" -> "args"
  | path -> Printf.sprintf "field '%s'" path

let field_path parent name =
  if String.equal parent "$" then name else parent ^ "." ^ name

let item_path parent index = Printf.sprintf "%s[%d]" parent index

let expected_types_label types =
  match types with
  | [] -> "value"
  | [ type_ ] -> type_
  | _ -> "one of " ^ String.concat types ~sep:", "

let required_fields schema =
  match schema_member "required" schema with
  | `List values ->
      List.filter_map values ~f:(function `String s -> Some s | _ -> None)
  | _ -> []

let rec validate_schema ~path (schema : Yojson.Safe.t) (value : Yojson.Safe.t) =
  let types = schema_types schema in
  let type_result =
    if
      List.is_empty types
      || List.exists types ~f:(fun t -> type_matches t value)
    then Ok ()
    else
      Error
        (Printf.sprintf "%s expected %s" (schema_path_label path)
           (expected_types_label types))
  in
  match type_result with
  | Error _ as e -> e
  | Ok () -> validate_schema_keywords ~path schema value

and validate_schema_keywords ~path (schema : Yojson.Safe.t)
    (value : Yojson.Safe.t) =
  let has_object_keywords =
    match
      (schema_member "required" schema, schema_member "properties" schema)
    with
    | `Null, `Null -> false
    | _ -> true
  in
  let object_result =
    if has_object_keywords then
      match value with
      | `Assoc fields -> validate_object_schema ~path schema fields
      | _ ->
          Error (Printf.sprintf "%s expected object" (schema_path_label path))
    else Ok ()
  in
  match object_result with
  | Error _ as e -> e
  | Ok () -> validate_array_schema ~path schema value

and validate_object_schema ~path schema fields =
  let required_result =
    List.fold (required_fields schema) ~init:(Ok ()) ~f:(fun acc name ->
        match acc with
        | Error _ as e -> e
        | Ok () -> (
            match List.Assoc.find fields name ~equal:String.equal with
            | Some _ -> Ok ()
            | None ->
                Error
                  (Printf.sprintf "missing required field '%s'"
                     (field_path path name))))
  in
  match required_result with
  | Error _ as e -> e
  | Ok () -> (
      match schema_member "properties" schema with
      | `Assoc properties ->
          List.fold properties ~init:(Ok ())
            ~f:(fun acc (name, property_schema) ->
              match acc with
              | Error _ as e -> e
              | Ok () -> (
                  match List.Assoc.find fields name ~equal:String.equal with
                  | None -> Ok ()
                  | Some value ->
                      validate_schema ~path:(field_path path name)
                        property_schema value))
      | _ -> Ok ())

and validate_array_schema ~path (schema : Yojson.Safe.t) (value : Yojson.Safe.t)
    =
  match (schema_member "items" schema, value) with
  | `Null, _ -> Ok ()
  | item_schema, `List values ->
      List.foldi values ~init:(Ok ()) ~f:(fun index acc item ->
          match acc with
          | Error _ as e -> e
          | Ok () ->
              validate_schema ~path:(item_path path index) item_schema item)
  | _, _ -> Ok ()

let validate_args_schema schema args =
  match schema with
  | None -> Ok ()
  | Some schema -> validate_schema ~path:"$" schema args

let req_string obj name =
  match json_string obj [ name ] with
  | Some s when not (String.is_empty (String.strip s)) -> Ok s
  | _ -> Error (Printf.sprintf "missing string field '%s'" name)

let is_tool_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

let is_plugin_id_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
  | _ -> false

let validate_name ~what ~allow_dot name =
  let ok_char = if allow_dot then is_plugin_id_char else is_tool_name_char in
  if String.is_empty name then Error (what ^ " cannot be empty")
  else if String.for_all name ~f:ok_char then Ok ()
  else
    Error
      (Printf.sprintf
         "%s '%s' contains unsupported characters; use letters, digits, '_'%s \
          and '-'"
         what name
         (if allow_dot then ", '.'" else ""))

let validate_plugin_id id =
  match validate_name ~what:"plugin id" ~allow_dot:true id with
  | Error _ as e -> e
  | Ok () when String.equal id "." || String.equal id ".." ->
      Error "plugin id cannot be '.' or '..'"
  | Ok () -> Ok ()

let validate_sdk_version version =
  if version <= 0 then Error "sdk_version must be positive"
  else if version > supported_sdk_version then
    Error
      (Printf.sprintf
         "unsupported sdk_version %d (this fp-agent supports sdk_version <= %d)"
         version supported_sdk_version)
  else Ok version

let parse_sdk_version json =
  match
    json_member json
      [ "sdk_version"; "sdkVersion"; "api_version"; "apiVersion" ]
  with
  | None -> Ok supported_sdk_version
  | Some (`Int version) -> validate_sdk_version version
  | Some (`String version) -> (
      match Int.of_string (String.strip version) with
      | version -> validate_sdk_version version
      | exception _ -> Error "sdk_version must be an integer")
  | Some _ -> Error "sdk_version must be an integer"

let kind_of_string = function
  | "read" -> Ok Tool.Read
  | "write" -> Ok Tool.Write
  | "exec" | "execute" -> Ok Tool.Exec
  | other -> Error ("unknown tool kind: " ^ other)

let parse_tool json =
  let timeout_sec =
    Option.value
      (json_int json [ "timeoutSec"; "timeout_sec"; "timeout" ])
      ~default:default_timeout_sec
  in
  match
    ( req_string json "name",
      req_string json "kind",
      req_string json "description",
      req_string json "command" )
  with
  | Ok tool_name, Ok kind, Ok tool_description, Ok tool_command -> (
      match
        ( validate_name ~what:"tool name" ~allow_dot:false tool_name,
          kind_of_string (String.lowercase kind) )
      with
      | Ok (), Ok _ when timeout_sec <= 0 ->
          Error (Printf.sprintf "tool '%s' timeout must be positive" tool_name)
      | Ok (), Ok tool_kind ->
          Ok
            {
              tool_name;
              tool_kind;
              tool_description;
              tool_command;
              tool_input_schema =
                json_member json [ "input_schema"; "inputSchema"; "parameters" ];
              tool_timeout_sec = timeout_sec;
            }
      | Error e, _ | _, Error e -> Error e)
  | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e ->
      Error e

let duplicate_tool_name tools =
  let rec loop seen = function
    | [] -> None
    | tool :: rest ->
        if List.mem seen tool.tool_name ~equal:String.equal then
          Some tool.tool_name
        else loop (tool.tool_name :: seen) rest
  in
  loop [] tools

let validate_tool_list tools =
  if List.is_empty tools then Error "plugin manifest requires at least one tool"
  else
    match duplicate_tool_name tools with
    | Some name -> Error ("duplicate tool name: " ^ name)
    | None -> Ok tools

let load_manifest dir =
  let path = Stdlib.Filename.concat dir manifest_file in
  match Yojson.Safe.from_file path with
  | exception exn ->
      Error (Printf.sprintf "cannot read %s: %s" path (Exn.to_string exn))
  | json -> (
      match
        ( req_string json "id",
          json_string json [ "name" ],
          json_string json [ "version" ],
          parse_sdk_version json,
          json_member json [ "tools" ] )
      with
      | Ok id, name, version, Ok sdk_version, Some (`List tool_jsons) -> (
          match validate_plugin_id id with
          | Error e -> Error e
          | Ok () -> (
              let tools =
                List.fold tool_jsons ~init:(Ok []) ~f:(fun acc tool_json ->
                    match acc with
                    | Error _ as e -> e
                    | Ok tools ->
                        Result.map (parse_tool tool_json) ~f:(fun tool ->
                            tool :: tools))
              in
              match tools with
              | Error e -> Error e
              | Ok tools -> (
                  match validate_tool_list (List.rev tools) with
                  | Error e -> Error e
                  | Ok tools ->
                      Ok
                        {
                          id;
                          name = Option.value name ~default:id;
                          version = Option.value version ~default:"0.0.0";
                          sdk_version;
                          dir;
                          tools;
                        })))
      | Error e, _, _, _, _ -> Error e
      | _, _, _, Error e, _ -> Error e
      | _, _, _, _, _ -> Error "plugin manifest requires a tools array")

let check = load_manifest

let install_home () =
  match getenv_nonempty "FP_AGENT_PLUGIN_HOME" with
  | Some path -> Some path
  | None ->
      Option.map (getenv_nonempty "HOME") ~f:(fun home ->
          Stdlib.Filename.concat home
            (Stdlib.Filename.concat ".local"
               (Stdlib.Filename.concat "share"
                  (Stdlib.Filename.concat "fp-agent" "plugins"))))

let split_path_list value =
  String.split value ~on:':' |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))

let default_search_roots () =
  let explicit =
    match getenv_nonempty "FP_AGENT_PLUGIN_PATH" with
    | Some paths -> split_path_list paths
    | None -> []
  in
  let local = [ ".fp-agent/plugins" ] in
  let home = Option.to_list (install_home ()) in
  explicit @ local @ home

let has_manifest dir =
  Stdlib.Sys.file_exists (Stdlib.Filename.concat dir manifest_file)

let dirs_in_root root =
  if has_manifest root then [ root ]
  else
    match Stdlib.Sys.readdir root with
    | exception _ -> []
    | entries ->
        Array.to_list entries
        |> List.filter_map ~f:(fun name ->
            let dir = Stdlib.Filename.concat root name in
            if
              Stdlib.Sys.file_exists dir
              && Stdlib.Sys.is_directory dir
              && has_manifest dir
            then Some dir
            else None)

let manifests () =
  default_search_roots ()
  |> List.concat_map ~f:dirs_in_root
  |> List.filter_map ~f:(fun dir ->
      match load_manifest dir with
      | Ok manifest -> Some manifest
      | Error _ -> None)

let installed_manifests () =
  match install_home () with
  | None -> []
  | Some home ->
      dirs_in_root home
      |> List.filter_map ~f:(fun dir ->
          match load_manifest dir with
          | Ok manifest -> Some manifest
          | Error _ -> None)

let output_of_result result =
  let stdout = String.strip result.Shell.stdout in
  let stderr = String.strip result.stderr in
  if not (String.is_empty stdout) then truncate stdout
  else if not (String.is_empty stderr) then truncate stderr
  else "(plugin produced no output)"

let run_plugin_tool manifest tool workspace args =
  match validate_args_schema tool.tool_input_schema args with
  | Error e -> Tool_result.Error { message = "schema validation failed: " ^ e }
  | Ok () ->
      let tmp = Stdlib.Filename.temp_file "fp_agent_plugin_args" ".json" in
      let cleanup () = try Unix.unlink tmp with Unix.Unix_error _ -> () in
      Exn.protect
        ~f:(fun () ->
          Stdlib.Out_channel.with_open_bin tmp (fun oc ->
              Stdlib.Out_channel.output_string oc (Yojson.Safe.to_string args));
          let command =
            Printf.sprintf
              "cd %s && FP_AGENT_WORKSPACE=%s FP_AGENT_PLUGIN_DIR=%s \
               FP_AGENT_TOOL_NAME=%s %s < %s"
              (Stdlib.Filename.quote manifest.dir)
              (Stdlib.Filename.quote (Workspace.root workspace))
              (Stdlib.Filename.quote manifest.dir)
              (Stdlib.Filename.quote tool.tool_name)
              tool.tool_command
              (Stdlib.Filename.quote tmp)
          in
          match Shell.run ~command ~timeout_sec:tool.tool_timeout_sec with
          | Error e -> Tool_result.Error { message = e }
          | Ok ({ exit_code = 0; _ } as result) ->
              Tool_result.Success { output = output_of_result result }
          | Ok result ->
              Tool_result.Error
                {
                  message =
                    Printf.sprintf "plugin %s failed (exit %d): %s"
                      tool.tool_name result.exit_code (output_of_result result);
                })
        ~finally:cleanup

let plugin_check kind workspace args =
  let path =
    match args with
    | `Assoc _ -> (
        match Yojson.Safe.Util.member "path" args with
        | `String path -> Some path
        | _ -> None)
    | _ -> None
  in
  match (kind, path) with
  | Tool.Read, Some path -> (
      match Workspace.resolve_path workspace path with
      | Ok _ -> Permission.Allow
      | Error reason -> Permission.Deny reason)
  | Tool.Write, Some path -> (
      match Workspace.validate_write_path workspace path with
      | Ok _ -> Permission.Allow
      | Error reason -> Permission.Deny reason)
  | _ -> Permission.Allow

let run_tool ~dir ~tool_name ~workspace ~args =
  match load_manifest dir with
  | Error e -> Error e
  | Ok manifest -> (
      match
        List.find manifest.tools ~f:(fun tool ->
            String.equal tool.tool_name tool_name)
      with
      | None -> Error ("unknown plugin tool: " ^ tool_name)
      | Some tool -> (
          match plugin_check tool.tool_kind workspace args with
          | Permission.Allow ->
              Ok (run_plugin_tool manifest tool workspace args)
          | Permission.Ask_user reason ->
              Ok
                (Tool_result.Error
                   { message = "requires user approval: " ^ reason })
          | Permission.Deny reason ->
              Ok (Tool_result.Error { message = "policy denied: " ^ reason })))

let register_manifest manifest =
  List.iter manifest.tools ~f:(fun plugin_tool ->
      if Option.is_none (Tool.find plugin_tool.tool_name) then
        Tool.register
          {
            name = plugin_tool.tool_name;
            kind = plugin_tool.tool_kind;
            description =
              Printf.sprintf "%s (plugin %s)" plugin_tool.tool_description
                manifest.id;
            input_schema = plugin_tool.tool_input_schema;
            check = plugin_check plugin_tool.tool_kind;
            run = run_plugin_tool manifest plugin_tool;
          })

let register_all () = List.iter (manifests ()) ~f:register_manifest

let rec mkdir_p dir =
  if (not (String.is_empty dir)) && not (Stdlib.Sys.file_exists dir) then (
    mkdir_p (Stdlib.Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let copy_file src dst =
  Stdlib.In_channel.with_open_bin src (fun input ->
      Stdlib.Out_channel.with_open_bin dst (fun output ->
          Stdlib.Out_channel.output_string output
            (Stdlib.In_channel.input_all input)))

let rec copy_tree src dst =
  if Stdlib.Sys.is_directory src then (
    mkdir_p dst;
    Stdlib.Sys.readdir src
    |> Array.iter ~f:(fun name ->
        if String.equal name ".git" || String.equal name "_build" then ()
        else
          copy_tree
            (Stdlib.Filename.concat src name)
            (Stdlib.Filename.concat dst name)))
  else copy_file src dst

let install src_dir =
  match (load_manifest src_dir, install_home ()) with
  | Error e, _ -> Error e
  | _, None -> Error "cannot determine plugin install home"
  | Ok manifest, Some home ->
      let dst = Stdlib.Filename.concat home manifest.id in
      if Stdlib.Sys.file_exists dst then
        Error ("plugin already installed: " ^ dst)
      else (
        mkdir_p home;
        try
          copy_tree src_dir dst;
          Ok dst
        with exn -> Error ("plugin install failed: " ^ Exn.to_string exn))

let rec remove_tree path =
  match Unix.lstat path with
  | { st_kind = Unix.S_DIR; _ } ->
      Stdlib.Sys.readdir path
      |> Array.iter ~f:(fun name ->
          remove_tree (Stdlib.Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let remove id =
  match (validate_plugin_id id, install_home ()) with
  | Error e, _ -> Error e
  | _, None -> Error "cannot determine plugin install home"
  | Ok (), Some home -> (
      let dst = Stdlib.Filename.concat home id in
      if not (Stdlib.Sys.file_exists dst) then
        Error ("plugin is not installed: " ^ id)
      else
        try
          remove_tree dst;
          Ok dst
        with exn -> Error ("plugin remove failed: " ^ Exn.to_string exn))

let sanitize_id_part s =
  let chars =
    String.to_list s
    |> List.map ~f:(fun c -> if is_plugin_id_char c then c else '-')
  in
  let id =
    String.of_char_list chars |> String.strip ~drop:(fun c -> Char.equal c '-')
  in
  if String.is_empty id then "plugin" else id

let scaffold ?id dir =
  let id =
    Option.value id
      ~default:
        ("local."
        ^ sanitize_id_part (Stdlib.Filename.basename (String.rstrip dir)))
  in
  match validate_plugin_id id with
  | Error e -> Error e
  | Ok () -> (
      let manifest_path = Stdlib.Filename.concat dir manifest_file in
      let script_path = Stdlib.Filename.concat dir "hello.sh" in
      let readme_path = Stdlib.Filename.concat dir "README.md" in
      let examples_dir = Stdlib.Filename.concat dir "examples" in
      let args_path = Stdlib.Filename.concat examples_dir "hello.args.json" in
      if Stdlib.Sys.file_exists manifest_path then
        Error ("plugin manifest already exists: " ^ manifest_path)
      else
        try
          mkdir_p dir;
          Stdlib.Out_channel.with_open_bin manifest_path (fun oc ->
              Stdlib.Out_channel.output_string oc
                (Printf.sprintf
                   {|{
  "id": "%s",
  "name": "%s",
  "version": "0.1.0",
  "sdk_version": %d,
  "tools": [
    {
      "name": "hello_world",
      "kind": "read",
      "description": "Returns a greeting and echoes the input JSON",
      "command": "sh hello.sh",
      "input_schema": {
        "type": "object",
        "properties": {
          "message": { "type": "string" }
        },
        "required": ["message"]
      }
    }
  ]
}
|}
                   id id supported_sdk_version));
          Stdlib.Out_channel.with_open_bin script_path (fun oc ->
              Stdlib.Out_channel.output_string oc
                "#!/bin/sh\nprintf 'hello from fp-agent plugin: '\ncat\n");
          mkdir_p examples_dir;
          Stdlib.Out_channel.with_open_bin args_path (fun oc ->
              Stdlib.Out_channel.output_string oc {|{"message":"hi"}|});
          Stdlib.Out_channel.with_open_bin readme_path (fun oc ->
              Stdlib.Out_channel.output_string oc
                (Printf.sprintf
                   {|# %s

Starter fp-agent plugin generated by `--new-plugin`.

## Validate

```sh
dune exec -- fp-agent --check-plugin .
```

## Run Locally

```sh
dune exec -- fp-agent --run-plugin-tool . \
  --plugin-tool hello_world \
  --plugin-args "$(cat examples/hello.args.json)"
```

The tool receives JSON args on stdin and can use:

- `FP_AGENT_WORKSPACE`
- `FP_AGENT_PLUGIN_DIR`
- `FP_AGENT_TOOL_NAME`

Install it with:

```sh
dune exec -- fp-agent --install-plugin .
```
|}
                   id));
          Ok dir
        with exn -> Error ("plugin scaffold failed: " ^ Exn.to_string exn))
