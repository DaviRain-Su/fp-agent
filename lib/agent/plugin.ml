open! Base

type plugin_tool = {
  tool_name : string;
  tool_kind : Tool.kind;
  tool_description : string;
  tool_command : string;
  tool_permissions : Yojson.Safe.t option;
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

type load_error = { dir : string; message : string }
type discovery = { manifests : manifest list; errors : load_error list }

type tool_conflict = {
  dir : string;
  plugin_id : string;
  tool_name : string;
  existing_owner : string;
}

type smoke_result = { tool_name : string; args_file : string; output : string }

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

let schema_enum_values schema =
  match schema_member "enum" schema with `List values -> values | _ -> []

let enum_values_label values =
  values |> List.map ~f:Yojson.Safe.to_string |> String.concat ~sep:", "

let property_names schema =
  match schema_member "properties" schema with
  | `Assoc properties -> List.map properties ~f:fst
  | _ -> []

let has_property schema name =
  List.mem (property_names schema) name ~equal:String.equal

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
  | Ok () -> (
      match validate_array_schema ~path schema value with
      | Error _ as e -> e
      | Ok () -> validate_enum_schema ~path schema value)

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
      let property_result =
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
        | _ -> Ok ()
      in
      match property_result with
      | Error _ as e -> e
      | Ok () -> validate_additional_properties ~path schema fields)

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

and validate_enum_schema ~path schema value =
  match schema_enum_values schema with
  | [] -> Ok ()
  | values ->
      if List.exists values ~f:(fun expected -> Poly.equal expected value) then
        Ok ()
      else
        Error
          (Printf.sprintf "%s expected one of: %s" (schema_path_label path)
             (enum_values_label values))

and validate_additional_properties ~path schema fields =
  let extras =
    List.filter fields ~f:(fun (name, _) -> not (has_property schema name))
  in
  match schema_member "additionalProperties" schema with
  | `Bool false -> (
      match extras with
      | [] -> Ok ()
      | (name, _) :: _ ->
          Error (Printf.sprintf "unexpected field '%s'" (field_path path name)))
  | `Assoc _ as additional_schema ->
      List.fold extras ~init:(Ok ()) ~f:(fun acc (name, value) ->
          match acc with
          | Error _ as e -> e
          | Ok () ->
              validate_schema ~path:(field_path path name) additional_schema
                value)
  | _ -> Ok ()

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

let string_of_kind = function
  | Tool.Read -> "read"
  | Tool.Write -> "write"
  | Tool.Exec -> "exec"

let permission_value_ok ~tool_name ~path = function
  | `String s when not (String.is_empty (String.strip s)) -> Ok ()
  | `String _ ->
      Error (Printf.sprintf "tool '%s' %s cannot be empty" tool_name path)
  | `Bool _ -> Ok ()
  | `List values ->
      List.fold values ~init:(Ok ()) ~f:(fun acc value ->
          match acc with
          | Error _ as e -> e
          | Ok () -> (
              match value with
              | `String s when not (String.is_empty (String.strip s)) -> Ok ()
              | `String _ ->
                  Error
                    (Printf.sprintf "tool '%s' %s cannot contain empty strings"
                       tool_name path)
              | _ ->
                  Error
                    (Printf.sprintf "tool '%s' %s must contain only strings"
                       tool_name path)))
  | _ ->
      Error
        (Printf.sprintf
           "tool '%s' %s must be a string, boolean, or string array" tool_name
           path)

let validate_permissions tool_name = function
  | None -> Ok None
  | Some (`Assoc fields as permissions) ->
      let result =
        List.fold fields ~init:(Ok ()) ~f:(fun acc (name, value) ->
            match acc with
            | Error _ as e -> e
            | Ok () ->
                if String.is_empty (String.strip name) then
                  Error
                    (Printf.sprintf "tool '%s' permissions keys cannot be empty"
                       tool_name)
                else
                  permission_value_ok ~tool_name ~path:("permissions." ^ name)
                    value)
      in
      Result.map result ~f:(fun () -> Some permissions)
  | Some (`List _ as permissions) ->
      Result.map
        (permission_value_ok ~tool_name ~path:"permissions" permissions)
        ~f:(fun () -> Some permissions)
  | Some (`String _ as permissions) ->
      Result.map
        (permission_value_ok ~tool_name ~path:"permissions" permissions)
        ~f:(fun () -> Some permissions)
  | Some _ ->
      Error
        (Printf.sprintf
           "tool '%s' permissions must be a string, string array, or object"
           tool_name)

let json_scalar_label = function
  | `String s -> s
  | `Bool b -> Bool.to_string b
  | value -> Yojson.Safe.to_string value

let permission_object_field_label (name, value) =
  let value =
    match value with
    | `List values ->
        "["
        ^ (values |> List.map ~f:json_scalar_label |> String.concat ~sep:",")
        ^ "]"
    | _ -> json_scalar_label value
  in
  name ^ "=" ^ value

let permissions_label = function
  | None -> "default"
  | Some (`String s) -> s
  | Some (`List values) ->
      values |> List.map ~f:json_scalar_label |> String.concat ~sep:", "
  | Some (`Assoc fields) ->
      fields
      |> List.map ~f:permission_object_field_label
      |> String.concat ~sep:", "
  | Some value -> Yojson.Safe.to_string value

let parse_tool json : (plugin_tool, string) Result.t =
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
          kind_of_string (String.lowercase kind),
          validate_permissions tool_name
            (json_member json [ "permissions"; "permission" ]) )
      with
      | Ok (), Ok _, _ when timeout_sec <= 0 ->
          Error (Printf.sprintf "tool '%s' timeout must be positive" tool_name)
      | Ok (), Ok tool_kind, Ok tool_permissions ->
          Ok
            {
              tool_name;
              tool_kind;
              tool_description;
              tool_command;
              tool_permissions;
              tool_input_schema =
                json_member json [ "input_schema"; "inputSchema"; "parameters" ];
              tool_timeout_sec = timeout_sec;
            }
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e ->
      Error e

let duplicate_tool_name (tools : plugin_tool list) =
  let rec loop (seen : string list) (tools : plugin_tool list) =
    match tools with
    | [] -> None
    | tool :: rest ->
        if List.mem seen tool.tool_name ~equal:String.equal then
          Some tool.tool_name
        else loop (tool.tool_name :: seen) rest
  in
  loop [] tools

let validate_tool_list (tools : plugin_tool list) =
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

let search_roots = default_search_roots

let has_manifest dir =
  Stdlib.Sys.file_exists (Stdlib.Filename.concat dir manifest_file)

let dirs_in_root root =
  if has_manifest root then [ root ]
  else
    match Stdlib.Sys.readdir root with
    | exception _ -> []
    | entries ->
        Array.sort entries ~compare:String.compare;
        Array.to_list entries
        |> List.filter_map ~f:(fun name ->
            let dir = Stdlib.Filename.concat root name in
            if
              Stdlib.Sys.file_exists dir
              && Stdlib.Sys.is_directory dir
              && has_manifest dir
            then Some dir
            else None)

let discover_dirs dirs =
  List.fold dirs ~init:{ manifests = []; errors = [] } ~f:(fun acc dir ->
      match load_manifest dir with
      | Ok manifest -> { acc with manifests = manifest :: acc.manifests }
      | Error message -> { acc with errors = { dir; message } :: acc.errors })
  |> fun discovery ->
  {
    manifests = List.rev discovery.manifests;
    errors = List.rev discovery.errors;
  }

let discover () =
  search_roots () |> List.concat_map ~f:dirs_in_root |> discover_dirs

let manifests () = (discover ()).manifests

let builtin_tool_owners () =
  List.fold Builtin_tools.tools
    ~init:(Map.empty (module String))
    ~f:(fun owners tool ->
      Map.set owners ~key:tool.Tool.name ~data:"built-in tool")

let tool_conflicts_of_manifests manifests =
  let register_manifest (owners, conflicts) (manifest : manifest) =
    List.fold manifest.tools ~init:(owners, conflicts)
      ~f:(fun (owners, conflicts) tool ->
        match Map.find owners tool.tool_name with
        | Some existing_owner ->
            ( owners,
              {
                dir = manifest.dir;
                plugin_id = manifest.id;
                tool_name = tool.tool_name;
                existing_owner;
              }
              :: conflicts )
        | None ->
            ( Map.set owners ~key:tool.tool_name ~data:("plugin " ^ manifest.id),
              conflicts ))
  in
  let _, conflicts =
    List.fold manifests ~init:(builtin_tool_owners (), []) ~f:register_manifest
  in
  List.rev conflicts

let tool_conflicts () = tool_conflicts_of_manifests (manifests ())

let installed_discovery () =
  match install_home () with
  | None -> { manifests = []; errors = [] }
  | Some home -> dirs_in_root home |> discover_dirs

let installed_manifests () = (installed_discovery ()).manifests

let installed_tool_conflicts () =
  tool_conflicts_of_manifests (installed_manifests ())

let absolute_dir dir =
  if Stdlib.Filename.is_relative dir then
    Stdlib.Filename.concat (Unix.getcwd ()) dir
  else dir

let same_dir a b = String.equal (absolute_dir a) (absolute_dir b)

let candidate_conflicts ?(replace = false) (candidate : manifest) =
  let existing =
    manifests ()
    |> List.filter ~f:(fun (manifest : manifest) ->
        (not (same_dir manifest.dir candidate.dir))
        && not (replace && String.equal manifest.id candidate.id))
  in
  tool_conflicts_of_manifests (existing @ [ candidate ])
  |> List.filter ~f:(fun (conflict : tool_conflict) ->
      same_dir conflict.dir candidate.dir
      && String.equal conflict.plugin_id candidate.id)

let conflict_message (conflict : tool_conflict) =
  Printf.sprintf "tool '%s' conflicts with %s" conflict.tool_name
    conflict.existing_owner

let validate_candidate_conflicts ?(replace = false) manifest =
  match candidate_conflicts ~replace manifest with
  | [] -> Ok manifest
  | conflicts ->
      Error
        ("plugin tool name conflict: "
        ^ (conflicts |> List.map ~f:conflict_message |> String.concat ~sep:"; ")
        )

let check ?(replace = false) dir =
  match load_manifest dir with
  | Error _ as e -> e
  | Ok manifest -> validate_candidate_conflicts ~replace manifest

let output_of_result result =
  let stdout = String.strip result.Shell.stdout in
  let stderr = String.strip result.stderr in
  if not (String.is_empty stdout) then truncate stdout
  else if not (String.is_empty stderr) then truncate stderr
  else "(plugin produced no output)"

let run_plugin_tool (manifest : manifest) tool workspace args =
  match validate_args_schema tool.tool_input_schema args with
  | Error e -> Tool_result.Error { message = "schema validation failed: " ^ e }
  | Ok () ->
      let permissions =
        Option.value_map tool.tool_permissions ~default:"{}"
          ~f:Yojson.Safe.to_string
      in
      let tmp = Stdlib.Filename.temp_file "fp_agent_plugin_args" ".json" in
      let cleanup () = try Unix.unlink tmp with Unix.Unix_error _ -> () in
      Exn.protect
        ~f:(fun () ->
          Stdlib.Out_channel.with_open_bin tmp (fun oc ->
              Stdlib.Out_channel.output_string oc (Yojson.Safe.to_string args));
          let command =
            Printf.sprintf
              "cd %s && FP_AGENT_WORKSPACE=%s FP_AGENT_PLUGIN_DIR=%s \
               FP_AGENT_PLUGIN_ID=%s FP_AGENT_PLUGIN_NAME=%s \
               FP_AGENT_PLUGIN_VERSION=%s FP_AGENT_PLUGIN_SDK_VERSION=%s \
               FP_AGENT_TOOL_NAME=%s FP_AGENT_TOOL_KIND=%s \
               FP_AGENT_TOOL_PERMISSIONS=%s FP_AGENT_ARGS_FILE=%s %s < %s"
              (Stdlib.Filename.quote manifest.dir)
              (Stdlib.Filename.quote (Workspace.root workspace))
              (Stdlib.Filename.quote manifest.dir)
              (Stdlib.Filename.quote manifest.id)
              (Stdlib.Filename.quote manifest.name)
              (Stdlib.Filename.quote manifest.version)
              (Stdlib.Filename.quote (Int.to_string manifest.sdk_version))
              (Stdlib.Filename.quote tool.tool_name)
              (Stdlib.Filename.quote (string_of_kind tool.tool_kind))
              (Stdlib.Filename.quote permissions)
              (Stdlib.Filename.quote tmp)
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

let read_json_file path =
  match Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all with
  | content -> (
      match Yojson.Safe.from_string content with
      | json -> Ok json
      | exception exn ->
          Error
            (Printf.sprintf "invalid JSON in %s: %s" path (Exn.to_string exn)))
  | exception exn ->
      Error
        (Printf.sprintf "cannot read plugin args file %s: %s" path
           (Exn.to_string exn))

let smoke_arg_candidates dir tool_name =
  let examples = Stdlib.Filename.concat dir "examples" in
  [
    Stdlib.Filename.concat examples (tool_name ^ ".args.json");
    Stdlib.Filename.concat examples (tool_name ^ ".json");
  ]
  @
  if String.equal tool_name "hello_world" then
    [ Stdlib.Filename.concat examples "hello.args.json" ]
  else []

let smoke_case_dir dir tool_name =
  Stdlib.Filename.concat (Stdlib.Filename.concat dir "examples") tool_name

let is_regular_file path =
  Stdlib.Sys.file_exists path
  &&
  match Stdlib.Sys.is_directory path with
  | true -> false
  | false -> true
  | exception _ -> false

let smoke_case_files dir tool_name =
  let case_dir = smoke_case_dir dir tool_name in
  if not (Stdlib.Sys.file_exists case_dir) then []
  else
    match Stdlib.Sys.is_directory case_dir with
    | false -> []
    | true -> (
        match Stdlib.Sys.readdir case_dir with
        | exception _ -> []
        | entries ->
            Array.sort entries ~compare:String.compare;
            Array.to_list entries
            |> List.filter_map ~f:(fun name ->
                if not (String.is_suffix name ~suffix:".json") then None
                else
                  let path = Stdlib.Filename.concat case_dir name in
                  if is_regular_file path then Some path else None))
    | exception _ -> []

let unique_paths paths =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | path :: rest ->
        if List.mem seen path ~equal:String.equal then loop seen acc rest
        else loop (path :: seen) (path :: acc) rest
  in
  loop [] [] paths

let smoke_args_files dir tool_name =
  (smoke_arg_candidates dir tool_name |> List.filter ~f:is_regular_file)
  @ smoke_case_files dir tool_name
  |> unique_paths

let smoke_error_line dir tool_name =
  let expected =
    smoke_arg_candidates dir tool_name |> String.concat ~sep:", "
  in
  Printf.sprintf
    "missing smoke args for tool %s; expected one of: %s or JSON files under %s"
    tool_name expected
    (smoke_case_dir dir tool_name)

let run_smoke_tool manifest workspace (tool : plugin_tool) args =
  match plugin_check tool.tool_kind workspace args with
  | Permission.Allow -> run_plugin_tool manifest tool workspace args
  | Permission.Ask_user reason ->
      Tool_result.Error { message = "requires user approval: " ^ reason }
  | Permission.Deny reason ->
      Tool_result.Error { message = "policy denied: " ^ reason }

let smoke ?(replace = false) ~workspace dir =
  match check ~replace dir with
  | Error _ as e -> e
  | Ok manifest ->
      let rec loop acc (tools : plugin_tool list) =
        match tools with
        | [] -> Ok (List.rev acc)
        | tool :: rest -> (
            match smoke_args_files dir tool.tool_name with
            | [] -> Error (smoke_error_line dir tool.tool_name)
            | args_files -> (
                let tool_results =
                  List.fold args_files ~init:(Ok acc) ~f:(fun acc args_file ->
                      match acc with
                      | Error _ as e -> e
                      | Ok acc -> (
                          match read_json_file args_file with
                          | Error _ as e -> e
                          | Ok args -> (
                              match
                                run_smoke_tool manifest workspace tool args
                              with
                              | Tool_result.Success { output } ->
                                  Ok
                                    ({
                                       tool_name = tool.tool_name;
                                       args_file;
                                       output;
                                     }
                                    :: acc)
                              | Tool_result.Error { message } ->
                                  Error
                                    (Printf.sprintf "%s (%s) failed: %s"
                                       tool.tool_name args_file message))))
                in
                match tool_results with
                | Error _ as e -> e
                | Ok acc -> loop acc rest))
      in
      loop [] manifest.tools

let register_manifest (manifest : manifest) =
  List.iter manifest.tools ~f:(fun plugin_tool ->
      if Option.is_none (Tool.find plugin_tool.tool_name) then
        let permissions =
          match plugin_tool.tool_permissions with
          | None -> ""
          | Some _ ->
              "; permissions: " ^ permissions_label plugin_tool.tool_permissions
        in
        let description =
          Printf.sprintf "%s (plugin %s%s)" plugin_tool.tool_description
            manifest.id permissions
        in
        Tool.register
          {
            name = plugin_tool.tool_name;
            kind = plugin_tool.tool_kind;
            description;
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

let rec remove_tree path =
  match Unix.lstat path with
  | { st_kind = Unix.S_DIR; _ } ->
      Stdlib.Sys.readdir path
      |> Array.iter ~f:(fun name ->
          remove_tree (Stdlib.Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let temp_install_dir home id =
  let path = Stdlib.Filename.temp_file ~temp_dir:home ("." ^ id ^ ".") ".tmp" in
  Unix.unlink path;
  path

let cleanup_staged_dir path =
  if Stdlib.Sys.file_exists path then try remove_tree path with _ -> ()

let install ?(replace = false) src_dir =
  match (load_manifest src_dir, install_home ()) with
  | Error e, _ -> Error e
  | _, None -> Error "cannot determine plugin install home"
  | Ok manifest, Some home -> (
      let dst = Stdlib.Filename.concat home manifest.id in
      if (not replace) && Stdlib.Sys.file_exists dst then
        Error ("plugin already installed: " ^ dst)
      else
        match validate_candidate_conflicts ~replace manifest with
        | Error _ as e -> e
        | Ok _ -> (
            mkdir_p home;
            let staged = temp_install_dir home manifest.id in
            try
              copy_tree src_dir staged;
              if Stdlib.Sys.file_exists dst then remove_tree dst;
              Unix.rename staged dst;
              Ok dst
            with exn ->
              cleanup_staged_dir staged;
              Error ("plugin install failed: " ^ Exn.to_string exn)))

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

let scaffold ?id ?tool_name ?(kind = "read") dir =
  let id =
    Option.value id
      ~default:
        ("local."
        ^ sanitize_id_part (Stdlib.Filename.basename (String.rstrip dir)))
  in
  let tool_name = Option.value tool_name ~default:"hello_world" in
  let kind = String.lowercase (String.strip kind) in
  match
    ( validate_plugin_id id,
      validate_name ~what:"tool name" ~allow_dot:false tool_name,
      kind_of_string kind )
  with
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
  | Ok (), Ok (), Ok tool_kind -> (
      let kind = string_of_kind tool_kind in
      let permissions =
        match tool_kind with
        | Tool.Read -> {|{"workspace":"read"}|}
        | Tool.Write -> {|{"workspace":"write"}|}
        | Tool.Exec -> {|{"shell":true}|}
      in
      let manifest_path = Stdlib.Filename.concat dir manifest_file in
      let script_path = Stdlib.Filename.concat dir "hello.sh" in
      let readme_path = Stdlib.Filename.concat dir "README.md" in
      let examples_dir = Stdlib.Filename.concat dir "examples" in
      let args_path =
        Stdlib.Filename.concat examples_dir (tool_name ^ ".args.json")
      in
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
      "name": "%s",
      "kind": "%s",
      "description": "Returns a greeting and echoes the input JSON",
      "command": "sh hello.sh",
      "permissions": %s,
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
                   id id supported_sdk_version tool_name kind permissions));
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

Initial tool kind: `%s`.
Initial permissions: `%s`.

## Interactive Development Loop

Start fp-agent, then validate, smoke test, install, and remove the plugin
without leaving the REPL or fullscreen TUI:

```text
> /plugin-check .
> /plugin-smoke .
> /plugin-dev --replace .
> /plugin-install --replace .
> /plugin-run . %s {"message":"hi"}
> /plugin-remove %s
```

`/plugin-dev --replace .` runs validation, smoke examples, install, and registry
reload in one step. `/plugin-install --replace` is available when you want to
run those steps manually; it reloads the in-process tool registry, so `/tools`
and the next model turn can see updated plugin tools immediately.

## CLI Equivalents

Run the full development loop:

```sh
dune exec -- fp-agent --dev-plugin . --replace-plugin
```

Add more smoke cases for this tool by placing JSON files under:

```text
examples/%s/
```

Validate:

```sh
dune exec -- fp-agent --check-plugin .
```

Smoke test one tool locally:

```sh
dune exec -- fp-agent --run-plugin-tool . \
  --plugin-tool %s \
  --plugin-args-file examples/%s.args.json
```

The tool receives JSON args on stdin and can use:

- `FP_AGENT_WORKSPACE`
- `FP_AGENT_PLUGIN_DIR`
- `FP_AGENT_PLUGIN_ID`
- `FP_AGENT_PLUGIN_NAME`
- `FP_AGENT_PLUGIN_VERSION`
- `FP_AGENT_PLUGIN_SDK_VERSION`
- `FP_AGENT_TOOL_NAME`
- `FP_AGENT_TOOL_KIND`
- `FP_AGENT_TOOL_PERMISSIONS`
- `FP_AGENT_ARGS_FILE`

Install it with:

```sh
dune exec -- fp-agent --install-plugin . --replace-plugin
```
|}
                   id kind
                   (permissions_label
                      (Some (Yojson.Safe.from_string permissions)))
                   tool_name id tool_name tool_name tool_name));
          Ok dir
        with exn -> Error ("plugin scaffold failed: " ^ Exn.to_string exn))
