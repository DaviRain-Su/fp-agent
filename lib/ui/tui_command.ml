open! Base

type context = {
  provider : string;
  model : string;
  api_base : string;
  workspace_root : string;
  sessions_root : string;
  session_dir : string;
  events : Event.t list;
  selected_event_index : int option;
}

let tool_kind_label = function
  | Tool.Read -> "read"
  | Tool.Write -> "write"
  | Tool.Exec -> "exec"

let command_section command lines = ("[tui] " ^ command) :: lines

let lines_of_text text =
  let lines = String.split_lines text in
  match List.filter lines ~f:(fun _ -> true) with [] -> [] | lines -> lines

let shell_lines ~command =
  match Shell.run ~command ~timeout_sec:30 with
  | Error e -> [ e ]
  | Ok { stdout; stderr; exit_code = 0 } ->
      let output =
        if not (String.is_empty stdout) then stdout
        else if not (String.is_empty stderr) then stderr
        else ""
      in
      lines_of_text output
  | Ok { stdout; stderr; exit_code } ->
      let output =
        if not (String.is_empty stderr) then stderr
        else if not (String.is_empty stdout) then stdout
        else "(no output)"
      in
      Printf.sprintf "command failed (exit %d)" exit_code
      :: lines_of_text output

let tools_lines () =
  Tool_loader.register_all ();
  let tool_lines =
    "Available tools:"
    :: List.map (Tool.all ()) ~f:(fun (tool : Tool.t) ->
        Printf.sprintf "  %-18s %-5s %s" tool.name
          (tool_kind_label tool.kind)
          tool.description)
  in
  match Plugin.tool_conflicts () with
  | [] -> tool_lines
  | conflicts ->
      tool_lines
      @ [ ""; "Plugin tool conflicts:" ]
      @ List.map conflicts ~f:(fun (conflict : Plugin.tool_conflict) ->
          Printf.sprintf "  - %s from %s skipped; already provided by %s"
            conflict.tool_name conflict.plugin_id conflict.existing_owner)

let tool_detail_lines query =
  Tool_loader.register_all ();
  let query = String.strip query in
  if String.is_empty query then [ "usage: /tool <tool-name>" ]
  else
    match Tool.find query with
    | None -> [ "no tool matching: " ^ query ]
    | Some tool -> View.tool_inspector_lines tool

let plugin_receipt_lines (plugin : Plugin.manifest) =
  match plugin.install_receipt with
  | None -> []
  | Some receipt ->
      let hash =
        Option.value_map receipt.package_sha256 ~default:"" ~f:(fun hash ->
            " sha256=" ^ hash)
      in
      [
        Printf.sprintf "  installed_from=%s path=%s%s" receipt.source_kind
          receipt.source_path hash;
      ]

let plugins_lines () =
  let discovery = Plugin.discover () in
  let manifest_lines =
    match discovery.manifests with
    | [] -> [ "(no plugins discovered)" ]
    | manifests ->
        List.concat_map manifests ~f:(fun (plugin : Plugin.manifest) ->
            Printf.sprintf "%s %s (%s, sdk %d)" plugin.id plugin.name
              plugin.version plugin.sdk_version
            :: ("  " ^ plugin.dir)
            :: plugin_receipt_lines plugin
            @ List.map plugin.tools ~f:(fun tool ->
                Printf.sprintf "  - %-18s %-5s permissions=%s %s" tool.tool_name
                  (tool_kind_label tool.tool_kind)
                  (Plugin.permissions_label tool.tool_permissions)
                  tool.tool_description)
            @ [ "" ])
        |> List.drop_last |> Option.value ~default:[]
  in
  match discovery.errors with
  | [] -> (
      match Plugin.tool_conflicts () with
      | [] -> manifest_lines
      | conflicts ->
          manifest_lines
          @ [ ""; "Plugin tool conflicts:" ]
          @ List.map conflicts ~f:(fun (conflict : Plugin.tool_conflict) ->
              Printf.sprintf "  - %s from %s skipped; already provided by %s"
                conflict.tool_name conflict.plugin_id conflict.existing_owner))
  | errors ->
      let error_lines =
        "Invalid plugins:"
        :: List.map errors ~f:(fun (error : Plugin.load_error) ->
            Printf.sprintf "  - %s: %s" error.dir error.message)
      in
      let conflict_lines =
        match Plugin.tool_conflicts () with
        | [] -> []
        | conflicts ->
            "" :: "Plugin tool conflicts:"
            :: List.map conflicts ~f:(fun (conflict : Plugin.tool_conflict) ->
                Printf.sprintf "  - %s from %s skipped; already provided by %s"
                  conflict.tool_name conflict.plugin_id conflict.existing_owner)
      in
      if List.is_empty manifest_lines then error_lines @ conflict_lines
      else manifest_lines @ [ "" ] @ error_lines @ conflict_lines

let plugin_diagnostics_lines () =
  let discovery = Plugin.discover () in
  let conflicts = Plugin.tool_conflicts () in
  let roots = Plugin.search_roots () in
  let install_home =
    Option.value (Plugin.install_home ()) ~default:"(unavailable)"
  in
  let root_lines =
    match roots with
    | [] -> [ "  (none)" ]
    | roots -> List.map roots ~f:(fun root -> "  - " ^ root)
  in
  let invalid_lines =
    match discovery.errors with
    | [] -> [ "Invalid plugins: 0" ]
    | errors ->
        "Invalid plugins:"
        :: List.map errors ~f:(fun (error : Plugin.load_error) ->
            Printf.sprintf "  - %s: %s" error.dir error.message)
  in
  let conflict_lines =
    match conflicts with
    | [] -> [ "Plugin tool conflicts: 0" ]
    | conflicts ->
        "Plugin tool conflicts:"
        :: List.map conflicts ~f:(fun (conflict : Plugin.tool_conflict) ->
            Printf.sprintf "  - %s from %s skipped; already provided by %s"
              conflict.tool_name conflict.plugin_id conflict.existing_owner)
  in
  [
    "Plugin diagnostics";
    "install_home: " ^ install_home;
    Printf.sprintf "valid_plugins: %d" (List.length discovery.manifests);
    Printf.sprintf "invalid_plugins: %d" (List.length discovery.errors);
    Printf.sprintf "tool_conflicts: %d" (List.length conflicts);
    "search_roots:";
  ]
  @ root_lines @ [ "" ] @ invalid_lines @ [ "" ] @ conflict_lines
  @ [
      "";
      "next: /plugins";
      "next: /plugin <id|tool>";
      "next: /plugin-check <dir|package>";
      "next: /plugin-package --output <file> <dir>";
      "next: /plugin-dev --replace <dir>";
    ]

let plugin_sdk_lines () =
  let template_lines =
    Plugin.scaffold_templates ()
    |> List.concat_map ~f:(fun (template : Plugin.scaffold_template_info) ->
        let aliases =
          match template.template_aliases with
          | [] -> ""
          | aliases ->
              Printf.sprintf " (aliases: %s)" (String.concat aliases ~sep:", ")
        in
        [
          Printf.sprintf "  - %s%s" template.template_id aliases;
          "      " ^ template.template_description;
          "      command: " ^ template.template_command;
          "      files: " ^ String.concat template.template_files ~sep:", ";
        ])
  in
  [
    "Plugin SDK";
    "manifest_file: " ^ Plugin.manifest_file;
    Printf.sprintf "supported_sdk_version: %d" Plugin.supported_sdk_version;
    "templates:";
  ]
  @ template_lines
  @ [
      "";
      "tool_env:";
      "  - FP_AGENT_WORKSPACE";
      "  - FP_AGENT_PLUGIN_DIR";
      "  - FP_AGENT_PLUGIN_ID";
      "  - FP_AGENT_PLUGIN_NAME";
      "  - FP_AGENT_PLUGIN_VERSION";
      "  - FP_AGENT_PLUGIN_SDK_VERSION";
      "  - FP_AGENT_TOOL_NAME";
      "  - FP_AGENT_TOOL_KIND";
      "  - FP_AGENT_TOOL_PERMISSIONS";
      "  - FP_AGENT_ARGS_FILE";
      "";
      "workflow:";
      "  /plugin-new --template python --id com.example.echo --tool-name \
       echo_json my-plugin";
      "  /plugin-dev --replace my-plugin";
      "  /plugin-package --output my-plugin.fp-plugin.tar.gz my-plugin";
      "  /plugin-run my-plugin echo_json \
       @my-plugin/examples/echo_json.args.json";
      "next: /plugin-new --template python <dir>";
      "next: /plugin-schema";
      "next: /plugin-doctor";
    ]

let plugin_schema_lines () =
  Yojson.Safe.pretty_to_string (Plugin.manifest_schema ()) |> String.split_lines

let plugin_matches query (plugin : Plugin.manifest) =
  String.equal plugin.id query
  || String.equal plugin.name query
  || List.exists plugin.tools ~f:(fun tool -> String.equal tool.tool_name query)

let plugin_detail_lines query =
  let query = String.strip query in
  if String.is_empty query then [ "usage: /plugin <plugin-id|tool-name>" ]
  else
    match List.find (Plugin.manifests ()) ~f:(plugin_matches query) with
    | None -> [ "no plugin or tool matching: " ^ query ]
    | Some plugin -> View.plugin_inspector_lines plugin

let current_model_lines ctx =
  [
    "provider: " ^ ctx.provider;
    "model: " ^ ctx.model;
    "api_base: " ^ ctx.api_base;
  ]

let preview_text ?(cols = 180) text =
  let flat = String.substr_replace_all text ~pattern:"\n" ~with_:" " in
  View.truncate ~cols (String.strip flat)

let flatten_line text = preview_text ~cols:10_000 text

let internal_user_message content =
  let content = String.strip content in
  List.exists
    [
      "[Code review preflight]";
      "Tool budget exhausted.";
      "Review budget exhausted.";
      "Your previous reply could not be processed";
      "That response is not a code review.";
    ] ~f:(fun prefix -> String.is_prefix content ~prefix)

let last_user_message events =
  List.find_map (List.rev events) ~f:(function
    | Event.User_message { content }
      when not (String.is_empty (String.strip content)) ->
        if internal_user_message content then None else Some content
    | _ -> None)

let protocol_label = function
  | Provider.Openai -> "openai"
  | Provider.Anthropic -> "anthropic"

let env_presence name =
  match Stdlib.Sys.getenv_opt name with
  | Some value when not (String.is_empty value) -> "set"
  | _ -> "missing"

let provider_auth_line provider_name =
  match Provider.of_string provider_name with
  | Some provider ->
      let key = Provider.key_env provider in
      let suffix =
        if Provider.requires_api_key provider then "" else " (optional)"
      in
      Printf.sprintf "    auth: %s=%s%s" key (env_presence key) suffix
  | None -> "    auth: custom config (api key hidden)"

let providers_lines ctx =
  match Config.available_providers () with
  | [] -> [ "(no configured providers; add providers to FP_AGENT_CONFIG)" ]
  | providers ->
      let provider_lines =
        List.concat_map providers
          ~f:(fun (entry : Config.provider_catalog_entry) ->
            let provider_mark =
              if String.equal entry.provider_name ctx.provider then "*" else " "
            in
            let models =
              match entry.provider_models with
              | [] -> "(no models configured)"
              | models -> String.concat models ~sep:", "
            in
            let active =
              if String.equal entry.provider_name ctx.provider then
                [ "    active_model: " ^ ctx.model ]
              else []
            in
            [
              Printf.sprintf "%s %s" provider_mark entry.provider_name;
              "    protocol: " ^ protocol_label entry.provider_protocol;
              "    api_base: " ^ entry.provider_api_base;
              provider_auth_line entry.provider_name;
              "    models: " ^ models;
            ]
            @ active)
      in
      ("Providers:" :: provider_lines)
      @ [
          "Use /provider <name> [model] to switch, /model <id> to switch by \
           model id, or /provider-add <name> <base-url> <model> to add one.";
        ]

let provider_config_file_lines (file : Config.provider_config_file_diagnostic) =
  let status =
    if file.config_exists then
      match file.config_error with None -> "ok" | Some _ -> "invalid"
    else "missing"
  in
  let provider_names =
    match file.config_provider_names with
    | [] -> []
    | names -> [ "      providers: " ^ String.concat names ~sep:", " ]
  in
  let error =
    match file.config_error with
    | None -> []
    | Some e -> [ "      error: " ^ e ]
  in
  (Printf.sprintf "  - %s [%s]" file.config_path status :: provider_names)
  @ error

let provider_profile_lines (profile : Config.custom_provider_diagnostic) =
  let status =
    match profile.custom_provider_error with
    | None -> "ok"
    | Some _ -> "invalid"
  in
  let protocol =
    Option.value_map profile.custom_provider_protocol ~default:"?"
      ~f:protocol_label
  in
  let api_base = Option.value profile.custom_provider_api_base ~default:"?" in
  let models =
    match profile.custom_provider_models with
    | [] -> "(no models configured)"
    | models -> String.concat models ~sep:", "
  in
  let default_model =
    Option.value profile.custom_provider_default_model ~default:"?"
  in
  let auth =
    if profile.custom_provider_has_api_key then "configured" else "empty"
  in
  let error =
    match profile.custom_provider_error with
    | None -> []
    | Some e -> [ "      error: " ^ e ]
  in
  [
    Printf.sprintf "  - %s [%s]" profile.custom_provider_name status;
    "      source: " ^ profile.custom_provider_path;
    "      protocol: " ^ protocol;
    "      api_base: " ^ api_base;
    "      auth: " ^ auth;
    "      default_model: " ^ default_model;
    "      models: " ^ models;
  ]
  @ error

let provider_catalog_lines (entry : Config.provider_catalog_entry) =
  let models =
    match entry.provider_models with
    | [] -> "(no models configured)"
    | models -> String.concat models ~sep:", "
  in
  [
    "  - " ^ entry.provider_name;
    "      protocol: " ^ protocol_label entry.provider_protocol;
    "      api_base: " ^ entry.provider_api_base;
    "      models: " ^ models;
  ]

let provider_diagnostics_lines () =
  let diagnostics = Config.provider_diagnostics () in
  let config_lines =
    match diagnostics.provider_config_files with
    | [] -> [ "  (no config paths)" ]
    | files -> List.concat_map files ~f:provider_config_file_lines
  in
  let custom_lines =
    match diagnostics.custom_provider_diagnostics with
    | [] -> [ "  (no custom provider profiles found)" ]
    | profiles -> List.concat_map profiles ~f:provider_profile_lines
  in
  let catalog_lines =
    match diagnostics.provider_catalog with
    | [] -> [ "  (no configured providers)" ]
    | entries -> List.concat_map entries ~f:provider_catalog_lines
  in
  [ "Provider diagnostics"; "config_paths:" ]
  @ config_lines
  @ [ ""; "custom_providers:" ]
  @ custom_lines
  @ [ ""; "provider_catalog:" ]
  @ catalog_lines
  @ [
      "";
      "next: /providers";
      "next: /models";
      "next: /provider <name> [model]";
      "next: /provider-add <name> <base-url> <model>";
    ]

let models_lines ctx =
  match Config.available_providers () with
  | [] -> [ "(no configured providers; add providers to FP_AGENT_CONFIG)" ]
  | providers ->
      let provider_lines =
        List.concat_map providers
          ~f:(fun (entry : Config.provider_catalog_entry) ->
            let provider_mark =
              if String.equal entry.provider_name ctx.provider then "*" else " "
            in
            let header =
              Printf.sprintf "%s %s @ %s" provider_mark entry.provider_name
                entry.provider_api_base
            in
            let models =
              match entry.provider_models with
              | [] -> [ "    (no models configured)" ]
              | models ->
                  List.map models ~f:(fun model ->
                      let model_mark =
                        if
                          String.equal entry.provider_name ctx.provider
                          && String.equal model ctx.model
                        then "*"
                        else " "
                      in
                      Printf.sprintf "    %s %s" model_mark model)
            in
            header :: models)
      in
      provider_lines
      @ [
          "Use /model <id> to switch by model id, /model-next to cycle the \
           current provider, or /provider <name> [model] to switch provider.";
        ]

let is_directory path =
  Stdlib.Sys.file_exists path
  &&
  match Stdlib.Sys.is_directory path with
  | true -> true
  | false -> false
  | exception _ -> false

let plan_label events =
  let line = View.plan_progress_line (View.plan_progress_of_events events) in
  Option.value (String.chop_prefix line ~prefix:"plan: ") ~default:line

let latest_turn_label events =
  List.rev events
  |> List.find_map ~f:(function
    | Event.Turn_completed { status; steps; summary = _ } ->
        Some (Printf.sprintf "%s/%d" (Event.turn_status_to_string status) steps)
    | _ -> None)
  |> Option.value ~default:"none"

let latest_workspace_label events =
  List.rev events
  |> List.find_map ~f:(function
    | Event.Workspace_snapshot { is_git = false; _ } -> Some "non-git"
    | Event.Workspace_snapshot { status = []; diff_stat = []; _ } ->
        Some "clean"
    | Event.Workspace_snapshot { status; diff_stat; _ } ->
        Some
          (Printf.sprintf "changed(%d/%d)" (List.length status)
             (List.length diff_stat))
    | _ -> None)
  |> Option.value ~default:"unknown"

let session_fork_label dir =
  match Session.read_meta dir with
  | None, None -> ""
  | parent, fork_at ->
      let parent =
        Option.value_map parent ~default:"" ~f:(fun p -> " parent=" ^ p)
      in
      let fork =
        Option.value_map fork_at ~default:"" ~f:(fun n ->
            Printf.sprintf " fork@%d" n)
      in
      parent ^ fork

let session_line ctx entry =
  let full = Stdlib.Filename.concat ctx.sessions_root entry in
  if not (is_directory full) then None
  else
    let mark = if String.equal full ctx.session_dir then "*" else " " in
    let fork = session_fork_label full in
    match Journal.read ~session_dir:full with
    | Error e ->
        Some
          (Printf.sprintf "  %s %s events=? read_error=%s%s" mark entry
             (preview_text ~cols:80 e) fork)
    | Ok events ->
        let last =
          last_user_message events
          |> Option.value_map ~default:"(none)" ~f:(preview_text ~cols:80)
        in
        Some
          (Printf.sprintf
             "  %s %s events=%d plan=%s turn=%s workspace=%s last=%s%s" mark
             entry (List.length events) (plan_label events)
             (latest_turn_label events)
             (latest_workspace_label events)
             last fork)

let sessions_lines ctx =
  match Stdlib.Sys.readdir ctx.sessions_root with
  | exception _ -> [ "(no sessions yet)" ]
  | entries ->
      Array.sort entries ~compare:String.compare;
      Array.to_list entries |> List.filter_map ~f:(session_line ctx)
      |> fun lines ->
      if List.is_empty lines then [ "(no sessions yet)" ] else lines

let tree_lines ctx =
  match Stdlib.Sys.readdir ctx.sessions_root with
  | exception _ -> [ "(no sessions yet)" ]
  | names ->
      Array.sort names ~compare:String.compare;
      let metas =
        Array.to_list names
        |> List.map ~f:(fun name ->
            ( name,
              Session.read_meta (Stdlib.Filename.concat ctx.sessions_root name)
            ))
      in
      let current_name = Stdlib.Filename.basename ctx.session_dir in
      let children_of parent =
        List.filter_map metas ~f:(fun (name, (parent_opt, fork_at)) ->
            if Option.equal String.equal parent_opt (Some parent) then
              Some (name, fork_at)
            else None)
      in
      let rec render indent name fork_at =
        let mark = if String.equal name current_name then "  *" else "" in
        let fork =
          match fork_at with
          | Some index -> Printf.sprintf " (fork@%d)" index
          | None -> ""
        in
        let line = indent ^ name ^ fork ^ mark in
        line
        :: List.concat_map (children_of name) ~f:(fun (child, child_fork_at) ->
            render (indent ^ "  ") child child_fork_at)
      in
      let roots =
        List.filter_map metas ~f:(fun (name, (parent_opt, fork_at)) ->
            if Option.is_none parent_opt then Some (name, fork_at) else None)
      in
      if List.is_empty roots then [ "(no session tree)" ]
      else
        List.concat_map roots ~f:(fun (name, fork_at) -> render "" name fork_at)

let diff_lines ctx =
  let git_dir = Stdlib.Filename.concat ctx.workspace_root ".git" in
  if not (Stdlib.Sys.file_exists git_dir) then
    [ "(workspace is not a git repo)" ]
  else
    let quote = Stdlib.Filename.quote in
    let root = quote ctx.workspace_root in
    let exclude = "':(exclude).ocaml-agent'" in
    let diff =
      shell_lines
        ~command:(Printf.sprintf "git -C %s diff -- . %s" root exclude)
    in
    let tracked =
      match diff with [] -> [ "(no tracked changes)" ] | lines -> lines
    in
    let untracked =
      shell_lines
        ~command:
          (Printf.sprintf
             "git -C %s ls-files --others --exclude-standard -- . %s" root
             exclude)
    in
    match untracked with
    | [] -> tracked
    | files -> tracked @ ("untracked:" :: files)

let diff_summary_lines ctx =
  let git_dir = Stdlib.Filename.concat ctx.workspace_root ".git" in
  if not (Stdlib.Sys.file_exists git_dir) then
    [ "(workspace is not a git repo)" ]
  else
    let quote = Stdlib.Filename.quote in
    let root = quote ctx.workspace_root in
    let exclude = "':(exclude).ocaml-agent'" in
    let status =
      shell_lines
        ~command:
          (Printf.sprintf "git -C %s status --short -- . %s" root exclude)
    in
    let stat =
      shell_lines
        ~command:(Printf.sprintf "git -C %s diff --stat -- . %s" root exclude)
    in
    match (status, stat) with
    | [], [] -> [ "(clean)" ]
    | status, [] -> status
    | [], stat -> stat
    | status, stat -> status @ [ ""; "diff --stat:" ] @ stat

let log_lines ctx =
  match ctx.events with
  | [] -> [ "(no events yet)" ]
  | events ->
      List.mapi events ~f:(fun index event ->
          Printf.sprintf "  %3d  %s" index (View.event_summary event))

let inspect_lines ctx arg =
  let selected =
    let arg = String.strip arg in
    if String.is_empty arg then Ok ctx.selected_event_index
    else
      try
        let index = Int.of_string arg in
        if index < 0 then Error "usage: /inspect [event-index]"
        else Ok (Some index)
      with _ -> Error "usage: /inspect [event-index]"
  in
  match (selected, ctx.events) with
  | Error e, _ -> [ e ]
  | _, [] -> [ "(no events yet)" ]
  | Ok None, _ -> [ "(no selected event)" ]
  | Ok (Some index), events -> (
      match List.nth events index with
      | None ->
          [
            Printf.sprintf "no event at index %d (0..%d)" index
              (List.length events - 1);
          ]
      | Some event ->
          Printf.sprintf "event %d" index :: View.event_inspector_lines event)

let usage_lines ctx =
  let usage = View.token_usage_of_events ctx.events in
  [
    Printf.sprintf "input_tokens: %d" usage.input_tokens;
    Printf.sprintf "output_tokens: %d" usage.output_tokens;
    Printf.sprintf "total_tokens: %d" (View.token_usage_total usage);
  ]

let content_kind = function
  | Llm.Text _ -> "text"
  | Llm.Thinking _ -> "thinking"
  | Llm.Tool_use _ -> "tool_use"
  | Llm.Tool_result _ -> "tool_result"

let content_preview_lines indent = function
  | Llm.Text text -> [ indent ^ "text: " ^ preview_text text ]
  | Llm.Thinking { text; signature } ->
      let signature =
        if String.is_empty signature then "" else " signature=yes"
      in
      [ indent ^ "thinking" ^ signature ^ ": " ^ preview_text text ]
  | Llm.Tool_use { id; name; input } ->
      [
        Printf.sprintf "%stool_use: %s id=%s" indent name id;
        indent ^ "args: " ^ preview_text (Yojson.Safe.to_string input);
      ]
  | Llm.Tool_result { id; content } ->
      [
        Printf.sprintf "%stool_result: id=%s" indent id;
        indent ^ "output: " ^ preview_text content;
      ]

let role_label = function Llm.User -> "user" | Llm.Assistant -> "assistant"

let turn_preview_lines index (turn : Llm.turn) =
  let role = role_label turn.role in
  let kinds =
    List.map turn.content ~f:content_kind |> String.concat ~sep:", "
  in
  let header = Printf.sprintf "  %d. %s (%s)" (index + 1) role kinds in
  header :: List.concat_map turn.content ~f:(content_preview_lines "     ")

let instruction_state workspace_root =
  match Workspace.create ~root:workspace_root with
  | Error _ -> "unavailable"
  | Ok workspace -> (
      match Project_instructions.load workspace with
      | None -> "none"
      | Some instructions ->
          Printf.sprintf "loaded (%d chars)" (String.length instructions))

let context_lines ctx =
  let state = Session_state.replay ctx.events in
  let turns = Session_state.turns state in
  let usage = View.token_usage_of_events ctx.events in
  let compactions =
    List.count ctx.events ~f:(function
      | Event.Context_compacted _ -> true
      | _ -> false)
  in
  let role_counts role =
    List.count turns ~f:(fun (turn : Llm.turn) -> Poly.equal turn.role role)
  in
  let header =
    [
      "Model context preview";
      Printf.sprintf "events: %d" (List.length ctx.events);
      Printf.sprintf "replayed_turns: %d" (List.length turns);
      Printf.sprintf "user_turns: %d" (role_counts Llm.User);
      Printf.sprintf "assistant_turns: %d" (role_counts Llm.Assistant);
      "agent_state: " ^ Agent_state.to_string (Session_state.agent_state state);
      Printf.sprintf "steps: %d" (Session_state.steps state);
      Printf.sprintf "compactions: %d" compactions;
      Printf.sprintf "tokens: input %d output %d total %d" usage.input_tokens
        usage.output_tokens
        (View.token_usage_total usage);
      "project_instructions: " ^ instruction_state ctx.workspace_root;
      "note: this previews the replayed conversation history; the next model \
       call also prepends the task-specific system prompt.";
      "";
    ]
  in
  match turns with
  | [] -> header @ [ "(no replayed turns yet)" ]
  | turns ->
      let turn_lines = List.mapi turns ~f:turn_preview_lines |> List.concat in
      header @ ("Replay turns:" :: turn_lines)

let latest_plan events =
  List.find_map (List.rev events) ~f:(function
    | Event.Plan_updated { items } -> Some items
    | _ -> None)

let plan_lines_of_items items =
  match items with
  | [] -> [ "(plan is empty)" ]
  | items ->
      "Session plan:"
      :: List.mapi items ~f:(fun index item ->
          Printf.sprintf "  %d. %s" (index + 1) (Event.plan_item_line item))

let plan_lines events =
  match latest_plan events with
  | None ->
      [
        "(no session plan)";
        "Use /plan-set todo inspect code; doing implement fix; done write tests";
      ]
  | Some items -> plan_lines_of_items items

let status_lines ctx =
  Tool_loader.register_all ();
  let discovery = Plugin.discover () in
  let usage = View.token_usage_of_events ctx.events in
  let plan = View.plan_progress_of_events ctx.events in
  let conflicts = Plugin.tool_conflicts () in
  let project_instructions =
    match Workspace.create ~root:ctx.workspace_root with
    | Error _ -> "unavailable"
    | Ok workspace -> (
        match Project_instructions.load workspace with
        | None -> "none"
        | Some _ -> "loaded")
  in
  [
    "workspace: " ^ ctx.workspace_root;
    "session: " ^ Stdlib.Filename.basename ctx.session_dir;
    "session_dir: " ^ ctx.session_dir;
    "provider: " ^ ctx.provider;
    "model: " ^ ctx.model;
    "api_base: " ^ ctx.api_base;
    Printf.sprintf "events: %d" (List.length ctx.events);
    Printf.sprintf "tokens: input %d output %d total %d" usage.input_tokens
      usage.output_tokens
      (View.token_usage_total usage);
    View.plan_progress_line plan;
    Printf.sprintf "plugins: %d valid / %d invalid / %d conflicts"
      (List.length discovery.manifests)
      (List.length discovery.errors)
      (List.length conflicts);
    "project_instructions: " ^ project_instructions;
    Printf.sprintf "tools: %d" (List.length (Tool.all ()));
  ]

let handoff_lines ctx =
  let usage = View.token_usage_of_events ctx.events in
  let plan = View.plan_progress_of_events ctx.events in
  let quoted_session = Stdlib.Filename.quote ctx.session_dir in
  let last_task =
    last_user_message ctx.events
    |> Option.value_map ~default:"(none)" ~f:(fun task ->
        View.truncate ~cols:160 (flatten_line task))
  in
  let current_plan =
    match latest_plan ctx.events with
    | None -> [ "  (no session plan)" ]
    | Some [] -> [ "  (plan is empty)" ]
    | Some items ->
        List.mapi items ~f:(fun index item ->
            Printf.sprintf "  %d. %s" (index + 1) (Event.plan_item_line item))
  in
  let recent_events =
    match ctx.events with
    | [] -> [ "  (no events yet)" ]
    | events ->
        List.mapi events ~f:(fun index event ->
            Printf.sprintf "  %3d  %s" index (View.event_summary event))
        |> View.window ~rows:8
  in
  let diff = List.map (diff_summary_lines ctx) ~f:(( ^ ) "  ") in
  [
    "Session handoff";
    "workspace: " ^ ctx.workspace_root;
    "session: " ^ Stdlib.Filename.basename ctx.session_dir;
    "session_dir: " ^ ctx.session_dir;
    "resume: dune exec -- fp-agent --resume " ^ quoted_session;
    "tui_resume: dune exec -- fp-agent --tui --resume " ^ quoted_session;
    "provider: " ^ ctx.provider;
    "model: " ^ ctx.model;
    "api_base: " ^ ctx.api_base;
    Printf.sprintf "events: %d" (List.length ctx.events);
    Printf.sprintf "tokens: input %d output %d total %d" usage.input_tokens
      usage.output_tokens
      (View.token_usage_total usage);
    View.plan_progress_line plan;
    "last_user_task: " ^ last_task;
    "";
    "Current plan:";
  ]
  @ current_plan
  @ ("" :: "Recent events:" :: recent_events)
  @ [ ""; "Workspace diff:" ] @ diff

let instruction_lines ctx =
  match Workspace.create ~root:ctx.workspace_root with
  | Error e -> [ "workspace error: " ^ e ]
  | Ok workspace -> (
      match Project_instructions.load workspace with
      | None ->
          [
            "(no project instruction files found)";
            "Checked AGENTS.md, CLAUDE.md, and .fp-agent/instructions.md.";
          ]
      | Some instructions -> lines_of_text instructions)

let run ctx command =
  let open Shell_command in
  match parse command with
  | Command (Help, _) ->
      Some (command_section command (String.split_lines (help_text ())))
  | Command (Tools, _) -> Some (command_section command (tools_lines ()))
  | Command (Tool, arg) ->
      Some (command_section command (tool_detail_lines arg))
  | Command (Plugins, _) -> Some (command_section command (plugins_lines ()))
  | Command (Plugin, arg) ->
      Some (command_section command (plugin_detail_lines arg))
  | Command (PluginDoctor, _) ->
      Some (command_section command (plugin_diagnostics_lines ()))
  | Command (PluginSdk, _) ->
      Some (command_section command (plugin_sdk_lines ()))
  | Command (PluginSchema, _) ->
      Some (command_section command (plugin_schema_lines ()))
  | Command (Sessions, _) -> Some (command_section command (sessions_lines ctx))
  | Command (Tree, _) -> Some (command_section command (tree_lines ctx))
  | Command (Model, "") ->
      Some (command_section command (current_model_lines ctx))
  | Command (Models, _) -> Some (command_section command (models_lines ctx))
  | Command (Providers, _) ->
      Some (command_section command (providers_lines ctx))
  | Command (ProviderDoctor, _) ->
      Some (command_section command (provider_diagnostics_lines ()))
  | Command (Diff, _) -> Some (command_section command (diff_lines ctx))
  | Command (Log, _) -> Some (command_section command (log_lines ctx))
  | Command (Inspect, arg) ->
      Some (command_section command (inspect_lines ctx arg))
  | Command (Plan, _) -> Some (command_section command (plan_lines ctx.events))
  | Command (Usage, _) -> Some (command_section command (usage_lines ctx))
  | Command (Status, _) -> Some (command_section command (status_lines ctx))
  | Command (Context, _) -> Some (command_section command (context_lines ctx))
  | Command (Handoff, _) -> Some (command_section command (handoff_lines ctx))
  | Command (Instructions, _) ->
      Some (command_section command (instruction_lines ctx))
  | Empty | Task _ | Unknown _ | Command _ -> None
