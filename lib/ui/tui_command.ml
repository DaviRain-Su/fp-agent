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
            :: List.map plugin.tools ~f:(fun tool ->
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
      "next: /plugin-check <dir>";
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
      "  /plugin-run my-plugin echo_json \
       @my-plugin/examples/echo_json.args.json";
      "next: /plugin-new --template python <dir>";
      "next: /plugin-doctor";
    ]

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

let sessions_lines ctx =
  match Stdlib.Sys.readdir ctx.sessions_root with
  | exception _ -> [ "(no sessions yet)" ]
  | entries ->
      Array.sort entries ~compare:String.compare;
      Array.to_list entries
      |> List.map ~f:(fun entry ->
          let full = Stdlib.Filename.concat ctx.sessions_root entry in
          let mark = if String.equal full ctx.session_dir then "  *" else "" in
          "  " ^ entry ^ mark)

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

let last_user_message events =
  List.find_map (List.rev events) ~f:(function
    | Event.User_message { content }
      when not (String.is_empty (String.strip content)) ->
        Some content
    | _ -> None)

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
  | Command (Sessions, _) -> Some (command_section command (sessions_lines ctx))
  | Command (Tree, _) -> Some (command_section command (tree_lines ctx))
  | Command (Model, "") ->
      Some (command_section command (current_model_lines ctx))
  | Command (Models, _) -> Some (command_section command (models_lines ctx))
  | Command (Diff, _) -> Some (command_section command (diff_lines ctx))
  | Command (Log, _) -> Some (command_section command (log_lines ctx))
  | Command (Inspect, arg) ->
      Some (command_section command (inspect_lines ctx arg))
  | Command (Plan, _) -> Some (command_section command (plan_lines ctx.events))
  | Command (Usage, _) -> Some (command_section command (usage_lines ctx))
  | Command (Status, _) -> Some (command_section command (status_lines ctx))
  | Command (Instructions, _) ->
      Some (command_section command (instruction_lines ctx))
  | Empty | Task _ | Unknown _ | Command _ -> None
