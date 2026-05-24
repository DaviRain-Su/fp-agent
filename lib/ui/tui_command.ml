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
  "Available tools:"
  :: List.map (Tool.all ()) ~f:(fun (tool : Tool.t) ->
      Printf.sprintf "  %-18s %-5s %s" tool.name
        (tool_kind_label tool.kind)
        tool.description)

let tool_detail_lines query =
  Tool_loader.register_all ();
  let query = String.strip query in
  if String.is_empty query then [ "usage: /tool <tool-name>" ]
  else
    match Tool.find query with
    | None -> [ "no tool matching: " ^ query ]
    | Some tool -> View.tool_inspector_lines tool

let plugins_lines () =
  match Plugin.manifests () with
  | [] -> [ "(no plugins discovered)" ]
  | manifests ->
      List.concat_map manifests ~f:(fun (plugin : Plugin.manifest) ->
          Printf.sprintf "%s %s (%s)" plugin.id plugin.name plugin.version
          :: ("  " ^ plugin.dir)
          :: List.map plugin.tools ~f:(fun tool ->
              Printf.sprintf "  - %-18s %-5s %s" tool.tool_name
                (tool_kind_label tool.tool_kind)
                tool.tool_description)
          @ [ "" ])
      |> List.drop_last |> Option.value ~default:[]

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
          "Use /provider <name> [model] to switch provider, or /model <id> \
           within the current provider.";
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
  | Command (Sessions, _) -> Some (command_section command (sessions_lines ctx))
  | Command (Tree, _) -> Some (command_section command (tree_lines ctx))
  | Command (Model, "") ->
      Some (command_section command (current_model_lines ctx))
  | Command (Models, _) -> Some (command_section command (models_lines ctx))
  | Command (Diff, _) -> Some (command_section command (diff_lines ctx))
  | Command (Log, _) -> Some (command_section command (log_lines ctx))
  | Command (Inspect, arg) ->
      Some (command_section command (inspect_lines ctx arg))
  | Command (Usage, _) -> Some (command_section command (usage_lines ctx))
  | Empty | Task _ | Unknown _ | Command _ -> None
