open! Base
open Fp_agent

let print_summary (outcome : Agent_loop.outcome) =
  Stdlib.print_newline ();
  Stdlib.Printf.printf "=== %s (after %d step(s)) ===\n"
    (String.uppercase (Agent_loop.status_to_string outcome.status))
    outcome.steps;
  Stdlib.print_endline outcome.summary

(* Show what changed: prefer git diff --stat when the workspace is a repo. *)
let print_changes root =
  let git_dir = Stdlib.Filename.concat root ".git" in
  if Stdlib.Sys.file_exists git_dir then
    match
      Shell.run
        ~command:
          (Printf.sprintf "git -C %s diff --stat" (Stdlib.Filename.quote root))
        ~timeout_sec:30
    with
    | Ok { stdout; _ } when not (String.is_empty (String.strip stdout)) ->
        Stdlib.print_newline ();
        Stdlib.print_endline "--- git diff --stat ---";
        Stdlib.print_string stdout
    | _ -> ()

(* A reporter renders the run: [on_event] handles each logged event, [tick] is
   called on a timer to animate a spinner while waiting, and [close] tears
   down. *)
type reporter = {
  on_event : Event.t -> unit;
  tick : unit -> unit;
  close : unit -> unit;
}

type tui_view = {
  reporter : reporter;
  append_lines : string list -> unit;
  take_submitted : unit -> string option;
  set_runtime : provider:string -> model:string -> api_base:string -> unit;
  set_session : session_dir:string -> events:Event.t list -> unit;
  refresh_tooling : unit -> Tool_loader.counts;
  request_approval : Tool_call.t -> string -> bool Lwt.t;
}

type approval_request = {
  tool_call : Tool_call.t;
  reason : string;
  resolver : bool Lwt.u;
}

let spinner_frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let now () = Unix.gettimeofday ()

(* Current phase, derived from state transitions and tool calls, with the time
   it started so the spinner can show elapsed seconds. *)
let make_phase () = (ref `Idle, ref (now ()))

let update_phase (phase, t0) (e : Event.t) =
  match e with
  | State_transition { to_state = Agent_state.Waiting_for_model; _ } ->
      phase := `Thinking;
      t0 := now ()
  | Tool_call tc ->
      phase := `Running (Event.describe_tool tc);
      t0 := now ()
  | Model_response { action = Model_action.Tool_calls calls } ->
      phase := `Running (Printf.sprintf "%d tools" (List.length calls));
      t0 := now ()
  | Model_response { action = Model_action.Final_answer _ } -> phase := `Idle
  | Assistant_message { content; _ } -> (
      match Llm.tool_uses content with
      | _ :: _ as calls ->
          phase := `Running (Printf.sprintf "%d tools" (List.length calls));
          t0 := now ()
      | [] -> phase := `Idle)
  | _ -> ()

let phase_label = function
  | `Idle -> None
  | `Thinking -> Some "thinking…"
  | `Running tool -> Some (Printf.sprintf "running %s…" tool)

let event_display_lines events = List.filter_map events ~f:Event.to_display

let tooling_counts_line (counts : Tool_loader.counts) =
  Printf.sprintf "tooling: %d plugin(s), %d tool(s)" counts.plugins counts.tools

let tools_reloaded_line counts = "tools reloaded; " ^ tooling_counts_line counts

let next_model_id ~current models =
  match models with
  | [] -> `No_models
  | [ only ] ->
      if String.equal only current then `Only_model only else `Next_model only
  | models -> (
      match
        List.findi models ~f:(fun _ model -> String.equal model current)
      with
      | None -> `Next_model (Option.value_exn (List.hd models))
      | Some (index, _) ->
          let next_index = (index + 1) % List.length models in
          `Next_model (Option.value_exn (List.nth models next_index)))

let model_provider_matches model =
  Config.available_providers ()
  |> List.filter ~f:(fun (entry : Config.provider_catalog_entry) ->
      List.mem entry.provider_models model ~equal:String.equal)

let resolve_model_provider ~current_provider model =
  let matches = model_provider_matches model in
  if
    List.exists matches ~f:(fun entry ->
        String.equal entry.provider_name current_provider)
  then `Current_provider
  else
    match matches with
    | [] -> `Current_provider
    | [ entry ] -> `Provider entry.provider_name
    | entries ->
        `Ambiguous (List.map entries ~f:(fun entry -> entry.provider_name))

(* Plain reporter: step lines + an animated spinner line on stderr. The spinner
   line is rewritten in place with CR + clear-to-EOL. *)
let make_plain_reporter () =
  let phase = make_phase () in
  let i = ref 0 in
  let streaming = ref false in
  let clear () = Stdlib.Printf.eprintf "\r\027[K" in
  let finish_stream_if_needed () =
    if !streaming then (
      Stdlib.Printf.eprintf "\n%!";
      streaming := false)
  in
  let on_event e =
    update_phase phase e;
    match e with
    | Model_delta { content } ->
        if not !streaming then clear ();
        streaming := true;
        Stdlib.Printf.eprintf "%s%!" content
    | _ -> (
        match Event.to_display e with
        | Some line ->
            finish_stream_if_needed ();
            clear ();
            Stdlib.Printf.eprintf "%s\n%!" line
        | None -> ())
  in
  let tick () =
    if !streaming then ()
    else
      match phase_label !(fst phase) with
      | None -> ()
      | Some label ->
          let frame = spinner_frames.(!i % Array.length spinner_frames) in
          Int.incr i;
          Stdlib.Printf.eprintf "\r\027[K%s %s (%.0fs)%!" frame label
            (now () -. !(snd phase))
  in
  let close () =
    clear ();
    Stdlib.Out_channel.flush Stdlib.stderr
  in
  { on_event; tick; close }

(* Full-screen view: header, status strip, timeline, optional inspector, and a
   footer phase line. *)
let make_tui_view ~initial_events ~provider ~model ~api_base ~workspace_root
    ~session_dir ~sessions_root ~header =
  let module I = Notty.I in
  let module A = Notty.A in
  let term = Notty_unix.Term.create () in
  let provider_ref = ref provider in
  let model_ref = ref model in
  let api_base_ref = ref api_base in
  let session_ref = ref session_dir in
  let plugin_count = ref 0 in
  let tool_count = ref 0 in
  let refresh_tooling () =
    let counts = Tool_loader.refresh_counts () in
    plugin_count := counts.plugins;
    tool_count := counts.tools;
    counts
  in
  ignore (refresh_tooling ());
  let lines = ref (event_display_lines initial_events) in
  let current_delta = ref "" in
  let phase = make_phase () in
  let events = ref initial_events in
  let shell =
    ref
      (Tui_shell.create ()
      |> Tui_shell.set_history (Tui_shell.history_of_events initial_events))
  in
  let approval = ref None in
  let submitted = ref [] in
  let i = ref 0 in
  let visible_lines () =
    if String.is_empty !current_delta then !lines
    else !lines @ View.display_lines !current_delta
  in
  let flush_delta () =
    if not (String.is_empty !current_delta) then (
      lines := !lines @ View.display_lines !current_delta;
      current_delta := "")
  in
  let append_lines new_lines =
    if not (List.is_empty new_lines) then (
      flush_delta ();
      lines := !lines @ new_lines)
  in
  let take_submitted () =
    match !submitted with
    | [] -> None
    | prompt :: rest ->
        submitted := rest;
        Some prompt
  in
  let set_runtime ~provider ~model ~api_base =
    provider_ref := provider;
    model_ref := model;
    api_base_ref := api_base
  in
  let set_session ~session_dir ~events:new_events =
    session_ref := session_dir;
    events := new_events;
    lines := event_display_lines new_events;
    current_delta := "";
    shell :=
      !shell
      |> Tui_shell.set_history (Tui_shell.history_of_events new_events)
      |> Tui_shell.set_event_count (List.length new_events)
  in
  let request_approval tool_call reason =
    match !approval with
    | Some _ ->
        append_lines
          [ "[tui] approval busy; denying: " ^ Event.describe_tool tool_call ];
        Lwt.return false
    | None ->
        let promise, resolver = Lwt.wait () in
        approval := Some { tool_call; reason; resolver };
        append_lines
          [
            "[tui] approval required: " ^ reason;
            "  " ^ Event.describe_tool tool_call;
          ];
        promise
  in
  let resolve_approval approved =
    match !approval with
    | None -> ()
    | Some request ->
        approval := None;
        Lwt.wakeup_later request.resolver approved;
        append_lines
          [
            (if approved then "[tui] approved: " else "[tui] denied: ")
            ^ Event.describe_tool request.tool_call;
          ]
  in
  let has_key_modifier mods modifier =
    List.mem mods modifier ~equal:Poly.equal
  in
  let text_of_uchar uchar =
    let add_byte buffer byte = Buffer.add_char buffer (Stdlib.Char.chr byte) in
    let code = Stdlib.Uchar.to_int uchar in
    let buffer = Buffer.create 4 in
    if code <= 0x7F then add_byte buffer code
    else if code <= 0x7FF then (
      add_byte buffer (0xC0 lor (code lsr 6));
      add_byte buffer (0x80 lor (code land 0x3F)))
    else if code <= 0xFFFF then (
      add_byte buffer (0xE0 lor (code lsr 12));
      add_byte buffer (0x80 lor ((code lsr 6) land 0x3F));
      add_byte buffer (0x80 lor (code land 0x3F)))
    else (
      add_byte buffer (0xF0 lor (code lsr 18));
      add_byte buffer (0x80 lor ((code lsr 12) land 0x3F));
      add_byte buffer (0x80 lor ((code lsr 6) land 0x3F));
      add_byte buffer (0x80 lor (code land 0x3F)));
    Buffer.contents buffer
  in
  let palette_or_empty_prompt () =
    Tui_shell.palette_open !shell || View.prompt_is_empty !shell.draft
  in
  let input_of_term_event = function
    | `Key (`Escape, _) -> Some Tui_shell.Escape
    | `Key (`Enter, mods) ->
        if has_key_modifier mods `Ctrl then Some Tui_shell.Ctrl_enter
        else if has_key_modifier mods `Shift then Some Tui_shell.Shift_enter
        else Some Tui_shell.Enter
    | `Key (`Backspace, _) -> Some Tui_shell.Backspace_key
    | `Key (`Delete, _) -> Some Tui_shell.Delete_key
    | `Key (`Arrow `Up, mods) when has_key_modifier mods `Ctrl ->
        Some Tui_shell.Ctrl_up
    | `Key (`Arrow `Down, mods) when has_key_modifier mods `Ctrl ->
        Some Tui_shell.Ctrl_down
    | `Key (`Arrow `Left, _) -> Some Tui_shell.Left
    | `Key (`Arrow `Right, _) -> Some Tui_shell.Right
    | `Key (`Arrow `Up, _) -> Some Tui_shell.Up
    | `Key (`Arrow `Down, _) -> Some Tui_shell.Down
    | `Key (`Page `Up, _) -> Some Tui_shell.Page_up
    | `Key (`Page `Down, _) -> Some Tui_shell.Page_down
    | `Key (`Home, _) -> Some Tui_shell.Home
    | `Key (`End, _) -> Some Tui_shell.End
    | `Key (`ASCII '/', mods)
      when (not (has_key_modifier mods `Ctrl)) && palette_or_empty_prompt () ->
        Some Tui_shell.Slash
    | `Key (`ASCII '?', mods)
      when (not (has_key_modifier mods `Ctrl)) && palette_or_empty_prompt () ->
        Some Tui_shell.Question
    | `Key (`ASCII c, mods) when not (has_key_modifier mods `Ctrl) ->
        Some (Tui_shell.Text (String.make 1 c))
    | `Key (`Uchar uchar, mods) when not (has_key_modifier mods `Ctrl) ->
        Some (Tui_shell.Text (text_of_uchar uchar))
    | `Mouse (`Press (`Scroll `Up), _, _) -> Some Tui_shell.Mouse_scroll_up
    | `Mouse (`Press (`Scroll `Down), _, _) -> Some Tui_shell.Mouse_scroll_down
    | `Resize _ | `End | `Paste _ | `Key _ | `Mouse _ -> None
  in
  let drain_input ~page_size =
    let apply input =
      match !approval with
      | Some _ -> (
          match Tui_shell.approval_decision_of_input input with
          | None -> ()
          | Some Tui_shell.Approve -> resolve_approval true
          | Some Tui_shell.Deny -> resolve_approval false)
      | None -> (
          let result = Tui_shell.handle_input ~page_size !shell input in
          shell := result.state;
          (match result.submitted with
          | None -> ()
          | Some prompt -> submitted := !submitted @ [ prompt ]);
          let feedback =
            match result.dispatched_command with
            | None -> Tui_shell.feedback_lines result
            | Some command ->
                let context : Tui_command.context =
                  {
                    provider = !provider_ref;
                    model = !model_ref;
                    api_base = !api_base_ref;
                    workspace_root;
                    sessions_root;
                    session_dir = !session_ref;
                    events = !events;
                    selected_event_index =
                      Tui_shell.selected_event_index result.state;
                  }
                in
                Option.value
                  (Tui_command.run context command)
                  ~default:(Tui_shell.feedback_lines result)
          in
          match feedback with [] -> () | feedback -> append_lines feedback)
    in
    let rec loop () =
      if Notty_unix.Term.pending term then (
        (match input_of_term_event (Notty_unix.Term.event term) with
        | None -> ()
        | Some input -> apply input);
        loop ())
    in
    loop ()
  in
  let redraw () =
    let w, h = Notty_unix.Term.size term in
    let body_rows = Int.max 1 (h - 4) in
    shell := Tui_shell.set_event_count (List.length !events) !shell;
    drain_input ~page_size:body_rows;
    let event_count = List.length !events in
    let shell_state = Tui_shell.set_event_count event_count !shell in
    shell := shell_state;
    let selected_event =
      Option.bind
        (Tui_shell.selected_event_index shell_state)
        ~f:(List.nth !events)
    in
    let selection_label = Tui_shell.selection_label shell_state in
    let palette_index = Tui_shell.selected_command_index shell_state in
    let palette_label = Tui_shell.palette_label shell_state in
    let palette_lines =
      if Tui_shell.palette_open shell_state then
        Some
          (View.command_palette_lines
             ?query:(Tui_shell.palette_query shell_state)
             ~selected:palette_index
             (Tui_shell.visible_command_entries shell_state))
      else None
    in
    let approval_lines =
      Option.map !approval ~f:(fun request ->
          View.approval_prompt_lines request.tool_call ~reason:request.reason)
    in
    let prompt_lines =
      if View.prompt_is_empty shell_state.draft then []
      else "" :: View.prompt_editor_lines shell_state.draft
    in
    let phase_text = phase_label !(fst phase) in
    let status : View.status =
      {
        provider = !provider_ref;
        model = !model_ref;
        session = Stdlib.Filename.basename !session_ref;
        phase = phase_text;
        events = event_count;
        usage = View.token_usage_of_events !events;
        plugins = !plugin_count;
        tools = !tool_count;
      }
    in
    let colored ~cols s =
      let attr =
        match View.classify s with
        | `Ok -> A.fg A.green
        | `Err -> A.fg A.red
        | `Action -> A.fg A.yellow
        | `Plain -> A.empty
      in
      I.string attr (View.pad_right ~cols s)
    in
    let row_at rows i = Option.value (List.nth rows i) ~default:"" in
    let body =
      match View.split_panes ~width:w with
      | None ->
          let shown =
            View.viewport ~rows:body_rows ~cols:w
              (Option.value approval_lines
                 ~default:
                   (Option.value palette_lines
                      ~default:(visible_lines () @ prompt_lines)))
          in
          I.vcat
            (List.init body_rows ~f:(fun idx ->
                 colored ~cols:w (row_at shown idx)))
      | Some panes ->
          let timeline =
            View.viewport ~rows:body_rows ~cols:panes.timeline_cols
              (visible_lines ())
          in
          let inspector =
            (match approval_lines with
              | Some lines -> lines
              | None -> (
                  match palette_lines with
                  | Some lines -> lines
                  | None ->
                      let event_lines =
                        match selected_event with
                        | None ->
                            View.inspector_lines status
                              ~focus_label:("Selected event: " ^ selection_label)
                              ~last_event:"waiting for first event"
                        | Some event ->
                            View.inspector_lines status
                              ~focus_label:("Selected event: " ^ selection_label)
                              ~last_event:(View.event_summary event)
                            @ ("" :: View.event_inspector_lines event)
                      in
                      prompt_lines @ event_lines))
            |> View.viewport ~rows:body_rows ~cols:panes.inspector_cols
          in
          I.vcat
            (List.init body_rows ~f:(fun idx ->
                 I.hcat
                   [
                     colored ~cols:panes.timeline_cols (row_at timeline idx);
                     I.string A.(fg (gray 8)) " │ ";
                     I.string
                       A.(fg (gray 14))
                       (View.pad_right ~cols:panes.inspector_cols
                          (row_at inspector idx));
                   ]))
    in
    let header_img =
      I.string A.(fg lightblue ++ st bold) (View.truncate ~cols:w header)
    in
    let status_img =
      I.string
        A.(fg (gray 14))
        (View.truncate ~cols:w (View.status_line status))
    in
    let rule = I.string A.(fg lightblue) (String.make (Int.max 1 w) '-') in
    let footer =
      let hint =
        if Option.is_some !approval then
          "approval required | Y approve | N/Esc deny"
        else if Tui_shell.palette_open shell_state then
          Printf.sprintf "%s | type filter | up/down choose | Esc close"
            palette_label
        else
          Printf.sprintf "%s | / palette | up/down inspect | End latest"
            selection_label
      in
      match phase_text with
      | None ->
          I.string A.(fg (gray 12)) (View.truncate ~cols:w ("done | " ^ hint))
      | Some label ->
          let frame = spinner_frames.(!i % Array.length spinner_frames) in
          I.string
            A.(fg cyan)
            (View.truncate ~cols:w
               (Printf.sprintf "%s %s (%.0fs) | %s" frame label
                  (now () -. !(snd phase))
                  hint))
    in
    Notty_unix.Term.image term
      (I.vcat [ header_img; status_img; rule; body; footer ])
  in
  redraw ();
  let on_event e =
    events := !events @ [ e ];
    shell := Tui_shell.set_event_count (List.length !events) !shell;
    update_phase phase e;
    (match e with
    | Model_delta { content } -> current_delta := !current_delta ^ content
    | Assistant_message { content; _ }
      when List.is_empty (Llm.tool_uses content) ->
        flush_delta ()
    | _ -> (
        match Event.to_display e with
        | Some line ->
            flush_delta ();
            lines := !lines @ [ line ]
        | None -> ()));
    redraw ()
  in
  let tick () =
    Int.incr i;
    redraw ()
  in
  let close () = Notty_unix.Term.release term in
  {
    reporter = { on_event; tick; close };
    append_lines;
    take_submitted;
    set_runtime;
    set_session;
    refresh_tooling;
    request_approval;
  }

(* Run [agent] (an Agent_loop.run promise) while ticking [reporter] on a timer
   for the spinner; stop ticking once the run resolves. *)
let run_with_reporter reporter agent =
  let stop = ref false in
  let rec ticker () =
    if !stop then Lwt.return_unit
    else
      Lwt.bind (Lwt_unix.sleep 0.12) (fun () ->
          reporter.tick ();
          ticker ())
  in
  let main =
    Lwt.bind agent (fun outcome ->
        stop := true;
        Lwt.return outcome)
  in
  Lwt.map (fun (outcome, ()) -> outcome) (Lwt.both main (ticker ()))

(* Blocking Y/n prompt on stdin; defaults to deny on EOF or anything but yes. *)
let prompt_approval tc reason =
  Stdlib.Printf.eprintf "\nAPPROVE? %s\n  %s\n  [y/N] %!" reason
    (Event.describe_tool tc);
  let answer =
    match Stdlib.In_channel.input_line Stdlib.stdin with
    | Some s -> String.lowercase (String.strip s)
    | None -> ""
  in
  Lwt.return (String.equal answer "y" || String.equal answer "yes")

let policy_of ~confirm =
  if confirm then { Policy.approve_commands = true; approve_writes = true }
  else Policy.default

(* Resolve config and workspace once; hand them to [f]. *)
let with_setup provider_opt api_base_opt model_opt workspace_opt max_steps_opt f
    =
  match
    Config.load ?provider:provider_opt ?api_base:api_base_opt ?model:model_opt
      ()
  with
  | Error e ->
      Stdlib.prerr_endline ("config error: " ^ e);
      1
  | Ok base_config -> (
      let workspace_root =
        Option.value workspace_opt ~default:base_config.workspace_root
      in
      let max_steps =
        Option.value max_steps_opt ~default:base_config.max_steps
      in
      let config = { base_config with workspace_root; max_steps } in
      match Workspace.create ~root:workspace_root with
      | Error e ->
          Stdlib.prerr_endline ("workspace error: " ^ e);
          1
      | Ok workspace -> f config workspace)

let warn_yolo yolo =
  if yolo then
    Stdlib.prerr_endline
      "⚠ YOLO mode: dangerous-command deny-list bypassed (workspace bounds \
       still apply)."

let run_oneshot config workspace ~confirm ~resume_opt ~tui ~yolo ~task =
  let root = Workspace.root workspace in
  let session_dir, initial_history =
    match resume_opt with
    | Some dir -> (
        match Transcript.of_session ~session_dir:dir with
        | Ok history -> (dir, history)
        | Error e ->
            Stdlib.prerr_endline ("resume warning: " ^ e);
            (dir, []))
    | None -> (Session.create ~base_dir:root, [])
  in
  Stdlib.Printf.eprintf "model: %s @ %s\nsession: %s%s\n%!" config.Config.model
    config.api_base session_dir
    (if List.is_empty initial_history then ""
     else
       Printf.sprintf " (resumed, %d prior messages)"
         (List.length initial_history));
  let event_log = Event_log.create ~session_dir in
  let model_client = Model_client.create ~config in
  let initial_events =
    match Journal.read ~session_dir with Ok events -> events | Error _ -> []
  in
  let reporter, on_approval =
    if tui then
      let view =
        make_tui_view ~initial_events ~provider:config.provider
          ~model:config.model ~api_base:config.api_base ~workspace_root:root
          ~session_dir
          ~sessions_root:
            (Stdlib.Filename.concat root
               (Stdlib.Filename.concat ".ocaml-agent" "sessions"))
          ~header:(Printf.sprintf "fp-agent  %s  —  %s" config.model task)
      in
      (view.reporter, view.request_approval)
    else (make_plain_reporter (), prompt_approval)
  in
  let policy = policy_of ~confirm in
  warn_yolo yolo;
  let outcome =
    Lwt_main.run
      (run_with_reporter reporter
         (Agent_loop.run ~on_event:reporter.on_event ~policy ~on_approval
            ~initial_history ~yolo ~config ~model_client ~event_log ~workspace
            ~task ()))
  in
  reporter.close ();
  Event_log.close event_log;
  print_summary outcome;
  print_changes root;
  match outcome.status with Agent_loop.Completed -> 0 | _ -> 1

let plugin_smoke_result_lines results =
  List.concat_map results ~f:(fun (result : Plugin.smoke_result) ->
      let output = String.strip result.output in
      Printf.sprintf "smoke ok: %s (%s)" result.tool_name result.args_file
      :: (if String.is_empty output then [] else String.split_lines output))

let plugin_new_usage = "usage: /plugin-new [--id ID] [--tool-name NAME] <dir>"

let parse_plugin_new_args raw =
  let tokens =
    String.split (String.strip raw) ~on:' '
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let rec loop id tool_name dirs = function
    | [] -> (
        match List.rev dirs with
        | [ dir ] -> Ok (id, tool_name, dir)
        | [] -> Error plugin_new_usage
        | _ -> Error "plugin new error: expected exactly one plugin directory")
    | ("--id" | "--plugin-id") :: value :: rest ->
        loop (Some value) tool_name dirs rest
    | ("--id" | "--plugin-id") :: [] -> Error plugin_new_usage
    | ("--tool-name" | "--plugin-tool-name") :: value :: rest ->
        loop id (Some value) dirs rest
    | ("--tool-name" | "--plugin-tool-name") :: [] -> Error plugin_new_usage
    | flag :: _ when String.is_prefix flag ~prefix:"--" ->
        Error ("plugin new error: unknown option " ^ flag)
    | dir :: rest -> loop id tool_name (dir :: dirs) rest
  in
  loop None None [] tokens

let plugin_new_lines args =
  match parse_plugin_new_args args with
  | Error e -> [ e ]
  | Ok (id, tool_name, dir) -> (
      match Plugin.scaffold ?id ?tool_name dir with
      | Error e -> [ "plugin scaffold error: " ^ e ]
      | Ok dst ->
          [
            "created plugin scaffold: " ^ dst;
            "next: /plugin-check " ^ dst;
            "next: /plugin-smoke " ^ dst;
            "next: /plugin-install --replace " ^ dst;
          ])

let plugin_next_lines (manifest : Plugin.manifest) =
  let tools =
    List.map manifest.tools ~f:(fun (tool : Plugin.plugin_tool) ->
        tool.tool_name)
  in
  [
    "plugin id: " ^ manifest.id;
    "tools: " ^ String.concat tools ~sep:", ";
    "next: /plugin " ^ manifest.id;
  ]
  @ List.map tools ~f:(fun tool -> "next: /tool " ^ tool)

let parse_plugin_dir_arg ~usage raw =
  let arg = String.strip raw in
  let replace_prefixes = [ "--replace "; "--replace-plugin " ] in
  if String.is_empty arg then Error usage
  else
    match
      List.find_map replace_prefixes ~f:(fun prefix ->
          if String.is_prefix arg ~prefix then
            Some (String.strip (String.drop_prefix arg (String.length prefix)))
          else None)
    with
    | Some "" -> Error usage
    | Some dir -> Ok (true, dir)
    | None
      when String.equal arg "--replace" || String.equal arg "--replace-plugin"
      ->
        Error usage
    | None -> Ok (false, arg)

let plugin_check_lines args =
  match
    parse_plugin_dir_arg ~usage:"usage: /plugin-check [--replace] <dir>" args
  with
  | Error e -> [ e ]
  | Ok (replace, dir) -> (
      match Plugin.check ~replace dir with
      | Error e -> [ "plugin check error: " ^ e ]
      | Ok manifest ->
          "plugin manifest ok:" :: View.plugin_inspector_lines manifest)

let plugin_install_lines args =
  match
    parse_plugin_dir_arg ~usage:"usage: /plugin-install [--replace] <dir>" args
  with
  | Error e -> [ e ]
  | Ok (replace, dir) -> (
      match Plugin.install ~replace dir with
      | Error e -> [ "plugin install error: " ^ e ]
      | Ok dst ->
          let counts = Tool_loader.refresh_counts () in
          let next =
            match Plugin.check dst with
            | Ok manifest -> plugin_next_lines manifest
            | Error e -> [ "plugin detail error after install: " ^ e ]
          in
          [ "installed plugin: " ^ dst; tools_reloaded_line counts ] @ next)

let run_plugin_dev ~workspace ~replace dir =
  match Plugin.check ~replace dir with
  | Error e -> Error ("plugin dev check error: " ^ e)
  | Ok manifest -> (
      match Plugin.smoke ~replace ~workspace dir with
      | Error e -> Error ("plugin dev smoke error: " ^ e)
      | Ok smoke_results -> (
          match Plugin.install ~replace dir with
          | Error e -> Error ("plugin dev install error: " ^ e)
          | Ok dst ->
              let counts = Tool_loader.refresh_counts () in
              let next =
                match Plugin.check dst with
                | Ok installed -> plugin_next_lines installed
                | Error e -> [ "plugin detail error after install: " ^ e ]
              in
              Ok
                ([
                   "plugin dev check ok: " ^ manifest.id; "plugin dev smoke ok:";
                 ]
                @ plugin_smoke_result_lines smoke_results
                @ [ "installed plugin: " ^ dst; tools_reloaded_line counts ]
                @ next)))

let plugin_dev_lines ~workspace args =
  match
    parse_plugin_dir_arg ~usage:"usage: /plugin-dev [--replace] <dir>" args
  with
  | Error e -> [ e ]
  | Ok (replace, dir) -> (
      match run_plugin_dev ~workspace ~replace dir with
      | Ok lines -> lines
      | Error e -> [ e ])

let plugin_remove_lines arg =
  let id = String.strip arg in
  if String.is_empty id then [ "usage: /plugin-remove <id>" ]
  else
    match Plugin.remove id with
    | Error e -> [ "plugin remove error: " ^ e ]
    | Ok dst ->
        let counts = Tool_loader.refresh_counts () in
        [
          "removed plugin: " ^ dst; tools_reloaded_line counts; "next: /plugins";
        ]

let plugin_smoke_lines ~workspace args =
  match
    parse_plugin_dir_arg ~usage:"usage: /plugin-smoke [--replace] <dir>" args
  with
  | Error e -> [ e ]
  | Ok (replace, dir) -> (
      match Plugin.smoke ~replace ~workspace dir with
      | Error e -> [ "plugin smoke error: " ^ e ]
      | Ok results -> plugin_smoke_result_lines results)

let plugin_run_usage = "usage: /plugin-run <dir> <tool> <json|@args-file>"

let parse_plugin_run_args raw =
  let raw = String.strip raw in
  match String.lsplit2 raw ~on:' ' with
  | None -> Error plugin_run_usage
  | Some (dir, rest) -> (
      let rest = String.strip rest in
      match String.lsplit2 rest ~on:' ' with
      | None -> Error plugin_run_usage
      | Some (tool_name, args_spec) ->
          let dir = String.strip dir in
          let tool_name = String.strip tool_name in
          let args_spec = String.strip args_spec in
          if
            String.is_empty dir || String.is_empty tool_name
            || String.is_empty args_spec
          then Error plugin_run_usage
          else Ok (dir, tool_name, args_spec))

let read_plugin_run_args args_spec =
  let read_file path =
    match Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all with
    | content -> Ok content
    | exception exn ->
        Error
          (Printf.sprintf "cannot read plugin args file %s: %s" path
             (Exn.to_string exn))
  in
  let json =
    if String.is_prefix args_spec ~prefix:"@" then
      let path = String.drop_prefix args_spec 1 |> String.strip in
      if String.is_empty path then Error plugin_run_usage else read_file path
    else Ok args_spec
  in
  match json with
  | Error _ as e -> e
  | Ok json -> (
      match Yojson.Safe.from_string json with
      | json -> Ok json
      | exception exn -> Error ("invalid plugin args JSON: " ^ Exn.to_string exn)
      )

let plugin_run_lines ~workspace args =
  match parse_plugin_run_args args with
  | Error e -> [ e ]
  | Ok (dir, tool_name, args_spec) -> (
      match read_plugin_run_args args_spec with
      | Error e -> [ "plugin run error: " ^ e ]
      | Ok args -> (
          match Plugin.run_tool ~dir ~tool_name ~workspace ~args with
          | Error e -> [ "plugin run error: " ^ e ]
          | Ok (Tool_result.Error { message }) ->
              [ "plugin run error: " ^ message ]
          | Ok (Tool_result.Success { output }) ->
              let output = String.strip output in
              ("plugin run ok: " ^ tool_name)
              ::
              (if String.is_empty output then [] else String.split_lines output)
          ))

let run_tui_repl config workspace ~confirm ~resume_opt ~yolo =
  let root = Workspace.root workspace in
  let sessions_root =
    Stdlib.Filename.concat root
      (Stdlib.Filename.concat ".ocaml-agent" "sessions")
  in
  let session =
    ref (Option.value resume_opt ~default:(Session.create ~base_dir:root))
  in
  let log = ref (Event_log.create ~session_dir:!session) in
  let config_ref = ref config in
  let model_client = ref (Model_client.create ~config) in
  let policy = policy_of ~confirm in
  let snapshots = Git_snapshot.create ~root in
  let events () =
    match Journal.read ~session_dir:!session with
    | Ok events -> events
    | Error _ -> []
  in
  let view =
    make_tui_view ~provider:!config_ref.provider ~model:!config_ref.model
      ~api_base:!config_ref.api_base ~workspace_root:root ~session_dir:!session
      ~sessions_root ~initial_events:(events ())
      ~header:
        (Printf.sprintf "fp-agent TUI  %s  Ctrl+Enter submit  / palette"
           !config_ref.model)
  in
  let latest_event_index events =
    if List.is_empty events then None else Some (List.length events - 1)
  in
  let command_context () =
    let events = events () in
    {
      Tui_command.provider = !config_ref.provider;
      model = !config_ref.model;
      api_base = !config_ref.api_base;
      workspace_root = root;
      sessions_root;
      session_dir = !session;
      events;
      selected_event_index = latest_event_index events;
    }
  in
  let oneline s =
    let flat = String.substr_replace_all s ~pattern:"\n" ~with_:" " in
    if String.length flat > 80 then String.prefix flat 80 ^ "..." else flat
  in
  let append_summary (outcome : Agent_loop.outcome) =
    view.append_lines
      [
        "";
        Printf.sprintf "=== %s (after %d step(s)) ==="
          (String.uppercase (Agent_loop.status_to_string outcome.status))
          outcome.steps;
        outcome.summary;
      ]
  in
  let run_task task =
    Git_snapshot.checkpoint snapshots;
    let initial_history =
      match Transcript.of_session ~session_dir:!session with
      | Ok history -> history
      | Error _ -> []
    in
    let outcome =
      Lwt_main.run
        (run_with_reporter view.reporter
           (Agent_loop.run ~on_event:view.reporter.on_event ~policy
              ~on_approval:view.request_approval ~initial_history ~yolo
              ~config:!config_ref ~model_client:!model_client ~event_log:!log
              ~workspace ~task ()))
    in
    append_summary outcome
  in
  let retry_last_task () =
    match Tui_command.last_user_message (events ()) with
    | None ->
        view.append_lines [ "[tui] /retry"; "no previous user task to retry" ]
    | Some task ->
        view.append_lines [ "[tui] /retry"; "retrying: " ^ oneline task ];
        run_task task
  in
  let compact_session () =
    match Transcript.of_session ~session_dir:!session with
    | Error e -> view.append_lines [ "[tui] /compact"; "compact failed: " ^ e ]
    | Ok turns -> (
        match Agent_loop.compact_event_of_turns turns with
        | None ->
            view.append_lines
              [
                "[tui] /compact";
                "nothing to compact yet (need more completed turns)";
              ]
        | Some (Event.Context_compacted { summary; recent } as event) ->
            Event_log.append !log event;
            view.reporter.on_event event;
            view.append_lines
              [
                "[tui] /compact";
                Printf.sprintf
                  "compacted older history into %d chars; kept %d recent \
                   turn(s)"
                  (String.length summary) (List.length recent);
              ]
        | Some _ -> ())
  in
  let current_model_lines command =
    Option.value
      (Tui_command.run (command_context ()) command)
      ~default:
        [
          "[tui] " ^ command;
          "provider: " ^ !config_ref.provider;
          "model: " ^ !config_ref.model;
          "api_base: " ^ !config_ref.api_base;
        ]
  in
  let apply_config next =
    let workspace_root = !config_ref.workspace_root in
    let max_steps = !config_ref.max_steps in
    config_ref := { next with workspace_root; max_steps };
    model_client := Model_client.create ~config:!config_ref;
    view.set_runtime ~provider:!config_ref.provider ~model:!config_ref.model
      ~api_base:!config_ref.api_base
  in
  let switch_model ?command model =
    match
      resolve_model_provider ~current_provider:!config_ref.provider model
    with
    | `Current_provider ->
        config_ref := { !config_ref with model };
        model_client := Model_client.create ~config:!config_ref;
        view.set_runtime ~provider:!config_ref.provider ~model:!config_ref.model
          ~api_base:!config_ref.api_base;
        view.append_lines
          (current_model_lines
             (Option.value command ~default:("/model " ^ model)))
    | `Provider provider -> (
        match Config.load ?provider:(Some provider) ?model:(Some model) () with
        | Error e ->
            view.append_lines [ "[tui] /model " ^ model; "model error: " ^ e ]
        | Ok next ->
            apply_config next;
            view.append_lines
              (current_model_lines
                 (Option.value command ~default:("/model " ^ model))))
    | `Ambiguous providers ->
        view.append_lines
          [
            "[tui] /model " ^ model;
            "model id appears in multiple providers: "
            ^ String.concat providers ~sep:", ";
            "use /provider <name> " ^ model;
          ]
  in
  let switch_model_direct ?command model =
    config_ref := { !config_ref with model };
    model_client := Model_client.create ~config:!config_ref;
    view.set_runtime ~provider:!config_ref.provider ~model:!config_ref.model
      ~api_base:!config_ref.api_base;
    view.append_lines
      (current_model_lines (Option.value command ~default:("/model " ^ model)))
  in
  let cycle_model () =
    match next_model_id ~current:!config_ref.model !config_ref.models with
    | `No_models ->
        view.append_lines
          [
            "[tui] /model-next";
            "no configured models for provider: " ^ !config_ref.provider;
          ]
    | `Only_model model ->
        view.append_lines
          [
            "[tui] /model-next";
            "only one configured model: " ^ model;
            "provider: " ^ !config_ref.provider;
            "model: " ^ !config_ref.model;
            "api_base: " ^ !config_ref.api_base;
          ]
    | `Next_model model -> switch_model_direct ~command:"/model-next" model
  in
  let switch_provider args =
    let parts =
      String.split args ~on:' ' |> List.map ~f:String.strip
      |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    match parts with
    | [] ->
        view.append_lines
          [ "[tui] /provider"; "usage: /provider <name> [model] [api-base]" ]
    | provider :: rest -> (
        let model, api_base =
          match rest with
          | [] -> (None, None)
          | [ model ] -> (Some model, None)
          | model :: api_base :: _ -> (Some model, Some api_base)
        in
        match Config.load ?provider:(Some provider) ?api_base ?model () with
        | Error e ->
            view.append_lines [ "[tui] /provider"; "provider error: " ^ e ]
        | Ok next ->
            apply_config next;
            view.append_lines (current_model_lines ("/provider " ^ args)))
  in
  let switch_session ?message dir =
    Event_log.close !log;
    session := dir;
    log := Event_log.create ~session_dir:dir;
    let events = events () in
    view.set_session ~session_dir:dir ~events;
    view.append_lines
      [ Option.value message ~default:("[tui] switched session: " ^ dir) ]
  in
  let new_session () =
    let dir = Session.create ~base_dir:root in
    switch_session ~message:("[tui] new session: " ^ dir) dir
  in
  let resume_session arg =
    let arg = String.strip arg in
    if String.is_empty arg then
      view.append_lines [ "[tui] /resume"; "usage: /resume <dir>" ]
    else
      let dir =
        if Stdlib.Filename.is_relative arg then
          Stdlib.Filename.concat sessions_root arg
        else arg
      in
      if Stdlib.Sys.file_exists (Stdlib.Filename.concat dir "events.jsonl") then
        switch_session dir
      else view.append_lines [ "[tui] /resume"; "no such session: " ^ dir ]
  in
  let fork_session arg =
    let at =
      let arg = String.strip arg in
      if String.is_empty arg then Ok None
      else
        try Ok (Some (Int.of_string arg))
        with _ -> Error "usage: /fork [<event-index>]  (see /log)"
    in
    match at with
    | Error e -> view.append_lines [ "[tui] /fork"; e ]
    | Ok at -> (
        match Session.fork ~base_dir:root ~parent_session_dir:!session ~at with
        | Error e -> view.append_lines [ "[tui] /fork"; "fork failed: " ^ e ]
        | Ok child ->
            switch_session child;
            view.append_lines [ "[tui] forked session: " ^ child ])
  in
  let undo () =
    view.append_lines ("[tui] /undo" :: Git_snapshot.undo snapshots)
  in
  let new_plugin arg =
    let lines = plugin_new_lines arg in
    let counts = view.refresh_tooling () in
    view.append_lines
      (("[tui] /plugin-new" :: lines) @ [ tooling_counts_line counts ])
  in
  let smoke_plugin arg =
    view.append_lines
      ("[tui] /plugin-smoke" :: plugin_smoke_lines ~workspace arg)
  in
  let run_plugin arg =
    view.append_lines ("[tui] /plugin-run" :: plugin_run_lines ~workspace arg)
  in
  let doctor_plugins () =
    view.append_lines
      ("[tui] /plugin-doctor" :: Tui_command.plugin_diagnostics_lines ())
  in
  let check_plugin arg =
    view.append_lines ("[tui] /plugin-check" :: plugin_check_lines arg)
  in
  let dev_plugin arg =
    view.append_lines ("[tui] /plugin-dev" :: plugin_dev_lines ~workspace arg);
    ignore (view.refresh_tooling ())
  in
  let install_plugin arg =
    view.append_lines ("[tui] /plugin-install" :: plugin_install_lines arg);
    ignore (view.refresh_tooling ())
  in
  let remove_plugin arg =
    view.append_lines ("[tui] /plugin-remove" :: plugin_remove_lines arg);
    ignore (view.refresh_tooling ())
  in
  let stop = ref false in
  let handle_submission raw =
    match Shell_command.parse raw with
    | Empty -> ()
    | Task task -> run_task task
    | Unknown command ->
        view.append_lines
          [ "[tui] unknown command: " ^ command ^ " (try /help)" ]
    | Command (Exit, _) ->
        view.append_lines [ "[tui] exiting" ];
        stop := true
    | Command (Model, "") -> view.append_lines (current_model_lines "/model")
    | Command (Model, model) -> switch_model model
    | Command (ModelNext, _) -> cycle_model ()
    | Command (Provider, args) -> switch_provider args
    | Command (NewSession, _) -> new_session ()
    | Command (Resume, arg) -> resume_session arg
    | Command (Fork, arg) -> fork_session arg
    | Command (Retry, _) -> retry_last_task ()
    | Command (Compact, _) -> compact_session ()
    | Command (Undo, _) -> undo ()
    | Command (PluginNew, arg) -> new_plugin arg
    | Command (PluginDev, arg) -> dev_plugin arg
    | Command (PluginCheck, arg) -> check_plugin arg
    | Command (PluginInstall, arg) -> install_plugin arg
    | Command (PluginRemove, arg) -> remove_plugin arg
    | Command (PluginSmoke, arg) -> smoke_plugin arg
    | Command (PluginRun, arg) -> run_plugin arg
    | Command (PluginDoctor, _) -> doctor_plugins ()
    | Command _ -> (
        match Tui_command.run (command_context ()) raw with
        | Some lines -> view.append_lines lines
        | None ->
            view.append_lines
              [
                "[tui] command is not available in fullscreen yet: "
                ^ String.strip raw;
              ])
  in
  Exn.protect
    ~f:(fun () ->
      view.append_lines
        ((if yolo then
            [
              "[tui] YOLO mode: dangerous-command deny-list bypassed \
               (workspace bounds still apply).";
            ]
          else [])
        @ (if confirm then
             [
               "[tui] confirm mode: writes and shell commands require approval \
                in the fullscreen view.";
             ]
           else [])
        @ [
            Printf.sprintf "[tui] session: %s" !session;
            "[tui] Ctrl+Enter submits the prompt; type /exit then Ctrl+Enter \
             to quit.";
          ]);
      while not !stop do
        view.reporter.tick ();
        (match view.take_submitted () with
        | None -> ()
        | Some raw -> handle_submission raw);
        ignore (Unix.select [] [] [] 0.05 : _ * _ * _)
      done;
      0)
    ~finally:(fun () ->
      view.reporter.close ();
      Event_log.close !log)

let tool_kind_label = function
  | Tool.Read -> "read"
  | Tool.Write -> "write"
  | Tool.Exec -> "exec"

let print_help () = Stdlib.print_endline (Shell_command.help_text ())

let print_tools () =
  Tool_loader.register_all ();
  Stdlib.print_endline "Available tools:";
  List.iter (Tool.all ()) ~f:(fun (tool : Tool.t) ->
      Stdlib.Printf.printf "  %-18s %-5s %s\n" tool.name
        (tool_kind_label tool.kind)
        tool.description);
  let conflicts = Plugin.tool_conflicts () in
  if not (List.is_empty conflicts) then (
    Stdlib.print_endline "Plugin tool conflicts:";
    List.iter conflicts ~f:(fun (conflict : Plugin.tool_conflict) ->
        Stdlib.Printf.printf "  - %s from %s skipped; already provided by %s\n"
          conflict.tool_name conflict.plugin_id conflict.existing_owner))

let print_tool_detail query =
  Tool_loader.register_all ();
  let query = String.strip query in
  if String.is_empty query then Stdlib.print_endline "usage: /tool <tool-name>"
  else
    match Tool.find query with
    | None -> Stdlib.Printf.printf "no tool matching: %s\n" query
    | Some tool ->
        List.iter (View.tool_inspector_lines tool) ~f:Stdlib.print_endline

let print_plugin_errors label errors =
  match errors with
  | [] -> ()
  | errors ->
      Stdlib.print_endline label;
      List.iter errors ~f:(fun (error : Plugin.load_error) ->
          Stdlib.Printf.printf "  - %s: %s\n" error.dir error.message)

let print_plugin_conflicts conflicts =
  match conflicts with
  | [] -> ()
  | conflicts ->
      Stdlib.print_endline "Plugin tool conflicts:";
      List.iter conflicts ~f:(fun (conflict : Plugin.tool_conflict) ->
          Stdlib.Printf.printf
            "  - %s from %s skipped; already provided by %s\n"
            conflict.tool_name conflict.plugin_id conflict.existing_owner)

let print_plugins () =
  let discovery = Plugin.discover () in
  (match discovery.manifests with
  | [] -> Stdlib.print_endline "(no plugins discovered)"
  | manifests ->
      List.iter manifests ~f:(fun (plugin : Plugin.manifest) ->
          Stdlib.Printf.printf "%s %s (%s, sdk %d)\n  %s\n" plugin.id
            plugin.name plugin.version plugin.sdk_version plugin.dir;
          List.iter plugin.tools ~f:(fun tool ->
              Stdlib.Printf.printf "  - %-18s %-5s %s\n" tool.tool_name
                (tool_kind_label tool.tool_kind)
                tool.tool_description)));
  print_plugin_errors "Invalid plugins:" discovery.errors;
  print_plugin_conflicts (Plugin.tool_conflicts ())

let plugin_matches query (plugin : Plugin.manifest) =
  String.equal plugin.id query
  || String.equal plugin.name query
  || List.exists plugin.tools ~f:(fun tool -> String.equal tool.tool_name query)

let print_plugin_detail query =
  let query = String.strip query in
  if String.is_empty query then
    Stdlib.print_endline "usage: /plugin <plugin-id|tool-name>"
  else
    match List.find (Plugin.manifests ()) ~f:(plugin_matches query) with
    | None -> Stdlib.Printf.printf "no plugin or tool matching: %s\n" query
    | Some plugin ->
        List.iter (View.plugin_inspector_lines plugin) ~f:Stdlib.print_endline

let print_sessions sessions_root current =
  match Stdlib.Sys.readdir sessions_root with
  | exception _ -> Stdlib.print_endline "(no sessions yet)"
  | entries ->
      Array.sort entries ~compare:String.compare;
      Array.iter entries ~f:(fun e ->
          let full = Stdlib.Filename.concat sessions_root e in
          let mark = if String.equal full current then "  *" else "" in
          Stdlib.Printf.printf "  %s%s\n" e mark)

let run_repl config workspace ~confirm ~resume_opt ~yolo =
  let root = Workspace.root workspace in
  let sessions_root =
    Stdlib.Filename.concat root
      (Stdlib.Filename.concat ".ocaml-agent" "sessions")
  in
  let config_ref = ref config in
  let model_client = ref (Model_client.create ~config) in
  let policy = policy_of ~confirm in
  let session =
    ref (Option.value resume_opt ~default:(Session.create ~base_dir:root))
  in
  let log = ref (Event_log.create ~session_dir:!session) in
  let switch ?message dir =
    Event_log.close !log;
    session := dir;
    log := Event_log.create ~session_dir:dir;
    Stdlib.Printf.printf "%s\n%!"
      (Option.value message ~default:("switched to " ^ dir))
  in
  let start_new_session () =
    let dir = Session.create ~base_dir:root in
    switch ~message:("new session: " ^ dir) dir
  in
  let snapshots = Git_snapshot.create ~root in
  let is_git = Stdlib.Sys.file_exists (Stdlib.Filename.concat root ".git") in
  let git args =
    Shell.run
      ~command:(Printf.sprintf "git -C %s %s" (Stdlib.Filename.quote root) args)
      ~timeout_sec:30
  in
  (* Keep the agent's own session log out of git operations so /undo never
     touches the event log we are actively writing. *)
  let exclude = "':(exclude).ocaml-agent'" in
  let undo () =
    List.iter (Git_snapshot.undo snapshots) ~f:Stdlib.print_endline
  in
  let show_diff () =
    if not is_git then Stdlib.print_endline "(workspace is not a git repo)"
    else (
      (match git ("diff -- . " ^ exclude) with
      | Ok { stdout; _ } when not (String.is_empty (String.strip stdout)) ->
          Stdlib.print_string stdout
      | _ -> Stdlib.print_endline "(no tracked changes)");
      match git ("ls-files --others --exclude-standard -- . " ^ exclude) with
      | Ok { stdout; _ } when not (String.is_empty (String.strip stdout)) ->
          Stdlib.print_endline "untracked:";
          Stdlib.print_string stdout
      | _ -> ())
  in
  let show_current_model () =
    Stdlib.Printf.printf "provider: %s\nmodel: %s\napi_base: %s\n%!"
      !config_ref.provider !config_ref.model !config_ref.api_base
  in
  let apply_config next =
    let workspace_root = !config_ref.workspace_root in
    let max_steps = !config_ref.max_steps in
    config_ref := { next with workspace_root; max_steps };
    model_client := Model_client.create ~config:!config_ref
  in
  let switch_model_direct model =
    config_ref := { !config_ref with model };
    model_client := Model_client.create ~config:!config_ref;
    show_current_model ()
  in
  let switch_model model =
    match
      resolve_model_provider ~current_provider:!config_ref.provider model
    with
    | `Current_provider -> switch_model_direct model
    | `Provider provider -> (
        match Config.load ?provider:(Some provider) ?model:(Some model) () with
        | Error e -> Stdlib.print_endline ("model error: " ^ e)
        | Ok next ->
            apply_config next;
            show_current_model ())
    | `Ambiguous providers ->
        Stdlib.Printf.printf
          "model id appears in multiple providers: %s\n\
           use /provider <name> %s\n\
           %!"
          (String.concat providers ~sep:", ")
          model
  in
  let cycle_model () =
    match next_model_id ~current:!config_ref.model !config_ref.models with
    | `No_models ->
        Stdlib.Printf.printf "no configured models for provider: %s\n%!"
          !config_ref.provider
    | `Only_model model ->
        Stdlib.Printf.printf "only one configured model: %s\n%!" model;
        show_current_model ()
    | `Next_model model -> switch_model_direct model
  in
  let print_models () =
    match Config.available_providers () with
    | [] ->
        Stdlib.print_endline
          "(no configured providers; add providers to FP_AGENT_CONFIG)"
    | providers ->
        List.iter providers ~f:(fun (entry : Config.provider_catalog_entry) ->
            let provider_mark =
              if String.equal entry.provider_name !config_ref.provider then "*"
              else " "
            in
            Stdlib.Printf.printf "%s %s @ %s\n" provider_mark
              entry.provider_name entry.provider_api_base;
            match entry.provider_models with
            | [] -> Stdlib.print_endline "    (no models configured)"
            | models ->
                List.iter models ~f:(fun model ->
                    let model_mark =
                      if
                        String.equal entry.provider_name !config_ref.provider
                        && String.equal model !config_ref.model
                      then "*"
                      else " "
                    in
                    Stdlib.Printf.printf "    %s %s\n" model_mark model));
        Stdlib.print_endline
          "Use /model <id> to switch by model id, /model-next to cycle the \
           current provider, or /provider <name> [model] to switch provider."
  in
  let switch_provider args =
    let parts =
      String.split args ~on:' ' |> List.map ~f:String.strip
      |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    match parts with
    | [] -> Stdlib.print_endline "usage: /provider <name> [model] [api-base]"
    | provider :: rest -> (
        let model, api_base =
          match rest with
          | [] -> (None, None)
          | [ model ] -> (Some model, None)
          | model :: api_base :: _ -> (Some model, Some api_base)
        in
        match Config.load ?provider:(Some provider) ?api_base ?model () with
        | Error e -> Stdlib.print_endline ("provider error: " ^ e)
        | Ok next ->
            apply_config next;
            show_current_model ())
  in
  let oneline s =
    let flat = String.substr_replace_all s ~pattern:"\n" ~with_:" " in
    if String.length flat > 60 then String.prefix flat 60 ^ "…" else flat
  in
  let describe_event (e : Event.t) =
    match e with
    | User_message { content } -> "user: " ^ oneline content
    | Model_delta { content } -> "model_delta: " ^ oneline content
    | Assistant_message { content; _ } -> (
        match Llm.tool_uses content with
        | _ :: _ as calls ->
            Printf.sprintf "model → %d tool call%s" (List.length calls)
              (if List.length calls = 1 then "" else "s")
        | [] -> "model: final answer")
    | Model_response { action = Tool_call tc } ->
        "model → " ^ Event.describe_tool tc
    | Model_response { action = Tool_calls calls } ->
        Printf.sprintf "model → %d tool calls" (List.length calls)
    | Model_response { action = Final_answer _ } -> "model: final answer"
    | Tool_call tc -> "tool_call " ^ Event.describe_tool tc
    | Tool_result_message { result = Success _; _ } -> "result ok"
    | Tool_result_message { result = Error _; _ } -> "result err"
    | Tool_result (Success _) -> "result ok"
    | Tool_result (Error _) -> "result err"
    | Context_compacted _ -> "context compacted"
    | Graph_event event -> "graph " ^ Graph_event.describe event
    | Policy_decision { permission; _ } ->
        "policy " ^ Permission.to_string permission
    | State_transition { to_state; _ } ->
        "state → " ^ Agent_state.to_string to_state
  in
  let print_log () =
    match Journal.read ~session_dir:!session with
    | Error e -> Stdlib.print_endline e
    | Ok [] -> Stdlib.print_endline "(no events yet)"
    | Ok events ->
        List.iteri events ~f:(fun i e ->
            Stdlib.Printf.printf "  %3d  %s\n" i (describe_event e))
  in
  let print_inspect arg =
    match Journal.read ~session_dir:!session with
    | Error e -> Stdlib.print_endline e
    | Ok [] -> Stdlib.print_endline "(no events yet)"
    | Ok events -> (
        let max_index = List.length events - 1 in
        let index =
          if String.is_empty arg then Ok max_index
          else
            try
              let i = Int.of_string arg in
              if i < 0 then Error "usage: /inspect [event-index]" else Ok i
            with _ -> Error "usage: /inspect [event-index]"
        in
        match index with
        | Error e -> Stdlib.print_endline e
        | Ok i -> (
            match List.nth events i with
            | None ->
                Stdlib.Printf.printf "no event at index %d (0..%d)\n" i
                  max_index
            | Some event ->
                Stdlib.Printf.printf "event %d\n" i;
                List.iter
                  (View.event_inspector_lines event)
                  ~f:Stdlib.print_endline))
  in
  let print_usage () =
    match Journal.read ~session_dir:!session with
    | Error e -> Stdlib.print_endline e
    | Ok events ->
        let usage = View.token_usage_of_events events in
        Stdlib.Printf.printf "input_tokens: %d\n" usage.input_tokens;
        Stdlib.Printf.printf "output_tokens: %d\n" usage.output_tokens;
        Stdlib.Printf.printf "total_tokens: %d\n" (View.token_usage_total usage)
  in
  let latest_event_index events =
    if List.is_empty events then None else Some (List.length events - 1)
  in
  let command_context () =
    let events =
      match Journal.read ~session_dir:!session with
      | Ok events -> events
      | Error _ -> []
    in
    {
      Tui_command.provider = !config_ref.provider;
      model = !config_ref.model;
      api_base = !config_ref.api_base;
      workspace_root = root;
      sessions_root;
      session_dir = !session;
      events;
      selected_event_index = latest_event_index events;
    }
  in
  let print_status () =
    List.iter
      (Tui_command.status_lines (command_context ()))
      ~f:Stdlib.print_endline
  in
  let print_tree () =
    match Stdlib.Sys.readdir sessions_root with
    | exception _ -> Stdlib.print_endline "(no sessions yet)"
    | names ->
        Array.sort names ~compare:String.compare;
        let metas =
          Array.to_list names
          |> List.map ~f:(fun n ->
              (n, Session.read_meta (Stdlib.Filename.concat sessions_root n)))
        in
        let current_name = Stdlib.Filename.basename !session in
        let children_of p =
          List.filter_map metas ~f:(fun (n, (par, fa)) ->
              if Option.equal String.equal par (Some p) then Some (n, fa)
              else None)
        in
        let rec render indent name fa =
          let mark = if String.equal name current_name then "  *" else "" in
          let fa_s =
            match fa with Some k -> Printf.sprintf " (fork@%d)" k | None -> ""
          in
          Stdlib.Printf.printf "%s%s%s%s\n" indent name fa_s mark;
          List.iter (children_of name) ~f:(fun (c, cfa) ->
              render (indent ^ "  ") c cfa)
        in
        List.iter metas ~f:(fun (n, (par, _)) ->
            if Option.is_none par then render "" n None)
  in
  let do_fork arg =
    let at =
      if String.is_empty arg then Ok None
      else
        try Ok (Some (Int.of_string arg))
        with _ -> Error "usage: /fork [<event-index>]  (see /log)"
    in
    match at with
    | Error e -> Stdlib.print_endline e
    | Ok at -> (
        match Session.fork ~base_dir:root ~parent_session_dir:!session ~at with
        | Ok child -> switch child
        | Error e -> Stdlib.print_endline ("fork failed: " ^ e))
  in
  Stdlib.Printf.eprintf
    "fp-agent REPL — model %s. Type /help for commands, /exit to quit.\n\
     session: %s\n\
     %!"
    !config_ref.Config.model !session;
  warn_yolo yolo;
  let run_task task =
    Git_snapshot.checkpoint snapshots;
    let initial_history =
      match Transcript.of_session ~session_dir:!session with
      | Ok h -> h
      | Error _ -> []
    in
    let reporter = make_plain_reporter () in
    let outcome =
      Lwt_main.run
        (run_with_reporter reporter
           (Agent_loop.run ~on_event:reporter.on_event ~policy
              ~on_approval:prompt_approval ~initial_history ~yolo
              ~config:!config_ref ~model_client:!model_client ~event_log:!log
              ~workspace ~task ()))
    in
    reporter.close ();
    print_summary outcome
  in
  let retry_last_task () =
    match Journal.read ~session_dir:!session with
    | Error e -> Stdlib.print_endline e
    | Ok events -> (
        match Tui_command.last_user_message events with
        | None -> Stdlib.print_endline "no previous user task to retry"
        | Some task ->
            Stdlib.Printf.printf "retrying: %s\n%!" (oneline task);
            run_task task)
  in
  let compact_session () =
    match Transcript.of_session ~session_dir:!session with
    | Error e -> Stdlib.print_endline ("compact failed: " ^ e)
    | Ok turns -> (
        match Agent_loop.compact_event_of_turns turns with
        | None ->
            Stdlib.print_endline
              "nothing to compact yet (need more completed turns)"
        | Some (Event.Context_compacted { summary; recent } as event) ->
            Event_log.append !log event;
            Stdlib.Printf.printf
              "compacted older history into %d chars; kept %d recent turn(s)\n\
               %!"
              (String.length summary) (List.length recent)
        | Some _ -> ())
  in
  let rec loop () =
    Stdlib.print_string "\n> ";
    Stdlib.Out_channel.flush Stdlib.stdout;
    match Stdlib.In_channel.input_line Stdlib.stdin with
    | None -> ()
    | Some raw -> (
        let open Shell_command in
        match parse raw with
        | Empty -> loop ()
        | Task task ->
            run_task task;
            loop ()
        | Unknown command ->
            Stdlib.print_endline ("unknown command: " ^ command ^ " (try /help)");
            loop ()
        | Command (Exit, _) -> ()
        | Command (Help, _) ->
            print_help ();
            loop ()
        | Command (Tools, _) ->
            print_tools ();
            loop ()
        | Command (Tool, arg) ->
            print_tool_detail arg;
            loop ()
        | Command (Plugins, _) ->
            print_plugins ();
            loop ()
        | Command (Plugin, arg) ->
            print_plugin_detail arg;
            loop ()
        | Command (PluginNew, arg) ->
            List.iter (plugin_new_lines arg) ~f:Stdlib.print_endline;
            loop ()
        | Command (PluginDev, arg) ->
            List.iter (plugin_dev_lines ~workspace arg) ~f:Stdlib.print_endline;
            loop ()
        | Command (PluginCheck, arg) ->
            List.iter (plugin_check_lines arg) ~f:Stdlib.print_endline;
            loop ()
        | Command (PluginInstall, arg) ->
            List.iter (plugin_install_lines arg) ~f:Stdlib.print_endline;
            loop ()
        | Command (PluginRemove, arg) ->
            List.iter (plugin_remove_lines arg) ~f:Stdlib.print_endline;
            loop ()
        | Command (PluginSmoke, arg) ->
            List.iter
              (plugin_smoke_lines ~workspace arg)
              ~f:Stdlib.print_endline;
            loop ()
        | Command (PluginRun, arg) ->
            List.iter (plugin_run_lines ~workspace arg) ~f:Stdlib.print_endline;
            loop ()
        | Command (PluginDoctor, _) ->
            List.iter
              (Tui_command.plugin_diagnostics_lines ())
              ~f:Stdlib.print_endline;
            loop ()
        | Command (Sessions, _) ->
            print_sessions sessions_root !session;
            loop ()
        | Command (Model, "") ->
            show_current_model ();
            loop ()
        | Command (Model, model) ->
            switch_model model;
            loop ()
        | Command (ModelNext, _) ->
            cycle_model ();
            loop ()
        | Command (Models, _) ->
            print_models ();
            loop ()
        | Command (Provider, args) ->
            switch_provider args;
            loop ()
        | Command (NewSession, _) ->
            start_new_session ();
            loop ()
        | Command (Diff, _) ->
            show_diff ();
            loop ()
        | Command (Retry, _) ->
            retry_last_task ();
            loop ()
        | Command (Compact, _) ->
            compact_session ();
            loop ()
        | Command (Undo, _) ->
            undo ();
            loop ()
        | Command (Log, _) ->
            print_log ();
            loop ()
        | Command (Inspect, arg) ->
            print_inspect arg;
            loop ()
        | Command (Usage, _) ->
            print_usage ();
            loop ()
        | Command (Status, _) ->
            print_status ();
            loop ()
        | Command (Instructions, _) ->
            List.iter
              (Tui_command.instruction_lines (command_context ()))
              ~f:Stdlib.print_endline;
            loop ()
        | Command (Tree, _) ->
            print_tree ();
            loop ()
        | Command (Fork, arg) ->
            do_fork arg;
            loop ()
        | Command (Resume, "") ->
            Stdlib.print_endline "usage: /resume <dir>";
            loop ()
        | Command (Resume, arg) ->
            let dir =
              if Stdlib.Filename.is_relative arg then
                Stdlib.Filename.concat sessions_root arg
              else arg
            in
            if
              Stdlib.Sys.file_exists (Stdlib.Filename.concat dir "events.jsonl")
            then switch dir
            else Stdlib.print_endline ("no such session: " ^ dir);
            loop ())
  in
  loop ();
  Event_log.close !log;
  0

let print_plugin_summary (plugin : Plugin.manifest) =
  Stdlib.Printf.printf "%s %s (%s, sdk %d)\n  %s\n" plugin.id plugin.name
    plugin.version plugin.sdk_version plugin.dir;
  List.iter plugin.tools ~f:(fun tool ->
      Stdlib.Printf.printf "  - %-18s %-5s %s\n" tool.tool_name
        (tool_kind_label tool.tool_kind)
        tool.tool_description)

let print_installed_plugins () =
  let discovery = Plugin.installed_discovery () in
  (match discovery.manifests with
  | [] -> Stdlib.print_endline "(no installed plugins)"
  | plugins ->
      Stdlib.print_endline "installed plugins:";
      List.iter plugins ~f:print_plugin_summary);
  print_plugin_errors "Invalid installed plugins:" discovery.errors;
  print_plugin_conflicts (Plugin.installed_tool_conflicts ())

let parse_json_arg json =
  match Yojson.Safe.from_string json with
  | json -> Ok json
  | exception exn -> Error ("invalid plugin args JSON: " ^ Exn.to_string exn)

let read_plugin_args_file path =
  match Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all with
  | content -> Ok content
  | exception exn ->
      Error
        (Printf.sprintf "cannot read plugin args file %s: %s" path
           (Exn.to_string exn))

let plugin_args_json args_json args_file =
  match (args_json, args_file) with
  | Some _, Some _ ->
      Error "use only one of --plugin-args or --plugin-args-file"
  | None, None ->
      Error
        "--plugin-args or --plugin-args-file is required with --run-plugin-tool"
  | Some json, None -> Ok json
  | None, Some path -> read_plugin_args_file path

let workspace_for_plugin_debug workspace_opt =
  let root =
    Option.value workspace_opt
      ~default:
        (Option.value
           (Stdlib.Sys.getenv_opt "WORKSPACE_ROOT")
           ~default:(Unix.getcwd ()))
  in
  Workspace.create ~root

let run_plugin_tool_cli dir tool_name args_json args_file workspace_opt =
  match
    ( tool_name,
      plugin_args_json args_json args_file,
      workspace_for_plugin_debug workspace_opt )
  with
  | None, _, _ ->
      Stdlib.prerr_endline
        "plugin tool error: --plugin-tool is required with --run-plugin-tool";
      1
  | _, Error e, _ ->
      Stdlib.prerr_endline ("plugin tool error: " ^ e);
      1
  | _, _, Error e ->
      Stdlib.prerr_endline ("plugin tool error: " ^ e);
      1
  | Some tool_name, Ok args_json, Ok workspace -> (
      match parse_json_arg args_json with
      | Error e ->
          Stdlib.prerr_endline ("plugin tool error: " ^ e);
          1
      | Ok args -> (
          match Plugin.run_tool ~dir ~tool_name ~workspace ~args with
          | Error e ->
              Stdlib.prerr_endline ("plugin tool error: " ^ e);
              1
          | Ok (Tool_result.Success { output }) ->
              Stdlib.print_endline output;
              0
          | Ok (Tool_result.Error { message }) ->
              Stdlib.prerr_endline ("plugin tool error: " ^ message);
              1))

let run_plugin_smoke_cli dir replace_plugin workspace_opt =
  match workspace_for_plugin_debug workspace_opt with
  | Error e ->
      Stdlib.prerr_endline ("plugin smoke error: " ^ e);
      1
  | Ok workspace -> (
      match Plugin.smoke ~replace:replace_plugin ~workspace dir with
      | Error e ->
          Stdlib.prerr_endline ("plugin smoke error: " ^ e);
          1
      | Ok results ->
          List.iter (plugin_smoke_result_lines results) ~f:(fun line ->
              Stdlib.Printf.printf "%s\n%!" line);
          0)

let run_plugin_dev_cli dir replace_plugin workspace_opt =
  match workspace_for_plugin_debug workspace_opt with
  | Error e ->
      Stdlib.prerr_endline ("plugin dev error: " ^ e);
      1
  | Ok workspace -> (
      match run_plugin_dev ~workspace ~replace:replace_plugin dir with
      | Error e ->
          Stdlib.prerr_endline e;
          1
      | Ok lines ->
          List.iter lines ~f:(fun line -> Stdlib.Printf.printf "%s\n%!" line);
          0)

let dispatch new_plugin plugin_id plugin_tool_name check_plugin install_plugin
    smoke_plugin dev_plugin replace_plugin list_plugins doctor_plugins
    remove_plugin run_plugin_tool plugin_tool plugin_args plugin_args_file task
    provider api_base model workspace max_steps confirm resume tui yolo =
  match
    ( new_plugin,
      plugin_id,
      plugin_tool_name,
      check_plugin,
      install_plugin,
      smoke_plugin,
      dev_plugin,
      list_plugins,
      doctor_plugins,
      remove_plugin,
      run_plugin_tool )
  with
  | Some path, plugin_id, plugin_tool_name, _, _, _, _, _, _, _, _ -> (
      match Plugin.scaffold ?id:plugin_id ?tool_name:plugin_tool_name path with
      | Ok dst ->
          Stdlib.Printf.printf "created plugin scaffold: %s\n" dst;
          0
      | Error e ->
          Stdlib.prerr_endline ("plugin scaffold error: " ^ e);
          1)
  | None, Some _, _, _, _, _, _, _, _, _, _ ->
      Stdlib.prerr_endline
        "plugin scaffold error: --plugin-id requires --new-plugin DIR";
      1
  | None, None, Some _, _, _, _, _, _, _, _, _ ->
      Stdlib.prerr_endline
        "plugin scaffold error: --plugin-tool-name requires --new-plugin DIR";
      1
  | None, None, None, Some path, _, _, _, _, _, _, _ -> (
      match Plugin.check ~replace:replace_plugin path with
      | Ok manifest ->
          Stdlib.print_endline "plugin manifest ok:";
          print_plugin_summary manifest;
          0
      | Error e ->
          Stdlib.prerr_endline ("plugin check error: " ^ e);
          1)
  | None, None, None, None, Some path, _, _, _, _, _, _ -> (
      match Plugin.install ~replace:replace_plugin path with
      | Ok dst ->
          Stdlib.Printf.printf "installed plugin: %s\n" dst;
          0
      | Error e ->
          Stdlib.prerr_endline ("plugin install error: " ^ e);
          1)
  | None, None, None, None, None, Some path, _, _, _, _, _ ->
      run_plugin_smoke_cli path replace_plugin workspace
  | None, None, None, None, None, None, Some path, _, _, _, _ ->
      run_plugin_dev_cli path replace_plugin workspace
  | None, None, None, None, None, None, None, true, _, _, _ ->
      print_installed_plugins ();
      0
  | None, None, None, None, None, None, None, false, true, _, _ ->
      List.iter
        (Tui_command.plugin_diagnostics_lines ())
        ~f:Stdlib.print_endline;
      0
  | None, None, None, None, None, None, None, false, false, Some id, _ -> (
      match Plugin.remove id with
      | Ok dst ->
          Stdlib.Printf.printf "removed plugin: %s\n" dst;
          0
      | Error e ->
          Stdlib.prerr_endline ("plugin remove error: " ^ e);
          1)
  | None, None, None, None, None, None, None, false, false, None, Some dir ->
      run_plugin_tool_cli dir plugin_tool plugin_args plugin_args_file workspace
  | None, None, None, None, None, None, None, false, false, None, None
    when replace_plugin ->
      Stdlib.prerr_endline
        "plugin error: --replace-plugin requires --install-plugin DIR or \
         --check-plugin DIR or --smoke-plugin DIR or --dev-plugin DIR";
      1
  | None, None, None, None, None, None, None, false, false, None, None ->
      with_setup provider api_base model workspace max_steps
        (fun config workspace ->
          match task with
          | Some task ->
              run_oneshot config workspace ~confirm ~resume_opt:resume ~tui
                ~yolo ~task
          | None when tui ->
              run_tui_repl config workspace ~confirm ~resume_opt:resume ~yolo
          | None -> run_repl config workspace ~confirm ~resume_opt:resume ~yolo)

let () =
  let open Cmdliner in
  let task =
    Arg.(
      value
      & pos 0 (some string) None
      & info [] ~docv:"TASK"
          ~doc:
            "The coding task for the agent to perform. Omit to start an \
             interactive REPL.")
  in
  let install_plugin =
    Arg.(
      value
      & opt (some string) None
      & info [ "install-plugin" ] ~docv:"DIR"
          ~doc:
            "Install a plugin directory containing fp-agent-plugin.json into \
             the plugin home, then exit.")
  in
  let replace_plugin =
    Arg.(
      value & flag
      & info
          [ "replace-plugin"; "force-plugin-install" ]
          ~doc:
            "Allow --install-plugin to replace an existing installed plugin \
             with the same id. With --check-plugin, validate replacement \
             compatibility by ignoring the installed plugin with the same id. \
             The new plugin is validated and staged before the old \
             installation is removed.")
  in
  let list_plugins =
    Arg.(
      value & flag
      & info [ "list-plugins" ]
          ~doc:"List plugins installed in the plugin home, then exit.")
  in
  let doctor_plugins =
    Arg.(
      value & flag
      & info
          [ "doctor-plugins"; "plugin-doctor" ]
          ~doc:
            "Show plugin search roots, install home, invalid manifests, and \
             tool-name conflicts, then exit.")
  in
  let remove_plugin =
    Arg.(
      value
      & opt (some string) None
      & info
          [ "remove-plugin"; "uninstall-plugin" ]
          ~docv:"ID"
          ~doc:
            "Remove an installed plugin by id from the plugin home, then exit.")
  in
  let run_plugin_tool =
    Arg.(
      value
      & opt (some string) None
      & info [ "run-plugin-tool" ] ~docv:"DIR"
          ~doc:
            "Run a plugin tool locally from DIR, then exit. Requires \
             --plugin-tool and either --plugin-args or --plugin-args-file.")
  in
  let smoke_plugin =
    Arg.(
      value
      & opt (some string) None
      & info [ "smoke-plugin" ] ~docv:"DIR"
          ~doc:
            "Validate a plugin directory and run every tool using \
             examples/<tool>.args.json, then exit.")
  in
  let dev_plugin =
    Arg.(
      value
      & opt (some string) None
      & info
          [ "dev-plugin"; "plugin-dev" ]
          ~docv:"DIR"
          ~doc:
            "Validate, smoke-test, install, refresh, and print next inspection \
             commands for a plugin directory, then exit.")
  in
  let plugin_tool =
    Arg.(
      value
      & opt (some string) None
      & info [ "plugin-tool" ] ~docv:"NAME"
          ~doc:"Plugin tool name for --run-plugin-tool.")
  in
  let plugin_args =
    Arg.(
      value
      & opt (some string) None
      & info [ "plugin-args" ] ~docv:"JSON"
          ~doc:"JSON argument object for --run-plugin-tool.")
  in
  let plugin_args_file =
    Arg.(
      value
      & opt (some string) None
      & info [ "plugin-args-file" ] ~docv:"FILE"
          ~doc:"Read the JSON argument object for --run-plugin-tool from FILE.")
  in
  let check_plugin =
    Arg.(
      value
      & opt (some string) None
      & info [ "check-plugin" ] ~docv:"DIR"
          ~doc:
            "Validate a plugin directory containing fp-agent-plugin.json, then \
             exit.")
  in
  let new_plugin =
    Arg.(
      value
      & opt (some string) None
      & info [ "new-plugin" ] ~docv:"DIR"
          ~doc:"Create a starter plugin directory, then exit.")
  in
  let plugin_id =
    Arg.(
      value
      & opt (some string) None
      & info [ "plugin-id" ] ~docv:"ID"
          ~doc:"Manifest id to use with --new-plugin.")
  in
  let plugin_tool_name =
    Arg.(
      value
      & opt (some string) None
      & info [ "plugin-tool-name" ] ~docv:"NAME"
          ~doc:"Initial tool name to use with --new-plugin.")
  in
  let provider =
    Arg.(
      value
      & opt (some string) None
      & info [ "p"; "provider" ] ~docv:"NAME"
          ~doc:
            "Model provider: kimi (default), zhipu, deepseek, local, or a \
             custom provider from FP_AGENT_CONFIG. Also reads the PROVIDER env \
             var.")
  in
  let api_base =
    Arg.(
      value
      & opt (some string) None
      & info [ "api-base" ] ~docv:"URL" ~doc:"Override the provider's base URL.")
  in
  let model =
    Arg.(
      value
      & opt (some string) None
      & info [ "m"; "model" ] ~docv:"ID"
          ~doc:"Override the model id (defaults to the provider's model).")
  in
  let workspace =
    Arg.(
      value
      & opt (some string) None
      & info [ "w"; "workspace" ] ~docv:"DIR"
          ~doc:"Workspace root (defaults to the WORKSPACE_ROOT env var or cwd).")
  in
  let max_steps =
    Arg.(
      value
      & opt (some int) None
      & info [ "max-steps" ] ~docv:"N"
          ~doc:"Maximum agent steps (defaults to the MAX_STEPS env var or 30).")
  in
  let confirm =
    Arg.(
      value & flag
      & info [ "confirm" ]
          ~doc:
            "Ask for confirmation before each shell command or file \
             modification. Fullscreen TUI mode renders the approval prompt in \
             the active view.")
  in
  let resume =
    Arg.(
      value
      & opt (some string) None
      & info [ "resume" ] ~docv:"SESSION_DIR"
          ~doc:
            "Resume from a previous session directory: replay its event log as \
             context and continue.")
  in
  let tui =
    Arg.(
      value & flag
      & info [ "tui" ]
          ~doc:"Render a full-screen live view of the run (runs autonomously).")
  in
  let yolo =
    Arg.(
      value & flag
      & info [ "yolo" ]
          ~doc:
            "Bypass the dangerous-command deny-list (workspace bounds still \
             apply). Use with care.")
  in
  let doc = "A type-safe local CLI code agent harness." in
  let term =
    Term.(
      const dispatch $ new_plugin $ plugin_id $ plugin_tool_name $ check_plugin
      $ install_plugin $ smoke_plugin $ dev_plugin $ replace_plugin
      $ list_plugins $ doctor_plugins $ remove_plugin $ run_plugin_tool
      $ plugin_tool $ plugin_args $ plugin_args_file $ task $ provider
      $ api_base $ model $ workspace $ max_steps $ confirm $ resume $ tui $ yolo)
  in
  let info = Cmd.info "fp-agent" ~version:"0.1.0" ~doc in
  Stdlib.exit (Cmd.eval' (Cmd.v info term))
