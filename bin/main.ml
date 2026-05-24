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
  | _ -> ()

let phase_label = function
  | `Idle -> None
  | `Thinking -> Some "thinking…"
  | `Running tool -> Some (Printf.sprintf "running %s…" tool)

(* Plain reporter: step lines + an animated spinner line on stderr. The spinner
   line is rewritten in place with CR + clear-to-EOL. *)
let make_plain_reporter () =
  let phase = make_phase () in
  let i = ref 0 in
  let clear () = Stdlib.Printf.eprintf "\r\027[K" in
  let on_event e =
    update_phase phase e;
    match Event.to_display e with
    | Some line ->
        clear ();
        Stdlib.Printf.eprintf "%s\n%!" line
    | None -> ()
  in
  let tick () =
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

(* Full-screen view: header, rule, scrolling body, and a footer status line
   with the spinner. *)
let make_tui_reporter ~header =
  let module I = Notty.I in
  let module A = Notty.A in
  let term = Notty_unix.Term.create () in
  let lines = ref [] in
  let phase = make_phase () in
  let i = ref 0 in
  let redraw () =
    let w, h = Notty_unix.Term.size term in
    let body_rows = Int.max 1 (h - 4) in
    let shown = View.window ~rows:body_rows !lines in
    let colored s =
      let attr =
        match View.classify s with
        | `Ok -> A.fg A.green
        | `Err -> A.fg A.red
        | `Action -> A.fg A.yellow
        | `Plain -> A.empty
      in
      I.string attr s
    in
    let header_img = I.string A.(fg lightblue ++ st bold) header in
    let rule = I.string A.(fg lightblue) (String.make (Int.max 1 w) '-') in
    let body = I.vcat (List.map shown ~f:colored) in
    let footer =
      match phase_label !(fst phase) with
      | None -> I.string A.(fg (gray 12)) "done — ctrl-c to exit"
      | Some label ->
          let frame = spinner_frames.(!i % Array.length spinner_frames) in
          I.string
            A.(fg cyan)
            (Printf.sprintf "%s %s (%.0fs)" frame label
               (now () -. !(snd phase)))
    in
    Notty_unix.Term.image term
      (I.vcat [ header_img; rule; body; I.void 0 1; footer ])
  in
  redraw ();
  let on_event e =
    update_phase phase e;
    (match Event.to_display e with
    | Some line -> lines := !lines @ [ line ]
    | None -> ());
    redraw ()
  in
  let tick () =
    Int.incr i;
    redraw ()
  in
  let close () = Notty_unix.Term.release term in
  { on_event; tick; close }

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
  let reporter =
    if tui then
      make_tui_reporter
        ~header:(Printf.sprintf "fp-agent  %s  —  %s" config.model task)
    else make_plain_reporter ()
  in
  let policy = policy_of ~confirm in
  warn_yolo yolo;
  let outcome =
    Lwt_main.run
      (run_with_reporter reporter
         (Agent_loop.run ~on_event:reporter.on_event ~policy
            ~on_approval:prompt_approval ~initial_history ~yolo ~config
            ~model_client ~event_log ~workspace ~task ()))
  in
  reporter.close ();
  Event_log.close event_log;
  print_summary outcome;
  print_changes root;
  match outcome.status with Agent_loop.Completed -> 0 | _ -> 1

let tool_catalog =
  [
    ("read_file", "{path}");
    ("write_file", "{path, content}");
    ("edit_file", "{path, old, new}");
    ("list_files", "{path}");
    ("search", "{query, path?}");
    ("make_dir", "{path}");
    ("run_command", "{command, cwd?}");
    ("apply_patch", "{patch}");
    ("multi_edit", "{edits}");
  ]

let print_help () =
  Stdlib.print_endline
    "Commands:\n\
    \  /help              show this help\n\
    \  /tools             list available tools\n\
    \  /sessions          list sessions in this workspace\n\
    \  /tree              show the session fork tree\n\
    \  /resume <dir>      switch to a session (name under sessions/ or a path)\n\
    \  /model [id]        show or switch the current model\n\
    \  /models            list configured provider models\n\
    \  /provider <name> [model] [api-base]\n\
    \                     switch provider (e.g. local qwen2.5-coder:7b)\n\
    \  /log               list this session's events with indices\n\
    \  /fork [<index>]    fork the session (at an event index, or the end)\n\
    \  /diff              show uncommitted changes (git)\n\
    \  /undo              revert the last turn's changes (git)\n\
    \  /exit, /quit       leave the REPL\n\
     Anything else is sent to the agent as a task (context carries across \
     turns)."

let print_tools () =
  Stdlib.print_endline "Available tools:";
  List.iter tool_catalog ~f:(fun (n, a) ->
      Stdlib.Printf.printf "  %-12s %s\n" n a)

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
  let switch dir =
    Event_log.close !log;
    session := dir;
    log := Event_log.create ~session_dir:dir;
    Stdlib.Printf.printf "switched to %s\n%!" dir
  in
  let is_git = Stdlib.Sys.file_exists (Stdlib.Filename.concat root ".git") in
  let git args =
    Shell.run
      ~command:(Printf.sprintf "git -C %s %s" (Stdlib.Filename.quote root) args)
      ~timeout_sec:30
  in
  (* Keep the agent's own session log out of git operations so /undo never
     touches the event log we are actively writing. *)
  let exclude = "':(exclude).ocaml-agent'" in
  (* One git snapshot per task turn, so /undo reverts just the last turn. *)
  let checkpoints = ref [] in
  let checkpoint () =
    if is_git then (
      ignore (git ("add -A -- . " ^ exclude) : (Shell.result, string) Result.t);
      let sha =
        match git "stash create" with
        | Ok { stdout; _ } -> String.strip stdout
        | Error _ -> ""
      in
      let sha =
        if String.is_empty sha then
          match git "rev-parse HEAD" with
          | Ok { stdout; _ } -> String.strip stdout
          | Error _ -> ""
        else sha
      in
      checkpoints := sha :: !checkpoints)
  in
  let undo () =
    match !checkpoints with
    | [] -> Stdlib.print_endline "nothing to undo"
    | sha :: rest ->
        checkpoints := rest;
        if (not is_git) || String.is_empty sha then
          Stdlib.print_endline "no snapshot to restore"
        else (
          ignore
            (git (Printf.sprintf "checkout %s -- . %s" sha exclude)
              : (Shell.result, string) Result.t);
          ignore
            (git "clean -fd -e .ocaml-agent" : (Shell.result, string) Result.t);
          Stdlib.print_endline "reverted the last turn's changes")
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
  let switch_model model =
    config_ref := { !config_ref with model };
    model_client := Model_client.create ~config:!config_ref;
    show_current_model ()
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
          "Use /provider <name> [model] to switch provider, or /model <id> \
           within the current provider."
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
            config_ref :=
              {
                next with
                workspace_root = !config_ref.workspace_root;
                max_steps = !config_ref.max_steps;
              };
            model_client := Model_client.create ~config:!config_ref;
            show_current_model ())
  in
  let oneline s =
    let flat = String.substr_replace_all s ~pattern:"\n" ~with_:" " in
    if String.length flat > 60 then String.prefix flat 60 ^ "…" else flat
  in
  let describe_event (e : Event.t) =
    match e with
    | User_message { content } -> "user: " ^ oneline content
    | Model_response { action = Tool_call tc } ->
        "model → " ^ Event.describe_tool tc
    | Model_response { action = Tool_calls calls } ->
        Printf.sprintf "model → %d tool calls" (List.length calls)
    | Model_response { action = Final_answer _ } -> "model: final answer"
    | Tool_call tc -> "tool_call " ^ Event.describe_tool tc
    | Tool_result (Success _) -> "result ok"
    | Tool_result (Error _) -> "result err"
    | Graph_event event -> "graph " ^ Graph_event.describe event
    | Policy_decision { permission; _ } ->
        "policy " ^ Permission.to_string permission
    | State_transition { to_state; _ } ->
        "state → " ^ Agent_state.to_string to_state
  in
  let print_log () =
    match Journal.read ~session_dir:!session with
    | Error e -> Stdlib.print_endline e
    | Ok events ->
        List.iteri events ~f:(fun i e ->
            Stdlib.Printf.printf "  %3d  %s\n" i (describe_event e))
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
    checkpoint ();
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
  let rec loop () =
    Stdlib.print_string "\n> ";
    Stdlib.Out_channel.flush Stdlib.stdout;
    match Stdlib.In_channel.input_line Stdlib.stdin with
    | None -> ()
    | Some raw ->
        let line = String.strip raw in
        if String.is_empty line then loop ()
        else if String.equal line "/exit" || String.equal line "/quit" then ()
        else if String.equal line "/help" then (
          print_help ();
          loop ())
        else if String.equal line "/tools" then (
          print_tools ();
          loop ())
        else if String.equal line "/sessions" then (
          print_sessions sessions_root !session;
          loop ())
        else if String.equal line "/model" then (
          show_current_model ();
          loop ())
        else if String.is_prefix line ~prefix:"/model " then (
          switch_model (String.strip (String.drop_prefix line 7));
          loop ())
        else if String.equal line "/models" then (
          print_models ();
          loop ())
        else if String.is_prefix line ~prefix:"/provider " then (
          switch_provider (String.strip (String.drop_prefix line 10));
          loop ())
        else if String.equal line "/diff" then (
          show_diff ();
          loop ())
        else if String.equal line "/undo" then (
          undo ();
          loop ())
        else if String.equal line "/log" then (
          print_log ();
          loop ())
        else if String.equal line "/tree" then (
          print_tree ();
          loop ())
        else if String.equal line "/fork" then (
          do_fork "";
          loop ())
        else if String.is_prefix line ~prefix:"/fork " then (
          do_fork (String.strip (String.drop_prefix line 6));
          loop ())
        else if String.is_prefix line ~prefix:"/resume " then (
          let arg = String.strip (String.drop_prefix line 8) in
          let dir =
            if Stdlib.Filename.is_relative arg then
              Stdlib.Filename.concat sessions_root arg
            else arg
          in
          if Stdlib.Sys.file_exists (Stdlib.Filename.concat dir "events.jsonl")
          then switch dir
          else Stdlib.print_endline ("no such session: " ^ dir);
          loop ())
        else if String.is_prefix line ~prefix:"/" then (
          Stdlib.print_endline ("unknown command: " ^ line ^ " (try /help)");
          loop ())
        else (
          run_task line;
          loop ())
  in
  loop ();
  Event_log.close !log;
  0

let dispatch task provider api_base model workspace max_steps confirm resume tui
    yolo =
  match task with
  | Some _ when confirm && tui ->
      Stdlib.prerr_endline
        "--confirm cannot be combined with --tui; run without --tui when \
         approval prompts are required.";
      1
  | _ ->
      with_setup provider api_base model workspace max_steps
        (fun config workspace ->
          match task with
          | Some task ->
              run_oneshot config workspace ~confirm ~resume_opt:resume ~tui
                ~yolo ~task
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
            "Ask for confirmation on stdin before each shell command or file \
             modification. Cannot be combined with --tui.")
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
      const dispatch $ task $ provider $ api_base $ model $ workspace
      $ max_steps $ confirm $ resume $ tui $ yolo)
  in
  let info = Cmd.info "fp-agent" ~version:"0.1.0" ~doc in
  Stdlib.exit (Cmd.eval' (Cmd.v info term))
