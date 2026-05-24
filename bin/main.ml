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

(* Full-screen live view driven by the event stream. Keeps a rolling buffer of
   display lines and redraws on each event; no keyboard input (the agent runs
   autonomously). *)
let make_tui_reporter ~header =
  let module I = Notty.I in
  let module A = Notty.A in
  let term = Notty_unix.Term.create () in
  let lines = ref [] in
  let redraw () =
    let _, h = Notty_unix.Term.size term in
    let body_rows = Int.max 1 (h - 3) in
    let shown = List.rev (List.take (List.rev !lines) body_rows) in
    let colored s =
      let attr =
        if String.is_prefix (String.lstrip s) ~prefix:"✓" then A.fg A.green
        else if String.is_prefix (String.lstrip s) ~prefix:"✗" then A.fg A.red
        else if String.is_prefix s ~prefix:"→" then A.fg A.yellow
        else A.empty
      in
      I.string attr s
    in
    let header_img = I.string A.(fg lightblue ++ st bold) header in
    let body = I.vcat (List.map shown ~f:colored) in
    Notty_unix.Term.image term (I.vcat [ header_img; I.void 0 1; body ])
  in
  redraw ();
  let on_event e =
    match Event.to_display e with
    | Some line ->
        lines := !lines @ [ line ];
        redraw ()
    | None -> ()
  in
  let close () = Notty_unix.Term.release term in
  (on_event, close)

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

let stderr_reporter e =
  match Event.to_display e with
  | Some line -> Stdlib.Printf.eprintf "%s\n%!" line
  | None -> ()

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
  let on_event, close_view =
    if tui then
      make_tui_reporter
        ~header:(Printf.sprintf "fp-agent  %s  —  %s" config.model task)
    else (stderr_reporter, fun () -> ())
  in
  let policy = policy_of ~confirm:(confirm && not tui) in
  warn_yolo yolo;
  let outcome =
    Lwt_main.run
      (Agent_loop.run ~on_event ~policy ~on_approval:prompt_approval
         ~initial_history ~yolo ~config ~model_client ~event_log ~workspace
         ~task ())
  in
  close_view ();
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
  ]

let print_help () =
  Stdlib.print_endline
    "Commands:\n\
    \  /help              show this help\n\
    \  /tools             list available tools\n\
    \  /sessions          list sessions in this workspace\n\
    \  /resume <dir>      switch to a session (name under sessions/ or a path)\n\
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
  let model_client = Model_client.create ~config in
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
  Stdlib.Printf.eprintf
    "fp-agent REPL — model %s. Type /help for commands, /exit to quit.\n\
     session: %s\n\
     %!"
    config.Config.model !session;
  warn_yolo yolo;
  let run_task task =
    let initial_history =
      match Transcript.of_session ~session_dir:!session with
      | Ok h -> h
      | Error _ -> []
    in
    let outcome =
      Lwt_main.run
        (Agent_loop.run ~on_event:stderr_reporter ~policy
           ~on_approval:prompt_approval ~initial_history ~yolo ~config
           ~model_client ~event_log:!log ~workspace ~task ())
    in
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
  with_setup provider api_base model workspace max_steps
    (fun config workspace ->
      match task with
      | Some task ->
          run_oneshot config workspace ~confirm ~resume_opt:resume ~tui ~yolo
            ~task
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
            "Model provider: kimi (default), zhipu, or deepseek. Also reads \
             the PROVIDER env var.")
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
             modification.")
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
