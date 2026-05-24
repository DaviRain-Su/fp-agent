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

let run_agent task provider_opt api_base_opt model_opt workspace_opt
    max_steps_opt confirm resume_opt tui =
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
      | Ok workspace -> (
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
          Stdlib.Printf.eprintf "model: %s @ %s\nsession: %s%s\n%!" config.model
            config.api_base session_dir
            (if List.is_empty initial_history then ""
             else
               Printf.sprintf " (resumed, %d prior messages)"
                 (List.length initial_history));
          let event_log = Event_log.create ~session_dir in
          let model_client = Model_client.create ~config in
          (* TUI takes over the screen, so it runs autonomously (no stdin
             approval prompts) and the stderr stream is replaced by the view. *)
          let on_event, close_view =
            if tui then
              make_tui_reporter
                ~header:(Printf.sprintf "fp-agent  %s  —  %s" config.model task)
            else
              ( (fun e ->
                  match Event.to_display e with
                  | Some line -> Stdlib.Printf.eprintf "%s\n%!" line
                  | None -> ()),
                fun () -> () )
          in
          let policy =
            if confirm && not tui then
              { Policy.approve_commands = true; approve_writes = true }
            else Policy.default
          in
          let outcome =
            Lwt_main.run
              (Agent_loop.run ~on_event ~policy ~on_approval:prompt_approval
                 ~initial_history ~config ~model_client ~event_log ~workspace
                 ~task ())
          in
          close_view ();
          Event_log.close event_log;
          print_summary outcome;
          print_changes root;
          match outcome.status with Agent_loop.Completed -> 0 | _ -> 1))

let () =
  let open Cmdliner in
  let task =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"TASK" ~doc:"The coding task for the agent to perform.")
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
  let doc = "A type-safe local CLI code agent harness." in
  let term =
    Term.(
      const run_agent $ task $ provider $ api_base $ model $ workspace
      $ max_steps $ confirm $ resume $ tui)
  in
  let info = Cmd.info "fp-agent" ~version:"0.1.0" ~doc in
  Stdlib.exit (Cmd.eval' (Cmd.v info term))
