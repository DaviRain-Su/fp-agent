open! Base

type t = { root : string; is_git : bool; checkpoints : string list ref }

let create ~root =
  {
    root;
    is_git = Stdlib.Sys.file_exists (Stdlib.Filename.concat root ".git");
    checkpoints = ref [];
  }

let exclude = "':(exclude).ocaml-agent'"

let git t args =
  Shell.run
    ~command:(Printf.sprintf "git -C %s %s" (Stdlib.Filename.quote t.root) args)
    ~timeout_sec:30

let output_or_empty = function
  | Ok { Shell.stdout; _ } -> String.strip stdout
  | Error _ -> ""

let checkpoint t =
  if t.is_git then (
    ignore (git t ("add -A -- . " ^ exclude) : (Shell.result, string) Result.t);
    let sha = output_or_empty (git t "stash create") in
    let sha =
      if String.is_empty sha then output_or_empty (git t "rev-parse HEAD")
      else sha
    in
    t.checkpoints := sha :: !(t.checkpoints))

let command_failed args = function
  | Error e -> Some e
  | Ok { Shell.exit_code = 0; _ } -> None
  | Ok { Shell.exit_code; stderr; stdout } ->
      let detail =
        if not (String.is_empty (String.strip stderr)) then stderr else stdout
      in
      Some
        (Printf.sprintf "git %s failed (exit %d): %s" args exit_code
           (String.strip detail))

let undo t =
  match !(t.checkpoints) with
  | [] -> [ "nothing to undo" ]
  | sha :: rest -> (
      t.checkpoints := rest;
      if (not t.is_git) || String.is_empty sha then [ "no snapshot to restore" ]
      else
        let checkout_args = Printf.sprintf "checkout %s -- . %s" sha exclude in
        match command_failed checkout_args (git t checkout_args) with
        | Some e -> [ e ]
        | None -> (
            let clean_args = "clean -fd -e .ocaml-agent" in
            match command_failed clean_args (git t clean_args) with
            | Some e -> [ e ]
            | None -> [ "reverted the last turn's changes" ]))
