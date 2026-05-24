open! Base

type id =
  | Help
  | Tools
  | Tool
  | Plugins
  | Plugin
  | PluginNew
  | PluginDev
  | PluginCheck
  | PluginInstall
  | PluginRemove
  | PluginSmoke
  | PluginRun
  | PluginDoctor
  | Sessions
  | Tree
  | NewSession
  | Resume
  | Model
  | ModelNext
  | Models
  | Provider
  | Log
  | Inspect
  | Usage
  | Status
  | Instructions
  | Compact
  | Fork
  | Diff
  | Retry
  | Undo
  | Exit

type entry = { command : string; description : string; group : string }
type acceptance = Execute of string | Draft of string

type spec = {
  id : id;
  command : string;
  description : string;
  aliases : string list;
  palette : bool;
  acceptance : acceptance;
}

type parse_result =
  | Empty
  | Task of string
  | Command of id * string
  | Unknown of string

let specs =
  [
    {
      id = Help;
      command = "/help";
      description = "show this help";
      aliases = [];
      palette = false;
      acceptance = Execute "/help";
    };
    {
      id = Tools;
      command = "/tools";
      description = "list available tools";
      aliases = [];
      palette = true;
      acceptance = Execute "/tools";
    };
    {
      id = Tool;
      command = "/tool <name>";
      description = "show tool details/schema";
      aliases = [];
      palette = true;
      acceptance = Draft "/tool ";
    };
    {
      id = Plugins;
      command = "/plugins";
      description = "list discovered plugins";
      aliases = [];
      palette = true;
      acceptance = Execute "/plugins";
    };
    {
      id = Plugin;
      command = "/plugin <id|tool>";
      description = "show plugin manifest/tool details";
      aliases = [];
      palette = true;
      acceptance = Draft "/plugin ";
    };
    {
      id = PluginNew;
      command =
        "/plugin-new [--id ID] [--tool-name NAME] [--kind KIND] [--template \
         NAME] <dir>";
      description = "create a plugin scaffold";
      aliases = [];
      palette = true;
      acceptance = Draft "/plugin-new ";
    };
    {
      id = PluginDev;
      command = "/plugin-dev [--replace] <dir>";
      description = "check, smoke, and install a plugin";
      aliases = [];
      palette = true;
      acceptance = Draft "/plugin-dev ";
    };
    {
      id = PluginCheck;
      command = "/plugin-check [--replace] <dir>";
      description = "validate a plugin directory";
      aliases = [];
      palette = true;
      acceptance = Draft "/plugin-check ";
    };
    {
      id = PluginInstall;
      command = "/plugin-install [--replace] <dir>";
      description = "install a plugin directory";
      aliases = [];
      palette = true;
      acceptance = Draft "/plugin-install ";
    };
    {
      id = PluginRemove;
      command = "/plugin-remove <id>";
      description = "remove an installed plugin";
      aliases = [ "/plugin-uninstall" ];
      palette = true;
      acceptance = Draft "/plugin-remove ";
    };
    {
      id = PluginSmoke;
      command = "/plugin-smoke [--replace] <dir>";
      description = "run plugin smoke tests";
      aliases = [];
      palette = true;
      acceptance = Draft "/plugin-smoke ";
    };
    {
      id = PluginRun;
      command = "/plugin-run <dir> <tool> <json|@file>";
      description = "run one plugin tool locally";
      aliases = [];
      palette = true;
      acceptance = Draft "/plugin-run ";
    };
    {
      id = PluginDoctor;
      command = "/plugin-doctor";
      description = "show plugin search path and diagnostics";
      aliases = [ "/plugins-doctor" ];
      palette = true;
      acceptance = Execute "/plugin-doctor";
    };
    {
      id = Sessions;
      command = "/sessions";
      description = "list sessions in this workspace";
      aliases = [];
      palette = true;
      acceptance = Execute "/sessions";
    };
    {
      id = Tree;
      command = "/tree";
      description = "show the session fork tree";
      aliases = [];
      palette = true;
      acceptance = Execute "/tree";
    };
    {
      id = NewSession;
      command = "/new";
      description = "start a fresh session";
      aliases = [];
      palette = true;
      acceptance = Draft "/new";
    };
    {
      id = Resume;
      command = "/resume <dir>";
      description = "switch to a session";
      aliases = [];
      palette = true;
      acceptance = Draft "/resume ";
    };
    {
      id = Model;
      command = "/model [id]";
      description = "show or switch the current model";
      aliases = [];
      palette = true;
      acceptance = Execute "/model";
    };
    {
      id = ModelNext;
      command = "/model-next";
      description = "cycle to the next configured model";
      aliases = [ "/model-cycle" ];
      palette = true;
      acceptance = Execute "/model-next";
    };
    {
      id = Models;
      command = "/models";
      description = "list configured provider models";
      aliases = [];
      palette = true;
      acceptance = Execute "/models";
    };
    {
      id = Provider;
      command = "/provider <name> [model] [api-base]";
      description = "switch provider catalog entry";
      aliases = [];
      palette = true;
      acceptance = Draft "/provider ";
    };
    {
      id = Log;
      command = "/log";
      description = "list this session's events with indices";
      aliases = [];
      palette = true;
      acceptance = Execute "/log";
    };
    {
      id = Inspect;
      command = "/inspect [index]";
      description = "show inspector details for an event";
      aliases = [];
      palette = true;
      acceptance = Execute "/inspect";
    };
    {
      id = Usage;
      command = "/usage";
      description = "show token usage from the event log";
      aliases = [];
      palette = true;
      acceptance = Execute "/usage";
    };
    {
      id = Status;
      command = "/status";
      description = "show runtime, session, usage, and plugin status";
      aliases = [];
      palette = true;
      acceptance = Execute "/status";
    };
    {
      id = Instructions;
      command = "/instructions";
      description = "show workspace project instructions";
      aliases = [];
      palette = true;
      acceptance = Execute "/instructions";
    };
    {
      id = Compact;
      command = "/compact";
      description = "summarize older session history";
      aliases = [];
      palette = true;
      acceptance = Draft "/compact";
    };
    {
      id = Fork;
      command = "/fork [index]";
      description = "fork the session";
      aliases = [];
      palette = true;
      acceptance = Draft "/fork ";
    };
    {
      id = Diff;
      command = "/diff";
      description = "show uncommitted changes";
      aliases = [];
      palette = true;
      acceptance = Execute "/diff";
    };
    {
      id = Retry;
      command = "/retry";
      description = "rerun the latest user task";
      aliases = [];
      palette = true;
      acceptance = Draft "/retry";
    };
    {
      id = Undo;
      command = "/undo";
      description = "revert the last turn's changes";
      aliases = [];
      palette = true;
      acceptance = Draft "/undo";
    };
    {
      id = Exit;
      command = "/exit";
      description = "leave the REPL";
      aliases = [ "/quit" ];
      palette = false;
      acceptance = Draft "/exit";
    };
  ]

let command_token command =
  match String.lsplit2 command ~on:' ' with
  | None -> command
  | Some (token, _) -> token

let group_of_id = function
  | Tools | Tool -> "Tools"
  | Plugins | Plugin | PluginNew | PluginDev | PluginCheck | PluginInstall
  | PluginRemove | PluginSmoke | PluginRun | PluginDoctor ->
      "Plugins"
  | Sessions | Tree | NewSession | Resume -> "Sessions"
  | Model | ModelNext | Models | Provider -> "Models"
  | Log | Inspect | Usage | Status | Instructions -> "Context"
  | Compact | Fork | Diff | Retry | Undo -> "Run Control"
  | Help | Exit -> "Shell"

let entry_of_spec spec =
  {
    command = spec.command;
    description = spec.description;
    group = group_of_id spec.id;
  }

let palette_entries =
  specs
  |> List.filter ~f:(fun spec -> spec.palette)
  |> List.map ~f:entry_of_spec

let help_line spec =
  let command =
    match spec.aliases with
    | [] -> spec.command
    | aliases -> String.concat (spec.command :: aliases) ~sep:", "
  in
  Printf.sprintf "  %-38s %s" command spec.description

let help_text () =
  let groups =
    List.group specs ~break:(fun left right ->
        not (String.equal (group_of_id left.id) (group_of_id right.id)))
  in
  let group_lines specs =
    match specs with
    | [] -> []
    | first :: _ -> (group_of_id first.id ^ ":") :: List.map specs ~f:help_line
  in
  String.concat ~sep:"\n"
    (("Commands:" :: List.concat_map groups ~f:group_lines)
    @ [
        " Anything else is sent to the agent as a task (context carries across \
         turns).";
      ])

let find_spec token =
  List.find specs ~f:(fun spec ->
      String.equal token (command_token spec.command)
      || List.exists spec.aliases ~f:(String.equal token))

let accept (entry : entry) =
  match
    List.find specs ~f:(fun spec -> String.equal spec.command entry.command)
  with
  | None -> Draft entry.command
  | Some spec -> spec.acceptance

let parse raw =
  let line = String.strip raw in
  if String.is_empty line then Empty
  else if not (String.is_prefix line ~prefix:"/") then Task line
  else
    let token, args =
      match String.lsplit2 line ~on:' ' with
      | None -> (line, "")
      | Some (token, args) -> (token, String.strip args)
    in
    match find_spec token with
    | None -> Unknown line
    | Some spec -> Command (spec.id, args)
