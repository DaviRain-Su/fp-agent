open! Base

type id =
  | Help
  | Tools
  | Tool
  | Plugins
  | Plugin
  | Sessions
  | Tree
  | Resume
  | Model
  | Models
  | Provider
  | Log
  | Inspect
  | Fork
  | Diff
  | Undo
  | Exit

type entry = { command : string; description : string }

type spec = {
  id : id;
  command : string;
  description : string;
  aliases : string list;
  palette : bool;
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
    };
    {
      id = Tools;
      command = "/tools";
      description = "list available tools";
      aliases = [];
      palette = true;
    };
    {
      id = Tool;
      command = "/tool <name>";
      description = "show tool details/schema";
      aliases = [];
      palette = true;
    };
    {
      id = Plugins;
      command = "/plugins";
      description = "list discovered plugins";
      aliases = [];
      palette = true;
    };
    {
      id = Plugin;
      command = "/plugin <id|tool>";
      description = "show plugin manifest/tool details";
      aliases = [];
      palette = true;
    };
    {
      id = Sessions;
      command = "/sessions";
      description = "list sessions in this workspace";
      aliases = [];
      palette = true;
    };
    {
      id = Tree;
      command = "/tree";
      description = "show the session fork tree";
      aliases = [];
      palette = true;
    };
    {
      id = Resume;
      command = "/resume <dir>";
      description = "switch to a session";
      aliases = [];
      palette = true;
    };
    {
      id = Model;
      command = "/model [id]";
      description = "show or switch the current model";
      aliases = [];
      palette = true;
    };
    {
      id = Models;
      command = "/models";
      description = "list configured provider models";
      aliases = [];
      palette = true;
    };
    {
      id = Provider;
      command = "/provider <name> [model] [api-base]";
      description = "switch provider catalog entry";
      aliases = [];
      palette = true;
    };
    {
      id = Log;
      command = "/log";
      description = "list this session's events with indices";
      aliases = [];
      palette = true;
    };
    {
      id = Inspect;
      command = "/inspect [index]";
      description = "show inspector details for an event";
      aliases = [];
      palette = true;
    };
    {
      id = Fork;
      command = "/fork [index]";
      description = "fork the session";
      aliases = [];
      palette = true;
    };
    {
      id = Diff;
      command = "/diff";
      description = "show uncommitted changes";
      aliases = [];
      palette = true;
    };
    {
      id = Undo;
      command = "/undo";
      description = "revert the last turn's changes";
      aliases = [];
      palette = true;
    };
    {
      id = Exit;
      command = "/exit";
      description = "leave the REPL";
      aliases = [ "/quit" ];
      palette = false;
    };
  ]

let command_token command =
  match String.lsplit2 command ~on:' ' with
  | None -> command
  | Some (token, _) -> token

let entry_of_spec spec =
  { command = spec.command; description = spec.description }

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
  String.concat ~sep:"\n"
    (("Commands:" :: List.map specs ~f:help_line)
    @ [
        " Anything else is sent to the agent as a task (context carries across \
         turns).";
      ])

let find_spec token =
  List.find specs ~f:(fun spec ->
      String.equal token (command_token spec.command)
      || List.exists spec.aliases ~f:(String.equal token))

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
