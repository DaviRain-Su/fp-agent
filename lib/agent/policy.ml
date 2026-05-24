open! Base

let check ?(yolo = false) ~workspace ~tool_call () =
  Tool_loader.register_all ();
  match Tool.find tool_call.Tool_call.name with
  | None -> Permission.Deny ("unknown tool: " ^ tool_call.Tool_call.name)
  | Some tool -> (
      match tool.Tool.check workspace tool_call.Tool_call.args with
      | Permission.Deny _ when yolo && Poly.equal tool.Tool.kind Tool.Exec ->
          Permission.Allow
      | permission -> permission)

type t = { approve_commands : bool; approve_writes : bool }

let default = { approve_commands = false; approve_writes = false }

let approval_reason t (tool_call : Tool_call.t) =
  Tool_loader.register_all ();
  match Tool.find tool_call.Tool_call.name with
  | Some tool when t.approve_commands && Poly.equal tool.Tool.kind Tool.Exec ->
      Some "shell command requires approval"
  | Some tool when t.approve_writes && Poly.equal tool.Tool.kind Tool.Write ->
      Some "file modification requires approval"
  | Some tool when t.approve_commands || t.approve_writes ->
      tool.Tool.approval_reason
  | _ -> None
