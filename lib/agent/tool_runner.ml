open! Base

let err message = Tool_result.Error { message }

let execute ws (tool_call : Tool_call.t) =
  Builtin_tools.register_all ();
  match Tool.find tool_call.Tool_call.name with
  | None -> err ("unknown tool: " ^ tool_call.Tool_call.name)
  | Some tool -> tool.Tool.run ws tool_call.Tool_call.args

let run ?(yolo = false) ~workspace ~tool_call () =
  match Policy.check ~yolo ~workspace ~tool_call () with
  | Permission.Deny reason -> err ("policy denied: " ^ reason)
  | Permission.Ask_user reason ->
      err ("requires user approval (not supported in MVP): " ^ reason)
  | Permission.Allow -> execute workspace tool_call
