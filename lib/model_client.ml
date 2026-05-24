open! Base

type t = { send : Message.t list -> (Model_action.t, string) Result.t Lwt.t }

let system_prompt =
  {|You are a coding agent operating inside a sandboxed workspace. You work toward the user's task by issuing one tool call at a time and observing the result.

On every turn you MUST reply with a SINGLE JSON object and nothing else. Do not wrap it in markdown code fences. Do not add prose before or after.

To call a tool:
{"action":"tool_call","tool":"<name>","args":{...}}

Available tools and their args:
- read_file   {"path": string}
- write_file  {"path": string, "content": string}
- edit_file   {"path": string, "old": string, "new": string}   (replaces the first exact occurrence of "old")
- run_command {"command": string, "cwd": string (optional)}
- list_files  {"path": string}

When the task is complete, finish with:
{"action":"final_answer","summary": string, "details": string (optional)}

Rules:
- All paths are relative to the workspace root and may not escape it.
- You cannot modify the .git directory. Dangerous shell commands are rejected.
- Prefer small, verifiable steps. Inspect files before editing them.|}

(* Lenient stripping of accidental ``` fences the model may add despite the
   contract. *)
let strip_fences s =
  let s = String.strip s in
  if String.is_prefix s ~prefix:"```" then
    let s =
      match String.index s '\n' with
      | Some i -> String.drop_prefix s (i + 1)
      | None -> s
    in
    let s = String.strip s in
    if String.is_suffix s ~suffix:"```" then
      String.strip (String.drop_suffix s 3)
    else s
  else s

let get_string obj name =
  match Yojson.Safe.Util.member name obj with
  | `String s -> Ok s
  | _ -> Error (Printf.sprintf "expected string field '%s'" name)

let get_string_opt obj name =
  match Yojson.Safe.Util.member name obj with `String s -> Some s | _ -> None

let build_tool tool args : (Tool_call.t, string) Result.t =
  match tool with
  | "read_file" ->
      Result.map (get_string args "path") ~f:(fun path ->
          Tool_call.Read_file { path })
  | "list_files" ->
      Result.map (get_string args "path") ~f:(fun path ->
          Tool_call.List_files { path })
  | "write_file" -> (
      match (get_string args "path", get_string args "content") with
      | Ok path, Ok content -> Ok (Tool_call.Write_file { path; content })
      | Error e, _ | _, Error e -> Error e)
  | "edit_file" -> (
      match
        (get_string args "path", get_string args "old", get_string args "new")
      with
      | Ok path, Ok old_text, Ok new_text ->
          Ok (Tool_call.Edit_file { path; old_text; new_text })
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | "run_command" ->
      Result.map (get_string args "command") ~f:(fun command ->
          Tool_call.Run_command { command; cwd = get_string_opt args "cwd" })
  | other -> Error ("unknown tool: " ^ other)

let parse_action content : (Model_action.t, string) Result.t =
  let content = strip_fences content in
  match Yojson.Safe.from_string content with
  | exception exn ->
      Error ("model output is not valid JSON: " ^ Exn.to_string exn)
  | json -> (
      match Yojson.Safe.Util.member "action" json with
      | `String "tool_call" -> (
          match get_string json "tool" with
          | Error e -> Error e
          | Ok tool ->
              let args = Yojson.Safe.Util.member "args" json in
              Result.map (build_tool tool args) ~f:(fun tc ->
                  Model_action.Tool_call tc))
      | `String "final_answer" ->
          let summary =
            Option.value (get_string_opt json "summary") ~default:""
          in
          let details =
            match get_string_opt json "details" with
            | Some d when not (String.is_empty d) -> "\n\n" ^ d
            | _ -> ""
          in
          Ok (Model_action.Final_answer { answer = summary ^ details })
      | `String other -> Error ("unknown action: " ^ other)
      | _ -> Error "missing or invalid 'action' field")

(* --- OpenAI chat completions (zhipu, deepseek) --- *)

let openai_request (config : Config.t) messages =
  let uri = Uri.of_string (config.api_base ^ "/chat/completions") in
  let headers =
    Cohttp.Header.of_list
      [
        ("Authorization", "Bearer " ^ config.api_key);
        ("Content-Type", "application/json");
      ]
  in
  let body_json =
    `Assoc
      [
        ("model", `String config.model);
        ( "messages",
          `List
            (List.map messages ~f:(fun (m : Message.t) ->
                 `Assoc
                   [ ("role", `String m.role); ("content", `String m.content) ]))
        );
        ("temperature", `Float 0.0);
      ]
  in
  (uri, headers, body_json)

let openai_extract body_str =
  match Yojson.Safe.from_string body_str with
  | exception exn ->
      Error ("invalid JSON in model response: " ^ Exn.to_string exn)
  | json -> (
      try
        let open Yojson.Safe.Util in
        Ok
          (json |> member "choices" |> index 0 |> member "message"
         |> member "content" |> to_string)
      with exn -> Error ("unexpected completion shape: " ^ Exn.to_string exn))

(* --- Anthropic Messages (Kimi for coding) --- *)

let anthropic_request (config : Config.t) messages =
  let uri = Uri.of_string (config.api_base ^ "/v1/messages") in
  let headers =
    Cohttp.Header.of_list
      [
        ("Authorization", "Bearer " ^ config.api_key);
        ("anthropic-version", "2023-06-01");
        ("Content-Type", "application/json");
      ]
  in
  let system =
    List.filter_map messages ~f:(fun (m : Message.t) ->
        if String.equal m.role "system" then Some m.content else None)
    |> String.concat ~sep:"\n\n"
  in
  let turns =
    List.filter messages ~f:(fun (m : Message.t) ->
        not (String.equal m.role "system"))
  in
  let body_json =
    `Assoc
      [
        ("model", `String config.model);
        ("max_tokens", `Int 4096);
        ("system", `String system);
        ( "messages",
          `List
            (List.map turns ~f:(fun (m : Message.t) ->
                 `Assoc
                   [ ("role", `String m.role); ("content", `String m.content) ]))
        );
      ]
  in
  (uri, headers, body_json)

let anthropic_extract body_str =
  match Yojson.Safe.from_string body_str with
  | exception exn ->
      Error ("invalid JSON in model response: " ^ Exn.to_string exn)
  | json -> (
      try
        let open Yojson.Safe.Util in
        let blocks = json |> member "content" |> to_list in
        match
          List.find_map blocks ~f:(fun b ->
              match member "text" b with `String s -> Some s | _ -> None)
        with
        | Some text -> Ok text
        | None -> Error "no text block in model response"
      with exn -> Error ("unexpected messages shape: " ^ Exn.to_string exn))

(* --- shared HTTP plumbing --- *)

let post_and_parse (uri, headers, body_json) ~extract =
  let body = Cohttp_lwt.Body.of_string (Yojson.Safe.to_string body_json) in
  Lwt.bind
    (Lwt.catch
       (fun () ->
         Lwt.map
           (fun x -> Ok x)
           (Cohttp_lwt_unix.Client.post ~headers ~body uri))
       (fun exn ->
         Lwt.return (Error ("HTTP request failed: " ^ Exn.to_string exn))))
    (function
      | Error e -> Lwt.return (Error e)
      | Ok (resp, rbody) ->
          Lwt.bind (Cohttp_lwt.Body.to_string rbody) (fun body_str ->
              let code =
                Cohttp.Code.code_of_status (Cohttp.Response.status resp)
              in
              if code >= 400 then
                Lwt.return (Error (Printf.sprintf "model API HTTP %d" code))
              else
                match extract body_str with
                | Error e -> Lwt.return (Error e)
                | Ok content -> Lwt.return (parse_action content)))

let real_send (config : Config.t) messages =
  match config.protocol with
  | Provider.Openai ->
      post_and_parse (openai_request config messages) ~extract:openai_extract
  | Provider.Anthropic ->
      post_and_parse
        (anthropic_request config messages)
        ~extract:anthropic_extract

let create ~config = { send = (fun messages -> real_send config messages) }
let create_mock ~send = { send }
let send t ~messages = t.send messages
