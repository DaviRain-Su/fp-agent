open! Base

type t = {
  send :
    on_delta:(string -> unit) ->
    Message.t list ->
    (Model_action.t, string) Result.t Lwt.t;
}

let system_prompt =
  {|You are a coding agent operating inside a bounded workspace. You work toward the user's task by issuing tool calls and observing the results.

On every turn you MUST reply with a SINGLE JSON object and nothing else. Do not wrap it in markdown code fences. Do not add prose before or after.

To call a tool:
{"action":"tool_call","tool":"<name>","args":{...}}

To call several independent tools in one turn:
{"action":"tool_calls","calls":[{"tool":"<name>","args":{...}}, ...]}
Only batch tools that can safely run in the same turn.

Available tools and their args:
- read_file   {"path": string}
- write_file  {"path": string, "content": string}
- edit_file   {"path": string, "old": string, "new": string}   (replaces the first exact occurrence of "old")
- run_command {"command": string, "cwd": string (optional)}
- list_files  {"path": string}
- search      {"query": string, "path": string (optional)}   (substring search across workspace files)
- make_dir    {"path": string}
- apply_patch {"patch": string}   (a unified diff applied with git apply)
- multi_edit  {"edits": [{"path": string, "old": string, "new": string}, ...]}   (applied atomically)

When the task is complete, finish with:
{"action":"final_answer","summary": string, "details": string (optional)}

Rules:
- All paths are relative to the workspace root and may not escape it.
- You cannot modify the .git directory. Dangerous shell commands are rejected.
- Prefer small, verifiable steps. Inspect files before editing them.
- For codebase tasks, you MUST inspect relevant files with tools before giving a final answer. Do not answer "Hello" or ask how to help after the user has already given a task.
- If the provider offers native tool calling, use native tool calls; otherwise use the JSON action format above.|}

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

let strip_control_fields = function
  | `Assoc fields ->
      `Assoc
        (List.filter fields ~f:(fun (name, _) ->
             not
               (List.mem
                  [ "action"; "tool"; "args"; "arguments" ]
                  name ~equal:String.equal)))
  | json -> json

let build_tool tool args : (Tool_call.t, string) Result.t =
  Builtin_tools.register_all ();
  match Tool.find tool with
  | Some _ -> Ok (Tool_call.make ~name:tool ~args:(strip_control_fields args))
  | None -> Error ("unknown tool: " ^ tool)

let is_tool_name n =
  Builtin_tools.register_all ();
  Option.is_some (Tool.find n)

let args_object json =
  match Yojson.Safe.Util.member "args" json with
  | `Assoc _ as a -> a
  | `Null -> (
      match Yojson.Safe.Util.member "arguments" json with
      | `Assoc _ as a -> a
      | _ -> (
          match Yojson.Safe.Util.member "input" json with
          | `Assoc _ as a -> a
          | _ -> json))
  | _ -> json

let parse_tool_object json : (Tool_call.t, string) Result.t =
  let member = Yojson.Safe.Util.member in
  let as_tool tool = build_tool tool (args_object json) in
  match member "action" json with
  | `String "tool_call" -> (
      match get_string json "tool" with
      | Error e -> Error e
      | Ok tool -> as_tool tool)
  | `String a when is_tool_name a -> as_tool a
  | `String other -> Error ("unknown tool action in batch: " ^ other)
  | _ -> (
      match (member "tool" json, member "type" json, member "name" json) with
      | `String tool, _, _ -> as_tool tool
      | _, `String "tool_use", `String tool -> as_tool tool
      | _, _, `String tool -> as_tool tool
      | _ -> Error "batch item is missing a tool name")

let is_tool_like json =
  let member = Yojson.Safe.Util.member in
  match
    ( member "action" json,
      member "tool" json,
      member "type" json,
      member "name" json )
  with
  | `String "tool_call", _, _, _ -> true
  | `String a, _, _, _ when is_tool_name a -> true
  | _, `String _, _, _ -> true
  | _, _, `String "tool_use", `String _ -> true
  | _, _, _, `String name when is_tool_name name -> true
  | _ -> false

let text_block_text json =
  let member = Yojson.Safe.Util.member in
  match (member "type" json, member "text" json) with
  | `String "text", `String s -> Some s
  | _ -> None

let parse_tool_list = function
  | [] -> Error "tool_calls requires at least one call"
  | items ->
      let items = List.filter items ~f:is_tool_like in
      if List.is_empty items then
        Error "tool_calls requires at least one tool-like item"
      else
        List.fold items ~init:(Ok []) ~f:(fun acc json ->
            match acc with
            | Error _ as e -> e
            | Ok calls ->
                Result.map (parse_tool_object json) ~f:(fun tc -> tc :: calls))
        |> Result.map ~f:List.rev

(* Models vary in how strictly they follow the contract. Accept both the
   nested form ({"action":"tool_call","tool":..,"args":{..}}) and the common
   flat form ({"action":"write_file","path":..}), and read tool args from
   "args"/"arguments" or, failing that, the top-level object. *)
let parse_object json : (Model_action.t, string) Result.t =
  let member = Yojson.Safe.Util.member in
  let as_tool tool =
    Result.map
      (build_tool tool (args_object json))
      ~f:(fun tc -> Model_action.Tool_call tc)
  in
  let action_of_tool_calls calls =
    match List.filter calls ~f:is_tool_like with
    | tool_items when not (List.is_empty tool_items) ->
        Result.map (parse_tool_list tool_items) ~f:(fun calls ->
            match calls with
            | [ tc ] -> Model_action.Tool_call tc
            | calls -> Model_action.Tool_calls calls)
    | _ ->
        let texts = List.filter_map calls ~f:text_block_text in
        if not (List.is_empty texts) then
          Ok
            (Model_action.Final_answer
               { answer = String.concat ~sep:"" texts })
        else Error "tool_calls did not contain tool_use/function items"
  in
  let final () =
    let summary = Option.value (get_string_opt json "summary") ~default:"" in
    let details =
      match get_string_opt json "details" with
      | Some d when not (String.is_empty d) -> "\n\n" ^ d
      | _ -> ""
    in
    Ok (Model_action.Final_answer { answer = summary ^ details })
  in
  match member "action" json with
  | `String "tool_call" -> (
      match get_string json "tool" with
      | Error e -> Error e
      | Ok tool -> as_tool tool)
  | `String "tool_calls" | `String "parallel_tool_calls" -> (
      match member "calls" json with
      | `List calls -> action_of_tool_calls calls
      | _ -> (
          match member "tool_calls" json with
          | `List calls -> action_of_tool_calls calls
          | _ -> Error "tool_calls requires a 'calls' array"))
  | `String "final_answer" -> final ()
  | `String a when is_tool_name a -> as_tool a
  | `String other -> Error ("unknown action: " ^ other)
  | _ -> (
      (* no usable "action": try a bare "tool" field, else treat a
             "summary" as a final answer. *)
      match (member "tool" json, member "type" json, member "name" json) with
      | `String tool, _, _ -> as_tool tool
      | _, `String "tool_use", `String tool -> as_tool tool
      | _, _, `String tool -> as_tool tool
      | _ -> (
          match get_string_opt json "summary" with
          | Some _ -> final ()
          | None -> Error "missing or invalid 'action' field"))

let parse_action content : (Model_action.t, string) Result.t =
  let content = strip_fences content in
  match Yojson.Safe.from_string content with
  | exception exn ->
      Error ("model output is not valid JSON: " ^ Exn.to_string exn)
  | raw -> (
      (* Some models wrap a single action in an array. A multi-item array is
         treated as a batch of tool calls. *)
      try
        match raw with
        | `List [] -> Error "empty JSON array from model"
        | `List [ x ] -> (
            match (is_tool_like x, text_block_text x) with
            | true, _ -> parse_object x
            | false, Some text ->
                Ok (Model_action.Final_answer { answer = text })
            | false, None -> parse_object x)
        | `List xs ->
            let tool_items = List.filter xs ~f:is_tool_like in
            if not (List.is_empty tool_items) then
              Result.map (parse_tool_list tool_items) ~f:(fun calls ->
                  match calls with
                  | [ tc ] -> Model_action.Tool_call tc
                  | calls -> Model_action.Tool_calls calls)
            else
              let texts = List.filter_map xs ~f:text_block_text in
              if not (List.is_empty texts) then
                Ok
                  (Model_action.Final_answer
                     { answer = String.concat ~sep:"" texts })
              else Error "JSON array did not contain tool_use or text blocks"
        | `String s -> Ok (Model_action.Final_answer { answer = s })
        | json -> parse_object json
      with Yojson.Safe.Util.Type_error (msg, _) ->
        Error ("unexpected JSON shape from model: " ^ msg))

(* --- Native tool schemas --- *)

let props names =
  `Assoc
    (List.map names ~f:(fun name ->
         (name, `Assoc [ ("type", `String "string") ])))

let object_schema ?(required = []) properties =
  `Assoc
    [
      ("type", `String "object");
      ("properties", properties);
      ("required", `List (List.map required ~f:(fun s -> `String s)));
      ("additionalProperties", `Bool true);
    ]

let tool_parameters name =
  match name with
  | "read_file" | "list_files" | "make_dir" ->
      object_schema ~required:[ "path" ] (props [ "path" ])
  | "write_file" ->
      object_schema ~required:[ "path"; "content" ]
        (props [ "path"; "content" ])
  | "edit_file" ->
      object_schema ~required:[ "path"; "old"; "new" ]
        (props [ "path"; "old"; "new" ])
  | "run_command" ->
      object_schema ~required:[ "command" ] (props [ "command"; "cwd" ])
  | "search" -> object_schema ~required:[ "query" ] (props [ "query"; "path" ])
  | "apply_patch" -> object_schema ~required:[ "patch" ] (props [ "patch" ])
  | "multi_edit" ->
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "edits",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "items",
                        object_schema ~required:[ "path"; "old"; "new" ]
                          (props [ "path"; "old"; "new" ]) );
                    ] );
              ] );
          ("required", `List [ `String "edits" ]);
        ]
  | _ -> object_schema (`Assoc [])

let openai_tools () =
  Builtin_tools.register_all ();
  Tool.all ()
  |> List.map ~f:(fun (tool : Tool.t) ->
      `Assoc
        [
          ("type", `String "function");
          ( "function",
            `Assoc
              [
                ("name", `String tool.name);
                ("description", `String tool.description);
                ("parameters", tool_parameters tool.name);
              ] );
        ])

let anthropic_tools () =
  Builtin_tools.register_all ();
  Tool.all ()
  |> List.map ~f:(fun (tool : Tool.t) ->
      `Assoc
        [
          ("name", `String tool.name);
          ("description", `String tool.description);
          ("input_schema", tool_parameters tool.name);
        ])

(* --- OpenAI chat completions (zhipu, deepseek) --- *)

let openai_request (config : Config.t) messages =
  let uri = Uri.of_string (config.api_base ^ "/chat/completions") in
  let auth =
    if String.is_empty config.api_key then []
    else [ ("Authorization", "Bearer " ^ config.api_key) ]
  in
  let headers =
    Cohttp.Header.of_list (auth @ [ ("Content-Type", "application/json") ])
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
        ("stream", `Bool true);
        ("tools", `List (openai_tools ()));
        ("tool_choice", `String "auto");
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
        let message = json |> member "choices" |> index 0 |> member "message" in
        match message |> member "tool_calls" with
        | `List calls when not (List.is_empty calls) ->
            let tool_blocks =
              List.filter_map calls ~f:(fun call ->
                  let fn = call |> member "function" in
                  match fn |> member "name" with
                  | `String name ->
                      let input =
                        match fn |> member "arguments" with
                        | `String s -> (
                            match Yojson.Safe.from_string s with
                            | `Assoc _ as obj -> obj
                            | _ -> `Assoc []
                            | exception _ -> `Assoc [])
                        | `Assoc _ as obj -> obj
                        | _ -> `Assoc []
                      in
                      Some
                        (`Assoc
                           [
                             ("type", `String "tool_use");
                             ("name", `String name);
                             ("input", input);
                           ])
                  | _ -> None)
            in
            Ok (Yojson.Safe.to_string (`List tool_blocks))
        | _ -> (
            match message |> member "content" with
            | `String s -> Ok s
            | `Null -> Error "model response had neither content nor tool_calls"
            | other -> Ok (Yojson.Safe.to_string other))
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
        ("stream", `Bool true);
        ( "messages",
          `List
            (List.map turns ~f:(fun (m : Message.t) ->
                 `Assoc
                   [ ("role", `String m.role); ("content", `String m.content) ]))
        );
        ("tools", `List (anthropic_tools ()));
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
        let tool_blocks =
          List.filter blocks ~f:(fun b ->
              match member "type" b with
              | `String "tool_use" -> true
              | _ -> false)
        in
        if not (List.is_empty tool_blocks) then
          Ok (Yojson.Safe.to_string (`List tool_blocks))
        else
          match
            List.find_map blocks ~f:(fun b ->
                match member "text" b with `String s -> Some s | _ -> None)
          with
          | Some text -> Ok text
          | None -> Error "no text or tool_use block in model response"
      with exn -> Error ("unexpected messages shape: " ^ Exn.to_string exn))

(* --- Streaming SSE extraction --- *)

let sse_payloads body_str =
  body_str |> String.split_lines
  |> List.filter_map ~f:(fun line ->
      let line = String.strip line in
      if String.is_prefix line ~prefix:"data: " then
        Some (String.drop_prefix line 6 |> String.strip)
      else if String.is_prefix line ~prefix:"data:" then
        Some (String.drop_prefix line 5 |> String.strip)
      else None)
  |> List.filter ~f:(fun data -> not (String.equal data "[DONE]"))

let parse_json_opt s =
  match Yojson.Safe.from_string s with j -> Some j | exception _ -> None

type anthropic_builder =
  | AText of Buffer.t
  | AThinking of Buffer.t
  | ATool of { id : string; name : string; args : Buffer.t }

let assoc_set r key data =
  r := (key, data) :: List.Assoc.remove !r key ~equal:Int.equal

let assoc_find r key = List.Assoc.find !r key ~equal:Int.equal
let json_string_or_empty = function `String s -> s | _ -> ""

let parse_json_object_or_empty s =
  match Yojson.Safe.from_string (if String.is_empty s then "{}" else s) with
  | `Assoc _ as obj -> obj
  | _ -> `Assoc []
  | exception _ -> `Assoc []

let looks_like_internal_action_text s =
  let s = String.strip s in
  String.is_empty s
  || String.is_prefix s ~prefix:"{"
  || String.is_prefix s ~prefix:"["
  || String.is_prefix s ~prefix:"```json"
  || String.is_substring s ~substring:"\"action\""
  || String.is_substring s ~substring:"\"tool_use\""
  || String.is_substring s ~substring:"\"tool_calls\""

let emit_visible_text ~on_delta text =
  if not (looks_like_internal_action_text text) then on_delta text

let tool_action_from_blocks blocks =
  let calls =
    List.filter_map blocks ~f:(fun block ->
        match parse_tool_object block with
        | Ok tc -> Some tc
        | Error _ -> None)
  in
  match calls with
  | [] -> Error "no valid tool_use/function blocks in provider response"
  | [ tc ] -> Ok (Model_action.Tool_call tc)
  | calls -> Ok (Model_action.Tool_calls calls)

let anthropic_stream_extract ~on_delta body_str =
  let builders = ref [] in
  let texts = ref [] in
  let tool_blocks = ref [] in
  let finish idx =
    match assoc_find builders idx with
    | Some (AText b) -> texts := Buffer.contents b :: !texts
    | Some (AThinking _) -> ()
    | Some (ATool { name; args; _ }) ->
        tool_blocks :=
          `Assoc
            [
              ("type", `String "tool_use");
              ("name", `String name);
              ("input", parse_json_object_or_empty (Buffer.contents args));
            ]
          :: !tool_blocks
    | None -> ()
  in
  List.iter (sse_payloads body_str) ~f:(fun data ->
      match parse_json_opt data with
      | None -> ()
      | Some json -> (
          let open Yojson.Safe.Util in
          match member "type" json with
          | `String "content_block_start" -> (
              let idx = json |> member "index" |> to_int in
              let block = json |> member "content_block" in
              match block |> member "type" with
              | `String "text" ->
                  assoc_set builders idx (AText (Buffer.create 256))
              | `String "thinking" ->
                  assoc_set builders idx (AThinking (Buffer.create 256))
              | `String "tool_use" ->
                  assoc_set builders idx
                    (ATool
                       {
                         id = json_string_or_empty (member "id" block);
                         name = json_string_or_empty (member "name" block);
                         args = Buffer.create 256;
                       })
              | _ -> ())
          | `String "content_block_delta" -> (
              let idx = json |> member "index" |> to_int in
              let delta = json |> member "delta" in
              match (member "type" delta, assoc_find builders idx) with
              | `String "text_delta", Some (AText b) ->
                  let s = json_string_or_empty (member "text" delta) in
                  Buffer.add_string b s
              | `String "thinking_delta", Some (AThinking b) ->
                  Buffer.add_string b
                    (json_string_or_empty (member "thinking" delta))
              | `String "input_json_delta", Some (ATool t) ->
                  Buffer.add_string t.args
                    (json_string_or_empty (member "partial_json" delta))
              | _ -> ())
          | `String "content_block_stop" ->
              finish (json |> member "index" |> to_int)
          | `String "error" ->
              texts := ("model stream error: " ^ data) :: !texts
          | _ -> ()));
  match List.rev !tool_blocks with
  | _ :: _ as tools -> tool_action_from_blocks tools
  | [] ->
      let text = String.concat ~sep:"" (List.rev !texts) in
      if String.is_empty text then
        Result.bind (anthropic_extract body_str) ~f:parse_action
      else if looks_like_internal_action_text text then parse_action text
      else (
        emit_visible_text ~on_delta text;
        Ok (Model_action.Final_answer { answer = text }))

type openai_tool_builder = {
  id : string ref;
  name : string ref;
  args : Buffer.t;
}

let openai_stream_extract ~on_delta body_str =
  let text = Buffer.create 256 in
  let tools = ref [] in
  let get_tool idx =
    match assoc_find tools idx with
    | Some t -> t
    | None ->
        let t = { id = ref ""; name = ref ""; args = Buffer.create 256 } in
        assoc_set tools idx t;
        t
  in
  List.iter (sse_payloads body_str) ~f:(fun data ->
      match parse_json_opt data with
      | None -> ()
      | Some json -> (
          let open Yojson.Safe.Util in
          match member "choices" json with
          | `List (choice :: _) -> (
              let delta = member "delta" choice in
              (match member "content" delta with
              | `String s -> Buffer.add_string text s
              | _ -> ());
              match member "tool_calls" delta with
              | `List calls ->
                  List.iter calls ~f:(fun call ->
                      let idx =
                        match member "index" call with `Int i -> i | _ -> 0
                      in
                      let tool = get_tool idx in
                      (match member "id" call with
                      | `String s when not (String.is_empty s) -> tool.id := s
                      | _ -> ());
                      let fn = member "function" call in
                      (match member "name" fn with
                      | `String s when not (String.is_empty s) -> tool.name := s
                      | _ -> ());
                      match member "arguments" fn with
                      | `String s -> Buffer.add_string tool.args s
                      | _ -> ())
              | _ -> ())
          | _ -> ()));
  let tool_blocks =
    !tools
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> List.filter_map ~f:(fun (_, t) ->
        if String.is_empty !(t.name) then None
        else
          Some
            (`Assoc
               [
                 ("type", `String "tool_use");
                 ("name", `String !(t.name));
                 ("input", parse_json_object_or_empty (Buffer.contents t.args));
               ]))
  in
  match tool_blocks with
  | _ :: _ -> tool_action_from_blocks tool_blocks
  | [] ->
      let text = Buffer.contents text in
      if String.is_empty text then Result.bind (openai_extract body_str) ~f:parse_action
      else if looks_like_internal_action_text text then parse_action text
      else (
        emit_visible_text ~on_delta text;
        Ok (Model_action.Final_answer { answer = text }))

(* --- shared HTTP plumbing --- *)

let post_and_parse ?(on_delta = fun _ -> ()) (uri, headers, body_json) ~extract
    =
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
          let stream = Cohttp_lwt.Body.to_stream rbody in
          Lwt.bind (Lwt_stream.to_list stream) (fun chunks ->
              let body_str = String.concat chunks in
              let code =
                Cohttp.Code.code_of_status (Cohttp.Response.status resp)
              in
              if code >= 400 then
                let snippet =
                  let s = String.strip body_str in
                  if String.length s > 400 then String.prefix s 400 ^ "…" else s
                in
                Lwt.return
                  (Error (Printf.sprintf "model API HTTP %d: %s" code snippet))
              else
                match extract ~on_delta body_str with
                | Error e -> Lwt.return (Error e)
                | Ok action -> Lwt.return (Ok action)))

let real_send (config : Config.t) ~on_delta messages =
  match config.protocol with
  | Provider.Openai ->
      post_and_parse ~on_delta
        (openai_request config messages)
        ~extract:openai_stream_extract
  | Provider.Anthropic ->
      post_and_parse ~on_delta
        (anthropic_request config messages)
        ~extract:anthropic_stream_extract

let create ~config =
  { send = (fun ~on_delta messages -> real_send config ~on_delta messages) }

let create_mock ~send = { send = (fun ~on_delta:_ messages -> send messages) }
let send ?(on_delta = fun _ -> ()) t ~messages = t.send ~on_delta messages
