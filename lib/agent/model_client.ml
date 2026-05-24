open! Base
open Llm

(* The public client keeps the agent loop small, but internally this file now
   follows the ocaml-agent Llm shape: provider protocols stream into normalized
   content blocks. Legacy JSON actions are parsed only as a fallback. *)
type t = {
  send :
    on_delta:(string -> unit) ->
    system:string ->
    Llm.turn list ->
    (Llm.content list * Llm.usage, string) Result.t Lwt.t;
}

let system_prompt =
  {|You are a coding agent operating inside a bounded workspace. You work toward the user's task by issuing tool calls and observing the results.

If the provider offers native tool calling, use native tool calls. When you are done, answer normally with a concise final response.

If native tool calling is unavailable or ignored, fall back to replying with a SINGLE JSON object and nothing else. Do not wrap fallback JSON in markdown code fences. Do not add prose before or after fallback JSON.

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

Fallback final answer:
{"action":"final_answer","summary": string, "details": string (optional)}

Rules:
- All paths are relative to the workspace root and may not escape it.
- You cannot modify the .git directory. Dangerous shell commands are rejected.
- Prefer small, verifiable steps. Inspect files before editing them.
- For codebase tasks, you MUST inspect relevant files with tools before giving a final answer. Do not answer "Hello" or ask how to help after the user has already given a task.
- If the provider offers native tool calling, use native tool calls; otherwise use the JSON action format above.|}

(* --- JSON action compatibility parser (legacy/fallback) --- *)

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
                  [ "action"; "tool"; "args"; "arguments"; "type"; "name" ]
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
      | `String s -> (
          match Yojson.Safe.from_string s with
          | `Assoc _ as a -> a
          | _ -> `Assoc []
          | exception _ -> `Assoc [])
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
      | _, _, `String tool when is_tool_name tool -> as_tool tool
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

let parse_tool_list items =
  match List.filter items ~f:is_tool_like with
  | [] -> Error "tool_calls requires at least one tool-like item"
  | items ->
      List.fold items ~init:(Ok []) ~f:(fun acc json ->
          match acc with
          | Error _ as e -> e
          | Ok calls ->
              Result.map (parse_tool_object json) ~f:(fun tc -> tc :: calls))
      |> Result.map ~f:List.rev

let action_of_tool_calls_json items =
  match List.filter items ~f:is_tool_like with
  | tool_items when not (List.is_empty tool_items) ->
      Result.map (parse_tool_list tool_items) ~f:(fun calls ->
          match calls with
          | [ tc ] -> Model_action.Tool_call tc
          | calls -> Model_action.Tool_calls calls)
  | _ ->
      let texts = List.filter_map items ~f:text_block_text in
      if not (List.is_empty texts) then
        Ok (Model_action.Final_answer { answer = String.concat ~sep:"" texts })
      else Error "JSON array did not contain tool_use or text blocks"

let parse_object json : (Model_action.t, string) Result.t =
  let member = Yojson.Safe.Util.member in
  let as_tool tool =
    Result.map
      (build_tool tool (args_object json))
      ~f:(fun tc -> Model_action.Tool_call tc)
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
      | `List calls -> action_of_tool_calls_json calls
      | _ -> (
          match member "tool_calls" json with
          | `List calls -> action_of_tool_calls_json calls
          | _ -> Error "tool_calls requires a 'calls' array"))
  | `String "final_answer" -> final ()
  | `String a when is_tool_name a -> as_tool a
  | `String other -> Error ("unknown action: " ^ other)
  | _ -> (
      match (member "tool" json, member "type" json, member "name" json) with
      | `String tool, _, _ -> as_tool tool
      | _, `String "tool_use", `String tool -> as_tool tool
      | _, _, `String tool when is_tool_name tool -> as_tool tool
      | _ -> (
          match get_string_opt json "summary" with
          | Some _ -> final ()
          | None -> Error "missing or invalid 'action' field"))

let parse_ppx_variant = function
  | `List [ `String "Tool_call"; json ] ->
      Result.map (parse_tool_object json) ~f:(fun tc ->
          Model_action.Tool_call tc)
  | `List [ `String "Tool_calls"; `List items ] ->
      action_of_tool_calls_json items
  | `List [ `String "Final_answer"; obj ] -> (
      match get_string_opt obj "answer" with
      | Some answer -> Ok (Model_action.Final_answer { answer })
      | None -> Error "Final_answer variant missing answer")
  | _ -> Error "not a ppx variant action"

let parse_action content : (Model_action.t, string) Result.t =
  let content = strip_fences content in
  match Yojson.Safe.from_string content with
  | exception exn ->
      Error ("model output is not valid JSON: " ^ Exn.to_string exn)
  | raw -> (
      match parse_ppx_variant raw with
      | Ok _ as ok -> ok
      | Error _ -> (
          try
            match raw with
            | `List [] -> Error "empty JSON array from model"
            | `List [ `String s ] ->
                Ok (Model_action.Final_answer { answer = s })
            | `List [ x ] -> (
                match (is_tool_like x, text_block_text x) with
                | true, _ -> parse_object x
                | false, Some text ->
                    Ok (Model_action.Final_answer { answer = text })
                | false, None -> Error "single-item JSON array is not an action"
                )
            | `List xs -> action_of_tool_calls_json xs
            | `String s -> Ok (Model_action.Final_answer { answer = s })
            | json -> parse_object json
          with Yojson.Safe.Util.Type_error (msg, _) ->
            Error ("unexpected JSON shape from model: " ^ msg)))

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

(* --- Provider serialization for normalized turns --- *)

let anthropic_block = function
  | Text s -> `Assoc [ ("type", `String "text"); ("text", `String s) ]
  | Thinking { text; signature } ->
      let fields =
        [ ("type", `String "thinking"); ("thinking", `String text) ]
      in
      let fields =
        if String.is_empty signature then fields
        else fields @ [ ("signature", `String signature) ]
      in
      `Assoc fields
  | Tool_use { id; name; input } ->
      `Assoc
        [
          ("type", `String "tool_use");
          ("id", `String id);
          ("name", `String name);
          ("input", input);
        ]
  | Tool_result { id; content } ->
      `Assoc
        [
          ("type", `String "tool_result");
          ("tool_use_id", `String id);
          ("content", `String content);
        ]

let anthropic_role = function User -> "user" | Assistant -> "assistant"

let anthropic_messages turns =
  turns
  |> List.filter ~f:(fun t -> not (List.is_empty t.content))
  |> List.map ~f:(fun t ->
      `Assoc
        [
          ("role", `String (anthropic_role t.role));
          ("content", `List (List.map t.content ~f:anthropic_block));
        ])

let openai_messages ~system turns =
  let sys_msg =
    `Assoc [ ("role", `String "system"); ("content", `String system) ]
  in
  let turn_to_messages t =
    match t.role with
    | User ->
        let user_texts =
          List.filter_map t.content ~f:(function Text s -> Some s | _ -> None)
        in
        let user_messages =
          match String.concat ~sep:"\n" user_texts with
          | "" -> []
          | text ->
              [ `Assoc [ ("role", `String "user"); ("content", `String text) ] ]
        in
        let tool_messages =
          List.filter_map t.content ~f:(function
            | Tool_result { id; content } ->
                Some
                  (`Assoc
                     [
                       ("role", `String "tool");
                       ("tool_call_id", `String id);
                       ("content", `String content);
                     ])
            | _ -> None)
        in
        user_messages @ tool_messages
    | Assistant ->
        let text =
          List.filter_map t.content ~f:(function Text s -> Some s | _ -> None)
          |> String.concat ~sep:"\n"
        in
        let reasoning =
          List.filter_map t.content ~f:(function
            | Thinking { text; _ } -> Some text
            | _ -> None)
          |> String.concat ~sep:"\n"
        in
        let tool_calls =
          List.filter_map t.content ~f:(function
            | Tool_use { id; name; input } ->
                Some
                  (`Assoc
                     [
                       ("id", `String id);
                       ("type", `String "function");
                       ( "function",
                         `Assoc
                           [
                             ("name", `String name);
                             ("arguments", `String (Yojson.Safe.to_string input));
                           ] );
                     ])
            | _ -> None)
        in
        let fields =
          [
            ("role", `String "assistant");
            ("content", if String.is_empty text then `Null else `String text);
          ]
        in
        let fields =
          if String.is_empty reasoning then fields
          else fields @ [ ("reasoning_content", `String reasoning) ]
        in
        let fields =
          if List.is_empty tool_calls then fields
          else fields @ [ ("tool_calls", `List tool_calls) ]
        in
        [ `Assoc fields ]
  in
  sys_msg :: List.concat_map turns ~f:turn_to_messages

(* --- Streaming helpers --- *)

let sse_data line =
  let line = String.strip line in
  if String.is_prefix line ~prefix:"data: " then
    Some (String.drop_prefix line 6)
  else if String.is_prefix line ~prefix:"data:" then
    Some (String.drop_prefix line 5)
  else None

type delta_visibility = Unknown | Visible | Hidden

type delta_filter = {
  on_delta : string -> unit;
  pending : Buffer.t;
  mutable visibility : delta_visibility;
}

let create_delta_filter on_delta =
  { on_delta; pending = Buffer.create 64; visibility = Unknown }

let looks_like_internal_action_text s =
  let s = String.strip s in
  String.is_empty s
  || String.is_prefix s ~prefix:"{"
  || String.is_prefix s ~prefix:"["
  || String.is_prefix s ~prefix:"```json"
  || String.is_substring s ~substring:"\"action\""
  || String.is_substring s ~substring:"\"tool_use\""
  || String.is_substring s ~substring:"\"tool_calls\""

let feed_delta filter chunk =
  match filter.visibility with
  | Visible -> filter.on_delta chunk
  | Hidden -> ()
  | Unknown ->
      Buffer.add_string filter.pending chunk;
      let pending = Buffer.contents filter.pending in
      let stripped = String.strip pending in
      if String.is_empty stripped then ()
      else if looks_like_internal_action_text stripped then
        filter.visibility <- Hidden
      else (
        filter.visibility <- Visible;
        Buffer.clear filter.pending;
        filter.on_delta pending)

let flush_delta filter final_text =
  match filter.visibility with
  | Visible | Hidden -> ()
  | Unknown ->
      if not (looks_like_internal_action_text final_text) then
        filter.on_delta final_text

let split_sse_lines chunks ~on_line =
  let buf = Buffer.create 256 in
  let rec drain () =
    let s = Buffer.contents buf in
    match String.index s '\n' with
    | None -> ()
    | Some i ->
        let line = String.prefix s i |> String.rstrip in
        let rest = String.drop_prefix s (i + 1) in
        Buffer.clear buf;
        Buffer.add_string buf rest;
        on_line line;
        drain ()
  in
  List.iter chunks ~f:(fun chunk ->
      Buffer.add_string buf chunk;
      drain ());
  let rest = Buffer.contents buf |> String.strip in
  if not (String.is_empty rest) then on_line rest

let parse_json_opt s =
  match Yojson.Safe.from_string s with j -> Some j | exception _ -> None

let json_string_or_empty = function `String s -> s | _ -> ""

let parse_json_object_or_empty s =
  match Yojson.Safe.from_string (if String.is_empty s then "{}" else s) with
  | `Assoc _ as obj -> obj
  | _ -> `Assoc []
  | exception _ -> `Assoc []

let content_of_action = function
  | Model_action.Final_answer { answer } -> [ Text answer ]
  | Tool_call tc ->
      [ Tool_use { id = "fp_json_0"; name = tc.name; input = tc.args } ]
  | Tool_calls calls ->
      List.mapi calls ~f:(fun i (tc : Tool_call.t) ->
          Tool_use
            {
              id = Printf.sprintf "fp_json_%d" i;
              name = tc.name;
              input = tc.args;
            })

let action_of_content_blocks blocks =
  let tool_blocks =
    List.filter blocks ~f:(function Tool_use _ -> true | _ -> false)
  in
  match tool_blocks with
  | _ :: _ -> (
      match
        List.find_map tool_blocks ~f:(function
          | Tool_use { name; input; _ } -> (
              match build_tool name input with
              | Ok _ -> None
              | Error e -> Some e)
          | _ -> None)
      with
      | Some e -> Error e
      | None -> Ok blocks)
  | [] ->
      let text =
        List.filter_map blocks ~f:(function Text s -> Some s | _ -> None)
        |> String.concat ~sep:""
      in
      if String.is_empty text then
        Error "provider returned no text or tool_use blocks"
      else if looks_like_internal_action_text text then
        Result.map (parse_action text) ~f:content_of_action
      else Ok blocks

(* --- Anthropic protocol --- *)

type anthropic_builder =
  | BText of Buffer.t
  | BThinking of { text : Buffer.t; signature : Buffer.t }
  | BTool of { id : string; name : string; json : Buffer.t }

let assoc_set r key data =
  r := (key, data) :: List.Assoc.remove !r key ~equal:Int.equal

let assoc_find r key = List.Assoc.find !r key ~equal:Int.equal

let anthropic_request (config : Config.t) ~system turns =
  let uri = Uri.of_string (config.api_base ^ "/v1/messages") in
  let auth =
    if String.is_empty config.api_key then []
    else [ ("x-api-key", config.api_key) ]
  in
  let provider_headers =
    if String.equal config.provider "kimi" then
      [ ("User-Agent", "KimiCLI/1.5") ]
    else []
  in
  let headers =
    Cohttp.Header.of_list
      (auth @ provider_headers
      @ [
          ("anthropic-version", "2023-06-01");
          ("Content-Type", "application/json");
        ])
  in
  let max_tokens = Option.value config.max_tokens ~default:4096 in
  let body_json =
    `Assoc
      [
        ("model", `String config.model);
        ("max_tokens", `Int max_tokens);
        ("system", `String system);
        ("stream", `Bool true);
        ("messages", `List (anthropic_messages turns));
        ("tools", `List (anthropic_tools ()));
      ]
  in
  (uri, headers, body_json)

let anthropic_complete ~on_delta payloads =
  let builders = ref [] in
  let blocks = ref [] in
  let delta_filter = create_delta_filter on_delta in
  let input_tokens = ref 0 in
  let output_tokens = ref 0 in
  let read_usage json =
    let open Yojson.Safe.Util in
    (match member "input_tokens" json with
    | `Int n -> input_tokens := n
    | _ -> ());
    match member "output_tokens" json with
    | `Int n -> output_tokens := n
    | _ -> ()
  in
  let finish idx =
    match assoc_find builders idx with
    | Some (BText b) -> blocks := Text (Buffer.contents b) :: !blocks
    | Some (BThinking { text; signature }) ->
        blocks :=
          Thinking
            {
              text = Buffer.contents text;
              signature = Buffer.contents signature;
            }
          :: !blocks
    | Some (BTool { id; name; json }) ->
        blocks :=
          Tool_use
            {
              id;
              name;
              input = parse_json_object_or_empty (Buffer.contents json);
            }
          :: !blocks
    | None -> ()
  in
  List.iter payloads ~f:(fun data ->
      match parse_json_opt data with
      | None -> ()
      | Some json -> (
          let open Yojson.Safe.Util in
          match member "type" json with
          | `String "message_start" ->
              read_usage (json |> member "message" |> member "usage")
          | `String "message_delta" -> read_usage (member "usage" json)
          | `String "content_block_start" -> (
              let idx = json |> member "index" |> to_int in
              let block = json |> member "content_block" in
              match member "type" block with
              | `String "text" ->
                  assoc_set builders idx (BText (Buffer.create 256))
              | `String "thinking" ->
                  assoc_set builders idx
                    (BThinking
                       {
                         text = Buffer.create 256;
                         signature = Buffer.create 64;
                       })
              | `String "tool_use" ->
                  assoc_set builders idx
                    (BTool
                       {
                         id = json_string_or_empty (member "id" block);
                         name = json_string_or_empty (member "name" block);
                         json = Buffer.create 256;
                       })
              | _ -> ())
          | `String "content_block_delta" -> (
              let idx = json |> member "index" |> to_int in
              let delta = json |> member "delta" in
              match (member "type" delta, assoc_find builders idx) with
              | `String "text_delta", Some (BText b) ->
                  let s = json_string_or_empty (member "text" delta) in
                  Buffer.add_string b s;
                  feed_delta delta_filter s
              | `String "thinking_delta", Some (BThinking b) ->
                  Buffer.add_string b.text
                    (json_string_or_empty (member "thinking" delta))
              | `String "signature_delta", Some (BThinking b) ->
                  Buffer.add_string b.signature
                    (json_string_or_empty (member "signature" delta))
              | `String "input_json_delta", Some (BTool t) ->
                  Buffer.add_string t.json
                    (json_string_or_empty (member "partial_json" delta))
              | _ -> ())
          | `String "content_block_stop" ->
              finish (json |> member "index" |> to_int)
          | _ -> ()));
  let blocks = List.rev !blocks in
  let final_text =
    List.filter_map blocks ~f:(function Text s -> Some s | _ -> None)
    |> String.concat ~sep:""
  in
  flush_delta delta_filter final_text;
  Result.map (action_of_content_blocks blocks) ~f:(fun content ->
      (content, { input_tokens = !input_tokens; output_tokens = !output_tokens }))

(* --- OpenAI chat completions protocol --- *)

let openai_request (config : Config.t) ~system turns =
  let uri = Uri.of_string (config.api_base ^ "/chat/completions") in
  let auth =
    if String.is_empty config.api_key then []
    else [ ("Authorization", "Bearer " ^ config.api_key) ]
  in
  let headers =
    Cohttp.Header.of_list (auth @ [ ("Content-Type", "application/json") ])
  in
  let fields =
    [
      ("model", `String config.model);
      ("messages", `List (openai_messages ~system turns));
      ("temperature", `Float 0.0);
      ("stream", `Bool true);
      ("tools", `List (openai_tools ()));
      ("tool_choice", `String "auto");
    ]
  in
  let fields =
    if config.compat.supports_usage_in_streaming then
      fields @ [ ("stream_options", `Assoc [ ("include_usage", `Bool true) ]) ]
    else fields
  in
  let fields =
    match (config.compat.max_tokens_field, config.max_tokens) with
    | Some field, Some max_tokens -> fields @ [ (field, `Int max_tokens) ]
    | _ -> fields
  in
  let body_json = `Assoc fields in
  (uri, headers, body_json)

type openai_tool_builder = {
  id : string ref;
  name : string ref;
  args : Buffer.t;
}

let openai_complete ~on_delta payloads =
  let text = Buffer.create 256 in
  let reasoning = Buffer.create 256 in
  let tools = ref [] in
  let delta_filter = create_delta_filter on_delta in
  let input_tokens = ref 0 in
  let output_tokens = ref 0 in
  let get_tool idx =
    match assoc_find tools idx with
    | Some t -> t
    | None ->
        let t = { id = ref ""; name = ref ""; args = Buffer.create 256 } in
        assoc_set tools idx t;
        t
  in
  List.iter payloads ~f:(fun data ->
      match parse_json_opt data with
      | None -> ()
      | Some json -> (
          let open Yojson.Safe.Util in
          (match member "usage" json with
          | `Null -> ()
          | usage -> (
              (match member "prompt_tokens" usage with
              | `Int n -> input_tokens := n
              | _ -> ());
              match member "completion_tokens" usage with
              | `Int n -> output_tokens := n
              | _ -> ()));
          match member "choices" json with
          | `List (choice :: _) -> (
              let delta = member "delta" choice in
              (match member "content" delta with
              | `String s when not (String.is_empty s) ->
                  Buffer.add_string text s;
                  feed_delta delta_filter s
              | _ -> ());
              (match member "reasoning_content" delta with
              | `String s when not (String.is_empty s) ->
                  Buffer.add_string reasoning s
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
  let text_blocks =
    if Buffer.length text > 0 then [ Text (Buffer.contents text) ] else []
  in
  let reasoning_blocks =
    if Buffer.length reasoning > 0 then
      [ Thinking { text = Buffer.contents reasoning; signature = "" } ]
    else []
  in
  let tool_blocks =
    !tools
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> List.filter_map ~f:(fun (_, t) ->
        if String.is_empty !(t.name) then None
        else
          Some
            (Tool_use
               {
                 id = !(t.id);
                 name = !(t.name);
                 input = parse_json_object_or_empty (Buffer.contents t.args);
               }))
  in
  let blocks = reasoning_blocks @ text_blocks @ tool_blocks in
  flush_delta delta_filter (Buffer.contents text);
  Result.map (action_of_content_blocks blocks) ~f:(fun content ->
      (content, { input_tokens = !input_tokens; output_tokens = !output_tokens }))

(* --- shared HTTP streaming plumbing --- *)

let collect_sse_payloads body =
  let payloads = ref [] in
  let on_line line =
    match sse_data line with
    | Some data when not (String.equal (String.strip data) "[DONE]") ->
        payloads := String.strip data :: !payloads
    | _ -> ()
  in
  Lwt.map
    (fun chunks ->
      split_sse_lines chunks ~on_line;
      List.rev !payloads)
    (Lwt_stream.to_list (Cohttp_lwt.Body.to_stream body))

let post_and_complete ?(on_delta = fun _ -> ()) (uri, headers, body_json)
    ~complete =
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
          let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
          if code >= 400 then
            Lwt.bind (Cohttp_lwt.Body.to_string rbody) (fun body_str ->
                let snippet =
                  let s = String.strip body_str in
                  if String.length s > 400 then String.prefix s 400 ^ "…" else s
                in
                Lwt.return
                  (Error (Printf.sprintf "model API HTTP %d: %s" code snippet)))
          else
            Lwt.map
              (fun payloads -> complete ~on_delta payloads)
              (collect_sse_payloads rbody))

let real_send (config : Config.t) ~on_delta ~system turns =
  match config.protocol with
  | Provider.Openai ->
      post_and_complete ~on_delta
        (openai_request config ~system turns)
        ~complete:openai_complete
  | Provider.Anthropic ->
      post_and_complete ~on_delta
        (anthropic_request config ~system turns)
        ~complete:anthropic_complete

let create ~config =
  {
    send =
      (fun ~on_delta ~system turns -> real_send config ~on_delta ~system turns);
  }

let create_mock ~send =
  { send = (fun ~on_delta:_ ~system:_ turns -> send turns) }

let send ?(on_delta = fun _ -> ()) ~system t ~turns =
  t.send ~on_delta ~system turns

let request_body_for_test ~config ~system ~turns =
  let _, _, body =
    match config.Config.protocol with
    | Provider.Openai -> openai_request config ~system turns
    | Provider.Anthropic -> anthropic_request config ~system turns
  in
  body

let request_headers_for_test ~config ~system ~turns =
  let _, headers, _ =
    match config.Config.protocol with
    | Provider.Openai -> openai_request config ~system turns
    | Provider.Anthropic -> anthropic_request config ~system turns
  in
  Cohttp.Header.to_list headers

let openai_complete_for_test payloads =
  openai_complete ~on_delta:(fun _ -> ()) payloads

let anthropic_complete_for_test payloads =
  anthropic_complete ~on_delta:(fun _ -> ()) payloads
