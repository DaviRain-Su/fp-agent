open! Base

type content =
  | Text of string
  | Thinking of { text : string; signature : string }
  | Tool_use of { id : string; name : string; input : Yojson.Safe.t }
  | Tool_result of { id : string; content : string }

type role = User | Assistant
type turn = { role : role; content : content list }
type usage = { input_tokens : int; output_tokens : int }

let zero_usage = { input_tokens = 0; output_tokens = 0 }
let text s = Text s
let user content = { role = User; content = [ Text content ] }
let assistant content = { role = Assistant; content }
let role_to_string = function User -> "user" | Assistant -> "assistant"

let role_of_string = function
  | "assistant" -> Assistant
  | "user" -> User
  | other -> failwith ("Llm.role_of_string: unknown role " ^ other)

let content_to_json = function
  | Text text -> `Assoc [ ("type", `String "text"); ("text", `String text) ]
  | Thinking { text; signature } ->
      `Assoc
        [
          ("type", `String "thinking");
          ("text", `String text);
          ("signature", `String signature);
        ]
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
          ("id", `String id);
          ("content", `String content);
        ]

let content_of_json json =
  let open Yojson.Safe.Util in
  match member "type" json with
  | `String "text" -> Text (json |> member "text" |> to_string)
  | `String "thinking" ->
      let text =
        match member "text" json with
        | `String s -> s
        | _ -> json |> member "thinking" |> to_string
      in
      let signature =
        match member "signature" json with `String s -> s | _ -> ""
      in
      Thinking { text; signature }
  | `String "tool_use" ->
      Tool_use
        {
          id = json |> member "id" |> to_string;
          name = json |> member "name" |> to_string;
          input = member "input" json;
        }
  | `String "tool_result" ->
      let id =
        match member "id" json with
        | `String s -> s
        | _ -> json |> member "tool_use_id" |> to_string
      in
      Tool_result { id; content = json |> member "content" |> to_string }
  | `String other -> failwith ("Llm.content_of_json: unknown content " ^ other)
  | _ -> failwith "Llm.content_of_json: missing type"

let role_to_json role = `String (role_to_string role)
let role_of_json = function `String s -> role_of_string s | _ -> User

let turn_to_json { role; content } =
  `Assoc
    [
      ("role", role_to_json role);
      ("content", `List (List.map content ~f:content_to_json));
    ]

let turn_of_json json =
  let open Yojson.Safe.Util in
  {
    role = role_of_json (member "role" json);
    content = json |> member "content" |> to_list |> List.map ~f:content_of_json;
  }

let usage_to_json { input_tokens; output_tokens } =
  `Assoc
    [
      ("input_tokens", `Int input_tokens); ("output_tokens", `Int output_tokens);
    ]

let usage_of_json json =
  let open Yojson.Safe.Util in
  let int_field name = match member name json with `Int n -> n | _ -> 0 in
  {
    input_tokens = int_field "input_tokens";
    output_tokens = int_field "output_tokens";
  }

let yojson_of_content = content_to_json
let content_of_yojson = content_of_json
let yojson_of_role = role_to_json
let role_of_yojson = role_of_json
let yojson_of_turn = turn_to_json
let turn_of_yojson = turn_of_json
let yojson_of_usage = usage_to_json
let usage_of_yojson = usage_of_json

let tool_uses content =
  List.filter_map content ~f:(function
    | Tool_use { id; name; input } -> Some (id, Tool_call.make ~name ~args:input)
    | _ -> None)

let final_text content =
  if List.exists content ~f:(function Tool_use _ -> true | _ -> false) then None
  else
    let text =
      List.filter_map content ~f:(function Text s -> Some s | _ -> None)
      |> String.concat ~sep:""
    in
    if String.is_empty text then None else Some text

let content_to_text = function
  | Text text -> Some text
  | Thinking { text; _ } when not (String.is_empty (String.strip text)) ->
      Some ("[thinking]\n" ^ text)
  | Thinking _ -> None
  | Tool_use { name; input; _ } ->
      Some ("[tool " ^ name ^ "] " ^ Yojson.Safe.to_string input)
  | Tool_result { content; _ } -> Some content

let turn_to_message { role; content } =
  let role = role_to_string role in
  let content =
    List.filter_map content ~f:content_to_text |> String.concat ~sep:"\n"
  in
  { Message.role; content }
