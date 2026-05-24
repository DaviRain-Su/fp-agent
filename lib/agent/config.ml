open! Base

type provider_compat = {
  supports_developer_role : bool;
  supports_reasoning_effort : bool;
  supports_usage_in_streaming : bool;
  max_tokens_field : string option;
}

type t = {
  provider : string;
  api_key : string;
  api_base : string;
  model : string;
  models : string list;
  protocol : Provider.protocol;
  compat : provider_compat;
  max_tokens : int option;
  max_steps : int;
  workspace_root : string;
}

type provider_catalog_entry = {
  provider_name : string;
  provider_api_base : string;
  provider_models : string list;
  provider_protocol : Provider.protocol;
}

type provider_config_file_diagnostic = {
  config_path : string;
  config_exists : bool;
  config_error : string option;
  config_provider_names : string list;
}

type custom_provider_diagnostic = {
  custom_provider_name : string;
  custom_provider_path : string;
  custom_provider_error : string option;
  custom_provider_api_base : string option;
  custom_provider_models : string list;
  custom_provider_protocol : Provider.protocol option;
  custom_provider_has_api_key : bool;
  custom_provider_default_model : string option;
}

type provider_diagnostics = {
  provider_config_files : provider_config_file_diagnostic list;
  custom_provider_diagnostics : custom_provider_diagnostic list;
  provider_catalog : provider_catalog_entry list;
}

let default_max_steps = 30

let default_compat =
  {
    supports_developer_role = true;
    supports_reasoning_effort = true;
    supports_usage_in_streaming = true;
    max_tokens_field = None;
  }

let local_compat =
  {
    supports_developer_role = false;
    supports_reasoning_effort = false;
    supports_usage_in_streaming = false;
    max_tokens_field = Some "max_tokens";
  }

let builtin_compat = function
  | Provider.Local -> local_compat
  | _ -> default_compat

let getenv name = Stdlib.Sys.getenv_opt name

(* Treat an empty value the same as unset, so a stray empty env var does not
   override a provider default with "". *)
let getenv_nonempty name =
  match getenv name with Some "" | None -> None | Some s -> Some s

let getenv_default name ~default = Option.value (getenv name) ~default

let dedupe_nonempty strings =
  List.fold strings ~init:[] ~f:(fun acc s ->
      let s = String.strip s in
      if String.is_empty s || List.mem acc s ~equal:String.equal then acc
      else s :: acc)
  |> List.rev

let env_models name =
  match getenv_nonempty name with
  | None -> []
  | Some s -> String.split s ~on:',' |> dedupe_nonempty

type provider_choice = Builtin of Provider.t | Custom of string

type custom_provider = {
  name : string;
  api_key : string;
  api_base : string;
  protocol : Provider.protocol;
  compat : provider_compat;
  default_model : string option;
  models : model_spec list;
}

and model_spec = { id : string; max_tokens : int option }

let json_member obj names =
  List.find_map names ~f:(fun name ->
      match Yojson.Safe.Util.member name obj with `Null -> None | v -> Some v)

let json_string obj names =
  match json_member obj names with Some (`String s) -> Some s | _ -> None

let json_bool obj names ~default =
  match json_member obj names with Some (`Bool b) -> b | _ -> default

let json_int obj names =
  match json_member obj names with Some (`Int i) -> Some i | _ -> None

let protocol_of_api = function
  | "openai" | "openai-chat" | "openai-completions" | "openai-compatible" ->
      Some Provider.Openai
  | "anthropic" | "anthropic-messages" -> Some Provider.Anthropic
  | _ -> None

let api_key_from_spec spec =
  if String.is_prefix spec ~prefix:"env:" then
    getenv_nonempty (String.drop_prefix spec 4) |> Option.value ~default:""
  else if String.is_prefix spec ~prefix:"$" then
    getenv_nonempty (String.drop_prefix spec 1) |> Option.value ~default:""
  else spec

let compat_of_json obj =
  match json_member obj [ "compat" ] with
  | Some (`Assoc _ as compat) ->
      {
        supports_developer_role =
          json_bool compat
            [ "supportsDeveloperRole"; "supports_developer_role" ]
            ~default:default_compat.supports_developer_role;
        supports_reasoning_effort =
          json_bool compat
            [ "supportsReasoningEffort"; "supports_reasoning_effort" ]
            ~default:default_compat.supports_reasoning_effort;
        supports_usage_in_streaming =
          json_bool compat
            [ "supportsUsageInStreaming"; "supports_usage_in_streaming" ]
            ~default:default_compat.supports_usage_in_streaming;
        max_tokens_field =
          json_string compat [ "maxTokensField"; "max_tokens_field" ];
      }
  | _ -> default_compat

let model_spec = function
  | `String id -> Some { id; max_tokens = None }
  | json -> (
      match json_string json [ "id"; "name"; "model" ] with
      | None -> None
      | Some id ->
          Some
            {
              id;
              max_tokens =
                json_int json
                  [
                    "maxTokens";
                    "max_tokens";
                    "maxOutputTokens";
                    "max_output_tokens";
                  ];
            })

let model_specs obj =
  match json_member obj [ "models" ] with
  | Some (`List models) -> List.filter_map models ~f:model_spec
  | _ -> []

let model_ids specs = List.map specs ~f:(fun spec -> spec.id)

let provider_fields json =
  match Yojson.Safe.Util.member "providers" json with
  | `Assoc fields -> fields
  | _ -> ( match json with `Assoc fields -> fields | _ -> [])

let parse_custom_provider name obj =
  let api =
    json_string obj [ "api"; "protocol" ]
    |> Option.value ~default:"openai-completions"
    |> String.lowercase
  in
  match
    ( json_string obj [ "baseUrl"; "base_url"; "apiBase"; "api_base" ],
      protocol_of_api api )
  with
  | None, _ -> Error "missing baseUrl/base_url/apiBase/api_base"
  | _, None -> Error ("unsupported provider api: " ^ api)
  | Some api_base, Some protocol ->
      let models = model_specs obj in
      let api_key =
        json_string obj [ "apiKey"; "api_key" ]
        |> Option.value_map ~default:"" ~f:api_key_from_spec
      in
      let default_model =
        Option.first_some
          (json_string obj [ "defaultModel"; "default_model"; "model" ])
          (List.hd (model_ids models))
      in
      Ok
        {
          name;
          api_key;
          api_base;
          protocol;
          compat = compat_of_json obj;
          default_model;
          models;
        }

let candidate_config_paths () =
  let explicit =
    match getenv_nonempty "FP_AGENT_CONFIG" with Some p -> [ p ] | None -> []
  in
  let home =
    match getenv_nonempty "HOME" with
    | Some home ->
        [
          Stdlib.Filename.concat home
            (Stdlib.Filename.concat ".config"
               (Stdlib.Filename.concat "fp-agent" "providers.json"));
        ]
    | None -> []
  in
  explicit @ [ ".fp-agent/providers.json"; ".fp-agent.json" ] @ home
  |> dedupe_nonempty

let load_custom_provider name =
  let parse path =
    match Yojson.Safe.from_file path with
    | exception _ -> None
    | json -> (
        match
          List.Assoc.find (provider_fields json) name ~equal:String.equal
        with
        | None -> None
        | Some obj -> Result.ok (parse_custom_provider name obj))
  in
  List.find_map (candidate_config_paths ()) ~f:parse

let provider_config_path ?path () =
  match path with
  | Some path when not (String.is_empty (String.strip path)) -> path
  | _ -> (
      match getenv_nonempty "FP_AGENT_CONFIG" with
      | Some path -> path
      | None -> ".fp-agent/providers.json")

let rec mkdir_p dir =
  if String.is_empty dir || String.equal dir "." || Stdlib.Sys.file_exists dir
  then ()
  else
    let parent = Stdlib.Filename.dirname dir in
    if not (String.equal parent dir) then mkdir_p parent;
    Unix.mkdir dir 0o755

let provider_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
  | _ -> false

let validate_custom_provider_name name =
  let name = String.strip name in
  if String.is_empty name then Error "provider name is required"
  else if Option.is_some (Provider.of_string name) then
    Error ("provider name is built-in: " ^ name)
  else if String.exists name ~f:(fun c -> not (provider_name_char c)) then
    Error
      "provider name may only contain letters, numbers, dash, underscore, or \
       dot"
  else Ok name

let validate_model_id model =
  let model = String.strip model in
  if String.is_empty model then Error "model id is required"
  else if String.exists model ~f:Char.is_whitespace then
    Error ("model id may not contain whitespace: " ^ model)
  else Ok model

let validate_models models =
  let models =
    models |> List.map ~f:String.strip
    |> List.filter ~f:(fun model -> not (String.is_empty model))
  in
  match List.map models ~f:validate_model_id |> Result.all with
  | Error e -> Error e
  | Ok models -> (
      match dedupe_nonempty models with
      | [] -> Error "at least one model is required"
      | models -> Ok models)

let compat_json compat =
  let fields =
    [
      ("supportsDeveloperRole", `Bool compat.supports_developer_role);
      ("supportsReasoningEffort", `Bool compat.supports_reasoning_effort);
      ("supportsUsageInStreaming", `Bool compat.supports_usage_in_streaming);
    ]
  in
  let fields =
    match compat.max_tokens_field with
    | None -> fields
    | Some field -> fields @ [ ("maxTokensField", `String field) ]
  in
  `Assoc fields

let model_json ?max_tokens model =
  let fields = [ ("id", `String model); ("name", `String model) ] in
  let fields =
    match max_tokens with
    | None -> fields
    | Some max_tokens -> fields @ [ ("maxTokens", `Int max_tokens) ]
  in
  `Assoc fields

let provider_json ?max_tokens ~api ~api_key ~api_base ~compat ~models () =
  `Assoc
    [
      ("baseUrl", `String api_base);
      ("api", `String api);
      ("apiKey", `String api_key);
      ("compat", compat_json compat);
      ("models", `List (List.map models ~f:(model_json ?max_tokens)));
    ]

let assoc_replace fields key value =
  (key, value) :: List.Assoc.remove fields key ~equal:String.equal

let update_provider_json ~replace ~name provider = function
  | `Assoc fields -> (
      match List.Assoc.find fields "providers" ~equal:String.equal with
      | Some (`Assoc providers) ->
          if (not replace) && List.Assoc.mem providers name ~equal:String.equal
          then Error ("provider already exists: " ^ name ^ " (pass --replace)")
          else
            Ok
              (`Assoc
                 (assoc_replace fields "providers"
                    (`Assoc (assoc_replace providers name provider))))
      | Some _ -> Error "provider config field `providers` must be an object"
      | None ->
          if (not replace) && List.Assoc.mem fields name ~equal:String.equal
          then Error ("provider already exists: " ^ name ^ " (pass --replace)")
          else Ok (`Assoc (assoc_replace fields name provider)))
  | _ -> Error "provider config root must be a JSON object"

let read_provider_config path =
  if Stdlib.Sys.file_exists path then
    match Yojson.Safe.from_file path with
    | json -> Ok json
    | exception exn ->
        Error
          (Printf.sprintf "invalid provider config %s: %s" path
             (Exn.to_string exn))
  else Ok (`Assoc [])

let write_json_file path json =
  mkdir_p (Stdlib.Filename.dirname path);
  Stdlib.Out_channel.with_open_bin path (fun oc ->
      Stdlib.Out_channel.output_string oc (Yojson.Safe.pretty_to_string json);
      Stdlib.Out_channel.output_char oc '\n')

let write_custom_provider ?path ?(api = "openai-completions") ?(api_key = "")
    ?(compat = default_compat) ?max_tokens ?(replace = false) ~name ~api_base
    ~models () =
  let path = provider_config_path ?path () in
  let api_base =
    if String.is_empty (String.strip api_base) then
      Error "provider base URL is required"
    else Ok (String.strip api_base)
  in
  let api =
    let api = String.strip api |> String.lowercase in
    match protocol_of_api api with
    | Some _ -> Ok api
    | None -> Error ("unsupported provider api: " ^ api)
  in
  let max_tokens =
    match max_tokens with
    | Some n when n <= 0 -> Error "max tokens must be positive"
    | _ -> Ok max_tokens
  in
  match
    ( validate_custom_provider_name name,
      api_base,
      api,
      validate_models models,
      max_tokens )
  with
  | Error e, _, _, _, _
  | _, Error e, _, _, _
  | _, _, Error e, _, _
  | _, _, _, Error e, _
  | _, _, _, _, Error e ->
      Error e
  | Ok name, Ok api_base, Ok api, Ok models, Ok max_tokens -> (
      match read_provider_config path with
      | Error e -> Error e
      | Ok json -> (
          let provider =
            provider_json ?max_tokens ~api ~api_key ~api_base ~compat ~models ()
          in
          match update_provider_json ~replace ~name provider json with
          | Error e -> Error e
          | Ok next -> (
              try
                write_json_file path next;
                Ok path
              with exn ->
                Error
                  (Printf.sprintf "provider config write failed: %s"
                     (Exn.to_string exn)))))

let custom_provider_names () =
  let names_from_file path =
    match Yojson.Safe.from_file path with
    | exception _ -> []
    | json -> provider_fields json |> List.map ~f:fst
  in
  candidate_config_paths ()
  |> List.concat_map ~f:names_from_file
  |> dedupe_nonempty

let custom_models custom =
  match custom.models with
  | [] -> Option.to_list custom.default_model
  | models -> model_ids models

let selected_model_max_tokens models selected_model =
  List.find_map models ~f:(fun spec ->
      if String.equal spec.id selected_model then spec.max_tokens else None)

let builtin_models = function
  | Provider.Local -> Provider.models Provider.Local @ env_models "LOCAL_MODELS"
  | provider -> Provider.models provider

let available_providers () =
  let builtin_entries =
    List.map Provider.all ~f:(fun provider ->
        {
          provider_name = Provider.to_string provider;
          provider_api_base = Provider.default_api_base provider;
          provider_models = dedupe_nonempty (builtin_models provider);
          provider_protocol = Provider.protocol provider;
        })
  in
  let custom_entries =
    custom_provider_names ()
    |> List.filter ~f:(fun name -> Option.is_none (Provider.of_string name))
    |> List.filter_map ~f:(fun name ->
        Option.map (load_custom_provider name) ~f:(fun custom ->
            {
              provider_name = custom.name;
              provider_api_base = custom.api_base;
              provider_models = dedupe_nonempty (custom_models custom);
              provider_protocol = custom.protocol;
            }))
  in
  builtin_entries @ custom_entries

let read_provider_fields path =
  match Yojson.Safe.from_file path with
  | exception exn ->
      Error
        (Printf.sprintf "invalid provider config %s: %s" path
           (Exn.to_string exn))
  | json -> Ok (provider_fields json)

let provider_config_file_diagnostic path =
  if not (Stdlib.Sys.file_exists path) then
    {
      config_path = path;
      config_exists = false;
      config_error = None;
      config_provider_names = [];
    }
  else
    match read_provider_fields path with
    | Error e ->
        {
          config_path = path;
          config_exists = true;
          config_error = Some e;
          config_provider_names = [];
        }
    | Ok fields ->
        {
          config_path = path;
          config_exists = true;
          config_error = None;
          config_provider_names = List.map fields ~f:fst;
        }

let custom_provider_diagnostic path name obj =
  match parse_custom_provider name obj with
  | Error e ->
      {
        custom_provider_name = name;
        custom_provider_path = path;
        custom_provider_error = Some e;
        custom_provider_api_base = None;
        custom_provider_models = [];
        custom_provider_protocol = None;
        custom_provider_has_api_key = false;
        custom_provider_default_model = None;
      }
  | Ok custom ->
      {
        custom_provider_name = custom.name;
        custom_provider_path = path;
        custom_provider_error = None;
        custom_provider_api_base = Some custom.api_base;
        custom_provider_models = dedupe_nonempty (custom_models custom);
        custom_provider_protocol = Some custom.protocol;
        custom_provider_has_api_key = not (String.is_empty custom.api_key);
        custom_provider_default_model = custom.default_model;
      }

let custom_provider_diagnostics () =
  candidate_config_paths ()
  |> List.concat_map ~f:(fun path ->
      if not (Stdlib.Sys.file_exists path) then []
      else
        match read_provider_fields path with
        | Error _ -> []
        | Ok fields ->
            List.filter_map fields ~f:(fun (name, obj) ->
                if Option.is_some (Provider.of_string name) then None
                else Some (custom_provider_diagnostic path name obj)))

let provider_diagnostics () =
  {
    provider_config_files =
      List.map (candidate_config_paths ()) ~f:provider_config_file_diagnostic;
    custom_provider_diagnostics = custom_provider_diagnostics ();
    provider_catalog = available_providers ();
  }

let resolve_provider provider =
  match Option.first_some provider (getenv_nonempty "PROVIDER") with
  | None -> Ok (Builtin Provider.default)
  | Some s -> (
      match Provider.of_string s with
      | Some p -> Ok (Builtin p)
      | None -> (
          match load_custom_provider s with
          | Some custom -> Ok (Custom custom.name)
          | None -> Error ("unknown provider: " ^ s)))

let load ?provider ?api_base ?model () =
  match resolve_provider provider with
  | Error e -> Error e
  | Ok (Builtin prov) -> (
      let key_env = Provider.key_env prov in
      let api_key = getenv_nonempty key_env in
      match api_key with
      | None when Provider.requires_api_key prov ->
          Error
            (Printf.sprintf "%s is not set (provider %s)" key_env
               (Provider.to_string prov))
      | _ ->
          let api_key = Option.value api_key ~default:"" in
          let api_base =
            Option.value
              (Option.first_some api_base (getenv_nonempty "API_BASE"))
              ~default:(Provider.default_api_base prov)
          in
          let model =
            Option.value
              (Option.first_some model (getenv_nonempty "MODEL_NAME"))
              ~default:(Provider.default_model prov)
          in
          let max_steps =
            match getenv "MAX_STEPS" with
            | Some s -> ( try Int.of_string s with _ -> default_max_steps)
            | None -> default_max_steps
          in
          let workspace_root =
            getenv_default "WORKSPACE_ROOT" ~default:(Unix.getcwd ())
          in
          let protocol = Provider.protocol prov in
          Ok
            {
              provider = Provider.to_string prov;
              api_key;
              api_base;
              model;
              models = dedupe_nonempty (builtin_models prov);
              protocol;
              compat = builtin_compat prov;
              max_tokens = None;
              max_steps;
              workspace_root;
            })
  | Ok (Custom name) -> (
      match load_custom_provider name with
      | None -> Error ("unknown provider: " ^ name)
      | Some custom -> (
          let api_base =
            Option.value
              (Option.first_some api_base (getenv_nonempty "API_BASE"))
              ~default:custom.api_base
          in
          match
            Option.first_some model
              (Option.first_some
                 (getenv_nonempty "MODEL_NAME")
                 custom.default_model)
          with
          | None ->
              Error
                (Printf.sprintf
                   "provider %s has no model; pass --model, set MODEL_NAME, or \
                    add models/defaultModel to FP_AGENT_CONFIG"
                   name)
          | Some model ->
              let max_steps =
                match getenv "MAX_STEPS" with
                | Some s -> ( try Int.of_string s with _ -> default_max_steps)
                | None -> default_max_steps
              in
              let workspace_root =
                getenv_default "WORKSPACE_ROOT" ~default:(Unix.getcwd ())
              in
              Ok
                {
                  provider = custom.name;
                  api_key = custom.api_key;
                  api_base;
                  model;
                  models = custom_models custom;
                  protocol = custom.protocol;
                  compat = custom.compat;
                  max_tokens = selected_model_max_tokens custom.models model;
                  max_steps;
                  workspace_root;
                }))
