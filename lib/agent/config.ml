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

let load_custom_provider name =
  let parse path =
    match Yojson.Safe.from_file path with
    | exception _ -> None
    | json -> (
        match
          List.Assoc.find (provider_fields json) name ~equal:String.equal
        with
        | None -> None
        | Some obj -> (
            let api =
              json_string obj [ "api"; "protocol" ]
              |> Option.value ~default:"openai-completions"
              |> String.lowercase
            in
            match
              ( json_string obj [ "baseUrl"; "base_url"; "apiBase"; "api_base" ],
                protocol_of_api api )
            with
            | Some api_base, Some protocol ->
                let models = model_specs obj in
                let api_key =
                  json_string obj [ "apiKey"; "api_key" ]
                  |> Option.value_map ~default:"" ~f:api_key_from_spec
                in
                let default_model =
                  Option.first_some
                    (json_string obj
                       [ "defaultModel"; "default_model"; "model" ])
                    (List.hd (model_ids models))
                in
                Some
                  {
                    name;
                    api_key;
                    api_base;
                    protocol;
                    compat = compat_of_json obj;
                    default_model;
                    models;
                  }
            | _ -> None))
  in
  List.find_map (candidate_config_paths ()) ~f:parse

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
