open! Base

type t = {
  api_key : string;
  api_base : string;
  model : string;
  max_steps : int;
  workspace_root : string;
}

let default_api_base = "https://api.openai.com/v1"
let default_model = "gpt-4o-mini"
let default_max_steps = 30
let getenv name = Stdlib.Sys.getenv_opt name
let getenv_default name ~default = Option.value (getenv name) ~default

let load () =
  match getenv "OPENAI_API_KEY" with
  | None | Some "" -> Error "OPENAI_API_KEY is not set"
  | Some api_key ->
      let api_base =
        getenv_default "OPENAI_API_BASE" ~default:default_api_base
      in
      let model = getenv_default "MODEL_NAME" ~default:default_model in
      let max_steps =
        match getenv "MAX_STEPS" with
        | Some s -> ( try Int.of_string s with _ -> default_max_steps)
        | None -> default_max_steps
      in
      let workspace_root =
        getenv_default "WORKSPACE_ROOT" ~default:(Unix.getcwd ())
      in
      Ok { api_key; api_base; model; max_steps; workspace_root }
