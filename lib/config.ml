open! Base

type t = {
  api_key : string;
  api_base : string;
  model : string;
  max_steps : int;
  workspace_root : string;
}

let default_max_steps = 30
let getenv name = Stdlib.Sys.getenv_opt name

(* Treat an empty value the same as unset, so a stray empty env var does not
   override a provider default with "". *)
let getenv_nonempty name =
  match getenv name with Some "" | None -> None | Some s -> Some s

let getenv_default name ~default = Option.value (getenv name) ~default

let resolve_provider provider =
  match Option.first_some provider (getenv_nonempty "PROVIDER") with
  | None -> Ok Provider.default
  | Some s -> (
      match Provider.of_string s with
      | Some p -> Ok p
      | None -> Error ("unknown provider: " ^ s))

let load ?provider ?api_base ?model () =
  match resolve_provider provider with
  | Error e -> Error e
  | Ok prov -> (
      let key_env = Provider.key_env prov in
      match getenv key_env with
      | None | Some "" ->
          Error
            (Printf.sprintf "%s is not set (provider %s)" key_env
               (Provider.to_string prov))
      | Some api_key ->
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
          Ok { api_key; api_base; model; max_steps; workspace_root })
