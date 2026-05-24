open! Base

type t = Kimi | Zhipu | Deepseek

(* Wire protocol the provider's endpoint speaks. Both carry our JSON action
   contract in the message body; only the HTTP request/response shape differs. *)
type protocol = Openai | Anthropic

let all = [ Kimi; Zhipu; Deepseek ]

let to_string = function
  | Kimi -> "kimi"
  | Zhipu -> "zhipu"
  | Deepseek -> "deepseek"

let of_string s =
  match String.lowercase (String.strip s) with
  | "kimi" | "moonshot" | "kimi-for-coding" -> Some Kimi
  | "zhipu" | "glm" | "zai" | "z.ai" -> Some Zhipu
  | "deepseek" -> Some Deepseek
  | _ -> None

let default = Kimi

let key_env = function
  | Kimi -> "KIMI_API_KEY"
  | Zhipu -> "ZAI_API_KEY"
  | Deepseek -> "DEEPSEEK_API_KEY"

(* Kimi for coding speaks the Anthropic Messages protocol (same as Claude
   Code); the others are OpenAI-compatible. *)
let protocol = function Kimi -> Anthropic | Zhipu | Deepseek -> Openai

(* Base URLs. The client appends "/chat/completions" for OpenAI providers and
   "/v1/messages" for Anthropic providers. *)
let default_api_base = function
  | Kimi -> "https://api.kimi.com/coding"
  | Zhipu -> "https://api.z.ai/api/paas/v4"
  | Deepseek -> "https://api.deepseek.com"

let default_model = function
  | Kimi -> "kimi-for-coding"
  | Zhipu -> "glm-4"
  | Deepseek -> "deepseek-v4-flash"
