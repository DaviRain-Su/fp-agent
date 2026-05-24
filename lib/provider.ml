open! Base

type t = Kimi | Zhipu | Deepseek

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

(* OpenAI-compatible chat-completion bases; the client appends
   "/chat/completions". *)
let default_api_base = function
  | Kimi -> "https://api.kimi.com/coding/v1"
  | Zhipu -> "https://api.z.ai/api/paas/v4"
  | Deepseek -> "https://api.deepseek.com"

let default_model = function
  | Kimi -> "kimi-for-coding"
  | Zhipu -> "glm-4"
  | Deepseek -> "deepseek-v4-flash"
