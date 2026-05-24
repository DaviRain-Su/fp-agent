open! Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t =
  | Tool_call of Tool_call.t
  | Tool_calls of Tool_call.t list
  | Final_answer of { answer : string }
[@@deriving yojson_of, of_yojson]

let to_yojson = yojson_of_t

let of_yojson json =
  match t_of_yojson json with
  | t -> Ok t
  | exception exn -> Error (Exn.to_string exn)
