open! Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = Allow | Deny of string | Ask_user of string
[@@deriving yojson_of, of_yojson]

let to_yojson = yojson_of_t

let of_yojson json =
  match t_of_yojson json with
  | t -> Ok t
  | exception exn -> Error (Exn.to_string exn)

let to_string = function
  | Allow -> "allow"
  | Deny reason -> "deny: " ^ reason
  | Ask_user reason -> "ask_user: " ^ reason

let is_allow = function Allow -> true | Deny _ | Ask_user _ -> false
let equal a b = Poly.equal a b
let pp fmt t = Stdlib.Format.pp_print_string fmt (to_string t)
