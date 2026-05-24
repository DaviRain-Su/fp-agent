open! Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = Success of { output : string } | Error of { message : string }
[@@deriving yojson_of, of_yojson]

let to_yojson = yojson_of_t

(* [Error] is shadowed by the variant constructor above, so the [result]
   constructors are written fully qualified here. *)
let of_yojson json =
  match t_of_yojson json with
  | t -> Stdlib.Ok t
  | exception exn -> Stdlib.Error (Exn.to_string exn)
