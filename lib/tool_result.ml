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

(* Text fed back to the model as an observation after a tool runs. *)
let to_observation = function
  | Success { output } -> "TOOL_RESULT ok=true\n" ^ output
  | Error { message } -> "TOOL_RESULT ok=false\n" ^ message
