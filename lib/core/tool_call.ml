open! Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type edit = { path : string; old_text : string; new_text : string }
[@@deriving yojson_of, of_yojson]

type t =
  | Read_file of { path : string }
  | Write_file of { path : string; content : string }
  | Edit_file of { path : string; old_text : string; new_text : string }
  | Run_command of { command : string; cwd : string option }
  | List_files of { path : string }
  | Search of { query : string; path : string option }
  | Make_dir of { path : string }
  | Apply_patch of { patch : string }
  | Multi_edit of { edits : edit list }
[@@deriving yojson_of, of_yojson]

let to_yojson = yojson_of_t

let of_yojson json =
  match t_of_yojson json with
  | t -> Ok t
  | exception exn -> Error (Exn.to_string exn)
