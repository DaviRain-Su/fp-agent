type t =
  | Read_file of { path : string }
  | Write_file of { path : string; content : string }
  | Edit_file of { path : string; old_text : string; new_text : string }
  | Run_command of { command : string; cwd : string option }
  | List_files of { path : string }
  | Search of { query : string; path : string option }
  | Make_dir of { path : string }
  | Apply_patch of { patch : string }

val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
