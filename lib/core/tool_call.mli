type t = { name : string; args : Yojson.Safe.t }
(** An open tool invocation: a tool name plus a JSON args object, resolved
    against a registry at execution time. *)

val make : name:string -> args:Yojson.Safe.t -> t
val arg : t -> string -> Yojson.Safe.t
val arg_string : t -> string -> string option
val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result

(** Constructors for the built-in tools. *)

val read_file : string -> t
val list_files : string -> t
val write_file : path:string -> content:string -> t
val edit_file : path:string -> old_text:string -> new_text:string -> t
val run_command : ?cwd:string -> string -> t
val search : ?path:string -> string -> t
val make_dir : string -> t
val apply_patch : string -> t
val multi_edit : (string * string * string) list -> t
