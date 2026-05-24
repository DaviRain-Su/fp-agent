(** A workspace is a directory that bounds all file operations. Paths are
    resolved against its root and may not escape it. *)

type t

val create : root:string -> (t, string) result
(** [create ~root] canonicalizes [root] and fails if it is not an existing
    directory. *)

val root : t -> string
(** The canonicalized absolute workspace root. *)

val resolve_path : t -> string -> (string, string) result
(** [resolve_path t path] resolves [path] (relative to the root, or absolute)
    and returns the canonical absolute path, or [Error] if it escapes the
    workspace. *)

val validate_write_path : t -> string -> (string, string) result
(** Like {!resolve_path} but additionally rejects writes that touch the [.git]
    directory. *)
