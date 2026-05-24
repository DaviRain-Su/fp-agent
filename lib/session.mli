val create : base_dir:string -> string
(** [create ~base_dir] creates and returns a fresh session directory under
    [base_dir]/.ocaml-agent/sessions/<timestamp>-<id>/, with a meta.json marking
    it as a root (no parent). *)

val read_meta : string -> string option * int option
(** [read_meta dir] returns the session's [(parent, forked_at)] from meta.json,
    where [parent] is the parent session's directory name. *)

val fork :
  base_dir:string ->
  parent_session_dir:string ->
  at:int option ->
  (string, string) result
(** [fork ~base_dir ~parent_session_dir ~at] creates a child session whose event
    log is the parent's first [at] events ([None] = all), recording the parent
    and fork point. Returns the child session directory. *)
