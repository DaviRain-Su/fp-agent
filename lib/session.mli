val create : base_dir:string -> string
(** [create ~base_dir] creates and returns a fresh session directory under
    [base_dir]/.ocaml-agent/sessions/<timestamp>-<id>/. *)
