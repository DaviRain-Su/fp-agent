(** A registered tool: descriptor + behavior. Built-in tools and third-party
    plugins register one of these; the runner resolves calls by name. *)

type kind = Read | Write | Exec

type t = {
  name : string;
  kind : kind;
  description : string;  (** one-line args spec shown to the model *)
  check : Workspace.t -> Yojson.Safe.t -> Permission.t;
      (** pre-execution policy verdict for the given args *)
  run : Workspace.t -> Yojson.Safe.t -> Tool_result.t;
}

val register : t -> unit
val find : string -> t option

val all : unit -> t list
(** All registered tools, sorted by name. *)
