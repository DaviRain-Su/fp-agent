open! Base

(* A tool descriptor: its name, a kind (used for approval gating), a one-line
   args description (shown to the model), a pre-execution policy check, and the
   runner. Built-in tools and third-party plugins register one of these. *)
type kind = Read | Write | Exec

type t = {
  name : string;
  kind : kind;
  description : string;
  input_schema : Yojson.Safe.t option;
  check : Workspace.t -> Yojson.Safe.t -> Permission.t;
  run : Workspace.t -> Yojson.Safe.t -> Tool_result.t;
}

let registry : (string, t) Hashtbl.t = Hashtbl.create (module String)
let register tool = Hashtbl.set registry ~key:tool.name ~data:tool
let find name = Hashtbl.find registry name
let clear () = Hashtbl.clear registry

let all () =
  Hashtbl.data registry
  |> List.sort ~compare:(fun a b -> String.compare a.name b.name)
