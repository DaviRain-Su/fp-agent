open! Base

(* A tool invocation is now open: a tool name plus a JSON args object, resolved
   against a registry at execution time. This lets new tools (built-in or
   third-party plugins) be added without changing this type. *)
type t = { name : string; args : Yojson.Safe.t }

let make ~name ~args = { name; args }
let arg t key = Yojson.Safe.Util.member key t.args
let arg_string t key = match arg t key with `String s -> Some s | _ -> None

(* JSON envelope is {"name":..,"args":..}. [t_of_yojson] raises on malformed
   input, matching the ppx_yojson_conv convention so [Event]'s derived codecs
   can call it. *)
let yojson_of_t t = `Assoc [ ("name", `String t.name); ("args", t.args) ]

let t_of_yojson = function
  | `Assoc _ as j -> (
      match Yojson.Safe.Util.member "name" j with
      | `String name ->
          let args =
            match Yojson.Safe.Util.member "args" j with
            | `Null -> `Assoc []
            | a -> a
          in
          { name; args }
      | _ -> failwith "Tool_call.t_of_yojson: missing string field 'name'")
  | _ -> failwith "Tool_call.t_of_yojson: expected an object"

let to_yojson = yojson_of_t

let of_yojson j =
  match t_of_yojson j with
  | t -> Ok t
  | exception exn -> Error (Exn.to_string exn)

(* Ergonomic constructors for the built-in tools. *)
let obj fields = `Assoc fields

let read_file path =
  make ~name:"read_file" ~args:(obj [ ("path", `String path) ])

let list_files path =
  make ~name:"list_files" ~args:(obj [ ("path", `String path) ])

let write_file ~path ~content =
  make ~name:"write_file"
    ~args:(obj [ ("path", `String path); ("content", `String content) ])

let edit_file ~path ~old_text ~new_text =
  make ~name:"edit_file"
    ~args:
      (obj
         [
           ("path", `String path);
           ("old", `String old_text);
           ("new", `String new_text);
         ])

let run_command ?cwd command =
  let base = [ ("command", `String command) ] in
  let fields =
    match cwd with Some c -> base @ [ ("cwd", `String c) ] | None -> base
  in
  make ~name:"run_command" ~args:(obj fields)

let search ?path query =
  let base = [ ("query", `String query) ] in
  let fields =
    match path with Some p -> base @ [ ("path", `String p) ] | None -> base
  in
  make ~name:"search" ~args:(obj fields)

let make_dir path = make ~name:"make_dir" ~args:(obj [ ("path", `String path) ])

let apply_patch patch =
  make ~name:"apply_patch" ~args:(obj [ ("patch", `String patch) ])

let multi_edit edits =
  let items =
    List.map edits ~f:(fun (path, old_text, new_text) ->
        obj
          [
            ("path", `String path);
            ("old", `String old_text);
            ("new", `String new_text);
          ])
  in
  make ~name:"multi_edit" ~args:(obj [ ("edits", `List items) ])
