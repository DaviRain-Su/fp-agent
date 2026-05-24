open! Base

type counts = { plugins : int; tools : int }

let register_all () =
  Tool.clear ();
  Builtin_tools.register_all ();
  Plugin.register_all ()

let refresh_counts () =
  register_all ();
  {
    plugins = List.length (Plugin.manifests ());
    tools = List.length (Tool.all ());
  }
