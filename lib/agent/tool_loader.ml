open! Base

let register_all () =
  Tool.clear ();
  Builtin_tools.register_all ();
  Plugin.register_all ()
