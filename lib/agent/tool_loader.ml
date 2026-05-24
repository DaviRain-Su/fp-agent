open! Base

let register_all () =
  Builtin_tools.register_all ();
  Plugin.register_all ()
