open! Base
open Fp_agent

let test_window () =
  Alcotest.(check (list string))
    "keeps last rows" [ "b"; "c" ]
    (View.window ~rows:2 [ "a"; "b"; "c" ]);
  Alcotest.(check (list string))
    "all when fewer" [ "a"; "b" ]
    (View.window ~rows:5 [ "a"; "b" ]);
  Alcotest.(check (list string)) "zero rows" [] (View.window ~rows:0 [ "a" ]);
  Alcotest.(check (list string)) "empty" [] (View.window ~rows:3 [])

let test_display_lines () =
  Alcotest.(check (list string))
    "splits multiline text"
    [ "one"; "two"; ""; "four" ]
    (View.display_lines "one\ntwo\n\nfour");
  Alcotest.(check (list string)) "empty text" [] (View.display_lines "")

let kind_str = function
  | `Ok -> "ok"
  | `Err -> "err"
  | `Action -> "action"
  | `Plain -> "plain"

let test_classify () =
  Alcotest.(check string) "ok" "ok" (kind_str (View.classify "  ✓ done"));
  Alcotest.(check string) "err" "err" (kind_str (View.classify "  ✗ nope"));
  Alcotest.(check string)
    "action" "action"
    (kind_str (View.classify "→ read_file a"));
  Alcotest.(check string) "plain" "plain" (kind_str (View.classify "hello"))

let () =
  Alcotest.run "view"
    [
      ( "view",
        [
          Alcotest.test_case "window" `Quick test_window;
          Alcotest.test_case "display_lines" `Quick test_display_lines;
          Alcotest.test_case "classify" `Quick test_classify;
        ] );
    ]
