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

let test_wrap_line () =
  Alcotest.(check (list string))
    "wraps long line" [ "abcd"; "efgh"; "ij" ]
    (View.wrap_line ~cols:4 "abcdefghij");
  Alcotest.(check (list string))
    "keeps short line" [ "abc" ]
    (View.wrap_line ~cols:4 "abc");
  Alcotest.(check (list string))
    "preserves empty line" [ "" ]
    (View.wrap_line ~cols:4 "");
  Alcotest.(check (list string)) "zero cols" [] (View.wrap_line ~cols:0 "abc")

let test_viewport () =
  Alcotest.(check (list string))
    "wraps before windowing" [ "ef"; "12"; "34" ]
    (View.viewport ~rows:3 ~cols:2 [ "abcdef"; "1234" ]);
  Alcotest.(check (list string))
    "zero rows" []
    (View.viewport ~rows:0 ~cols:2 [ "abcdef" ]);
  Alcotest.(check (list string))
    "zero cols" []
    (View.viewport ~rows:3 ~cols:0 [ "abcdef" ])

let test_truncate_and_pad () =
  Alcotest.(check string) "no truncation" "abc" (View.truncate ~cols:4 "abc");
  Alcotest.(check string) "truncates" "abc…" (View.truncate ~cols:4 "abcdef");
  Alcotest.(check string) "one col" "…" (View.truncate ~cols:1 "abcdef");
  Alcotest.(check string) "zero col" "" (View.truncate ~cols:0 "abcdef");
  Alcotest.(check string) "pads" "abc  " (View.pad_right ~cols:5 "abc");
  Alcotest.(check string)
    "pads truncated" "abcd…"
    (View.pad_right ~cols:5 "abcdef")

let test_split_panes () =
  Alcotest.(check bool)
    "narrow terminal is single pane" true
    (Option.is_none (View.split_panes ~width:80));
  match View.split_panes ~width:120 with
  | None -> Alcotest.fail "expected wide terminal panes"
  | Some panes ->
      Alcotest.(check int) "timeline cols" 77 panes.timeline_cols;
      Alcotest.(check int) "inspector cols" 40 panes.inspector_cols

let test_status_and_inspector () =
  let status : View.status =
    {
      provider = "local-llm";
      model = "qwen36-rtx";
      session = "2026-session";
      phase = Some "running read_file…";
      events = 12;
      plugins = 2;
      tools = 11;
    }
  in
  let line = View.status_line status in
  Alcotest.(check bool)
    "status has provider/model" true
    (String.is_substring line ~substring:"local-llm/qwen36-rtx");
  Alcotest.(check bool)
    "status has plugins" true
    (String.is_substring line ~substring:"plugins 2");
  Alcotest.(check (list string))
    "inspector lines"
    [
      "Inspector";
      "provider: local-llm";
      "model: qwen36-rtx";
      "session: 2026-session";
      "phase: running read_file…";
      "events: 12";
      "plugins: 2";
      "tools: 11";
      "";
      "Last event";
      "→ read_file README.md";
    ]
    (View.inspector_lines status ~last_event:"→ read_file README.md")

let test_event_summary () =
  let event = Event.Tool_call (Tool_call.read_file "README.md") in
  Alcotest.(check string) "kind" "tool_call" (View.event_kind event);
  Alcotest.(check string)
    "summary" "→ read_file README.md" (View.event_summary event);
  let user = Event.User_message { content = "hello\nworld" } in
  Alcotest.(check string)
    "user summary flattens" "user: hello world" (View.event_summary user)

let test_event_inspector_lines () =
  let event =
    Event.Tool_call
      (Tool_call.make ~name:"search"
         ~args:(`Assoc [ ("query", `String "Plugin"); ("path", `String "lib") ]))
  in
  let lines = View.event_inspector_lines event in
  let joined = String.concat lines ~sep:"\n" in
  Alcotest.(check bool)
    "shows kind" true
    (String.is_substring joined ~substring:"kind: tool_call");
  Alcotest.(check bool)
    "shows tool" true
    (String.is_substring joined ~substring:"tool: search");
  Alcotest.(check bool)
    "shows args" true
    (String.is_substring joined ~substring:"\"query\": \"Plugin\"");
  Alcotest.(check bool)
    "shows json preview" true
    (String.is_substring joined ~substring:"JSON")

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
          Alcotest.test_case "wrap_line" `Quick test_wrap_line;
          Alcotest.test_case "viewport" `Quick test_viewport;
          Alcotest.test_case "truncate_and_pad" `Quick test_truncate_and_pad;
          Alcotest.test_case "split_panes" `Quick test_split_panes;
          Alcotest.test_case "status_and_inspector" `Quick
            test_status_and_inspector;
          Alcotest.test_case "event_summary" `Quick test_event_summary;
          Alcotest.test_case "event_inspector_lines" `Quick
            test_event_inspector_lines;
          Alcotest.test_case "classify" `Quick test_classify;
        ] );
    ]
