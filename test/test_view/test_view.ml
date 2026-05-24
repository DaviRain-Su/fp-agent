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

let test_event_selection () =
  Alcotest.(check (option int))
    "empty selection has no index" None
    (View.selection_index ~event_count:0 View.Follow_latest);
  Alcotest.(check string)
    "empty selection label" "no events"
    (View.selection_label ~event_count:0 View.Follow_latest);
  Alcotest.(check (option int))
    "follow latest resolves newest" (Some 4)
    (View.selection_index ~event_count:5 View.Follow_latest);
  let pinned =
    View.move_selection ~event_count:5 ~delta:(-1) View.Follow_latest
  in
  Alcotest.(check (option int))
    "move up pins previous event" (Some 3)
    (View.selection_index ~event_count:5 pinned);
  Alcotest.(check string)
    "pinned label" "event 4/5"
    (View.selection_label ~event_count:5 pinned);
  let latest = View.move_selection ~event_count:5 ~delta:1 pinned in
  Alcotest.(check (option int))
    "moving to newest follows latest" (Some 4)
    (View.selection_index ~event_count:5 latest);
  Alcotest.(check string)
    "latest label" "latest (5/5)"
    (View.selection_label ~event_count:5 latest);
  let first = View.select_event ~event_count:5 ~index:(-100) in
  Alcotest.(check (option int))
    "select clamps low" (Some 0)
    (View.selection_index ~event_count:5 first);
  let newest = View.select_event ~event_count:5 ~index:100 in
  Alcotest.(check string)
    "select newest follows" "latest (5/5)"
    (View.selection_label ~event_count:5 newest)

let test_command_palette () =
  let count = List.length View.command_palette_entries in
  Alcotest.(check bool) "palette has commands" true (count > 0);
  Alcotest.(check (option int))
    "closed palette has no index" None
    (View.palette_index ~command_count:count View.Palette_closed);
  let opened = View.toggle_palette ~command_count:count View.Palette_closed in
  Alcotest.(check (option int))
    "toggle opens first command" (Some 0)
    (View.palette_index ~command_count:count opened);
  let moved = View.move_palette ~command_count:count ~delta:2 opened in
  Alcotest.(check (option int))
    "move selects command" (Some 2)
    (View.palette_index ~command_count:count moved);
  let clamped = View.move_palette ~command_count:count ~delta:100 moved in
  Alcotest.(check (option int))
    "move clamps high"
    (Some (count - 1))
    (View.palette_index ~command_count:count clamped);
  Alcotest.(check string)
    "palette label"
    (Printf.sprintf "command %d/%d" count count)
    (View.palette_label ~command_count:count clamped);
  let closed = View.toggle_palette ~command_count:count clamped in
  Alcotest.(check (option int))
    "toggle closes open palette" None
    (View.palette_index ~command_count:count closed);
  let filtered =
    View.filter_command_palette_entries ~query:"api-base"
      View.command_palette_entries
  in
  Alcotest.(check int) "filter finds provider" 1 (List.length filtered);
  Alcotest.(check string)
    "filter command" "/provider <name> [model] [api-base]"
    (Option.value_exn (List.hd filtered)).command;
  let queried =
    View.set_palette_query ~command_count:(List.length filtered)
      ~query:"api-base" opened
  in
  Alcotest.(check (option int))
    "query keeps selection" (Some 0)
    (View.palette_index ~command_count:(List.length filtered) queried);
  Alcotest.(check string)
    "query label" "command 1/1 for \"api-base\""
    (View.palette_label ~command_count:(List.length filtered) queried);
  let selected = 1 in
  let lines =
    View.command_palette_lines ~selected:(Some selected)
      View.command_palette_entries
  in
  let joined = String.concat lines ~sep:"\n" in
  let selected_command =
    Option.value_exn (List.nth View.command_palette_entries selected)
  in
  Alcotest.(check bool)
    "renders title" true
    (String.is_substring joined ~substring:"Command Palette");
  Alcotest.(check bool)
    "renders accept hint" true
    (String.is_substring joined ~substring:"Enter accept");
  Alcotest.(check bool)
    "renders selected marker" true
    (String.is_substring joined ~substring:("> " ^ selected_command.command));
  Alcotest.(check bool)
    "renders plugin command" true
    (String.is_substring joined ~substring:"/plugins");
  let empty_lines =
    View.command_palette_lines ~query:"nomatch" ~selected:None []
    |> String.concat ~sep:"\n"
  in
  Alcotest.(check bool)
    "renders no matches" true
    (String.is_substring empty_lines ~substring:"no matching commands")

let test_prompt_editor () =
  Alcotest.(check string) "empty text" "" View.prompt_empty.text;
  Alcotest.(check int) "empty cursor" 0 View.prompt_empty.cursor;
  Alcotest.(check bool)
    "empty draft" true
    (View.prompt_is_empty View.prompt_empty);
  let editor =
    View.prompt_empty
    |> View.prompt_insert_text "hello"
    |> View.prompt_newline
    |> View.prompt_insert_text "world"
  in
  Alcotest.(check string) "multiline text" "hello\nworld" editor.text;
  Alcotest.(check int) "cursor at end" 11 editor.cursor;
  let edited =
    editor
    |> View.prompt_move ~delta:(-5)
    |> View.prompt_insert_text "wide "
    |> View.prompt_backspace |> View.prompt_delete
  in
  Alcotest.(check string) "edits around cursor" "hello\nwideorld" edited.text;
  Alcotest.(check int) "cursor after edits" 10 edited.cursor;
  Alcotest.(check int) "home cursor" 0 (View.prompt_home edited).cursor;
  Alcotest.(check int)
    "end cursor"
    (String.length edited.text)
    (View.prompt_end edited).cursor;
  Alcotest.(check string)
    "cursor clamps" "abc" (View.prompt_make ~cursor:100 "abc").text;
  Alcotest.(check int)
    "cursor clamp value" 3 (View.prompt_make ~cursor:100 "abc").cursor;
  let rendered = String.concat (View.prompt_editor_lines edited) ~sep:"\n" in
  Alcotest.(check bool)
    "renders prompt title" true
    (String.is_substring rendered ~substring:"Prompt");
  Alcotest.(check bool)
    "renders visible cursor" true
    (String.is_substring rendered ~substring:"wide|orld")

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

let test_plugin_inspector_lines () =
  let plugin : Plugin.manifest =
    {
      id = "com.example.echo";
      name = "Echo Tools";
      version = "0.1.0";
      dir = "/tmp/echo";
      tools =
        [
          {
            tool_name = "echo_json";
            tool_kind = Tool.Read;
            tool_description = "Echoes JSON";
            tool_command = "sh echo.sh";
            tool_input_schema =
              Some
                (`Assoc
                   [
                     ("type", `String "object");
                     ( "properties",
                       `Assoc
                         [ ("message", `Assoc [ ("type", `String "string") ]) ]
                     );
                   ]);
            tool_timeout_sec = 7;
          };
        ];
    }
  in
  let joined = String.concat (View.plugin_inspector_lines plugin) ~sep:"\n" in
  Alcotest.(check bool)
    "shows plugin id" true
    (String.is_substring joined ~substring:"id: com.example.echo");
  Alcotest.(check bool)
    "shows command" true
    (String.is_substring joined ~substring:"command: sh echo.sh");
  Alcotest.(check bool)
    "shows timeout" true
    (String.is_substring joined ~substring:"timeout: 7s");
  Alcotest.(check bool)
    "shows schema" true
    (String.is_substring joined ~substring:"input_schema:")

let test_tool_inspector_lines () =
  let tool : Tool.t =
    {
      name = "echo_json";
      kind = Tool.Read;
      description = "Echoes JSON";
      input_schema =
        Some
          (`Assoc
             [
               ("type", `String "object");
               ("required", `List [ `String "message" ]);
               ( "properties",
                 `Assoc [ ("message", `Assoc [ ("type", `String "string") ]) ]
               );
             ]);
      check = (fun _ _ -> Permission.Allow);
      run = (fun _ _ -> Tool_result.Success { output = "ok" });
    }
  in
  let joined = String.concat (View.tool_inspector_lines tool) ~sep:"\n" in
  Alcotest.(check bool)
    "shows tool name" true
    (String.is_substring joined ~substring:"name: echo_json");
  Alcotest.(check bool)
    "shows kind" true
    (String.is_substring joined ~substring:"kind: read");
  Alcotest.(check bool)
    "shows schema" true
    (String.is_substring joined ~substring:"input_schema:");
  Alcotest.(check bool)
    "shows required field" true
    (String.is_substring joined ~substring:"\"required\":")

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
          Alcotest.test_case "event_selection" `Quick test_event_selection;
          Alcotest.test_case "command_palette" `Quick test_command_palette;
          Alcotest.test_case "prompt_editor" `Quick test_prompt_editor;
          Alcotest.test_case "event_summary" `Quick test_event_summary;
          Alcotest.test_case "event_inspector_lines" `Quick
            test_event_inspector_lines;
          Alcotest.test_case "plugin_inspector_lines" `Quick
            test_plugin_inspector_lines;
          Alcotest.test_case "tool_inspector_lines" `Quick
            test_tool_inspector_lines;
          Alcotest.test_case "classify" `Quick test_classify;
        ] );
    ]
