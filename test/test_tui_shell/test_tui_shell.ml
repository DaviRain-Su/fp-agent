open! Base
open Fp_agent

let apply action state = (Tui_shell.handle state action).state

let input ?(page_size = 3) key state =
  (Tui_shell.handle_input ~page_size state key).state

let accepted_command = function
  | None -> None
  | Some (command : View.command_entry) -> Some command.command

let palette_command_at index =
  Option.map (List.nth View.command_palette_entries index)
    ~f:(fun (command : View.command_entry) -> command.command)

let test_prompt_submit () =
  let state =
    Tui_shell.create ()
    |> apply (Tui_shell.Insert_text "inspect README")
    |> apply Tui_shell.Newline
    |> apply (Tui_shell.Insert_text "summarize risks")
  in
  Alcotest.(check string)
    "draft text" "inspect README\nsummarize risks" state.draft.text;
  let result = Tui_shell.handle state Tui_shell.Submit_prompt in
  Alcotest.(check (option string))
    "submitted prompt" (Some "inspect README\nsummarize risks") result.submitted;
  Alcotest.(check string) "draft cleared" "" result.state.draft.text;
  let empty_submit = Tui_shell.handle result.state Tui_shell.Submit_prompt in
  Alcotest.(check (option string)) "empty no submit" None empty_submit.submitted

let test_prompt_input_mapping () =
  let state =
    Tui_shell.create ()
    |> input (Tui_shell.Text "inspect README")
    |> input Tui_shell.Enter
    |> input (Tui_shell.Text "summarize risks")
  in
  Alcotest.(check string)
    "enter inserts newline" "inspect README\nsummarize risks" state.draft.text;
  let result = Tui_shell.handle_input ~page_size:3 state Tui_shell.Ctrl_enter in
  Alcotest.(check (option string))
    "ctrl enter submits" (Some "inspect README\nsummarize risks")
    result.submitted;
  Alcotest.(check string) "submit clears draft" "" result.state.draft.text

let test_palette_state () =
  let state = Tui_shell.create ~command_count:4 () in
  Alcotest.(check bool) "closed" false (Tui_shell.palette_open state);
  let state = apply Tui_shell.Toggle_palette state in
  Alcotest.(check (option int))
    "opened first" (Some 0)
    (Tui_shell.selected_command_index state);
  let state = apply (Tui_shell.Move_palette 10) state in
  Alcotest.(check (option int))
    "clamped high" (Some 3)
    (Tui_shell.selected_command_index state);
  Alcotest.(check string)
    "palette label" "command 4/4"
    (Tui_shell.palette_label state);
  let state = apply Tui_shell.Palette_home state in
  Alcotest.(check (option int))
    "home" (Some 0)
    (Tui_shell.selected_command_index state);
  let state = apply Tui_shell.Close_palette state in
  Alcotest.(check (option int))
    "closed index" None
    (Tui_shell.selected_command_index state)

let test_palette_input_mapping () =
  let state = Tui_shell.create ~command_count:5 () in
  let state = input Tui_shell.Slash state in
  Alcotest.(check (option int))
    "slash opens" (Some 0)
    (Tui_shell.selected_command_index state);
  let state = input Tui_shell.Down state in
  Alcotest.(check (option int))
    "down moves palette" (Some 1)
    (Tui_shell.selected_command_index state);
  let state = input ~page_size:2 Tui_shell.Page_down state in
  Alcotest.(check (option int))
    "page down moves palette" (Some 3)
    (Tui_shell.selected_command_index state);
  let state = input Tui_shell.End state in
  Alcotest.(check (option int))
    "end moves palette" (Some 4)
    (Tui_shell.selected_command_index state);
  let state = input (Tui_shell.Text "ignored while palette open") state in
  Alcotest.(check (option int))
    "text ignored while open" (Some 4)
    (Tui_shell.selected_command_index state);
  let result = Tui_shell.handle_input ~page_size:3 state Tui_shell.Enter in
  Alcotest.(check bool)
    "enter closes palette" false
    (Tui_shell.palette_open result.state);
  Alcotest.(check (option string))
    "enter accepts selected command" (palette_command_at 4)
    (accepted_command result.accepted_command);
  Alcotest.(check (option string))
    "enter dispatches no-arg command" (Some "/sessions")
    result.dispatched_command;
  Alcotest.(check string)
    "direct command leaves draft empty" "" result.state.draft.text

let test_palette_accept_draft_mapping () =
  let state =
    Tui_shell.create () |> input Tui_shell.Slash |> input Tui_shell.Down
  in
  let result = Tui_shell.handle_input ~page_size:3 state Tui_shell.Enter in
  Alcotest.(check (option string))
    "accepts tool command" (Some "/tool <name>")
    (accepted_command result.accepted_command);
  Alcotest.(check (option string))
    "draft command does not dispatch" None result.dispatched_command;
  Alcotest.(check string) "seeds draft" "/tool " result.state.draft.text;
  Alcotest.(check int) "draft cursor at end" 6 result.state.draft.cursor

let test_event_selection_state () =
  let state = Tui_shell.create () |> Tui_shell.set_event_count 5 in
  Alcotest.(check (option int))
    "latest" (Some 4)
    (Tui_shell.selected_event_index state);
  let state = apply (Tui_shell.Move_event (-2)) state in
  Alcotest.(check (option int))
    "pinned event" (Some 2)
    (Tui_shell.selected_event_index state);
  Alcotest.(check string)
    "selection label" "event 3/5"
    (Tui_shell.selection_label state);
  let state = Tui_shell.set_event_count 2 state in
  Alcotest.(check (option int))
    "clamped after event shrink" (Some 1)
    (Tui_shell.selected_event_index state);
  Alcotest.(check string)
    "latest after clamp" "latest (2/2)"
    (Tui_shell.selection_label state);
  let state = apply Tui_shell.Event_home state in
  Alcotest.(check (option int))
    "first event" (Some 0)
    (Tui_shell.selected_event_index state);
  let state = apply Tui_shell.Event_end state in
  Alcotest.(check string)
    "follow latest" "latest (2/2)"
    (Tui_shell.selection_label state)

let test_event_input_mapping () =
  let state = Tui_shell.create () |> Tui_shell.set_event_count 6 in
  let state = input Tui_shell.Up state in
  Alcotest.(check (option int))
    "up selects previous event" (Some 4)
    (Tui_shell.selected_event_index state);
  let state = input ~page_size:2 Tui_shell.Page_up state in
  Alcotest.(check (option int))
    "page up jumps events" (Some 2)
    (Tui_shell.selected_event_index state);
  let state = input Tui_shell.Mouse_scroll_down state in
  Alcotest.(check (option int))
    "scroll down moves events" (Some 3)
    (Tui_shell.selected_event_index state);
  let state = input Tui_shell.End state in
  Alcotest.(check string)
    "end follows latest" "latest (6/6)"
    (Tui_shell.selection_label state)

let test_home_end_prompt_vs_events () =
  let state = Tui_shell.create () |> Tui_shell.set_event_count 4 in
  let state = input Tui_shell.Home state in
  Alcotest.(check (option int))
    "home without draft selects first event" (Some 0)
    (Tui_shell.selected_event_index state);
  let state = input Tui_shell.End state in
  Alcotest.(check string)
    "end without draft follows latest" "latest (4/4)"
    (Tui_shell.selection_label state);
  let state = input (Tui_shell.Text "abc") state in
  let state = input Tui_shell.Home state in
  Alcotest.(check int) "home with draft moves cursor" 0 state.draft.cursor;
  let state = input Tui_shell.End state in
  Alcotest.(check int) "end with draft moves cursor" 3 state.draft.cursor

let () =
  Alcotest.run "tui_shell"
    [
      ( "controller",
        [
          Alcotest.test_case "prompt_submit" `Quick test_prompt_submit;
          Alcotest.test_case "prompt_input_mapping" `Quick
            test_prompt_input_mapping;
          Alcotest.test_case "palette_state" `Quick test_palette_state;
          Alcotest.test_case "palette_input_mapping" `Quick
            test_palette_input_mapping;
          Alcotest.test_case "palette_accept_draft_mapping" `Quick
            test_palette_accept_draft_mapping;
          Alcotest.test_case "event_selection_state" `Quick
            test_event_selection_state;
          Alcotest.test_case "event_input_mapping" `Quick
            test_event_input_mapping;
          Alcotest.test_case "home_end_prompt_vs_events" `Quick
            test_home_end_prompt_vs_events;
        ] );
    ]
