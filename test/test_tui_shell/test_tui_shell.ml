open! Base
open Fp_agent

let apply action state = (Tui_shell.handle state action).state

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

let () =
  Alcotest.run "tui_shell"
    [
      ( "controller",
        [
          Alcotest.test_case "prompt_submit" `Quick test_prompt_submit;
          Alcotest.test_case "palette_state" `Quick test_palette_state;
          Alcotest.test_case "event_selection_state" `Quick
            test_event_selection_state;
        ] );
    ]
