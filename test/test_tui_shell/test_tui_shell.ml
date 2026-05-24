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

let rm_rf path =
  ignore
    (Shell.run
       ~command:(Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote path))
       ~timeout_sec:10
      : (Shell.result, string) Result.t)

let mkdir_p path =
  ignore
    (Shell.run
       ~command:(Printf.sprintf "mkdir -p %s" (Stdlib.Filename.quote path))
       ~timeout_sec:10
      : (Shell.result, string) Result.t)

let write path content =
  mkdir_p (Stdlib.Filename.dirname path);
  Stdlib.Out_channel.with_open_bin path (fun oc ->
      Stdlib.Out_channel.output_string oc content)

let with_temp_dir prefix f =
  let root = Stdlib.Filename.temp_dir prefix "" in
  Exn.protect ~f:(fun () -> f root) ~finally:(fun () -> rm_rf root)

let tui_context ?(events = []) ?selected_event_index root =
  let session_dir = Session.create ~base_dir:root in
  {
    Tui_command.provider = "local-llm";
    model = "qwen36-rtx";
    api_base = "http://localhost/v1";
    workspace_root = root;
    sessions_root = Stdlib.Filename.dirname session_dir;
    session_dir;
    events;
    selected_event_index;
  }

let output command context =
  match Tui_command.run context command with
  | Some lines -> String.concat lines ~sep:"\n"
  | None -> Alcotest.failf "command was not handled: %s" command

let provider_config =
  {|{
  "local-llm": {
    "baseUrl": "http://localhost/v1",
    "api": "openai-completions",
    "apiKey": "dummy",
    "models": [
      { "id": "qwen36-rtx", "name": "qwen36-rtx" }
    ]
  }
}
|}

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
  Alcotest.(check bool)
    "submit feedback" true
    (String.is_substring
       (String.concat ~sep:"\n" (Tui_shell.feedback_lines result))
       ~substring:"[tui] prompt submitted: inspect README");
  Alcotest.(check string) "submit clears draft" "" result.state.draft.text

let test_prompt_history_input_mapping () =
  let submit text state =
    (Tui_shell.handle_input ~page_size:3
       (input (Tui_shell.Text text) state)
       Tui_shell.Ctrl_enter)
      .state
  in
  let state =
    Tui_shell.create () |> submit "first task" |> submit "second task"
  in
  Alcotest.(check string)
    "ctrl up recalls latest" "second task"
    (input Tui_shell.Ctrl_up state).draft.text;
  let state = state |> input Tui_shell.Ctrl_up |> input Tui_shell.Ctrl_up in
  Alcotest.(check string) "ctrl up recalls older" "first task" state.draft.text;
  let state = input Tui_shell.Ctrl_up state in
  Alcotest.(check string)
    "history clamps at oldest" "first task" state.draft.text;
  let state = input Tui_shell.Ctrl_down state in
  Alcotest.(check string)
    "ctrl down recalls newer" "second task" state.draft.text;
  let state = input Tui_shell.Ctrl_down state in
  Alcotest.(check string) "ctrl down restores empty draft" "" state.draft.text;
  let state =
    Tui_shell.create () |> submit "saved task" |> input (Tui_shell.Text "draft")
  in
  let state = state |> input Tui_shell.Ctrl_up |> input Tui_shell.Ctrl_down in
  Alcotest.(check string) "restores stashed draft" "draft" state.draft.text

let test_prompt_history_can_be_seeded () =
  let state =
    Tui_shell.create ()
    |> input (Tui_shell.Text "draft")
    |> Tui_shell.set_history [ ""; "old task"; "old task"; "new task" ]
  in
  Alcotest.(check string) "preserves draft" "draft" state.draft.text;
  let state = input Tui_shell.Ctrl_up state in
  Alcotest.(check string) "seed latest" "new task" state.draft.text;
  let state = input Tui_shell.Ctrl_up state in
  Alcotest.(check string) "seed dedupes" "old task" state.draft.text

let test_prompt_history_from_events_filters_internal_messages () =
  let history =
    Tui_shell.history_of_events
      [
        Event.User_message { content = "first task" };
        Event.User_message { content = "[Code review preflight]\nstatus" };
        Event.User_message
          {
            content =
              "Your previous reply could not be processed (bad json). Reply.";
          };
        Event.Assistant_message { content = []; usage = Llm.zero_usage };
        Event.User_message { content = "  " };
        Event.User_message { content = "second task" };
      ]
  in
  Alcotest.(check (list string))
    "history from events"
    [ "first task"; "second task" ]
    history

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
  let state = Tui_shell.create ~command_count:13 () in
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
    "end moves palette" (Some 12)
    (Tui_shell.selected_command_index state);
  let result = Tui_shell.handle_input ~page_size:3 state Tui_shell.Enter in
  Alcotest.(check bool)
    "enter closes palette" false
    (Tui_shell.palette_open result.state);
  Alcotest.(check (option string))
    "enter accepts selected command" (palette_command_at 12)
    (accepted_command result.accepted_command);
  let selected_command = palette_command_at 12 in
  Alcotest.(check (option string))
    "enter dispatches no-arg command" selected_command result.dispatched_command;
  Alcotest.(check (list string))
    "dispatch feedback"
    (Option.to_list
       (Option.map selected_command ~f:(fun command ->
            "[tui] accepted command: " ^ command)))
    (Tui_shell.feedback_lines result);
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
  Alcotest.(check int) "draft cursor at end" 6 result.state.draft.cursor;
  Alcotest.(check (list string))
    "draft feedback"
    [ "[tui] draft command: /tool <name>"; "[tui] draft text: /tool " ]
    (Tui_shell.feedback_lines result)

let test_palette_filter_input_mapping () =
  let state =
    Tui_shell.create () |> input Tui_shell.Slash
    |> input (Tui_shell.Text "api-base")
  in
  Alcotest.(check (option string))
    "query updated" (Some "api-base")
    (Tui_shell.palette_query state);
  Alcotest.(check int)
    "query filters commands" 1
    (List.length (Tui_shell.visible_command_entries state));
  let result = Tui_shell.handle_input ~page_size:3 state Tui_shell.Enter in
  Alcotest.(check (option string))
    "filtered accept command" (Some "/provider <name> [model] [api-base]")
    (accepted_command result.accepted_command);
  Alcotest.(check string)
    "filtered accept seeds draft" "/provider " result.state.draft.text;
  let state =
    Tui_shell.create () |> input Tui_shell.Slash
    |> input (Tui_shell.Text "api-base")
    |> input Tui_shell.Delete_key
  in
  Alcotest.(check (option string))
    "delete clears query" (Some "")
    (Tui_shell.palette_query state)

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

let approval_str = function
  | Some Tui_shell.Approve -> "approve"
  | Some Tui_shell.Deny -> "deny"
  | None -> "none"

let test_approval_input_mapping () =
  Alcotest.(check string)
    "y approves" "approve"
    (approval_str (Tui_shell.approval_decision_of_input (Tui_shell.Text "y")));
  Alcotest.(check string)
    "yes approves" "approve"
    (approval_str (Tui_shell.approval_decision_of_input (Tui_shell.Text "yes")));
  Alcotest.(check string)
    "n denies" "deny"
    (approval_str (Tui_shell.approval_decision_of_input (Tui_shell.Text "n")));
  Alcotest.(check string)
    "escape denies" "deny"
    (approval_str (Tui_shell.approval_decision_of_input Tui_shell.Escape));
  Alcotest.(check string)
    "enter denies" "deny"
    (approval_str (Tui_shell.approval_decision_of_input Tui_shell.Enter));
  Alcotest.(check string)
    "other ignored" "none"
    (approval_str
       (Tui_shell.approval_decision_of_input (Tui_shell.Text "maybe")))

let test_tui_command_model_log_and_inspect () =
  with_temp_dir "fp_agent_tui_command_events" (fun root ->
      let config_path = Stdlib.Filename.concat root "providers.json" in
      write config_path provider_config;
      Unix.putenv "FP_AGENT_CONFIG" config_path;
      let events =
        [
          Event.User_message { content = "inspect README" };
          Event.Assistant_message
            {
              content = [ Llm.Text "ok" ];
              usage = { input_tokens = 21; output_tokens = 7 };
            };
        ]
      in
      let context = tui_context ~events ~selected_event_index:0 root in
      let model = output "/model" context in
      Alcotest.(check bool)
        "model command header" true
        (String.is_substring model ~substring:"[tui] /model");
      Alcotest.(check bool)
        "model provider" true
        (String.is_substring model ~substring:"provider: local-llm");
      let providers = output "/providers" context in
      Alcotest.(check bool)
        "providers command header" true
        (String.is_substring providers ~substring:"[tui] /providers");
      Alcotest.(check bool)
        "providers lists deepseek" true
        (String.is_substring providers ~substring:"deepseek");
      Alcotest.(check bool)
        "providers includes protocol" true
        (String.is_substring providers ~substring:"protocol: openai");
      Alcotest.(check bool)
        "providers hides custom key" true
        (String.is_substring providers ~substring:"api key hidden");
      Alcotest.(check bool)
        "model switch is stateful" true
        (Option.is_none (Tui_command.run context "/model qwen-coder"));
      Alcotest.(check bool)
        "provider switch is stateful" true
        (Option.is_none
           (Tui_command.run context "/provider local-llm qwen36-rtx"));
      Alcotest.(check bool)
        "plan-set is stateful" true
        (Option.is_none (Tui_command.run context "/plan-set todo inspect code"));
      Alcotest.(check bool)
        "plan-add is stateful" true
        (Option.is_none (Tui_command.run context "/plan-add todo run tests"));
      Alcotest.(check bool)
        "plan-update is stateful" true
        (Option.is_none (Tui_command.run context "/plan-update 1 done"));
      Alcotest.(check bool)
        "plan-clear is stateful" true
        (Option.is_none (Tui_command.run context "/plan-clear"));
      Alcotest.(check bool)
        "new session is stateful" true
        (Option.is_none (Tui_command.run context "/new"));
      Alcotest.(check bool)
        "compact is stateful" true
        (Option.is_none (Tui_command.run context "/compact"));
      Alcotest.(check bool)
        "retry is stateful" true
        (Option.is_none (Tui_command.run context "/retry"));
      Alcotest.(check bool)
        "review is stateful" true
        (Option.is_none (Tui_command.run context "/review current changes"));
      Alcotest.(check (option string))
        "last user task" (Some "inspect README")
        (Tui_command.last_user_message events);
      Alcotest.(check (option string))
        "last user task skips empty retry target" (Some "inspect README")
        (Tui_command.last_user_message
           (events @ [ Event.User_message { content = "  " } ]));
      Alcotest.(check (option string))
        "last user task skips internal prompt" (Some "inspect README")
        (Tui_command.last_user_message
           (events
           @ [
               Event.User_message
                 { content = "[Code review preflight]\nstatus" };
             ]));
      let log = output "/log" context in
      Alcotest.(check bool)
        "log includes event summary" true
        (String.is_substring log ~substring:"0  user: inspect README");
      let inspect = output "/inspect" context in
      Alcotest.(check bool)
        "inspect includes selected index" true
        (String.is_substring inspect ~substring:"event 0");
      Alcotest.(check bool)
        "inspect includes event kind" true
        (String.is_substring inspect ~substring:"kind: user_message");
      let usage = output "/usage" context in
      Alcotest.(check bool)
        "usage command shows input" true
        (String.is_substring usage ~substring:"input_tokens: 21");
      Alcotest.(check bool)
        "usage command shows total" true
        (String.is_substring usage ~substring:"total_tokens: 28");
      let context_preview = output "/context" context in
      Alcotest.(check bool)
        "context command header" true
        (String.is_substring context_preview ~substring:"[tui] /context");
      Alcotest.(check bool)
        "context shows replay turns" true
        (String.is_substring context_preview ~substring:"replayed_turns: 2");
      Alcotest.(check bool)
        "context shows user text" true
        (String.is_substring context_preview ~substring:"text: inspect README");
      Alcotest.(check bool)
        "context shows assistant text" true
        (String.is_substring context_preview ~substring:"text: ok");
      let plan_context =
        tui_context
          ~events:
            (events
            @ [
                Event.Plan_updated
                  {
                    items =
                      [
                        { Event.status = Event.Todo; text = "inspect code" };
                        { Event.status = Event.Doing; text = "implement plan" };
                        { Event.status = Event.Done; text = "write tests" };
                      ];
                  };
              ])
          root
      in
      let plan = output "/plan" plan_context in
      Alcotest.(check bool)
        "plan command shows header" true
        (String.is_substring plan ~substring:"[tui] /plan");
      Alcotest.(check bool)
        "plan command shows item" true
        (String.is_substring plan ~substring:"2. [doing] implement plan");
      let plan_status = output "/status" plan_context in
      Alcotest.(check bool)
        "status shows plan progress" true
        (String.is_substring plan_status ~substring:"plan: 1/3 done");
      let handoff = output "/handoff" plan_context in
      Alcotest.(check bool)
        "handoff command header" true
        (String.is_substring handoff ~substring:"[tui] /handoff");
      Alcotest.(check bool)
        "handoff includes resume command" true
        (String.is_substring handoff ~substring:"resume: dune exec -- fp-agent");
      Alcotest.(check bool)
        "handoff includes last user task" true
        (String.is_substring handoff ~substring:"last_user_task: inspect README");
      Alcotest.(check bool)
        "handoff includes plan progress" true
        (String.is_substring handoff ~substring:"plan: 1/3 done");
      Alcotest.(check bool)
        "handoff includes recent events" true
        (String.is_substring handoff ~substring:"Recent events:");
      let status = output "/status" context in
      Alcotest.(check bool)
        "status command header" true
        (String.is_substring status ~substring:"[tui] /status");
      Alcotest.(check bool)
        "status shows workspace" true
        (String.is_substring status ~substring:("workspace: " ^ root));
      Alcotest.(check bool)
        "status shows provider" true
        (String.is_substring status ~substring:"provider: local-llm");
      Alcotest.(check bool)
        "status shows events" true
        (String.is_substring status ~substring:"events: 2");
      Alcotest.(check bool)
        "status shows tokens" true
        (String.is_substring status
           ~substring:"tokens: input 21 output 7 total 28");
      Alcotest.(check bool)
        "status shows no plan" true
        (String.is_substring status ~substring:"plan: none");
      Alcotest.(check bool)
        "status shows tool count" true
        (String.is_substring status ~substring:"tools:");
      Alcotest.(check bool)
        "status shows instruction state" true
        (String.is_substring status ~substring:"project_instructions: none");
      let compact_context =
        tui_context
          ~events:
            [
              Event.Context_compacted
                {
                  summary = "older summary";
                  recent = [ Llm.user "recent task" ];
                };
            ]
          root
      in
      let compact_preview = output "/context" compact_context in
      Alcotest.(check bool)
        "context shows compaction count" true
        (String.is_substring compact_preview ~substring:"compactions: 1");
      Alcotest.(check bool)
        "context shows summary turn" true
        (String.is_substring compact_preview
           ~substring:"[Earlier conversation summary]");
      let inspect_by_index = output "/inspect 0" context in
      Alcotest.(check bool)
        "inspect accepts explicit index" true
        (String.is_substring inspect_by_index ~substring:"event 0"))

let test_tui_command_project_instructions () =
  with_temp_dir "fp_agent_tui_command_instructions" (fun root ->
      write
        (Stdlib.Filename.concat root "RTK.md")
        "Prefer repo-specific test evidence.\n";
      write
        (Stdlib.Filename.concat root "AGENTS.md")
        "Follow workspace conventions.\n@RTK.md\n";
      let context = tui_context root in
      let instructions = output "/instructions" context in
      Alcotest.(check bool)
        "instructions command header" true
        (String.is_substring instructions ~substring:"[tui] /instructions");
      Alcotest.(check bool)
        "instructions include agents" true
        (String.is_substring instructions ~substring:"--- AGENTS.md ---");
      Alcotest.(check bool)
        "instructions include referenced file" true
        (String.is_substring instructions ~substring:"--- RTK.md ---");
      Alcotest.(check bool)
        "instructions include referenced content" true
        (String.is_substring instructions
           ~substring:"Prefer repo-specific test evidence.");
      let status = output "/status" context in
      Alcotest.(check bool)
        "status shows loaded instructions" true
        (String.is_substring status ~substring:"project_instructions: loaded"))

let test_tui_command_sessions_and_diff () =
  with_temp_dir "fp_agent_tui_command_sessions" (fun root ->
      let context = tui_context root in
      let log = Event_log.create ~session_dir:context.session_dir in
      Event_log.append log (Event.User_message { content = "inspect README" });
      Event_log.append log
        (Event.Plan_updated
           {
             items =
               [
                 { Event.status = Event.Done; text = "inspect README" };
                 { Event.status = Event.Todo; text = "write tests" };
               ];
           });
      Event_log.append log
        (Event.Workspace_snapshot
           {
             is_git = true;
             status = [ " M README.md" ];
             diff_stat = [ " README.md | 1 +" ];
           });
      Event_log.append log
        (Event.Turn_completed
           {
             status = Event.Turn_status_completed;
             steps = 2;
             summary = "updated README session docs";
           });
      Event_log.close log;
      let child =
        match
          Session.fork ~base_dir:root ~parent_session_dir:context.session_dir
            ~at:(Some 1)
        with
        | Ok child -> child
        | Error e -> Alcotest.failf "fork session fixture: %s" e
      in
      let sessions = output "/sessions" context in
      Alcotest.(check bool)
        "sessions marks current" true
        (String.is_substring sessions ~substring:"  *");
      Alcotest.(check bool)
        "sessions show event count" true
        (String.is_substring sessions ~substring:"events=4");
      Alcotest.(check bool)
        "sessions show plan progress" true
        (String.is_substring sessions ~substring:"plan=1/2 done");
      Alcotest.(check bool)
        "sessions show turn status" true
        (String.is_substring sessions ~substring:"turn=completed/2");
      Alcotest.(check bool)
        "sessions show workspace status" true
        (String.is_substring sessions ~substring:"workspace=changed(1/1)");
      Alcotest.(check bool)
        "sessions keep legacy fallback status" true
        (String.is_substring sessions ~substring:"turn=none workspace=unknown");
      Alcotest.(check bool)
        "sessions show last task" true
        (String.is_substring sessions ~substring:"last=inspect README");
      Alcotest.(check bool)
        "sessions show fork parent" true
        (String.is_substring sessions
           ~substring:("parent=" ^ Stdlib.Filename.basename context.session_dir));
      Alcotest.(check bool)
        "sessions show fork point" true
        (String.is_substring sessions ~substring:"fork@1");
      Alcotest.(check bool)
        "sessions include child" true
        (String.is_substring sessions
           ~substring:(Stdlib.Filename.basename child));
      Alcotest.(check bool)
        "resume is stateful" true
        (Option.is_none (Tui_command.run context "/resume child-session"));
      Alcotest.(check bool)
        "new session is stateful" true
        (Option.is_none (Tui_command.run context "/new"));
      Alcotest.(check bool)
        "fork is stateful" true
        (Option.is_none (Tui_command.run context "/fork 0"));
      Alcotest.(check bool)
        "compact is stateful" true
        (Option.is_none (Tui_command.run context "/compact"));
      Alcotest.(check bool)
        "undo is stateful" true
        (Option.is_none (Tui_command.run context "/undo"));
      Alcotest.(check bool)
        "retry is stateful" true
        (Option.is_none (Tui_command.run context "/retry"));
      let diff = output "/diff" context in
      Alcotest.(check bool)
        "non-git diff message" true
        (String.is_substring diff ~substring:"(workspace is not a git repo)"))

let test_tui_command_plugins_and_tools () =
  with_temp_dir "fp_agent_tui_command_plugins" (fun root ->
      let plugin_dir = Stdlib.Filename.concat root "plugin" in
      let bad_plugin_dir = Stdlib.Filename.concat root "bad-plugin" in
      let conflict_plugin_dir = Stdlib.Filename.concat root "conflict-plugin" in
      write
        (Stdlib.Filename.concat plugin_dir Plugin.manifest_file)
        {|{
  "id": "com.example.tui",
  "name": "TUI Test Plugin",
  "version": "0.1.0",
  "tools": [
    {
      "name": "tui_echo",
      "kind": "read",
      "description": "Echoes TUI input",
      "command": "sh echo.sh",
      "permissions": { "workspace": "read", "network": false },
      "input_schema": {
        "type": "object",
        "properties": { "message": { "type": "string" } },
        "required": ["message"]
      }
    }
  ]
}
|};
      write (Stdlib.Filename.concat plugin_dir "echo.sh") "cat\n";
      write
        (Stdlib.Filename.concat bad_plugin_dir Plugin.manifest_file)
        {|{"id":"com.example.bad","tools":[]}|};
      write
        (Stdlib.Filename.concat conflict_plugin_dir Plugin.manifest_file)
        {|{
  "id": "com.example.conflict",
  "tools": [
    {
      "name": "read_file",
      "kind": "read",
      "description": "Conflicts with a built-in tool",
      "command": "sh echo.sh"
    }
  ]
}|};
      write (Stdlib.Filename.concat conflict_plugin_dir "echo.sh") "cat\n";
      Unix.putenv "FP_AGENT_PLUGIN_PATH"
        (String.concat ~sep:":"
           [ plugin_dir; bad_plugin_dir; conflict_plugin_dir ]);
      Unix.putenv "FP_AGENT_PLUGIN_HOME" (Stdlib.Filename.concat root "home");
      let context = tui_context root in
      let plugins = output "/plugins" context in
      Alcotest.(check bool)
        "plugins include manifest" true
        (String.is_substring plugins ~substring:"com.example.tui");
      Alcotest.(check bool)
        "plugins include invalid diagnostics" true
        (String.is_substring plugins ~substring:"Invalid plugins:");
      Alcotest.(check bool)
        "plugins include invalid reason" true
        (String.is_substring plugins ~substring:"at least one tool");
      Alcotest.(check bool)
        "plugins include conflict reason" true
        (String.is_substring plugins
           ~substring:
             "read_file from com.example.conflict skipped; already provided by \
              built-in tool");
      let tools = output "/tools" context in
      Alcotest.(check bool)
        "tools include plugin tool" true
        (String.is_substring tools ~substring:"tui_echo");
      Alcotest.(check bool)
        "tools include conflict reason" true
        (String.is_substring tools ~substring:"Plugin tool conflicts:");
      let tool = output "/tool tui_echo" context in
      Alcotest.(check bool)
        "tool detail includes schema" true
        (String.is_substring tool ~substring:"input_schema:");
      let plugin = output "/plugin tui_echo" context in
      Alcotest.(check bool)
        "plugin detail includes command" true
        (String.is_substring plugin ~substring:"command: sh echo.sh");
      Alcotest.(check bool)
        "plugin detail includes permissions" true
        (String.is_substring plugin ~substring:"permissions: workspace=read");
      Alcotest.(check bool)
        "plugin run is stateful" true
        (Option.is_none
           (Tui_command.run context
              {|/plugin-run plugin tui_echo {"message":"hi"}|}));
      let doctor = output "/plugin-doctor" context in
      Alcotest.(check bool)
        "doctor command header" true
        (String.is_substring doctor ~substring:"[tui] /plugin-doctor");
      Alcotest.(check bool)
        "doctor shows install home" true
        (String.is_substring doctor ~substring:"install_home:");
      Alcotest.(check bool)
        "doctor shows search roots" true
        (String.is_substring doctor ~substring:"search_roots:");
      Alcotest.(check bool)
        "doctor shows invalid count" true
        (String.is_substring doctor ~substring:"invalid_plugins: 1");
      Alcotest.(check bool)
        "doctor shows conflict count" true
        (String.is_substring doctor ~substring:"tool_conflicts: 1");
      Alcotest.(check bool)
        "doctor shows next dev command" true
        (String.is_substring doctor ~substring:"next: /plugin-dev --replace");
      let sdk = output "/plugin-sdk" context in
      Alcotest.(check bool)
        "sdk command header" true
        (String.is_substring sdk ~substring:"[tui] /plugin-sdk");
      Alcotest.(check bool)
        "sdk shows manifest file" true
        (String.is_substring sdk
           ~substring:"manifest_file: fp-agent-plugin.json");
      Alcotest.(check bool)
        "sdk shows python template" true
        (String.is_substring sdk ~substring:"python (aliases: python3, py)");
      Alcotest.(check bool)
        "sdk shows env" true
        (String.is_substring sdk ~substring:"FP_AGENT_TOOL_PERMISSIONS"))

let () =
  Alcotest.run "tui_shell"
    [
      ( "controller",
        [
          Alcotest.test_case "prompt_submit" `Quick test_prompt_submit;
          Alcotest.test_case "prompt_input_mapping" `Quick
            test_prompt_input_mapping;
          Alcotest.test_case "prompt_history_input_mapping" `Quick
            test_prompt_history_input_mapping;
          Alcotest.test_case "prompt_history_can_be_seeded" `Quick
            test_prompt_history_can_be_seeded;
          Alcotest.test_case "prompt_history_from_events" `Quick
            test_prompt_history_from_events_filters_internal_messages;
          Alcotest.test_case "palette_state" `Quick test_palette_state;
          Alcotest.test_case "palette_input_mapping" `Quick
            test_palette_input_mapping;
          Alcotest.test_case "palette_accept_draft_mapping" `Quick
            test_palette_accept_draft_mapping;
          Alcotest.test_case "palette_filter_input_mapping" `Quick
            test_palette_filter_input_mapping;
          Alcotest.test_case "event_selection_state" `Quick
            test_event_selection_state;
          Alcotest.test_case "event_input_mapping" `Quick
            test_event_input_mapping;
          Alcotest.test_case "home_end_prompt_vs_events" `Quick
            test_home_end_prompt_vs_events;
          Alcotest.test_case "approval_input_mapping" `Quick
            test_approval_input_mapping;
          Alcotest.test_case "tui_command_model_log_inspect" `Quick
            test_tui_command_model_log_and_inspect;
          Alcotest.test_case "tui_command_sessions_diff" `Quick
            test_tui_command_sessions_and_diff;
          Alcotest.test_case "tui_command_project_instructions" `Quick
            test_tui_command_project_instructions;
          Alcotest.test_case "tui_command_plugins_tools" `Quick
            test_tui_command_plugins_and_tools;
        ] );
    ]
