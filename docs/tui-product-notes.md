# TUI Product Notes

This is the working direction for evolving `fp-agent` toward a Claude
Code/Codex/Pi/Opencode-class agent while keeping a distinct shape.

## Product Pillars

- Event-sourced sessions: every UI view is derived from the JSONL event log, so
  resume, fork, replay, and audit remain first-class instead of bolted on.
- Native tool transparency: show model text, tool calls, policy decisions,
  parallel batches, usage, and compaction as separate timeline events.
- Review mode: code-review requests should bias the model toward diff-first,
  evidence-grounded findings while preserving the original user request in the
  event log.
- Plugin-first surface: `/tools` and `/plugins` should make third-party
  capabilities visible, inspectable, and approval-aware.
- Local-first control: provider/model switching, custom endpoints, and bounded
  workspace policy stay accessible from the TUI.

## Near-Term TUI Shape

- Left/primary timeline: assistant text streaming, tool call groups, results,
  errors, and final answer.
- Right/secondary inspector: selected event JSON, tool args/result, usage, and
  policy decision.
- Footer command palette: `/model`, `/provider`, `/provider-add`, `/plugins`,
  `/providers`, `/plugin-sdk`, `/tools`, `/new`, `/resume`, `/fork`, `/retry`,
  `/plan`, `/review`, `/plan-set`, `/plan-add`, `/plan-update`, `/plan-clear`,
  `/compact`, `/status`, `/context`, `/handoff`, `/instructions`, `/undo`,
  `/diff`.
- Status strip: provider/model, session id, step count, token usage, current
  phase, and active plugin count.
- Review mode: navigate prior events, fork from an event, and replay compacted
  context summaries.

## Implemented Guardrails

- Timeline viewport helpers are pure and unit-tested: long event lines wrap to
  terminal width before the newest visible rows are selected.
- Wide TUI renders a two-pane timeline + inspector view. The inspector exposes
  provider/model/session, phase, event count, plugin count, tool count, and the
  selected event from the same event stream that drives the transcript.
- The inspector follows the latest event by default, but Up/Down, `j`/`k`,
  PageUp/PageDown, Home, End, and mouse-wheel input can pin or resume the event
  selection during a live run.
- The inspector expands the selected event into a stable type label, summary,
  key tool/policy/result fields, and a JSON preview, keeping audit details close
  to the live run.
- `/` or `?` opens a TUI command palette overlay with the core REPL commands
  for model/provider switching, plugin/tool inspection, session navigation,
  forks, diffs, and undo; Up/Down move through entries and Enter/Esc closes it.
- Multiline prompt editing is now modeled in pure `View` helpers: draft text,
  byte cursor, insert/newline/backspace/delete, cursor movement, empty checks,
  and rendering with a visible cursor. This is the reusable base for the
  fullscreen interactive shell.
- `Tui_shell` centralizes prompt submission, command palette movement, and
  event inspection selection in a pure controller. The current TUI already uses
  it for palette and event-selection actions.
- `Tui_shell` now also owns the abstract keyboard/mouse input mapping for
  palette priority, multiline prompt composition, Ctrl+Enter submission, and
  event browsing. The Notty renderer translates terminal events into those
  inputs instead of carrying shell behavior itself.
- The Notty renderer now feeds ordinary ASCII and Unicode keypresses into the
  prompt draft when the palette is closed, so seeded commands like `/tool ` can
  be completed in the fullscreen view. The live draft renders with the same
  prompt editor lines used by unit tests.
- The fullscreen shell now records submitted prompt history. `Ctrl+Up` and
  `Ctrl+Down` browse prior prompts while preserving plain Up/Down for event
  inspection. Resume and fork flows seed that history from the event log while
  filtering agent-internal retry/preflight messages.
- `--tui` without an initial task now starts a fullscreen interactive shell.
  Ctrl+Enter drains the prompt into the session loop: slash commands render in
  the timeline and ordinary task prompts run the agent against the same
  event-sourced session log.
- The fullscreen shell now treats `/model <id>`, `/model-next`, and `/provider
  <name> [model] [api-base]` as stateful commands. They rebuild the model
  client, update the status strip, and subsequent task submissions use the
  switched runtime. `/model <id>` can also switch provider when the model id
  uniquely belongs to another configured provider.
- `/providers` now renders a provider discovery panel with built-in and custom
  profiles, protocol, API base, auth hint, models, and active model markers
  while hiding API key values.
- `/provider-add <name> <base-url> <model[,model...]>` saves a provider profile
  into `FP_AGENT_CONFIG` or `.fp-agent/providers.json`, including local-server
  compatibility flags, so custom OpenAI-compatible endpoints can be added from
  the shell before `/provider` switches to them.
- The fullscreen shell now treats `/new`, `/resume <dir>`, and `/fork [index]`
  as stateful commands. They switch the active session directory, reopen the
  event log, reload inspector context, and continue later turns from that
  session.
- `/sessions` now renders a session browser instead of a bare directory list,
  showing the current marker, event count, plan progress, last user task, and
  parent/fork metadata for each session.
- Token usage is now derived from assistant-message events and shown in the
  status/inspector surfaces; `/usage` renders input, output, and total tokens
  from the current event log.
- `/status` now exposes a shared REPL/TUI runtime summary with workspace,
  session, provider/model, event count, token usage, plugin diagnostics, and
  registered tool count.
- `/context` now previews the event-sourced conversation context that a future
  model turn inherits: replayed turns, tool-use/result summaries, compaction
  count, token totals, agent state, and project instruction state.
- `/handoff` now renders a copyable continuation summary from the event log:
  resume commands, runtime, token usage, current plan, last user task, recent
  events, and workspace diff summary.
- `/review [focus]` now gives REPL/TUI users an explicit code-review entry
  point. It wraps the focus into a code-review task, triggering review guidance
  and preflight without relying on the model to infer intent from free text.
- `/instructions` now exposes the exact workspace project instructions that are
  appended to the model system prompt, so REPL/TUI users can audit repo guidance
  before spending a model call.
- Fullscreen TUI confirm mode now renders approval prompts in the active view.
  `Y` approves a pending tool call, while `N`, Enter, or Esc deny it; normal
  prompt and palette input is paused until the approval is resolved.
- Fullscreen TUI `/undo` now shares the REPL git checkpoint stack. Each
  submitted task captures the worktree before execution, and `/undo` restores
  that checkpoint without touching `.ocaml-agent` session logs.
- Fullscreen TUI and REPL `/retry` now read the active event log, find the
  latest non-empty user task, and submit it again through the current runtime.
  The command palette seeds `/retry` as a draft instead of auto-dispatching it,
  keeping reruns explicit.
- Fullscreen TUI and REPL `/plan-set`, `/plan-add`, `/plan-update`, and
  `/plan-clear` now append `Plan_updated` events with todo/doing/done items,
  while `/plan`, `/log`, and `/inspect` render the latest plan from the event
  log. `/status`, the fullscreen status strip, and the inspector also surface
  plan completion progress. Plan state survives resume/fork because it is data,
  not transient UI state.
- The command palette now groups commands by workflow area such as Tools,
  Plugins, Sessions, Models, Context, and Run Control. Filtering also matches
  those group labels, so queries like `plugins install` can jump directly to
  the right plugin command.
- Fullscreen TUI and REPL `/compact` now append a `Context_compacted` event to
  the active log. The raw history remains auditable, while future replay uses a
  bounded summary plus recent turns.
- Plugin manifests now expose `sdk_version` compatibility metadata. Scaffolded
  plugins write it explicitly, while check/install/run reject unsupported future
  SDK versions.
- Plugin runtime commands now receive richer SDK environment metadata:
  workspace, plugin id/name/version/sdk version, tool name/kind, and the args
  JSON file path used for stdin.
- Plugin tools can now declare optional `permissions` audit metadata. The
  manifest loader validates it, plugin/tool surfaces show it, registered tool
  descriptions carry it, runtime commands receive the raw JSON via
  `FP_AGENT_TOOL_PERMISSIONS`, and sensitive declarations participate in
  `--confirm` approval prompts for model-triggered plugin calls. `/plugin`
  now shows the exact approval reason next to the permission metadata.
- Plugin `input_schema` validation now supports JSON Schema `enum`, so local
  runs, smoke checks, and model-triggered plugin calls reject invalid option
  values before executing plugin code.
- Plugin `input_schema` validation now also honors object
  `additionalProperties`, letting plugin authors reject undeclared model
  arguments before their command runs.
- Plugin scaffolding now accepts `--plugin-id`, letting developers create a
  starter manifest with the final package id instead of the directory-derived
  `local.*` default.
- Plugin scaffolding now also accepts `--plugin-tool-name`, so the manifest,
  README, and smoke-test args file start with the developer's real first tool.
- Plugin scaffolding accepts `--plugin-kind` / `--kind` so the starter manifest
  can begin as a `read`, `write`, or `exec` tool without manual JSON edits.
- Plugin scaffolding accepts `--plugin-template` / `--template`, with `shell`
  and `python` starters that generate the matching command, script files,
  README, manifest, and smoke args. Python starters now include a local
  `fp_agent_sdk.py` helper with JSON arg parsing, `ToolContext`, result
  serialization, and error handling.
- Plugin local development now has `--smoke-plugin`, which validates a plugin
  and runs each tool against `examples/<tool>.args.json` plus any sorted
  `examples/<tool>/*.json` case files before install.
- Plugin smoke checks are now available inside the REPL and fullscreen TUI as
  `/plugin-smoke [--replace] <dir>`, backed by the same SDK smoke runner as the
  CLI.
- Plugin authors can now run one local tool inside the REPL or fullscreen TUI
  with `/plugin-run <dir> <tool> <json|@file>`, so the interactive workflow can
  validate real tool output without a model call.
- Plugin development now has `/plugin-dev [--replace] <dir>` and
  `--dev-plugin DIR`, which validate, smoke-test, install, refresh tools, and
  print inspection next steps in one command.
- Plugin install management is now available inside the REPL and fullscreen TUI
  as `/plugin-new`, `/plugin-check`, `/plugin-install`, and `/plugin-remove`,
  with tool registry and status-count reloads after install/remove so current
  sessions see updated tools.
- Plugin scaffold/install/remove output now includes actionable next commands:
  scaffold points at validation, smoke, and replace-install; install points at
  the installed plugin id, each registered tool, and runnable `/plugin-run`
  examples when args files exist; remove points back to `/plugins`.
- Plugin installation now supports `--replace-plugin` for iterative SDK
  development. Replacement validates and stages the new plugin before removing
  the old installed copy, so reinstalling a local plugin is one command without
  making failed updates destructive.
- Plugin discovery now keeps diagnostics for invalid manifests. `/plugins` and
  `--list-plugins` show broken plugin directories and validation errors while
  still registering every valid plugin found on the same search path.
- Plugin developers now have `/plugin-doctor` and `--doctor-plugins` for
  read-only discovery diagnostics: plugin home, search roots, valid/invalid
  counts, tool-name conflicts, and next inspection commands.
- Plugin developers can run `/plugin-sdk` or `--plugin-sdk` to see the manifest
  SDK version, built-in scaffold templates, runtime environment variables, and
  the shortest local development loop without opening the docs.
- Plugin tool-name conflicts are now reported in CLI/TUI tool and plugin
  surfaces. Built-ins and earlier discovered plugins keep precedence, while the
  skipped plugin/tool pair is shown to the developer.
- `--check-plugin` and `--install-plugin` now reject plugin tool names that
  would be shadowed after install. `--check-plugin --replace-plugin` validates
  update compatibility while ignoring the currently installed plugin with the
  same id.
- The command palette controller now distinguishes close from accept: Enter
  returns the highlighted command entry, giving the future fullscreen shell a
  tested command-dispatch point while preserving the current overlay behavior.
- REPL slash commands and the TUI command palette are now backed by shared
  command metadata and parser tests. This keeps `/model` vs `/models`, aliases
  like `/quit`, and future command-palette dispatch behavior from drifting.
- Palette acceptance now has explicit safety semantics: safe no-arg commands
  can dispatch directly, while commands needing arguments or likely explicit
  confirmation seed the prompt draft for user completion.
- The command palette now carries a search query in pure state. Typing while the
  palette is open filters commands case-insensitively, supports empty-result
  rendering, and keeps selection clamped to the filtered list.
- Palette accept results now emit tested feedback lines. The Notty TUI appends
  accepted commands and seeded command drafts to the timeline instead of
  silently dropping them.
- The Notty TUI now executes safe read-only palette commands directly:
  `/tools`, `/plugins`, `/models`, `/providers`, `/model`, `/sessions`, `/tree`,
  `/diff`, `/status`, `/context`, `/handoff`, `/instructions`, `/log`, and
  `/inspect` render their output into the timeline without leaving fullscreen
  mode.
- The REPL exposes the same inspector through `/inspect [index]`, so event-log
  review works even outside a full-screen TUI and can target historical events.
- The REPL exposes plugin inspection through `/plugin <id|tool>`, showing
  command, kind, timeout, and schema details for one plugin.
- The REPL exposes registered tool inspection through `/tool <name>`, so
  built-in and plugin tool schemas can be checked without asking the model.
- Code-review tasks and `/review [focus]` add review-specific system guidance
  without rewriting the logged user event, preserving audit and replay fidelity.
- Project instructions from `AGENTS.md`, `CLAUDE.md`, and
  `.fp-agent/instructions.md` now load into the model system prompt, with
  workspace-bounded whole-line `@relative` includes and no event-log leakage.
- CLI regression tests cover plugin lifecycle commands, REPL plugin/tool
  discovery, custom provider model listing/switching, and the `--confirm --tui`
  argument path before a real terminal is opened.

The web/product-reference pass still needs to be done with gstack `/browse`
once that browser tool is available in this repo.
