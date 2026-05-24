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
- Footer command palette: `/model`, `/provider`, `/plugins`, `/tools`,
  `/new`, `/resume`, `/fork`, `/retry`, `/compact`, `/status`,
  `/instructions`, `/undo`, `/diff`.
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
- `--tui` without an initial task now starts a fullscreen interactive shell.
  Ctrl+Enter drains the prompt into the session loop: slash commands render in
  the timeline and ordinary task prompts run the agent against the same
  event-sourced session log.
- The fullscreen shell now treats `/model <id>` and `/provider <name> [model]
  [api-base]` as stateful commands. They rebuild the model client, update the
  status strip, and subsequent task submissions use the switched runtime.
- The fullscreen shell now treats `/new`, `/resume <dir>`, and `/fork [index]`
  as stateful commands. They switch the active session directory, reopen the
  event log, reload inspector context, and continue later turns from that
  session.
- Token usage is now derived from assistant-message events and shown in the
  status/inspector surfaces; `/usage` renders input, output, and total tokens
  from the current event log.
- `/status` now exposes a shared REPL/TUI runtime summary with workspace,
  session, provider/model, event count, token usage, plugin diagnostics, and
  registered tool count.
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
- Fullscreen TUI and REPL `/compact` now append a `Context_compacted` event to
  the active log. The raw history remains auditable, while future replay uses a
  bounded summary plus recent turns.
- Plugin manifests now expose `sdk_version` compatibility metadata. Scaffolded
  plugins write it explicitly, while check/install/run reject unsupported future
  SDK versions.
- Plugin runtime commands now receive richer SDK environment metadata:
  workspace, plugin id/name/version/sdk version, tool name/kind, and the args
  JSON file path used for stdin.
- Plugin scaffolding now accepts `--plugin-id`, letting developers create a
  starter manifest with the final package id instead of the directory-derived
  `local.*` default.
- Plugin scaffolding now also accepts `--plugin-tool-name`, so the manifest,
  README, and smoke-test args file start with the developer's real first tool.
- Plugin local development now has `--smoke-plugin`, which validates a plugin
  and runs each tool against `examples/<tool>.args.json` before install.
- Plugin installation now supports `--replace-plugin` for iterative SDK
  development. Replacement validates and stages the new plugin before removing
  the old installed copy, so reinstalling a local plugin is one command without
  making failed updates destructive.
- Plugin discovery now keeps diagnostics for invalid manifests. `/plugins` and
  `--list-plugins` show broken plugin directories and validation errors while
  still registering every valid plugin found on the same search path.
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
  `/tools`, `/plugins`, `/models`, `/model`, `/sessions`, `/tree`, `/diff`,
  `/status`, `/instructions`, `/log`, and `/inspect` render their output into
  the timeline without leaving fullscreen mode.
- The REPL exposes the same inspector through `/inspect [index]`, so event-log
  review works even outside a full-screen TUI and can target historical events.
- The REPL exposes plugin inspection through `/plugin <id|tool>`, showing
  command, kind, timeout, and schema details for one plugin.
- The REPL exposes registered tool inspection through `/tool <name>`, so
  built-in and plugin tool schemas can be checked without asking the model.
- Code-review tasks add review-specific system guidance without rewriting the
  logged user event, preserving audit and replay fidelity.
- Project instructions from `AGENTS.md`, `CLAUDE.md`, and
  `.fp-agent/instructions.md` now load into the model system prompt, with
  workspace-bounded whole-line `@relative` includes and no event-log leakage.
- CLI regression tests cover plugin lifecycle commands, REPL plugin/tool
  discovery, custom provider model listing/switching, and the `--confirm --tui`
  argument path before a real terminal is opened.

The web/product-reference pass still needs to be done with gstack `/browse`
once that browser tool is available in this repo.
