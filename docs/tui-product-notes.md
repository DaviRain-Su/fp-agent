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
  `/resume`, `/fork`, `/undo`, `/diff`.
- Status strip: provider/model, session id, step count, token usage, current
  phase, and active plugin count.
- Review mode: navigate prior events, fork from an event, and replay compacted
  context summaries.

## Implemented Guardrails

- Timeline viewport helpers are pure and unit-tested: long event lines wrap to
  terminal width before the newest visible rows are selected.
- Wide TUI renders a two-pane timeline + inspector view. The inspector exposes
  provider/model/session, phase, event count, plugin count, tool count, and last
  event from the same event stream that drives the transcript.
- The inspector now expands the latest event into a stable type label, summary,
  key tool/policy/result fields, and a JSON preview, keeping audit details close
  to the live run.
- The REPL exposes the same inspector through `/inspect [index]`, so event-log
  review works even outside a full-screen TUI and can target historical events.
- The REPL exposes plugin inspection through `/plugin <id|tool>`, showing
  command, kind, timeout, and schema details for one plugin.
- The REPL exposes registered tool inspection through `/tool <name>`, so
  built-in and plugin tool schemas can be checked without asking the model.
- Code-review tasks add review-specific system guidance without rewriting the
  logged user event, preserving audit and replay fidelity.
- CLI regression tests cover plugin lifecycle commands, REPL plugin/tool
  discovery, custom provider model listing/switching, and `--confirm --tui`
  rejection.

The web/product-reference pass still needs to be done with gstack `/browse`
once that browser tool is available in this repo.
