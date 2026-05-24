# fp-agent

A minimal, type-safe **Code Agent Harness** written in OCaml.

`fp-agent` takes a coding task on the command line, talks to an
OpenAI-compatible model, and lets the model read, edit, and run code inside a
bounded workspace — looping until it produces a final answer. The goal of this
MVP is not the smartest agent but a clean, auditable, extensible harness: typed
tool calls, an explicit state machine, a policy layer, and a full JSONL event
log of every step.

## Requirements

- OCaml 5.x and [opam](https://opam.ocaml.org/)
- [dune](https://dune.build/) 3.x

Install dependencies:

```sh
opam install . --deps-only --with-test
```

## Build, test, format

```sh
dune build       # compile
dune runtest     # unit + e2e tests
dune fmt         # format (requires .ocamlformat, included)
```

## Usage

```sh
export OPENAI_API_KEY=sk-...            # required
export OPENAI_API_BASE=https://api.openai.com/v1   # optional
export MODEL_NAME=gpt-4o-mini           # optional

dune exec -- fp-agent "fix the failing test in lib/foo.ml"
```

Options:

- `-w`, `--workspace DIR` — workspace root (default: `WORKSPACE_ROOT` or cwd)
- `--max-steps N` — max agent steps (default: `MAX_STEPS` or 30)

On completion the agent prints a status summary and, if the workspace is a git
repo, `git diff --stat`. Every run also writes a full trace to
`.ocaml-agent/sessions/<timestamp>-<id>/events.jsonl`.

### Environment variables

| Variable | Meaning | Default |
| --- | --- | --- |
| `OPENAI_API_KEY` | Model API key (required) | — |
| `OPENAI_API_BASE` | OpenAI-compatible endpoint base | `https://api.openai.com/v1` |
| `MODEL_NAME` | Model name | `gpt-4o-mini` |
| `MAX_STEPS` | Max agent loop steps | `30` |
| `WORKSPACE_ROOT` | Workspace root directory | current directory |

## How it works

The agent runs a typed state machine:

```
Initializing -> Waiting_for_model -> Executing_tool -> Observing_result
                      ^------------------------------------/
                      v
                 Completed / Failed
```

Each turn the model returns a single JSON action (see the system prompt in
`lib/model_client.ml`): either a `tool_call` or a `final_answer`. Tool calls are
checked by the policy layer, executed by the tool runner, and the result is fed
back as an observation.

### Modules

| Module | Responsibility |
| --- | --- |
| `tool_call` / `tool_result` | Typed tool calls and results (+ JSON) |
| `model_action` / `event` | Model actions and event-log entries |
| `agent_state` | State machine and legal transitions |
| `workspace` | Path resolution and workspace bounds |
| `shell` | Command execution with timeout |
| `policy` / `permission` | Allow/deny decisions, dangerous-command deny-list |
| `tool_runner` | Executes tools behind the policy |
| `session` / `event_log` | Session dirs and JSONL audit log |
| `config` / `message` / `model_client` | Env config, chat messages, HTTP client |
| `agent_loop` | The model↔tool loop |

### Safety

- All file paths are confined to the workspace; `../` escapes are rejected.
- Writes to `.git/` are blocked.
- Dangerous shell commands (e.g. `rm -rf /`, `mkfs`, piping a download into a
  shell) are denied.
- API keys never reach the event log.

Shell commands run via `/bin/sh -c` and inherit the current environment
(including `OPENAI_API_KEY`); this MVP does not sandbox that.

## Out of scope (MVP)

Multi-agent, TUI, browser automation, web search, vector memory, automatic git
commits, remote sandboxes, and session resume/replay. The event log carries a
`schema_version` so a replay engine can be added later.
