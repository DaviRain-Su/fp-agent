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

Pick a provider and set its API key, then run:

```sh
export KIMI_API_KEY=...                  # default provider (Kimi for coding)
dune exec -- fp-agent "fix the failing test in lib/foo.ml"

# or another provider
export ZAI_API_KEY=...
dune exec -- fp-agent --provider zhipu "add a docstring to lib/foo.ml"
```

### Interactive REPL

Omit the task to start a REPL that keeps context across turns:

```sh
dune exec -- fp-agent
> create a file hello.txt with the text hi
> now add a second line "bye" to it
> /sessions          # list sessions in this workspace
> /resume <name>     # switch to a past session
> /tools             # preview available tools
> /help
> /exit
```

Each turn replays the session's event log as context, so the agent remembers
earlier turns. Meta-commands start with `/`; anything else is a task.

Options:

- `-p`, `--provider NAME` — `kimi` (default), `zhipu`, or `deepseek`
- `-m`, `--model ID` — override the model id
- `--api-base URL` — override the provider's base URL
- `-w`, `--workspace DIR` — workspace root (default: `WORKSPACE_ROOT` or cwd)
- `--max-steps N` — max agent steps (default: `MAX_STEPS` or 30)
- `--confirm` — ask for approval before each shell command or file write
- `--resume SESSION_DIR` — replay a previous session's event log as context and continue
- `--tui` — full-screen live view of the run (autonomous; needs a real terminal)

On completion the agent prints a status summary and, if the workspace is a git
repo, `git diff --stat`. Every run also writes a full trace to
`.ocaml-agent/sessions/<timestamp>-<id>/events.jsonl`.

### Providers

Each provider has a built-in key env var, base URL, and default model. All carry
the same JSON action contract; the wire protocol differs (Kimi for coding speaks
the Anthropic Messages API, the others OpenAI chat completions).

| Provider | Key env var | Base URL | Default model | Protocol | Status |
| --- | --- | --- | --- | --- | --- |
| `kimi` (default) | `KIMI_API_KEY` | `https://api.kimi.com/coding` | `kimi-for-coding` | Anthropic | verified end-to-end |
| `deepseek` | `DEEPSEEK_API_KEY` | `https://api.deepseek.com` | `deepseek-v4-flash` | OpenAI | verified end-to-end |
| `zhipu` | `ZAI_API_KEY` | `https://api.z.ai/api/paas/v4` | `glm-4` | OpenAI | wiring verified; needs account balance |

For DeepSeek Pro: `--model deepseek-v4-pro`.

> The `zhipu` integration is correct but unverified live: the test key returned
> z.ai error `1113` ("Insufficient balance or no resource package"). Recharge the
> account, then `--provider zhipu` (optionally `--model glm-5.1`) works as-is.

### Environment variables

| Variable | Meaning | Default |
| --- | --- | --- |
| `PROVIDER` | Provider to use | `kimi` |
| `<PROVIDER>_API_KEY` | Selected provider's API key (required) | — |
| `API_BASE` | Override the provider's base URL | provider default |
| `MODEL_NAME` | Override the model id | provider default |
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

Tools: `read_file`, `write_file`, `edit_file`, `list_files`, `run_command`,
`search` (substring search across workspace files), `make_dir`,
`apply_patch` (a unified diff applied with `git apply`), and `multi_edit`
(several edits applied atomically across files).

`--yolo` bypasses the dangerous-command deny-list (workspace bounds still
apply) — use with care.

Use `--confirm` to require interactive approval before each shell command or
file modification.

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
