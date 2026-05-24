# fp-agent

A minimal, type-safe **Code Agent Harness** written in OCaml.

`fp-agent` takes a coding task (one-shot or in an interactive REPL), talks to a
model — Kimi, DeepSeek, or Zhipu — and lets it read, edit, search, and run code
inside a bounded workspace, looping until it produces a final answer. The
emphasis is a clean, auditable, extensible harness: typed tool calls, an
explicit state machine, a policy layer with optional approval, and a full JSONL
event log of every step.

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

# or a custom OpenAI-compatible endpoint
export FP_AGENT_CONFIG=./providers.json
dune exec -- fp-agent --provider local-llm --model qwen36-rtx "fix tests"
```

### Interactive REPL

Omit the task to start a REPL that keeps context across turns:

```sh
dune exec -- fp-agent
> create a file hello.txt with the text hi
> now add a second line "bye" to it
> /diff              # show uncommitted changes (git)
> /undo              # revert the last turn's changes (git)
> /log               # list this session's events with indices
> /fork 3            # fork a new branch at event index 3 (or /fork for the end)
> /tree              # show the session fork tree
> /sessions          # list sessions in this workspace
> /resume <name>     # switch to a past session
> /models            # list all configured provider/model ids
> /model qwen36-rtx  # switch model inside the REPL
> /provider local-llm qwen36-rtx
> /tools             # preview available tools
> /tool read_file    # inspect a tool's kind/schema/description
> /plugin echo_json  # inspect a plugin by id or tool name
> /inspect 12        # inspect event 12: tool args/result/policy/JSON
> /help
> /exit
```

Each turn replays the session's event log as context, so the agent remembers
earlier turns. Meta-commands start with `/`; anything else is a task.

Code review requests get a review-specific system instruction: the agent starts
from `git status --short` and `git diff --stat`, inspects changed files and
related code, and reports concise findings with evidence. The original user
request remains unchanged in the event log for audit and resume.

### Plugins

Third-party tools can be added as plugin directories with a
`fp-agent-plugin.json` manifest. A plugin tool receives JSON args on stdin and
returns its result on stdout.

```sh
export FP_AGENT_PLUGIN_PATH=$PWD/examples/plugins/echo
dune exec -- fp-agent
> /plugins
> /plugin echo_json
> /tool echo_json
> /tools
```

Install a plugin into the user plugin home:

```sh
dune exec -- fp-agent --new-plugin my-plugin
dune exec -- fp-agent --check-plugin my-plugin
dune exec -- fp-agent --run-plugin-tool my-plugin \
  --plugin-tool hello_world \
  --plugin-args '{"message":"hi"}'
dune exec -- fp-agent --install-plugin examples/plugins/echo
dune exec -- fp-agent --list-plugins
dune exec -- fp-agent --remove-plugin local.my-plugin
```

Plugins are discovered from `FP_AGENT_PLUGIN_PATH`, `.fp-agent/plugins`, and
`FP_AGENT_PLUGIN_HOME` / `~/.local/share/fp-agent/plugins`. See
`docs/plugins.md` for the SDK contract.

### Event-sourced sessions and forking

The event log is the source of truth: the agent's conversation state is a pure
fold over the events (`Session_state.reduce`/`replay`), not a separate mutable
copy. Two consequences:

- **Resume** (`--resume` / `/resume`) reconstructs state by replaying a
  session's log.
- **Fork** (`/fork [index]`) branches a session at an event index — the child's
  log is a prefix of the parent's, so replaying it yields the state at that
  point. Sessions therefore form a **tree** (`/tree`), letting you explore
  alternative continuations without disturbing the original branch.
- **Inspect** (`/inspect [index]`) renders the same event inspector used by the
  TUI: event kind, summary, tool args, policy/result details, and a JSON
  preview. With no index it inspects the latest event.
- **TUI event navigation** (`--tui`) lets the inspector follow the latest event
  by default, or pin a historical event with Up/Down, `j`/`k`, PageUp/PageDown,
  Home, and End.
- **TUI command palette** (`/` or `?` in `--tui`) shows the core REPL commands
  for models, providers, plugins, tools, sessions, forks, diffs, and undo.
- **TUI prompt editor groundwork** keeps multiline draft editing pure and
  testable: insert text, newline, delete/backspace, cursor movement, and
  rendering with a visible cursor.
- **TUI shell controller groundwork** centralizes prompt submission, command
  palette movement, and event inspection selection in a pure state machine.
  It also owns the abstract keyboard/mouse input mapping, including palette
  priority, event browsing, multiline prompt editing, and Ctrl+Enter submit
  semantics for the future interactive shell.

Options:

- `-p`, `--provider NAME` — `kimi` (default), `zhipu`, `deepseek`, `local`,
  or a custom provider from `FP_AGENT_CONFIG`
- `-m`, `--model ID` — override the model id
- `--api-base URL` — override the provider's base URL
- `-w`, `--workspace DIR` — workspace root (default: `WORKSPACE_ROOT` or cwd)
- `--max-steps N` — max agent steps (default: `MAX_STEPS` or 30)
- `--confirm` — ask for approval before each shell command or file write (not
  compatible with `--tui`)
- `--yolo` — bypass the dangerous-command deny-list (workspace bounds still apply)
- `--resume SESSION_DIR` — replay a previous session's event log as context and continue
- `--tui` — full-screen live view of the run (autonomous; needs a real terminal)
- `--new-plugin DIR` — create a starter plugin directory, then exit
- `--check-plugin DIR` — validate a plugin directory, then exit
- `--run-plugin-tool DIR --plugin-tool NAME --plugin-args JSON` — run a plugin
  tool locally for development, then exit
- `--install-plugin DIR` — validate and install a plugin directory, then exit
- `--list-plugins` — list plugins installed in the plugin home, then exit
- `--remove-plugin ID` / `--uninstall-plugin ID` — remove an installed plugin
  from the plugin home, then exit

On completion the agent prints a status summary and, if the workspace is a git
repo, `git diff --stat`. Every run also writes a full trace to
`.ocaml-agent/sessions/<timestamp>-<id>/events.jsonl`.

### Providers

Built-in providers have a key env var, base URL, and default model. Custom
providers can be loaded from JSON. All carry the same JSON action contract; the
wire protocol differs (Kimi for coding speaks the Anthropic Messages API, the
others OpenAI chat completions).

| Provider | Key env var | Base URL | Default model | Protocol | Status |
| --- | --- | --- | --- | --- | --- |
| `kimi` (default) | `KIMI_API_KEY` | `https://api.kimi.com/coding` | `kimi-for-coding` | Anthropic | verified end-to-end |
| `deepseek` | `DEEPSEEK_API_KEY` | `https://api.deepseek.com` | `deepseek-v4-flash` | OpenAI | verified end-to-end |
| `zhipu` | `ZAI_API_KEY` | `https://api.z.ai/api/paas/v4` | `glm-4` | OpenAI | wiring verified; needs account balance |
| `local` | optional `LOCAL_API_KEY` | `http://localhost:11434/v1` | `local-model` | OpenAI | built-in convenience profile |

For DeepSeek Pro: `--model deepseek-v4-pro`.

> The `zhipu` integration is correct but unverified live: the test key returned
> z.ai error `1113` ("Insufficient balance or no resource package"). Recharge the
> account, then `--provider zhipu` (optionally `--model glm-5.1`) works as-is.

Custom providers are looked up in `FP_AGENT_CONFIG`, `.fp-agent/providers.json`,
`.fp-agent.json`, or `~/.config/fp-agent/providers.json`. The file can be a
top-level provider map or `{ "providers": { ... } }`. A pi-style subset works:

```json
{
  "local-llm": {
    "baseUrl": "http://101.132.142.56:18080/v1",
    "api": "openai-completions",
    "apiKey": "dummy",
    "compat": {
      "supportsDeveloperRole": false,
      "supportsReasoningEffort": false,
      "supportsUsageInStreaming": false,
      "maxTokensField": "max_tokens"
    },
    "models": [
      {
        "id": "qwen36-rtx",
        "name": "qwen36-rtx",
        "contextWindow": 131072,
        "maxTokens": 8192
      }
    ]
  }
}
```

The implementation uses `baseUrl`, `api`, `apiKey`, `models[].id` or
`models[].name`, `models[].maxTokens`, `compat.supportsUsageInStreaming`, and
`compat.maxTokensField`. In the REPL, `/models` lists built-in providers plus
custom providers from these files, and `/provider <name> <model>` switches to
one of them.

### Environment variables

| Variable | Meaning | Default |
| --- | --- | --- |
| `PROVIDER` | Provider to use | `kimi` |
| `<PROVIDER>_API_KEY` | Built-in provider API key (`LOCAL_API_KEY` optional) | — |
| `FP_AGENT_CONFIG` | Custom provider config file | optional |
| `API_BASE` | Override the provider's base URL | provider default |
| `MODEL_NAME` | Override the model id | provider default |
| `LOCAL_MODELS` | Comma-separated extra model ids for the built-in `local` provider | optional |
| `MAX_STEPS` | Max agent loop steps | `30` |
| `WORKSPACE_ROOT` | Workspace root directory | current directory |
| `FP_AGENT_PLUGIN_PATH` | Colon-separated plugin dirs or parent dirs | optional |
| `FP_AGENT_PLUGIN_HOME` | Install/discovery directory for plugins | `~/.local/share/fp-agent/plugins` |

## How it works

The agent runs a typed state machine:

```
Initializing -> Waiting_for_model -> Executing_tool -> Observing_result
                      ^------------------------------------/
                      v
                 Completed / Failed
```

Each turn the model returns a JSON action (see the system prompt in
`lib/model_client.ml`): a `tool_call`, a batch `tool_calls`, or a
`final_answer`. Tool calls are checked by the policy layer, executed by the tool
runner, and the results are fed back as observations. Batch calls are logged in
request order; execution is detached through Lwt so independent calls can run
concurrently while replay still sees a stable event sequence.

Tools: `read_file`, `write_file`, `edit_file`, `list_files`, `run_command`,
`search` (substring search across workspace files), `make_dir`,
`apply_patch` (a unified diff applied with `git apply`), and `multi_edit`
(several edits applied atomically across files).

`--yolo` bypasses the dangerous-command deny-list (workspace bounds still
apply) — use with care.

Use `--confirm` to require interactive approval before each shell command or
file modification.

### Modules

`lib/` is one library (`fp_agent`) with modules grouped by concern via
`(include_subdirs unqualified)`, so module names stay flat (`Tool_call`,
`Workspace`, …) regardless of folder.

**`lib/core/`** — pure types:

| Module | Responsibility |
| --- | --- |
| `tool_call` / `tool_result` | Typed tool calls and results (+ JSON) |
| `model_action` / `event` / `graph_event` | Model actions, event-log entries, and graph lifecycle events |
| `agent_state` | State machine and legal transitions |
| `permission` / `message` | Policy decisions and chat messages |

**`lib/io/`** — side effects:

| Module | Responsibility |
| --- | --- |
| `workspace` | Path resolution and workspace bounds |
| `shell` | Command execution with timeout |
| `session` / `event_log` | Session dirs (fork tree) and JSONL audit log |
| `journal` | Reads an event log back |

**`lib/agent/`** — the agent itself:

| Module | Responsibility |
| --- | --- |
| `config` / `provider` | Env config and per-provider key/base/model/protocol |
| `model_client` | OpenAI/Anthropic HTTP client + action parsing |
| `policy` / `tool_runner` | Deny-list/approval and tool execution |
| `session_state` / `transcript` | Event-sourced state (fold over the log) |
| `agent_loop` | The model↔tool loop |
| `graph_runtime` | P4 graph runtime skeleton: Agent/Tool/Parallel/Sequence/Router nodes |

**`lib/ui/`** — `view` (pure TUI helpers: windowing, line classification).

### Safety

- All file paths are confined to the workspace; `../` escapes are rejected.
- Writes to `.git/` are blocked.
- Dangerous shell commands (e.g. `rm -rf /`, `mkfs`, piping a download into a
  shell) are denied.
- Provider API keys are not written by the harness, and shell child processes
  run with likely secret environment variables removed.

Shell commands run via `/bin/sh -c` and inherit a scrubbed environment. This is
not an OS-level sandbox: command output is still fed back to the model and
written to the event log.

## Out of scope

Multi-agent orchestration, browser automation, web search, vector memory,
automatic git commits, and remote sandboxes. The event log carries a
`schema_version` so a full replay engine can be added later (today `--resume`
reconstructs context from it).
