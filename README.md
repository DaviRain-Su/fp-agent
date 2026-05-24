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
> /retry             # rerun the latest user task
> /log               # list this session's events with indices
> /usage             # show token usage from the event log
> /status            # show runtime/session/plugin status
> /instructions      # show project instructions loaded for the model
> /compact           # summarize older session history
> /fork 3            # fork a new branch at event index 3 (or /fork for the end)
> /tree              # show the session fork tree
> /sessions          # list sessions in this workspace
> /new               # start a fresh session without prior history
> /resume <name>     # switch to a past session
> /models            # list all configured provider/model ids
> /model qwen36-rtx  # switch model inside the REPL
> /provider local-llm qwen36-rtx
> /tools             # preview available tools
> /tool read_file    # inspect a tool's kind/schema/description
> /plugin echo_json  # inspect a plugin by id or tool name
> /plugin-new --id local.my-plugin --tool-name my_tool my-plugin
> /plugin-check my-plugin
> /plugin-install --replace my-plugin
> /plugin-remove local.my-plugin
> /plugin-smoke --replace my-plugin
> /inspect 12        # inspect event 12: tool args/result/policy/JSON
> /help
> /exit
```

Each turn replays the session's event log as context, so the agent remembers
earlier turns. Meta-commands start with `/`; anything else is a task.

Use `--tui` without a task to start the fullscreen interactive shell. It uses
the same session log and command palette as one-shot TUI runs; Ctrl+Enter
submits the current draft as either a slash command or an agent task. `/model
<id>` and `/provider <name> [model] [api-base]` update the active runtime for
later turns inside the TUI; `/resume <dir>` and `/fork [index]` switch the
active event-sourced session.

Code review requests get a review-specific system instruction: the agent starts
from `git status --short` and `git diff --stat`, inspects changed files and
related code, and reports concise findings with evidence. The original user
request remains unchanged in the event log for audit and resume.

### Project instructions

`fp-agent` reads workspace-local instruction files from `AGENTS.md`,
`CLAUDE.md`, and `.fp-agent/instructions.md`, then appends them to the model
system prompt. Whole-line `@relative/path.md` includes are expanded relative to
the file that references them, and includes must stay inside the workspace.
Project instructions are not written into the event log, so session logs remain
focused on user-visible events and audit data. Use `/instructions` in the REPL
or TUI to preview the exact instruction text before a model call.

### Plugins

Third-party tools can be added as plugin directories with a
`fp-agent-plugin.json` manifest. A plugin tool receives JSON args on stdin and
returns its result on stdout. `input_schema` is enforced before the plugin
command runs, so developers get local feedback from `--run-plugin-tool` without
spending a model call. Manifests can declare `sdk_version`; unsupported future
SDK versions are rejected by `--check-plugin`, install, and local tool runs.
Plugin commands also receive runtime env such as `FP_AGENT_WORKSPACE`,
`FP_AGENT_PLUGIN_ID`, `FP_AGENT_TOOL_KIND`, and `FP_AGENT_ARGS_FILE`.

```sh
export FP_AGENT_PLUGIN_PATH=$PWD/examples/plugins/echo
dune exec -- fp-agent
> /plugins
> /plugin echo_json
> /plugin-new --id local.my-plugin --tool-name my_tool my-plugin
> /plugin-check my-plugin
> /plugin-install --replace my-plugin
> /plugin-smoke --replace my-plugin
> /plugin-remove local.my-plugin
> /tool echo_json
> /tools
```

Create, test, and install a plugin into the user plugin home:

```sh
dune exec -- fp-agent --new-plugin my-plugin
dune exec -- fp-agent --new-plugin my-plugin --plugin-id com.example.my_plugin
dune exec -- fp-agent --new-plugin my-plugin --plugin-tool-name my_tool
dune exec -- fp-agent --check-plugin my-plugin
dune exec -- fp-agent --smoke-plugin my-plugin
dune exec -- fp-agent
> /plugin-new --id local.my-plugin --tool-name my_tool my-plugin
> /plugin-check my-plugin
> /plugin-install --replace my-plugin
> /plugin-smoke --replace my-plugin
> /plugin-remove local.my-plugin
dune exec -- fp-agent --check-plugin my-plugin --replace-plugin
dune exec -- fp-agent --run-plugin-tool my-plugin \
  --plugin-tool hello_world \
  --plugin-args-file my-plugin/examples/hello_world.args.json
dune exec -- fp-agent --install-plugin my-plugin
dune exec -- fp-agent --install-plugin my-plugin --replace-plugin
dune exec -- fp-agent --list-plugins
dune exec -- fp-agent --remove-plugin local.my-plugin
```

Plugins are discovered from `FP_AGENT_PLUGIN_PATH`, `.fp-agent/plugins`, and
`FP_AGENT_PLUGIN_HOME` / `~/.local/share/fp-agent/plugins`. See
`docs/plugins.md` for the SDK contract. `/plugins` and `--list-plugins` also
surface invalid manifest diagnostics instead of silently hiding broken plugin
directories, and report tool-name conflicts when a plugin would shadow a
built-in or earlier discovered plugin tool.

### Event-sourced sessions and forking

The event log is the source of truth: the agent's conversation state is a pure
fold over the events (`Session_state.reduce`/`replay`), not a separate mutable
copy. Two consequences:

- **Resume** (`--resume` / `/resume`) reconstructs state by replaying a
  session's log.
- **New session** (`/new`) starts a fresh root session in the same workspace
  without replaying the current task history.
- **Retry** (`/retry`) finds the latest user task in the current event log and
  submits it again. The command is a seeded draft in the TUI palette so an
  accidental palette Enter cannot rerun work without an explicit submit.
- **Fork** (`/fork [index]`) branches a session at an event index — the child's
  log is a prefix of the parent's, so replaying it yields the state at that
  point. Sessions therefore form a **tree** (`/tree`), letting you explore
  alternative continuations without disturbing the original branch.
- **Inspect** (`/inspect [index]`) renders the same event inspector used by the
  TUI: event kind, summary, tool args, policy/result details, and a JSON
  preview. With no index it inspects the latest event.
- **Status** (`/status`) summarizes workspace, session, provider/model,
  event count, token usage, plugin diagnostics, and registered tool count.
- **Project instructions** (`/instructions`) shows the workspace instruction
  files that will be appended to the model system prompt.
- **Compaction** (`/compact`) appends a `Context_compacted` event that replaces
  older model-visible turns with a bounded summary while preserving the recent
  turns needed to continue safely. The raw event log remains intact.
- **TUI event navigation** (`--tui`) lets the inspector follow the latest event
  by default, or pin a historical event with Up/Down, PageUp/PageDown, Home,
  and End.
- **TUI command palette** (`/` or `?` in `--tui`) shows the core REPL commands
  for models, providers, plugins, tools, sessions, event logs, usage, forks,
  diffs, and undo. The palette and REPL parser share the same command metadata,
  and palette acceptance either dispatches safe no-arg commands or seeds the
  prompt draft for commands that need arguments. Typing while the palette is open
  filters commands by name or description, and accepted commands/drafts are
  echoed into the TUI timeline. Read-only commands such as `/tools`, `/plugins`,
  `/models`, `/model`, `/usage`, `/status`, `/instructions`, `/diff`, `/log`,
  and `/inspect` render their results directly inside the fullscreen view.
- **Plugin smoke checks** (`/plugin-smoke [--replace] <dir>`) validate a
  plugin and run its `examples/<tool>.args.json` files from inside the REPL or
  fullscreen TUI, so plugin developers can keep the current agent session open
  while testing SDK changes.
- **Plugin install management** (`/plugin-new [--id ID] [--tool-name NAME]
  <dir>`, `/plugin-check <dir>`, `/plugin-install [--replace] <dir>`,
  `/plugin-remove <id>`) scaffolds, validates, installs, and removes plugins
  from the REPL or fullscreen TUI, then reloads the in-process tool registry so
  `/tools` and later model calls see the updated plugin set.
- **TUI model/provider switching** lets `/model <id>` and `/provider <name>
  [model] [api-base]` change the runtime used by later TUI task submissions.
- **TUI session navigation** lets `/new`, `/resume <dir>`, and `/fork [index]`
  switch the active fullscreen session and continue writing to the selected
  event log.
- **TUI undo** lets `/undo` restore the git worktree to the checkpoint captured
  before the previous submitted TUI task, while leaving `.ocaml-agent` session
  logs alone.
- **TUI retry** lets `/retry` rerun the latest user task in the active
  event-sourced session, using the currently selected provider/model runtime.
- **TUI compaction** lets `/compact` manually summarize older context in the
  active session when a long run needs a smaller model-visible history.
- **TUI prompt editor** keeps multiline draft editing pure and testable:
  inserted text, seeded palette drafts, newline, delete/backspace, cursor
  movement, and rendering with a visible cursor all show inside the fullscreen
  view.
- **TUI prompt history** records submitted prompts in the fullscreen shell.
  `Ctrl+Up` and `Ctrl+Down` browse previous prompts without stealing plain
  Up/Down from event inspection.
- **TUI shell controller groundwork** centralizes prompt submission, command
  palette movement, and event inspection selection in a pure state machine.
  It also owns the abstract keyboard/mouse input mapping, including palette
  priority, event browsing, prompt history, multiline prompt editing, and
  Ctrl+Enter submit semantics for the interactive shell. Palette Enter returns
  the selected command entry so fullscreen command handling does not need to
  re-parse terminal key events.

Options:

- `-p`, `--provider NAME` — `kimi` (default), `zhipu`, `deepseek`, `local`,
  or a custom provider from `FP_AGENT_CONFIG`
- `-m`, `--model ID` — override the model id
- `--api-base URL` — override the provider's base URL
- `-w`, `--workspace DIR` — workspace root (default: `WORKSPACE_ROOT` or cwd)
- `--max-steps N` — max agent steps (default: `MAX_STEPS` or 30)
- `--confirm` — ask for approval before each shell command or file write;
  fullscreen TUI prompts accept `Y` to approve and `N`/Esc to deny
- `--yolo` — bypass the dangerous-command deny-list (workspace bounds still apply)
- `--resume SESSION_DIR` — replay a previous session's event log as context and continue
- `--tui` — full-screen live view for one-shot tasks, or an interactive
  fullscreen shell when no task is supplied (needs a real terminal)
- `--new-plugin DIR` — create a starter plugin directory, then exit
- `--plugin-id ID` — manifest id to use with `--new-plugin`
- `--plugin-tool-name NAME` — initial tool name to use with `--new-plugin`
- `--check-plugin DIR` — validate a plugin directory, then exit
- `--smoke-plugin DIR` — validate a plugin directory and run each tool with
  `examples/<tool>.args.json`
- `--replace-plugin` with `--check-plugin` — validate replacement
  compatibility against the currently installed plugin with the same id
- `--run-plugin-tool DIR --plugin-tool NAME --plugin-args JSON` — run a plugin
  tool locally for development, then exit
- `--plugin-args-file FILE` — read local plugin tool JSON args from a file
- `--install-plugin DIR` — validate and install a plugin directory, then exit
- `--replace-plugin` — allow `--install-plugin` to replace an existing
  installed plugin after staging the new copy
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
file modification. In fullscreen TUI mode the approval prompt is rendered in
the active view instead of stdin.

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
