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
dune exec -- fp-agent --add-provider local-llm \
  --provider-base http://101.132.142.56:18080/v1 \
  --provider-model qwen36-rtx \
  --provider-api-key dummy \
  --provider-local-compat
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
> /review security   # run the code-review workflow with an optional focus
> /plan              # show the latest session plan
> /plan-set todo inspect code; doing implement fix; done write tests
> /plan-add todo run regression tests
> /plan-update 2 done implement fix
> /plan-clear
> /log               # list this session's events with indices
> /usage             # show token usage from the event log
> /status            # show runtime/session/plugin status
> /context           # preview model-visible replay context
> /handoff           # print a copyable session handoff summary
> /instructions      # show project instructions loaded for the model
> /compact           # summarize older session history
> /fork 3            # fork a new branch at event index 3 (or /fork for the end)
> /tree              # show the session fork tree
> /sessions          # list sessions with events, plan, fork, and last task
> /new               # start a fresh session without prior history
> /resume <name>     # switch to a past session
> /models            # list all configured provider/model ids
> /providers         # list provider profiles, protocol, auth hint, and models
> /model qwen36-rtx  # switch model inside the REPL
> /model-next        # cycle current provider's configured models
> /provider local-llm qwen36-rtx
> /provider-add local-llm http://101.132.142.56:18080/v1 qwen36-rtx --api-key dummy --local-compat
> /tools             # preview available tools
> /tool read_file    # inspect a tool's kind/schema/description
> /plugin-doctor     # show plugin search paths and diagnostics
> /plugin-sdk        # show SDK contract, env vars, and scaffold templates
> /plugin echo_json  # inspect a plugin by id or tool name
> /plugin-new --id local.my-plugin --tool-name my_tool --kind read --template python my-plugin
> /plugin-dev --replace my-plugin
> /plugin-check my-plugin
> /plugin-package --output my-plugin.fp-plugin.tar.gz my-plugin
> /plugin-install --replace my-plugin
> /plugin-smoke --replace my-plugin
> /plugin-run my-plugin my_tool '{"message":"hi"}'
> /plugin-remove local.my-plugin
> /inspect 12        # inspect event 12: tool args/result/policy/JSON
> /help
> /exit
```

Each turn replays the session's event log as context, so the agent remembers
earlier turns. Meta-commands start with `/`; anything else is a task.

Use `--tui` without a task to start the fullscreen interactive shell. It uses
the same session log and command palette as one-shot TUI runs; Ctrl+Enter
submits the current draft as either a slash command or an agent task. `/model
<id>` can switch to another configured provider when the model id uniquely
matches that provider; `/providers` shows provider profiles, protocol, API
base, auth status, and models without exposing API keys; `/model-next` cycles
the current provider's models; and `/provider <name> [model] [api-base]`
updates the active runtime directly.
`/resume <dir>` and `/fork [index]` switch the active event-sourced session.

Code review requests, including explicit `/review [focus]`, get a
review-specific system instruction: the agent starts from `git status --short`
and `git diff --stat`, inspects changed files and related code, and reports
concise findings with evidence. The original user request remains unchanged in
the event log for audit and resume.

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
command runs, so developers get local feedback from `/plugin-run` or
`--run-plugin-tool` without spending a model call. The local schema subset
includes common JSON Schema constraints such as `type`, `enum`, `required`,
object `properties`, and array `items`; object schemas can also set
`additionalProperties: false` to reject undeclared arguments before plugin code
runs. Manifests can declare `sdk_version`; unsupported future SDK versions are
rejected by `--check-plugin`, install, and local tool runs. Plugin commands also
receive runtime env such as `FP_AGENT_WORKSPACE`, `FP_AGENT_PLUGIN_ID`,
`FP_AGENT_TOOL_KIND`, `FP_AGENT_TOOL_PERMISSIONS`, and
`FP_AGENT_ARGS_FILE`. Tool manifests may include optional `permissions` audit
metadata such as `{ "workspace": "read", "network": false }`; it is validated,
shown in `/plugins` and `/plugin`, passed through to SDK wrappers, and used by
`--confirm` to require approval for sensitive plugin permissions such as
network, shell, env, secrets, tokens, or workspace writes. `/plugin` also shows
the exact approval reason that would appear before a model-triggered call.
The Python scaffold includes a local `fp_agent_sdk.py` helper that reads JSON
args, constructs a `ToolContext` from those env vars, and serializes handler
results, so plugin authors can start from a handler instead of raw stdin/stdout
plumbing.

```sh
export FP_AGENT_PLUGIN_PATH=$PWD/examples/plugins/echo
dune exec -- fp-agent
> /plugins
> /plugin-doctor
> /plugin-sdk
> /plugin echo_json
> /plugin-new --id local.my-plugin --tool-name my_tool --kind read --template python my-plugin
> /plugin-dev --replace my-plugin
> /plugin-check my-plugin
> /plugin-package --output my-plugin.fp-plugin.tar.gz my-plugin
> /plugin-install --replace my-plugin
> /plugin-smoke --replace my-plugin
> /plugin-run my-plugin my_tool '{"message":"hi"}'
> /plugin-remove local.my-plugin
> /tool echo_json
> /tools
```

Create, test, and install a plugin into the user plugin home:

```sh
dune exec -- fp-agent --new-plugin my-plugin
dune exec -- fp-agent --new-plugin my-plugin --plugin-id com.example.my_plugin
dune exec -- fp-agent --new-plugin my-plugin --plugin-tool-name my_tool
dune exec -- fp-agent --new-plugin my-plugin --plugin-kind exec
dune exec -- fp-agent --new-plugin my-plugin --plugin-template python
dune exec -- fp-agent --dev-plugin my-plugin --replace-plugin
dune exec -- fp-agent --check-plugin my-plugin
dune exec -- fp-agent --smoke-plugin my-plugin
dune exec -- fp-agent
> /plugin-new --id local.my-plugin --tool-name my_tool --kind read --template python my-plugin
> /plugin-dev --replace my-plugin
> /plugin-check my-plugin
> /plugin-install --replace my-plugin
> /plugin-smoke --replace my-plugin
> /plugin-run my-plugin my_tool '{"message":"hi"}'
> /plugin-remove local.my-plugin
dune exec -- fp-agent --doctor-plugins
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
`docs/plugins.md` for the SDK contract. `/plugin-sdk` and `--plugin-sdk` print
the manifest contract, supported SDK version, runtime environment variables,
starter templates, and the shortest local development loop. `/plugin-doctor`
and `--doctor-plugins` show the search roots, install home, invalid manifest
diagnostics, and tool-name conflicts. `/plugins` and `--list-plugins` also
surface invalid manifest diagnostics instead of silently hiding broken plugin
directories, and report tool-name conflicts when a plugin would shadow a built-in
or earlier discovered plugin tool.
Scaffold, package, install, and dev commands print follow-up `/plugin`,
`/tool`, `/plugin-install`, and `/plugin-run` commands when the manifest
includes runnable example args.

### Event-sourced sessions and forking

The event log is the source of truth: the agent's conversation state is a pure
fold over the events (`Session_state.reduce`/`replay`), not a separate mutable
copy. Two consequences:

- **Resume** (`--resume` / `/resume`) reconstructs state by replaying a
  session's log. `/sessions` lists resumable sessions with event counts, plan
  progress, latest turn status, latest workspace snapshot, last user task,
  current-session marker, and fork metadata.
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
  event count, token usage, plan progress, plugin diagnostics, and registered
  tool count.
- **Context preview** (`/context`) replays the current event log and shows the
  conversation turns, compaction count, token totals, agent state, and project
  instruction state that the next task will inherit before the task-specific
  system prompt is prepended.
- **Handoff** (`/handoff`) prints a copyable continuation summary with the
  resume commands, runtime, token usage, current plan, last user task, recent
  events, and workspace diff summary.
- **Workspace snapshot events** are appended after each agent task, recording
  `git status --short` and `git diff --stat` while excluding `.ocaml-agent`.
  The timeline and inspector can therefore show what the last turn changed
  without relying only on a live shell command.
- **Turn completion events** record each agent task's final status, model step
  count, and summary in the event log. This makes the end of a run explicit in
  `/log`, `/inspect`, resume, fork, and handoff-oriented workflows.
- **Project instructions** (`/instructions`) shows the workspace instruction
  files that will be appended to the model system prompt.
- **Session plan** (`/plan`, `/plan-set`, `/plan-add`, `/plan-update`,
  `/plan-clear`) stores a visible todo/doing/done plan as an event-log entry, so
  long tasks can carry an auditable working plan across REPL/TUI resume, fork,
  status, inspect, and log views. The model can also call the built-in
  `update_plan` tool, so automatic long-running tasks update the same
  event-sourced plan instead of keeping progress only in hidden reasoning.
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
  `/models`, `/providers`, `/model`, `/usage`, `/status`, `/context`,
  `/handoff`, `/instructions`, `/plan`, `/diff`, `/log`, and `/inspect` render
  their results directly inside the fullscreen view.
- **Plugin local runs** (`/plugin-run <dir> <tool> <json|@file>`) execute one
  plugin tool with inline JSON or a JSON args file from inside the REPL or
  fullscreen TUI, using the same validation and workspace guard as
  `--run-plugin-tool`.
- **Plugin smoke checks** (`/plugin-smoke [--replace] <dir>`) validate a
  plugin and run `examples/<tool>.args.json` plus any
  `examples/<tool>/*.json` case files from inside the REPL or fullscreen TUI,
  so plugin developers can keep the current agent session open while testing
  SDK changes.
- **Plugin install management** (`/plugin-new [--id ID] [--tool-name NAME]
  [--kind KIND] [--template NAME] <dir>`, `/plugin-dev [--replace] <dir>`,
  `/plugin-check <dir|package>`, `/plugin-package [--output FILE] <dir>`,
  `/plugin-install [--replace] <dir|package>`, `/plugin-remove <id>`)
  scaffolds, validates, smoke-tests, packages, installs, and removes plugins
  from the REPL or fullscreen TUI, then reloads the in-process tool registry so
  `/tools`, the status strip, and later model calls see the updated plugin set.
- **Plugin SDK discovery** (`/plugin-sdk`, `--plugin-sdk`) lists the supported
  manifest SDK version, scaffold templates, runtime environment variables, and
  next commands for plugin authors.
- **TUI model/provider switching** lets `/model <id>`, `/model-next`, and
  `/provider <name> [model] [api-base]` change the runtime used by later TUI
  task submissions.
- **Provider discovery** (`/providers`) shows every built-in and custom
  provider profile with protocol, API base, auth hint, models, and the active
  model without exposing API keys.
- **TUI session navigation** lets `/new`, `/resume <dir>`, and `/fork [index]`
  switch the active fullscreen session and continue writing to the selected
  event log.
- **TUI undo** lets `/undo` restore the git worktree to the checkpoint captured
  before the previous submitted TUI task, while leaving `.ocaml-agent` session
  logs alone.
- **TUI retry** lets `/retry` rerun the latest user task in the active
  event-sourced session, using the currently selected provider/model runtime.
- **TUI review** lets `/review [focus]` run the code-review workflow from the
  current fullscreen session, triggering diff-first review guidance and
  preflight while preserving the review request in the event log.
- **TUI compaction** lets `/compact` manually summarize older context in the
  active session when a long run needs a smaller model-visible history.
- **TUI prompt editor** keeps multiline draft editing pure and testable:
  inserted text, seeded palette drafts, newline, delete/backspace, cursor
  movement, and rendering with a visible cursor all show inside the fullscreen
  view.
- **TUI prompt history** records submitted prompts in the fullscreen shell.
  `Ctrl+Up` and `Ctrl+Down` browse previous prompts without stealing plain
  Up/Down from event inspection. Resumed and forked sessions seed this history
  from the event log, skipping agent-internal retry/preflight messages.
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
- `--add-provider NAME --provider-base URL --provider-model ID` — save a
  custom provider profile, then exit; repeat `--provider-model` or pass
  comma-separated ids
- `--provider-api-key KEY` — API key literal or `env:NAME` reference for
  `--add-provider`
- `--provider-local-compat` — OpenAI-compatible local-server defaults for
  `--add-provider` (`max_tokens`, no developer role, no streaming usage)
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
- `--plugin-kind KIND` / `--tool-kind KIND` — initial tool kind for
  `--new-plugin`: `read`, `write`, or `exec`
- `--plugin-template NAME` / `--template NAME` — initial scaffold template for
  `--new-plugin`: `shell` or `python`
- `--check-plugin DIR|PACKAGE` — validate a plugin directory or package and
  show install-time package metadata, then exit
- `--smoke-plugin DIR` — validate a plugin directory and run each tool with
  `examples/<tool>.args.json` plus `examples/<tool>/*.json` cases
- `--dev-plugin DIR` / `--plugin-dev DIR` — validate, smoke-test, install,
  refresh, and print plugin/tool inspection next steps, then exit
- `--package-plugin DIR` / `--plugin-package DIR` — validate, smoke-test, and
  create a distributable `.fp-plugin.tar.gz` package
- `--plugin-package-output FILE` — output path for `--package-plugin`
- `--replace-plugin` with `--check-plugin` — validate replacement
  compatibility against the currently installed plugin with the same id
- `--run-plugin-tool DIR --plugin-tool NAME --plugin-args JSON` — run a plugin
  tool locally for development, then exit
- `--plugin-args-file FILE` — read local plugin tool JSON args from a file
- `--install-plugin DIR|PACKAGE` — validate and install a plugin directory or
  `.fp-plugin.tar.gz` package, then exit
- `--replace-plugin` — allow `--install-plugin` or `--dev-plugin` to replace an
  existing installed plugin after staging the new copy
- `--list-plugins` — list plugins installed in the plugin home, then exit
- `--doctor-plugins` / `--plugin-doctor` — show plugin discovery/install
  diagnostics, then exit
- `--plugin-sdk` / `--plugin-templates` — show the plugin SDK contract,
  scaffold templates, runtime env vars, and workflow, then exit
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

```sh
dune exec -- fp-agent --add-provider local-llm \
  --provider-base http://101.132.142.56:18080/v1 \
  --provider-model qwen36-rtx \
  --provider-api-key dummy \
  --provider-local-compat \
  --provider-max-tokens 8192

# or inside the REPL/TUI:
> /provider-add local-llm http://101.132.142.56:18080/v1 qwen36-rtx --api-key dummy --local-compat --max-tokens 8192
> /provider local-llm qwen36-rtx
```

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
custom providers from these files, and `/providers` adds protocol, API base, and
auth hints without printing API keys. `/model <id>` switches to the uniquely
matching configured provider/model, while `/provider <name> <model>` keeps an
explicit provider override available.

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
| `agent_loop` | The model↔tool loop, including eventful meta-tools such as `update_plan` |
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
