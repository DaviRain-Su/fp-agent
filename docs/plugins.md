# fp-agent Plugin SDK

Plugins let third-party developers add tools without recompiling `fp-agent`.
A plugin is a directory with a `fp-agent-plugin.json` manifest and any scripts
or binaries it needs.

## Manifest

```json
{
  "id": "com.example.echo",
  "name": "Echo Tools",
  "version": "0.1.0",
  "sdk_version": 1,
  "tools": [
    {
      "name": "echo_json",
      "kind": "read",
      "description": "Echoes the JSON args it receives",
      "command": "sh echo.sh",
      "input_schema": {
        "type": "object",
        "properties": {
          "message": { "type": "string" }
        },
        "required": ["message"]
      }
    }
  ]
}
```

Tool names must be unique within a manifest and use letters, digits, `_`, or
`-`; this keeps them compatible with native tool-calling APIs. `kind` is one of
`read`, `write`, or `exec` and drives approval policy in `--confirm` mode.
Optional `timeout`, `timeoutSec`, or `timeout_sec` values must be positive
seconds.

`sdk_version` declares the fp-agent plugin manifest contract version. It
defaults to `1` for older plugins, and `fp-agent --check-plugin` / install /
local run commands reject versions newer than the current binary supports. The
accepted aliases are `sdkVersion`, `api_version`, and `apiVersion`.

## Runtime Contract

When the model calls a plugin tool, `fp-agent`:

1. Runs the tool command from the plugin directory.
2. Sends the tool call args as JSON on stdin.
3. Exposes:
   - `FP_AGENT_WORKSPACE`
   - `FP_AGENT_PLUGIN_DIR`
   - `FP_AGENT_PLUGIN_ID`
   - `FP_AGENT_PLUGIN_NAME`
   - `FP_AGENT_PLUGIN_VERSION`
   - `FP_AGENT_PLUGIN_SDK_VERSION`
   - `FP_AGENT_TOOL_NAME`
   - `FP_AGENT_TOOL_KIND`
   - `FP_AGENT_ARGS_FILE`
4. Treats stdout as the tool result when the command exits `0`.
5. Treats non-zero exit as a tool error.

`FP_AGENT_ARGS_FILE` points at the temporary JSON args file that is also piped
to stdin. It exists for plugin SDKs or scripts that prefer reading a file path
over consuming stdin directly.

Before the command starts, `input_schema` is validated locally. The supported
subset is intentionally small and portable: `type`, `enum`, `required`, object
`properties`, object `additionalProperties`, and array `items`. Supported types
are `object`, `array`, `string`, `number`, `integer`, `boolean`, and `null`;
unsupported schema keywords are ignored. Set `additionalProperties` to `false`
when undeclared object fields should be rejected, or to a schema object when
extra fields should share one validation rule. If validation fails, the tool
returns a schema validation error and the plugin command is not executed.

Plugin tools are still bounded by `fp-agent` policy. If a plugin call includes a
`path` arg, `read` tools must resolve inside the workspace and `write` tools
must pass the workspace write guard.

## Discovery

At startup and before tool execution, `fp-agent` discovers plugins from:

- `FP_AGENT_PLUGIN_PATH` (colon-separated plugin dirs or parent dirs)
- `.fp-agent/plugins`
- `FP_AGENT_PLUGIN_HOME`, or `~/.local/share/fp-agent/plugins`

Use the REPL command:

```text
/plugins
/plugin-doctor
/plugin echo_json
/plugin-new --id com.example.echo --tool-name echo_json --kind read my-plugin
/plugin-dev --replace my-plugin
/plugin-check my-plugin
/plugin-install --replace my-plugin
/plugin-smoke --replace my-plugin
/plugin-run my-plugin echo_json '{"message":"hi"}'
/plugin-remove com.example.echo
/tool echo_json
/tools
```

`/plugins` reports both loaded plugins and invalid manifests discovered on the
same search path. Invalid plugins are not registered as tools, but their
directory and validation error are shown so extension developers can fix a bad
install without guessing why a tool disappeared. It also reports tool-name
conflicts when a plugin tries to reuse a built-in tool name or a name already
claimed by an earlier discovered plugin; the conflicting plugin tool is skipped
while the rest of the valid plugin remains visible.

`/plugin-doctor` prints the effective plugin search roots, resolved install
home, valid/invalid plugin counts, tool-name conflict count, invalid manifest
details, and next inspection commands. Use it when a plugin was installed but a
tool is missing from `/tools`.

`/plugin <plugin-id|tool-name>` prints the manifest details for one plugin:
directory, version, tool kind, command, timeout, and input schema.

`/tool <tool-name>` prints the registered tool descriptor exactly as the model
sees it: kind, description, and input schema. This works for both built-in and
plugin tools.

`/plugin-smoke [--replace] <dir>` validates a plugin directory and runs every
tool against `examples/<tool>.args.json` plus any sorted
`examples/<tool>/*.json` case files without leaving the current REPL or
fullscreen TUI session.

`/plugin-run <dir> <tool> <json|@file>` runs one plugin tool locally from the
current REPL or fullscreen TUI. Inline JSON is accepted directly, and `@file`
reads JSON args from disk:

```text
/plugin-run my-plugin hello_world @my-plugin/examples/hello_world.args.json
```

`/plugin-dev [--replace] <dir>` runs the normal local development loop in one
step: validate the manifest, run smoke examples, install the plugin, refresh the
tool registry, and print the next `/plugin` and `/tool` inspection commands.

`/plugin-new [--id ID] [--tool-name NAME] [--kind KIND] <dir>`,
`/plugin-check [--replace] <dir>`, `/plugin-install [--replace] <dir>`, and
`/plugin-remove <id>` expose the same workflow as individual steps inside a live
REPL or fullscreen TUI session. Install/remove commands reload the in-process
tool registry, so `/tools` and later model calls see the updated plugin set.

## Install

Create a starter plugin:

```sh
dune exec -- fp-agent --new-plugin my-plugin
```

Use `--plugin-id` when you know the final manifest id up front:

```sh
dune exec -- fp-agent --new-plugin my-plugin --plugin-id com.example.my_plugin
```

Use `--plugin-tool-name` when the starter should scaffold the real first tool
instead of `hello_world`, and `--plugin-kind` / `--tool-kind` when that tool
should start as `read`, `write`, or `exec`:

```sh
dune exec -- fp-agent --new-plugin my-plugin --plugin-tool-name my_tool
dune exec -- fp-agent --new-plugin my-plugin --plugin-kind exec
```

The scaffold includes `fp-agent-plugin.json`, `hello.sh`, a README with the
local development commands, and `examples/<tool>.args.json` for a first
`--run-plugin-tool` smoke test. Add more JSON files under `examples/<tool>/` to
run multiple smoke cases for the same tool.

Validate it before installing:

```sh
dune exec -- fp-agent --check-plugin my-plugin
```

Validation rejects tool names that would be hidden by built-in tools or by
already discovered plugins. When validating an update to an installed plugin,
use the same replacement flag as install:

```sh
dune exec -- fp-agent --check-plugin my-plugin --replace-plugin
```

Run one tool locally while developing, without calling a model:

```sh
dune exec -- fp-agent --run-plugin-tool my-plugin \
  --plugin-tool hello_world \
  --plugin-args '{"message":"hi"}'
```

You can also keep reusable smoke-test inputs in files. The default convention is
`examples/<tool>.args.json`, and additional cases can live under
`examples/<tool>/*.json`. `--smoke-plugin` runs every matching file for each
tool:

```sh
dune exec -- fp-agent --smoke-plugin my-plugin
dune exec -- fp-agent --dev-plugin my-plugin --replace-plugin
dune exec -- fp-agent
> /plugin-new --id com.example.echo --tool-name echo_json --kind read my-plugin
> /plugin-dev --replace my-plugin
> /plugin-check my-plugin
> /plugin-install --replace my-plugin
> /plugin-smoke --replace my-plugin
> /plugin-run my-plugin echo_json '{"message":"hi"}'
> /plugin-remove com.example.echo

dune exec -- fp-agent --run-plugin-tool my-plugin \
  --plugin-tool hello_world \
  --plugin-args-file my-plugin/examples/hello_world.args.json
```

`--dev-plugin DIR` (alias `--plugin-dev DIR`) runs the same one-step development
loop without opening the REPL: validate, smoke-test, install, refresh, and print
the next plugin/tool inspection commands. Add `--replace-plugin` when iterating
on an already installed plugin. Scaffold, install, and dev commands also print a
`/plugin-run` next step when `examples/<tool>.args.json` is present, so the
generated plugin can be exercised immediately.

The command loads the manifest, validates the JSON args, runs the tool from the
plugin directory, and prints stdout. It uses `--workspace` or `WORKSPACE_ROOT`
for `FP_AGENT_WORKSPACE` and applies the same path guard used by plugin tools in
agent runs.

Install a local plugin directory:

```sh
dune exec -- fp-agent --install-plugin examples/plugins/echo
```

The installer validates the manifest and copies the plugin into the plugin
home. It does not overwrite an existing plugin with the same id, and it rejects
tool-name conflicts before copying.

During plugin development, reinstall a changed plugin with:

```sh
dune exec -- fp-agent --install-plugin examples/plugins/echo --replace-plugin
```

Replacement is staged before the old installation is removed, so invalid
manifests or copy failures do not delete the existing installed plugin.

List installed plugins:

```sh
dune exec -- fp-agent --list-plugins
```

Inspect discovery and install diagnostics:

```sh
dune exec -- fp-agent --doctor-plugins
```

The list command also reports invalid installed manifests under
`Invalid installed plugins:` while continuing to show any valid installed
plugins. Installed plugin tool-name conflicts are reported under
`Plugin tool conflicts:`.

Remove an installed plugin by id:

```sh
dune exec -- fp-agent --remove-plugin com.example.echo
```

Removal is scoped to the plugin home. It does not delete development plugin
directories referenced by `FP_AGENT_PLUGIN_PATH`.

For tests or local development, use:

```sh
export FP_AGENT_PLUGIN_PATH=$PWD/examples/plugins/echo
dune exec -- fp-agent
```
