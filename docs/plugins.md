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
subset is intentionally small and portable: `type`, `required`, object
`properties`, and array `items`. Supported types are `object`, `array`,
`string`, `number`, `integer`, `boolean`, and `null`; unsupported schema
keywords are ignored. If validation fails, the tool returns a schema validation
error and the plugin command is not executed.

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
/plugin echo_json
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

`/plugin <plugin-id|tool-name>` prints the manifest details for one plugin:
directory, version, tool kind, command, timeout, and input schema.

`/tool <tool-name>` prints the registered tool descriptor exactly as the model
sees it: kind, description, and input schema. This works for both built-in and
plugin tools.

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
instead of `hello_world`:

```sh
dune exec -- fp-agent --new-plugin my-plugin --plugin-tool-name my_tool
```

The scaffold includes `fp-agent-plugin.json`, `hello.sh`, a README with the
local development commands, and `examples/<tool>.args.json` for a first
`--run-plugin-tool` smoke test.

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

You can also keep reusable smoke-test inputs in files. The convention is
`examples/<tool>.args.json`, and `--smoke-plugin` runs every tool with its
matching file:

```sh
dune exec -- fp-agent --smoke-plugin my-plugin

dune exec -- fp-agent --run-plugin-tool my-plugin \
  --plugin-tool hello_world \
  --plugin-args-file my-plugin/examples/hello_world.args.json
```

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
