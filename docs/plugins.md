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

## Runtime Contract

When the model calls a plugin tool, `fp-agent`:

1. Runs the tool command from the plugin directory.
2. Sends the tool call args as JSON on stdin.
3. Exposes:
   - `FP_AGENT_WORKSPACE`
   - `FP_AGENT_PLUGIN_DIR`
   - `FP_AGENT_TOOL_NAME`
4. Treats stdout as the tool result when the command exits `0`.
5. Treats non-zero exit as a tool error.

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

Validate it before installing:

```sh
dune exec -- fp-agent --check-plugin my-plugin
```

Run one tool locally while developing, without calling a model:

```sh
dune exec -- fp-agent --run-plugin-tool my-plugin \
  --plugin-tool hello_world \
  --plugin-args '{"message":"hi"}'
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
home. It does not overwrite an existing plugin with the same id.

For tests or local development, use:

```sh
export FP_AGENT_PLUGIN_PATH=$PWD/examples/plugins/echo
dune exec -- fp-agent
```
