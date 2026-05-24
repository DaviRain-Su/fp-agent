# PRD: OCaml Code Agent Harness MVP

## 1. Introduction / Overview

本项目目标是使用 **OCaml** 实现一个最小可用的 **Code Agent Harness**。

第一版聚焦于本地 CLI 场景：用户通过命令行输入开发任务，Agent 可以读取代码、编辑文件、运行命令、观察结果，并循环推进任务，最终输出总结。

本项目的核心价值不是立即实现最强 Agent，而是建立一个类型安全、可审计、可恢复、可扩展的 Agent Harness 工程骨架。

---

## 2. Goals

- 使用 OCaml 构建一个本地 CLI Code Agent。
- 建立类型安全的 tool call 系统。
- 建立明确的 agent state machine。
- 支持基础文件操作和 shell 命令执行。
- 支持结构化 LLM 输出解析。
- 支持 event log，记录完整执行轨迹。
- 为后续扩展 planner、multi-agent、sandbox、TUI 打基础。

---

## 3. Quality Gates

These commands must pass for every user story:

- `dune build` - 编译通过(前置门禁)
- `dune fmt` - OCaml 代码格式检查
- `dune runtest` - 单元测试与 expect test

---

## 4. User Stories

### US-001: 初始化 OCaml 项目骨架

**Description:** As a developer, I want a standard OCaml project scaffold so that the agent harness can be built, tested, and extended consistently.

**Acceptance Criteria:**

- [ ] 项目使用 `dune` 作为构建系统。
- [ ] 项目包含 `bin/`、`lib/`、`test/` 目录。
- [ ] 项目依赖 `base` 或 `core`。
- [ ] 可以运行 CLI 入口程序。
- [ ] `dune runtest` 可以成功执行。

---

### US-002: 定义 Typed Tool Call 系统

**Description:** As a harness engineer, I want all agent tools represented as typed OCaml variants so that invalid tool calls can be rejected before execution.

**Acceptance Criteria:**

- [ ] 定义 `Tool_call.t` 类型。
- [ ] 支持 `Read_file`、`Write_file`、`Edit_file`、`Run_command`、`List_files`。
- [ ] 每种 tool call 都有明确字段。
- [ ] 支持 tool call 的 JSON encode/decode。
- [ ] 无效 JSON tool call 会返回结构化错误。

---

### US-003: 实现基础 Tool Runner

**Description:** As a code agent, I want to execute approved tools so that I can inspect and modify a local repository.

**Acceptance Criteria:**

- [ ] `Read_file` 可以读取 workspace 内文件。
- [ ] `Write_file` 可以写入 workspace 内文件。
- [ ] `Edit_file` 使用 exact old/new text replacement。
- [ ] `Run_command` 可以执行 shell 命令并捕获 stdout/stderr/exit code。
- [ ] `List_files` 可以列出目录内容。
- [ ] tool runner 返回统一的 `Tool_result.t`。

---

### US-004: 添加 Workspace 边界控制

**Description:** As a harness engineer, I want all file operations constrained to a workspace so that the agent cannot accidentally modify files outside the project.

**Acceptance Criteria:**

- [ ] 所有文件路径都基于 workspace root 解析。
- [ ] 禁止 `../` 逃逸 workspace。
- [ ] 禁止直接修改 `.git/` 目录。
- [ ] 非法路径返回明确错误。
- [ ] workspace root 可以通过 CLI 参数或当前目录设置。

---

### US-005: 实现 Agent State Machine

**Description:** As a harness engineer, I want the agent loop represented as a typed state machine so that execution flow is explicit and testable.

**Acceptance Criteria:**

- [ ] 定义 `Agent_state.t`。
- [ ] 至少包含 `Initializing`、`Waiting_for_model`、`Executing_tool`、`Observing_result`、`Completed`、`Failed` 状态。
- [ ] 状态转换通过明确函数完成。
- [ ] 非法状态转换可以被测试覆盖。
- [ ] agent loop 有最大步数限制，避免无限循环。

---

### US-006: 集成 LLM Model Adapter

**Description:** As a code agent, I want to call an OpenAI-compatible model API so that I can receive structured next actions.

**Acceptance Criteria:**

- [ ] 支持从配置或环境变量读取 API key。
- [ ] 支持 OpenAI-compatible chat completion endpoint。
- [ ] prompt 明确要求模型输出结构化 JSON。
- [ ] 模型输出可以解析为 `Model_action.t`。
- [ ] 支持两类 action：`Tool_call` 和 `Final_answer`。
- [ ] 解析失败时返回可诊断错误。

---

### US-007: 实现 Agent Loop

**Description:** As a user, I want to give the CLI a coding task so that the agent can iteratively call tools and work toward completion.

**Acceptance Criteria:**

- [ ] 用户可以运行类似 `ocaml-agent "fix failing test"` 的命令。
- [ ] agent 会将用户任务发送给模型。
- [ ] agent 可以执行模型请求的 tool call。
- [ ] tool result 会反馈给模型进入下一轮。
- [ ] agent 遇到 `Final_answer` 时结束。
- [ ] agent 达到最大步数时安全退出。

---

### US-008: 添加 Event Log

**Description:** As a harness engineer, I want every agent step recorded so that sessions can be debugged, replayed, and audited.

**Acceptance Criteria:**

- [ ] 每次运行创建 session directory。
- [ ] event log 以 JSONL 格式保存。
- [ ] 记录 user message。
- [ ] 记录 model response。
- [ ] 记录 tool call。
- [ ] 记录 tool result。
- [ ] 记录 state transition。
- [ ] event log 中不得泄露 API key。

---

### US-009: 添加基础 Policy Layer

**Description:** As a harness engineer, I want every tool call checked against policy before execution so that unsafe actions can be blocked.

**Acceptance Criteria:**

- [ ] 定义 `Policy.t` 和 `Permission.t`。
- [ ] 支持 `Allow`、`Deny`、`Ask_user` 三种结果。
- [ ] 文件写入必须限制在 workspace 内。
- [ ] 修改 `.git/` 被拒绝。
- [ ] 明显危险命令，例如 `rm -rf /`，被拒绝。
- [ ] policy decision 被写入 event log。

---

### US-010: 输出 Git Diff 与 Final Summary

**Description:** As a user, I want the agent to summarize its changes so that I can review what happened.

**Acceptance Criteria:**

- [ ] agent 结束时可以显示修改摘要。
- [ ] 如果 workspace 是 git repo，可以显示 `git diff --stat`。
- [ ] final answer 包含已执行的重要步骤。
- [ ] final answer 包含测试或命令执行结果。
- [ ] 如果任务失败，summary 明确说明失败原因。

---

## 5. Functional Requirements

- FR-1: 系统必须提供 CLI 入口，接收用户任务文本。
- FR-2: 系统必须将所有 agent tool call 建模为 OCaml typed variants。
- FR-3: 系统必须支持 JSON 格式的 model action 解析。
- FR-4: 系统必须支持基础文件读取、写入、编辑、目录列举和命令执行。
- FR-5: 系统必须将所有文件操作限制在 workspace root 内。
- FR-6: 系统必须在执行 tool call 前进行 policy check。
- FR-7: 系统必须记录完整 event log。
- FR-8: 系统必须支持最大执行步数限制。
- FR-9: 系统必须在结束时输出 final summary。
- FR-10: 系统必须支持 `dune build`、`dune fmt`、`dune runtest` 作为质量门禁。
- FR-11: 系统必须对 shell 命令施加超时(默认 60s),超时返回结构化错误。
- FR-12: 系统必须对 tool result 文本输出施加大小上限(默认 32 KB),超出部分截断并标注。
- FR-13: 系统必须对二进制文件读取返回明确错误,而非塞入对话。
- FR-14: 系统必须管理多轮对话历史,在接近 context 上限时截断旧消息(保留 system prompt 与最近若干轮)。
- FR-15: 系统在模型输出无法解析为 `Model_action.t` 时,必须将解析错误作为 observation 反馈给模型并重试,重试次数达上限(默认 2 次)后判定失败。
- FR-16: `Model_client` 必须以可注入抽象的形式实现,以便测试时替换为 mock。

---

## 6. Non-Goals / Out of Scope

第一版不包含：

- 多 agent 调度。
- TUI 界面。
- 浏览器自动化。
- Web search。
- 长期记忆系统。
- 向量数据库。
- 自动 git commit。
- 远程 sandbox。
- OxCaml 特性。
- 分布式 worker。
- 完整 planner/reasoner 框架。
- 自动 PR 创建。

---

## 7. Technical Considerations

推荐技术栈（MVP 选型已确定）：

- Language: OCaml 5.x
- Build: Dune
- Standard Library: Base
- CLI: Cmdliner
- JSON: Yojson + ppx_yojson_conv
- Tests: Alcotest / ppx_expect
- Logging: Logs
- HTTP Client: **`cohttp-lwt-unix` + `lwt`**(MVP 确定选型，OCaml-native 标准方案）
- Config: 环境变量（MVP）

### 并发模型

引入 `lwt` 作为异步运行时。仅 `Model_client`(HTTP I/O)和 `Agent_loop`(驱动 I/O)运行在 Lwt 之上；
纯逻辑模块(`Tool_call`、`Workspace`、`Policy`、`Agent_state` 等)保持同步、无 Lwt 依赖，便于测试。
`bin/main.ml` 在入口处用 `Lwt_main.run` 收口。

### 环境变量

| 变量 | 含义 | 默认值 |
| --- | --- | --- |
| `OPENAI_API_KEY` | 模型 API key（必填，缺失则启动即失败） | 无 |
| `OPENAI_API_BASE` | OpenAI-compatible endpoint base | `https://api.openai.com/v1` |
| `MODEL_NAME` | 模型名 | `gpt-4o-mini` |
| `MAX_STEPS` | agent loop 最大步数 | `30` |
| `WORKSPACE_ROOT` | workspace 根目录 | 当前目录（可被 CLI `--workspace` 覆盖） |

### Session 目录

每次运行创建 `.ocaml-agent/sessions/<YYYY-MM-DD-HH-MM-SS>-<short-uuid>/`，
内含 `events.jsonl`。MVP 不做自动清理(数据保留策略：留待后续)。

### 关键限制与超时

| 项 | 默认值 |
| --- | --- |
| shell 命令超时 | 60s |
| tool result 文本上限 | 32 KB（超出截断并标注 `…[truncated]`） |
| model action 解析重试 | 2 次 |
| agent loop 最大步数 | 30（`MAX_STEPS`） |

建议模块结构：

```text
bin/
  main.ml

lib/
  agent_state.ml
  agent_loop.ml
  tool_call.ml
  tool_result.ml
  tool_runner.ml
  model_action.ml
  model_client.ml
  workspace.ml
  policy.ml
  event.ml
  event_log.ml
  session.ml
  shell.ml
  config.ml

test/
  test_tool_call.ml
  test_workspace.ml
  test_policy.ml
  test_agent_state.ml
```

---

## 8. Model Interaction Contract

### 8.1 输出契约

模型每一轮必须输出**单个 JSON 对象**，对应 `Model_action.t` 两类 action 之一。

Tool call：

```json
{
  "action": "tool_call",
  "tool": "read_file",
  "args": { "path": "lib/foo.ml" }
}
```

各工具的 `args` 字段：

| tool | args |
| --- | --- |
| `read_file` | `{ "path": string }` |
| `write_file` | `{ "path": string, "content": string }` |
| `edit_file` | `{ "path": string, "old": string, "new": string }` |
| `run_command` | `{ "command": string }` |
| `list_files` | `{ "path": string }` |

Final answer：

```json
{
  "action": "final_answer",
  "summary": "已修复失败测试，新增边界检查。",
  "details": "...可选的详细说明..."
}
```

### 8.2 System Prompt 契约

system prompt 必须包含：

- agent 角色与目标说明。
- 可用工具清单及其 `args` schema（与 8.1 表一致）。
- 「每轮只输出一个 JSON 对象，不要包裹 markdown 代码块或额外文本」的硬性要求。
- workspace 边界与 policy 约束的简要说明。

### 8.3 Tool Result 回喂格式

tool result 以 `role: "user"`（或等价 observation 角色）的文本消息回喂，结构如下：

```text
TOOL_RESULT tool=read_file ok=true
<stdout / 文件内容 / 列表，超过 32KB 截断并追加 …[truncated]>
```

失败时 `ok=false`，正文为错误原因（包含 policy 拒绝原因、超时、路径非法等）。
`run_command` 额外包含 `exit_code` 与分离的 stdout/stderr。

### 8.4 对话历史管理

- system prompt 始终保留在历史首位。
- 历史按轮次累积；当估算 token 接近上限时，从最旧的非 system 消息开始丢弃，保留最近若干轮与初始用户任务。
- MVP 采用简单字符长度启发式估算，不引入 tokenizer 依赖。

---

## 9. Error Handling & Resilience

- **模型输出畸形**：解析失败 → 将解析错误作为 observation 回喂 → 重试，达上限(默认 2)后 `Failed`。
- **HTTP 失败**：网络/5xx 错误返回结构化错误；MVP 不做自动退避重试(留待后续)，直接进入 `Failed` 并写入 event log。
- **shell 超时**：返回 timeout 错误结果,作为 observation 回喂,loop 继续。
- **命令非零退出**：不视为 harness 错误,仍返回 stdout/stderr/exit_code 供模型判断。
- **缺少 API key**：启动即 fail fast,给出明确指引。
- **达到 max steps**：完成当前步后安全退出,summary 标注「未完成,达步数上限」。
- **非 git repo**：跳过 `git diff --stat`,改用已修改文件列表。

---

## 10. Security Considerations

- **路径穿越**：所有路径 canonicalize 后必须落在 workspace root 内,`../` 逃逸一律拒绝。
- **`.git/` 保护**：读取可放行,写入/编辑一律拒绝。
- **危险命令 deny-list**：`rm -rf /`、`mkfs`、`dd ... of=/dev/...`、`> /dev/sd*`、`curl ... | sh` 等通过正则拒绝。
- **API key 不入日志**：event log 必须从模型请求/响应中剥离 Authorization 头与 key。
- **环境变量继承风险**：`run_command` 经 `/bin/sh -c` 执行会继承当前进程环境(含 `OPENAI_API_KEY`)。MVP 记录此风险,后续可考虑环境清洗;不在第一版做沙箱隔离。

---

## 11. Observability

- 使用 `Logs` 库,默认级别 `Info`,可经 `--verbose`/env 提升到 `Debug`。
- 级别约定:`Error` = harness 故障;`Warning` = policy 拒绝/超时/截断;`Info` = state 转换与每步摘要;`Debug` = 完整请求/响应(脱敏后)。
- event log(`events.jsonl`)是结构化审计真相来源,与人类可读 `Logs` 输出分离。
- event log 每条记录带 `schema_version` 字段,为未来 replay 预留稳定格式(MVP 不实现 replay 引擎,但格式向前兼容)。

---

## 12. Success Metrics

- 可以通过 CLI 启动 agent。
- agent 可以完成一个简单代码任务，例如修改 README 或修复小型测试。
- 所有 tool call 都经过 typed decode 和 policy check。
- 所有执行步骤都被写入 event log。
- `dune build`、`dune fmt`、`dune runtest` 通过。
- 代码结构足够清晰，后续可以扩展 planner、TUI、sandbox、multi-agent。

---

## 13. Resolved Decisions

原 Open Questions 已在规划阶段定案：

- **LLM API**：使用 OpenAI-compatible chat completion endpoint(`OPENAI_API_BASE` 可指向任意兼容服务)。
- **HTTP client**：`cohttp-lwt-unix` + `lwt`(OCaml-native 标准方案)。
- **`Edit_file`**：仅支持 exact old/new text replacement;无匹配报错,多匹配替换第一处。不支持 unified diff patch。
- **resume session**：MVP 不支持,留待后续。
- **event log 可 replay 格式**：MVP 不实现 replay 引擎,但 `events.jsonl` 每条带 `schema_version`,格式向前兼容,为未来 replay 预留。

---

## 14. Post-MVP Implementation Status

> 本节记录 MVP 之后实际落地、**超出或修正了上面 §6 Non-Goals / §13 决策**的内容。上面的章节保留为原始规划,本节为当前事实来源。

已实现(超出原 MVP 范围):

- **多 provider**:`kimi`(默认,Kimi for coding,**Anthropic Messages 协议**)、`deepseek`(`deepseek-v4-flash`/`-pro`,OpenAI 协议)、`zhipu`(GLM,OpenAI 协议)。原计划仅"OpenAI-compatible";现按 provider 选择 key 环境变量、base URL、默认模型与**协议**(`lib/provider.ml`)。
- **交互式 REPL**:无任务参数即进入,跨轮保留上下文;meta 命令 `/help`、`/tools`、`/sessions`、`/resume`、`/diff`、`/undo`、`/exit`。
- **会话恢复(原 §13 标注"不支持")**:`--resume <session_dir>` 及 REPL `/resume`,经 `lib/transcript.ml` 从 event log 重建对话历史。
- **TUI(原 §6 Non-Goal)**:`--tui` 全屏实时视图(notty),含动态 spinner 等待状态;纯逻辑抽到 `lib/view.ml` 并有单测。
- **新增工具**:`search`(工作区文本搜索)、`make_dir`、`apply_patch`(git apply 应用 unified diff——修正 §13 "不支持 patch")、`multi_edit`(多处编辑原子应用)。`edit_file` 仍为 exact replacement。
- **人工审批**:`--confirm` 对写操作/命令逐项 stdin 征询(对应 `Permission.Ask_user` 落地)。
- **YOLO 模式**:`--yolo` 绕过危险命令 deny-list(仍保留 workspace 边界)。
- **policy 决策审计**:`Event.Policy_decision` 写入 event log(US-009 验收点)。
- **实时进度**:`on_event` 回调 + 并发 spinner,等模型/跑工具时显示动态状态。
- **shell 环境收紧**:`run_command` 仍通过 `/bin/sh -c` 执行,但子进程环境会移除 `*_API_KEY`、`*_TOKEN`、`*_SECRET`、`*_PASSWORD` 等疑似密钥变量;这不是 OS sandbox,命令输出仍会进入 observation/event log。
- **`apply_patch` policy 预检**:执行 `git apply` 前从 unified diff/git diff header 提取路径并套用 workspace / `.git` 写入边界。
- **`--confirm --tui` 显式拒绝**:避免全屏 TUI 模式静默绕过人工审批。

仍为 Non-Goal(未实现):多 agent 调度、浏览器自动化、Web search、长期记忆/向量数据库、自动 git commit、远程 sandbox、OxCaml 特性、event log replay 引擎。
