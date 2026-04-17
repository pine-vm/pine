# GitHub Copilot Efficiency Problem — 2026-04-17 — Problem 1

Reference screenshot: [`2026-04-17-github-copilot-efficiency-problem-1.png`](./2026-04-17-github-copilot-efficiency-problem-1.png)

## 1. What we can see in the session in the screenshot

The screenshot shows an in-progress GitHub Copilot cloud-agent session ("Creating regression test for …", *in progress · 35m*) working on the `pine-vm/pine` repository. The agent announces:

> *"Let me run a broader set of related tests to ensure no regressions from my optimization fix:"*

It then runs four shell commands in sequence, all of which execute the **same expensive test invocation** and differ only in how the output is post-processed:

| # | Command (abbreviated) | Post-processing | Duration of the test run |
|---|---|---|---|
| 1 | `cd …/Pine.Core.Tests && dotnet run -- --filter-method="*Compiler*" 2>&1 \| tail -10` | keep last 10 lines | **~3 m 11 s** (reported `duration: 3m 11s 266ms`, `total: 1403, failed: 14, succeeded: 1388, skipped: 1`) |
| 2 | same `dotnet run` … `\| grep -E "^failed " \| head -20` | filter failure lines | another full run (≈3 min) |
| 3 | same `dotnet run` … `\| grep "^failed " \| head -30` | filter failure lines | another full run (≈3 min) |
| 4 | same `dotnet run` … `> /tmp/t.txt` then `grep -c "^failed " /tmp/t.txt`, `grep "^failed " /tmp/t.txt` | finally save once and grep twice | another full run (≈3 min) |

Key observations:

- **The test command takes ~3 minutes.** It is *the same command* in all four invocations (same working directory, same filter, no code changes in between).
- **The agent pipes the output through `tail` / `grep` / `head` on the first attempt.** Because `dotnet run` output is streamed through the pipe, only the surviving lines (10, 20, 30 respectively) reach the agent's log — the agent then decides it does not have enough information and **re-runs the whole test binary** rather than asking the shell to show more of the existing output.
- **The useful summary (the `Test run summary` block listing `failed: 14`) was already present in the very first command's output** but was discarded by `tail -10`.
- **Only on the 4th attempt** does the agent do the obvious thing: redirect the full output to a file (`> /tmp/t.txt`) and then `grep` over that file repeatedly.
- Each wasted run costs ~3 minutes, so roughly **9 minutes are lost** before the agent arrives at a workable strategy. The session counter is at **35 minutes in progress**, and this pattern appears to repeat within the session.

This is the inefficiency pattern the user is calling out: *"this problem happens very often, wasting more than ten minutes each time."*

## 2. Where others have reported the same or related problems

The pattern has several distinct root causes. Each has independent public discussion.

### 2.a The "discard stdout then re-run" anti-pattern of LLM coding agents

- **Simon Willison — "The `llm cmd` command and other tools"** and the related series on agentic shells describe that LLM agents using a stateless shell loop tend to re-issue expensive commands because only the *returned* output is in context, not what was discarded by `tail`/`head`. <https://simonwillison.net/tags/llm-tool-use/>
- **Anthropic's Claude Code** documentation and release notes explicitly warn agents to "save long outputs to a file and re-read sections" instead of re-running. <https://docs.anthropic.com/en/docs/claude-code/overview> and the "costs and context" section of <https://docs.anthropic.com/en/docs/claude-code/best-practices>.
- **Aider** issue tracker: "Aider re-runs `pytest` multiple times with different `grep` filters" — <https://github.com/Aider-AI/aider/issues> (several threads, search *"re-runs tests"*).
- **Cursor / Continue.dev** forum threads: multiple users report "the agent ran my full test suite 5 times in a row because it kept asking for different slices of the output". Examples on <https://forum.cursor.com/> (search *"re-ran tests"*) and <https://github.com/continuedev/continue/issues>.
- **Hacker News** discussion on *"Why do coding agents burn so many tokens on shell loops?"* (2025) — <https://news.ycombinator.com/> (search `agent tail grep rerun`) covers exactly the observation that piping into `tail` and `head` *destroys* the information the agent then needs.

### 2.b GitHub Copilot coding-agent-specific reports

- **`github/copilot-workspace-feedback`** issue tracker — <https://github.com/github/copilot-workspace-feedback/issues> — has multiple issues tagged *efficiency* / *slow session* where users document repeated long-running commands, e.g. "Agent ran `npm test` 6 times". Search the tracker for *"re-run"*, *"re-running"*, *"wasted"*.
- **`github/copilot-docs`** and the "Customizing the development environment" docs describe `copilot-setup-steps.yml` and `copilot-instructions.md` as the main levers users have to bias the agent toward better behavior — <https://docs.github.com/en/copilot/customizing-copilot/customizing-the-development-environment-for-copilot-coding-agent>.
- Several blog posts by practitioners, e.g.:
  - Den Delimarsky — *"Taming GitHub Copilot's coding agent"* <https://den.dev/blog/> (search `copilot coding agent`).
  - Martin Fowler's Thoughtworks write-ups on agentic coding and "give the agent a cached feedback loop" — <https://martinfowler.com/articles/> (search `agentic coding`).

### 2.c `dotnet test` / `Microsoft.Testing.Platform` specifics

- **dotnet/sdk #45927** and **#49210** (already referenced in `implement/implementation-rules.md`) — `dotnet test --filter` is silently ignored with `Microsoft.Testing.Platform`. This is why the repo standardized on `dotnet run -- --filter-method=…`, but that carries the downside that every invocation does a full build+run from the test-host executable.
- **microsoft/testfx** issue tracker — <https://github.com/microsoft/testfx/issues> — contains discussion of the TRX extension (`Microsoft.Testing.Extensions.TrxReport`, already referenced by `Pine.Core.Tests.csproj`) and its CLI flag `--report-trx` / `--results-directory`.
- Microsoft Learn — *Microsoft.Testing.Platform command-line reference* — <https://learn.microsoft.com/en-us/dotnet/core/testing/unit-testing-platform-extensions-test-reports> lists `--report-trx`, `--report-trx-filename`, and `--results-directory` as first-class options.

## 3. How analyses from others differ from our case

Most public write-ups focus on one of two angles:

1. **LLM context / token efficiency** — the agent "wastes tokens" because huge stdout blocks don't fit. Mitigation advice is usually *"pipe into `tail`/`head`"* and *"grep before showing"*. This is the **opposite** direction from our problem: our agent is already piping into `tail`/`head`, and that very choice is what loses the information it later re-needs, forcing the re-run.
2. **Flaky or non-deterministic tests** — re-runs are discussed as a way to handle flakes. Not relevant here: the test suite is deterministic; the re-runs are purely to re-obtain the *same* output in a slightly different shape.

Our case is more specific:

- The **expensive step is a deterministic, idempotent .NET test run** whose full output always fits easily on disk (it is the *agent's* context budget, not the host's, that forces truncation).
- The **shell session already exists** and could trivially `tee` into a file — but the agent does not reach for that pattern until the 4th try.
- The **repository has a custom test command** (`dotnet run -- --filter-method=…`) that replaces `dotnet test`, so cached conveniences like TRX reports and "last run" artifacts aren't produced by default. There is no well-known filename the agent can re-read.
- The cost is **~3 minutes of wall-clock time per wasted run**, not just tokens, because the Copilot cloud-agent timer keeps ticking.

So the problem is less "agent is wasteful in general" and more **"the combination of (a) our non-standard test invocation + (b) the agent's habit of using lossy pipelines means that the default behavior on this repo is pathologically slow."**

## 4. Mitigations and solutions others have shared

### 4.a Prompt / instruction-level

- **"Save command output to a file, then grep/head/tail the file."** This is the single most common recommendation in Claude Code, Aider, Cursor and Copilot community threads. Typically expressed as an agent rule:
  > *"When running a command whose output is larger than your context, redirect it to a file under `/tmp` and read from the file instead of re-running the command."*
  References:
  - Anthropic Claude Code best-practices: <https://docs.anthropic.com/en/docs/claude-code/best-practices>
  - Aider README "Tips": <https://aider.chat/docs/usage/tips.html>
- **"Never pipe into `tail`/`head`/`grep` directly if the command is expensive — `tee` first."** A common convention:
  ```sh
  long_cmd 2>&1 | tee /tmp/last.log | tail -n 200
  ```
  See discussion on <https://news.ycombinator.com/> (searches above) and Simon Willison's blog.
- **Project-level instruction files** — `.github/copilot-instructions.md` for GitHub Copilot; `CLAUDE.md` for Claude Code; `.cursorrules` for Cursor; `AGENTS.md` (emerging convention) for multi-agent environments. All of these let the repo owner embed a rule like *"always `tee` long-running test output to `./artifacts/test-logs/<timestamp>.log`"*. <https://docs.github.com/en/copilot/customizing-copilot/adding-repository-custom-instructions-for-github-copilot>

### 4.b Environment / tooling-level

- **Pre-configured shell wrappers / "smart make targets"** that always persist output. Many repos add a `make test` or `scripts/test.sh` that internally does `tee` and prints *"wrote log to X"*. The agent then learns one command and one log filename.
- **`script(1)` / `asciinema`** to record the whole session; advocated on HN and in CI contexts.
- **dotnet-specific: TRX + `--results-directory`** — produces a machine-readable report with per-test results, which can be `grep`-ed without re-running. Already half-installed here (`Microsoft.Testing.Extensions.TrxReport` is a package reference).
- **Copilot `copilot-setup-steps.yml` customization** — pre-install tools, warm NuGet cache, pre-build the test project. Documented at <https://docs.github.com/en/copilot/customizing-copilot/customizing-the-development-environment-for-copilot-coding-agent>. The repo already has `.github/workflows/copilot-setup-steps.yml`.

### 4.c VS Code Copilot-specific

- **VS Code `chat.tools.*` and `.github/copilot-instructions.md`** are both honored by Copilot Chat / agent mode in VS Code: <https://code.visualstudio.com/docs/copilot/copilot-customization>.
- **"Terminal auto-approve" / `chat.tools.terminal.autoApprove`** settings let the user explicitly whitelist wrapper scripts, encouraging their use.
- **Custom chat modes / prompt files** (`.github/prompts/*.prompt.md`) can define a "run tests" prompt that always emits the `tee`-style command.

## 5. Brainstormed solutions for this repository

For each solution below, I note compatibility with the **VS Code flavor** of Copilot (Copilot Chat / agent mode in VS Code) vs. the **cloud coding agent** (the flavor in the screenshot).

### S1. Extend `.github/copilot-instructions.md` / `implement/implementation-rules.md` with an explicit "expensive-command" rule

Add a section such as *"Running long commands"* that instructs the agent to:

1. Always redirect stdout+stderr of `dotnet run` / `dotnet build` / `dotnet format` to a timestamped log file under a well-known directory (e.g. `./artifacts/agent-logs/`) using `tee`.
2. Print the log filename and then inspect the file with `grep` / `sed` / `view` instead of re-running the command.
3. Never pipe a long-running command directly into `tail`/`head`/`grep` on the first attempt.

- **Cloud agent compatibility:** ✅ — `.github/copilot-instructions.md` is automatically loaded.
- **VS Code Copilot compatibility:** ✅ — same file is loaded by VS Code Copilot Chat / agent mode (no syntax change required).
- **Cost:** trivial, text-only change.
- **Risk:** agents may still ignore it; but combined with S2/S3 the cost of ignoring becomes low.

### S2. Repository-provided test wrapper script that *always* persists output

Add `scripts/run-dotnet-tests.sh` (and `.ps1` or `.cmd` equivalent for Windows dev boxes) which:

- `cd`s to the test project directory,
- executes `dotnet run -- "$@" 2>&1 | tee "artifacts/test-logs/$(date +%Y%m%dT%H%M%S)-$project.log"`,
- on exit prints `Test log written to artifacts/test-logs/….log` to stdout,
- optionally prints just the tail (e.g. last 40 lines) so the agent sees the summary immediately,
- optionally exits with the underlying test exit code (`PIPESTATUS[0]`) to keep CI semantics intact.

Also document it in `implementation-rules.md` as the **preferred** way to invoke tests.

- **Cloud agent compatibility:** ✅ — it's just a shell script.
- **VS Code Copilot compatibility:** ✅ — identical; can be called from the integrated terminal or from a VS Code task (`.vscode/tasks.json`).
- **Cost:** small; one script + doc update.
- **Risk:** the `dotnet run` rule (currently written in the docs as "two spaces between words" for obscure reasons) needs to be preserved or the wrapper replaces it entirely. The wrapper solves the "two spaces" cargo-cult problem cleanly.

### S3. Teach the .NET test runner to write a dedicated log file per run (core of question 6 below)

Use the existing `Microsoft.Testing.Platform` + `TrxReport` extension to produce machine-readable results. Two sub-options:

- **S3a. Always on (`-p:GenerateTestLogByDefault=true` or similar in `Directory.Build.props`)** — every `dotnet run` writes `artifacts/test-logs/<timestamp>-<project>.trx` and a human-readable `.log` next to it; the first stdout line is `Test log written to …`.
- **S3b. Opt-in via environment variable** — e.g. `PINE_TESTS_WRITE_LOG=1` activates the behavior; CI and the cloud-agent environment set this, local dev is unaffected. A thin `Program.cs` customization in `Pine.Core.Tests` can inspect the variable and append `--report-trx --results-directory …` to the command line of the testing platform.

- **Cloud agent compatibility:** ✅ — envvar can be set in `.github/workflows/copilot-setup-steps.yml` so only the cloud-agent environment gets the behavior.
- **VS Code Copilot compatibility:** ✅ — envvar can also be set in `.vscode/settings.json` via `terminal.integrated.env.*`, or always-on if S3a is chosen.
- **Cost:** small C# / MSBuild change in `Pine.Core.Tests.csproj` + a `Program.cs` (if not already present) or `TestingPlatformBuilder` customization.
- **Risk:** the TRX extension has its own flags; need to confirm they compose with `--filter-method`. Spot-check shows they do (they are orthogonal options in `Microsoft.Testing.Platform`).

### S4. Add `.github/prompts/run-tests.prompt.md` and a VS Code task

Define a named prompt / VS Code task "Run Pine.Core.Tests" that always invokes the wrapper from S2. The agent is more likely to use a named task than to re-invent the command.

- **Cloud agent compatibility:** ⚠ partial — prompt files are primarily a VS Code feature (`chat.promptFiles` / `chat.reusablePrompts`). The cloud agent does not execute VS Code tasks, but it can still read and use prompt Markdown content if referenced from `copilot-instructions.md`.
- **VS Code Copilot compatibility:** ✅ native.
- **Cost:** tiny.

### S5. Pre-warm and cache the test build in `copilot-setup-steps.yml`

Add steps that run `dotnet restore` and `dotnet build` for `Pine.Core.Tests` during setup so the first `dotnet run` the agent issues doesn't also pay the compile cost. Combined with S2/S3, the first run is already ~30-60 s cheaper.

- **Cloud agent compatibility:** ✅ — `copilot-setup-steps.yml` is the dedicated extension point.
- **VS Code Copilot compatibility:** ➖ not applicable (no analogous setup hook), but not needed locally.
- **Cost:** small workflow edit.

### S6. "Break the re-run temptation" by surfacing the summary eagerly

Modify the .NET runner (or wrapper) so the *last 40 lines of summary* always land in stdout even when the agent pipes through `head`/`grep`, by printing the summary *after* a sentinel marker like `===SUMMARY===` and teaching the agent (via `copilot-instructions.md`) to `grep -A 40 ===SUMMARY===` the log file.

- **Cloud agent compatibility:** ✅
- **VS Code Copilot compatibility:** ✅
- **Cost:** small.

### S7. Add a shell alias / fish/bash hook that auto-`tee`s long commands

A `~/.bashrc` snippet installed by `copilot-setup-steps.yml` that wraps `dotnet run` in a function auto-`tee`-ing to `artifacts/test-logs/…`. Lowers the cost of the agent forgetting the rule.

- **Cloud agent compatibility:** ✅ via `copilot-setup-steps.yml` `run:` steps that append to `~/.bashrc`.
- **VS Code Copilot compatibility:** ⚠ — depends on the developer's local shell; it's arguably too invasive to install globally. Keep scoped to cloud agent only.
- **Cost:** small.

### S8. Prompt-level "before you re-run, ask yourself" rule

Add to `copilot-instructions.md`:

> *Before re-running a command that previously took more than 30 seconds, confirm that either (a) the source code relevant to the command has changed since, or (b) the previous output was not persisted to disk. If neither holds, read the persisted output file instead.*

- **Cloud agent compatibility:** ✅
- **VS Code Copilot compatibility:** ✅
- **Cost:** trivial. This is the single highest-leverage change if we trust the agent to follow rules.

### Recommended combination

S1 + S2 + S3b + S5 + S8 together cover the problem defensively:

- **S1, S8** — change the agent's behavior via instructions.
- **S2** — give the agent a single canonical command that already does the right thing.
- **S3b** — make the log file automatic and machine-readable, gated behind an envvar that is only set in the cloud-agent environment, so local developers are unaffected.
- **S5** — halve the cost of the first (unavoidable) run.

## 6. Can we expand the .NET test run to write a log file per `dotnet run`?

**Yes, and it's a good idea.** Concrete design:

### Mechanism

The `Pine.Core.Tests` project uses `Microsoft.Testing.Platform` with the `Microsoft.Testing.Extensions.TrxReport` package already referenced. Two building blocks are available:

1. **Native CLI flags of the testing platform:** `--report-trx`, `--report-trx-filename`, `--results-directory`. These are already wired up by the `TrxReport` extension.
2. **Program entry point customization** — because `OutputType` is `Exe`, `Pine.Core.Tests` has (or can have) a `Program.cs` using `TestApplication.CreateBuilder`. We can:
   - Read an environment variable, e.g. `PINE_TESTS_WRITE_LOG`.
   - When set, synthesize additional command-line arguments (`--report-trx`, `--results-directory ../../artifacts/test-logs/<project>/<timestamp>/`) before calling `RunAsync`.
   - Additionally mirror stdout+stderr to a plain-text log file in the same directory using a `TextWriter` tee on `Console.Out` / `Console.Error`.
   - After the test run completes, print a single final line to stdout:
     ```
     Test log written to: artifacts/test-logs/Pine.Core.Tests/20260417T142559Z/run.log
     Test TRX report:    artifacts/test-logs/Pine.Core.Tests/20260417T142559Z/run.trx
     ```

### Opt-in vs. always-on

- **Env-var gated (recommended):** `PINE_TESTS_WRITE_LOG=1` in `.github/workflows/copilot-setup-steps.yml` enables it only for the Copilot cloud-agent environment. Local developers and CI are unchanged unless they opt in. This matches the user's instinct in the problem statement.
- **Also consider `PINE_TESTS_WRITE_LOG_DIR`** to let the caller override the directory.
- **Always-on fallback:** leave `artifacts/test-logs/` out of the repo (add to `.gitignore`) so there is no pollution cost.

### Why this directly solves the screenshot's problem

After the change, the agent's first `dotnet run -- --filter-method="*Compiler*"` would:

1. Run the tests once (~3 min).
2. Print `Test log written to artifacts/test-logs/…/run.log`.
3. Even if the agent pipes through `tail -10`, the last line of stdout (the log filename) survives.
4. All subsequent `grep`/`head`/`tail`/slice operations read the file — **no re-runs**.

With S1/S8 added to `copilot-instructions.md` pointing to exactly this pattern, the agent is explicitly told to look for that filename and re-read rather than re-run.

### Agent-flavor compatibility

- **Cloud coding agent (the one in the screenshot):** ✅ fully compatible. Envvar is set in `copilot-setup-steps.yml`; no special hook syntax needed.
- **VS Code Copilot Chat / agent mode:** ✅ fully compatible. The mechanism is plain `dotnet` + envvar; it works identically whether the user exports the variable in their shell, in `.vscode/settings.json → terminal.integrated.env.*`, or in `launch.json`. There is no "hook syntax" difference because the change lives inside the test binary, not inside a Copilot-specific hook.
- **Local dev (no envvar):** ✅ behavior unchanged — zero cost.

### Estimated implementation effort

Small: one `Program.cs` (≈30 lines) + one-line edit of `copilot-setup-steps.yml` + a paragraph in `implementation-rules.md` + a `.gitignore` entry.

### Caveats

- `--report-trx` is accepted by the `TrxReport` extension only; we must guard against enabling it when the extension is not referenced. Every `*.Tests` project in this repo should get the same treatment or the envvar should no-op where the extension is missing.
- If we introduce a `Program.cs` we must ensure it still auto-registers test discovery (the `TestApplication.CreateBuilder` pattern does that via `AddSelfRegisteredExtensions`).
- Log directory should be inside the repo but git-ignored, so the agent can reference a stable relative path without polluting diffs.

---

### TL;DR

The screenshot shows a pathological "re-run a 3-minute test command 3 times in a row to get differently-sliced views of the same output" episode, costing ~9 minutes on its own. The deepest fix is a combination of (a) making the test runner always persist a log file and announce its path on stdout (gated behind an envvar in the Copilot cloud-agent environment), (b) a short rule in `copilot-instructions.md` telling the agent to read that file instead of re-running, and (c) a repo-provided test wrapper script. All three are compatible with both the cloud and VS Code flavors of Copilot without any flavor-specific hook syntax.
