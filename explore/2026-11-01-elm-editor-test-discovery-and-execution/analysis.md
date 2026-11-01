# Discovering and running Elm tests in editors: comparative design analysis

Research date: **2026-11-01**.

## Conclusions

For Pine, the strongest combination is **VS Code's native Testing API for the UX, compiler-backed discovery in the language server, and a small, explicitly versioned testing extension to the existing LSP connection**. Discovery and execution should reuse `Pine.Core.Elm.Testing`, not reimplement Elm test semantics in TypeScript or parse the CLI's terminal output.

**rust-analyzer is the most directly relevant Rust editor design, and my preferred productivity reference for this project.** It puts semantic discovery in the language server, exposes both local run actions and a workspace test tree, and keeps its VS Code adapter relatively thin. This is a design judgment about fit and feedback-loop quality, not a measured claim that it outperforms every Rust IDE. Its experimental test protocol also has limitations worth avoiding.

The Microsoft comparison needs an important qualification: **Microsoft recommends C# Dev Kit for C# testing in VS Code, but C# Dev Kit is closed source.** Its documented UX is useful evidence; its private implementation is not an OSS implementation reference. Microsoft's open-source VSTest and Microsoft.Testing.Platform (MTP) provide inspectable discovery, execution, and protocol designs instead.

Vitest demonstrates a particularly productive continuous-testing workflow, Jest demonstrates the compatibility costs of a CLI adapter with source/result reconciliation, and Playwright demonstrates a runner-owned test server with rich, domain-specific inspection.

## Scope, versions, and evidence

These are source/documentation inspections, **not installations or performance benchmarks**. Released versions below were checked through official release metadata; implementations are linked at their corresponding tags. Runner versions and extension versions are separate products and must not be conflated.

| Tool or component | Exact scope inspected | Official repository and distribution identity | Openness / qualification |
| --- | --- | --- | --- |
| VS Code Testing API | Stable API available since **VS Code 1.59**; documentation snapshot `5a54a294d553935c53374f43c668e37d08f0118e`, dated 2026-10-30 | [Editor source](https://github.com/microsoft/vscode); [versioned API guide][vs-api] | Editor source is MIT. This is an extension-host API, not a testing wire protocol. |
| Language Server Protocol | Current specification **3.18.0** and preceding **3.17**; 3.18 machine-readable method inventory checked | [Official specification repository](https://github.com/microsoft/language-server-protocol); [3.18 meta model][lsp-model] | Neither specification defines a standard end-to-end testing protocol. Custom methods still require client support. |
| Microsoft C# Dev Kit | Documented testing UX at the same documentation snapshot; **11.9.39 is a prerelease build**, recorded in the official repository on 2026-10-26 | Extension **`ms-dotnettools.csdevkit`**; [official feedback/docs repository](https://github.com/microsoft/vscode-dotnettools), inspected at `6aeb5e415972183a5984d6b8eb98dc7a34b4281b`; [Microsoft version report][cs-build] | Closed source. The repository is not the extension implementation. The report verifies a concrete prerelease build, **not the latest stable version or its publication date**. |
| C# extension underlying Dev Kit | **[2.160.4](https://github.com/dotnet/vscode-csharp/releases/tag/v2.160.4)**, released 2026-09-18 | Extension **`ms-dotnettools.csharp`**; [official source at tag](https://github.com/dotnet/vscode-csharp/tree/v2.160.4) | MIT source; distinct from Dev Kit. Included to identify the open-source language-service layer, not to attribute Dev Kit's Test Explorer implementation to it. |
| Microsoft VSTest | **[18.11.0](https://github.com/microsoft/vstest/releases/tag/v18.11.0)**, released 2026-10-13 | [Official source at tag](https://github.com/microsoft/vstest/tree/v18.11.0) | MIT. Inspectable adapter interfaces and test-platform messages. |
| Microsoft.Testing.Platform / MSTest | **MTP 2.5.2 / [MSTest 4.5.2](https://github.com/microsoft/testfx/releases/tag/v4.5.2)**, released together on 2026-10-27; JSON-RPC server-mode schema **1.0**, negotiated protocol string **`1.0.0`** | [Official source at `v4.5.2`](https://github.com/microsoft/testfx/tree/v4.5.2); [separate package version declarations][mtp-versions] | MIT. The repository release tag is the MSTest version, **not** the MTP package version. |
| rust-analyzer | Weekly release **[2026-10-26](https://github.com/rust-lang/rust-analyzer/releases/tag/2026-10-26)**, published 2026-10-26 | Extension **`rust-lang.rust-analyzer`**; [official source at release tag](https://github.com/rust-lang/rust-analyzer/tree/2026-10-26) | MIT OR Apache-2.0. The source manifest contains `0.5.0-dev`; use the weekly release tag to identify this inspection, not that development placeholder. |
| cargo-nextest | **[0.9.150](https://github.com/nextest-rs/nextest/releases/tag/cargo-nextest-0.9.150)**, released 2026-10-26 | [Official source at `cargo-nextest-0.9.150`](https://github.com/nextest-rs/nextest/tree/cargo-nextest-0.9.150) | MIT OR Apache-2.0. A runner/CLI reference, **not** a VS Code adapter or language server. |
| Vitest + official VS Code extension | Runner **[5.0.5](https://github.com/vitest-dev/vitest/releases/tag/v5.0.5)**, released 2026-10-15; extension **[1.52.2](https://github.com/vitest-dev/vscode/releases/tag/v1.52.2)**, released 2026-09-30 | Runner [source at tag](https://github.com/vitest-dev/vitest/tree/v5.0.5); extension **`vitest.explorer`**, [source at tag](https://github.com/vitest-dev/vscode/tree/v1.52.2) | MIT. Current worker path and runner API inspected; older supported Vitest versions use a legacy worker path. |
| Jest + community VS Code extension | Runner **[30.5.2](https://github.com/jestjs/jest/releases/tag/v30.5.2)**, released 2026-09-18; extension **[6.4.4](https://github.com/jest-community/vscode-jest/releases/tag/v6.4.4)**, released 2025-06-29 | Runner [official source at tag](https://github.com/jestjs/jest/tree/v30.5.2); extension **`Orta.vscode-jest`**, [maintainer source at tag](https://github.com/jest-community/vscode-jest/tree/v6.4.4) | MIT. The extension is community-maintained, not a Microsoft product or part of Jest's runner repository. |
| Playwright Test + official VS Code extension | Runner **[1.64.1](https://github.com/microsoft/playwright/releases/tag/v1.64.1)**, released 2026-10-14; extension **[1.1.20](https://github.com/microsoft/playwright-vscode/releases/tag/v1.1.20)**, released 2026-10-16 | Runner [official source at tag](https://github.com/microsoft/playwright/tree/v1.64.1); extension **`ms-playwright.playwright`**, [source at tag](https://github.com/microsoft/playwright-vscode/tree/v1.1.20) | Apache-2.0. The extension includes shared/copied runner protocol types. |

### Evidence boundaries

The Dev Kit Marketplace/gallery could not be reached during research. An apparent version listing is not enough to establish stable-channel status: the official report explicitly calls **11.9.39 prerelease**. Accordingly, this analysis does not manufacture a latest-stable Dev Kit version or infer its private transport from Microsoft's open-source runners.

The screenshots below are **existing official-project documentation illustrations**, remotely embedded with attribution. They are not screenshots of versions installed for this research. Where an image predates the inspected release, its caption says so. They illustrate interactions, not evidence of a feature's introduction date, current pixel layout, performance, or backend protocol.

## Three different design layers

| Layer | Questions it answers | Examples |
| --- | --- | --- |
| **UX design** | Where do tests appear? How does a developer run one test, inspect a failure, cancel, or rerun? | Testing sidebar, gutter actions, failure diffs, continuous-run toggle, replay a fuzz failure. |
| **Protocol design** | What crosses a component boundary, with what identity, lifecycle, compatibility, and cancellation rules? | Discovery requests, source ranges, run IDs, scoped tree updates, result events, version negotiation. |
| **Runner and integration architecture** | Who discovers and executes tests? What is cached, compiled, evaluated, isolated, watched, or configured? | Semantic analysis versus runtime enumeration; child processes versus VMs; project configuration; dependency invalidation. |

These choices are largely independent. Two extensions can look nearly identical because they both use VS Code's Testing API, while one talks to a language server, another invokes a CLI, and another maintains a WebSocket connection to its test runner.

Debugging, coverage, workspace trust, reproducibility, and distribution/licensing are additional cross-cutting concerns. A Run button does not imply that a debugger or coverage engine exists.

## Shared UX foundation: the native VS Code Testing API

The [Testing API guide][vs-api] describes the common foundation:

- A `TestController` owns a hierarchy of `TestItem`s. Items have IDs, labels, optional source URIs/ranges, children, and discovery/error state.
- `canResolveChildren` and `resolveHandler` allow lazy discovery. The extension decides when to watch files, refresh, or inspect an open document.
- Run profiles distinguish **Run**, **Debug**, and **Coverage**. A profile can restrict eligible tests through tags.
- A `TestRunRequest` contains optional inclusion and exclusion sets. An extension must implement selection semantics; the API does not select runner tests for it.
- A `TestRun` publishes queued/started/passed/failed/errored/skipped states, messages, durations, and output, then ends. VS Code aggregates parent status.
- Failure messages can contain source locations and expected/actual diffs. Coverage has its own summary and lazy-detail APIs.

**UX implication:** build on this instead of introducing an Elm-specific test sidebar. Users retain familiar keyboard commands, navigation, filtering, result history, and accessibility behavior. Add Elm-specific actions only where the shared surface is insufficient.

**Protocol implication:** none of these TypeScript objects is an LSP message. They exist inside the extension host. The adapter still needs a backend contract.

**Compatibility implication:** the core API dates from 1.59, but not every later feature does. Vitest documents coverage as requiring VS Code 1.88; the inspected Jest and Playwright manifests require 1.88.1 and 1.93 respectively. Pine's current manifest requires 1.75, so basic test integration need not automatically adopt those extensions' higher floors.

### LSP, JSON-RPC, and DAP are not interchangeable

[LSP 3.17][lsp] and the current [3.18 specification][lsp-current] standardize document synchronization, diagnostics, CodeLens, commands, generic progress, and request cancellation, but not an end-to-end test tree/discovery/execution contract. The [3.18 meta model][lsp-model] contains 95 requests/notifications and no testing methods. A standard CodeLens can invoke a command, but it does not carry a standardized test result model.

A custom test extension can reuse LSP's JSON-RPC connection and framing without becoming a standard LSP feature. Similarly, MTP uses JSON-RPC but is not a language server. Playwright's runner protocol and Vitest's RPC are not LSP merely because they use structured messages.

Debug sessions are a separate concern, usually integrated through VS Code debugging and a Debug Adapter Protocol implementation. Test discovery IDs can help choose what to debug; a testing protocol alone cannot provide breakpoints or variable inspection.

## 1. Microsoft's C# testing experience and open-source test platforms

### UX design: C# Dev Kit

[Microsoft's testing guide][cs-ux] recommends C# Dev Kit and documents xUnit, NUnit, and MSTest integration. Tests appear in the native Testing view and beside class/method definitions; developers can run or debug a method, a class, or a larger selection. Results update both places, and stack traces link back to source. Coverage is a separate supported workflow.

A particularly important behavior is **build-dependent discovery**. The guide says the test project must be built before its tests are recognized; Refresh Tests can build and discover. The [FAQ][cs-faq] also says newly added tests require a build and cleaning removes discovered test DLLs.

![Microsoft documentation screenshot showing a C# test hierarchy, aggregated failures, and per-test run/debug actions](https://media.githubusercontent.com/media/microsoft/vscode-docs/5a54a294d553935c53374f43c668e37d08f0118e/docs/csharp/images/testing/view-test-results.png)

*Source: Microsoft, [C# testing documentation][cs-ux], image at the pinned documentation revision. This historical illustration shows a `net7.0` project; it does not identify the Dev Kit build used to capture it. Documentation license: [Creative Commons Attribution 3.0 United States][vscode-docs-license]; image linked without modification.*

**Benefit:** authoritative runner/framework discovery supports cases that cannot be inferred from method syntax alone, including parameterized tests.

**Cost:** a broken or expensive build can prevent a useful tree from appearing. There is a gap between "I just wrote a test" and "the editor knows it exists." Pine should distinguish cheap declaration-level discovery from successful evaluation of the complete test tree rather than copy that gap unquestioningly.

### Protocol design: VSTest

VSTest's framework adapters implement [`ITestDiscoverer`][vstest-discoverer] and [`ITestExecutor`][vstest-executor]. Discovery receives test containers and publishes test cases through a sink. Execution accepts either containers or selected `TestCase`s and supports cancellation.

The [versioned message definitions][vstest-messages] include:

| Purpose | VSTest message examples |
| --- | --- |
| Compatibility | `ProtocolVersion` |
| Discovery | `TestDiscovery.Start`, `TestDiscovery.TestFound`, `TestDiscovery.Completed`, `TestDiscovery.Cancel` |
| Execution | `TestExecution.StartWithTests`, `TestExecution.StatsChange`, `TestExecution.Completed`, `TestExecution.Cancel`, `TestExecution.Abort` |
| Debugger coordination | `TestExecution.AttachDebugger`, related acknowledgement messages |

This is an **adapter/test-host protocol**, not C# LSP. It is evidence for separating discovery, selection, execution, and debugger coordination, but not proof that every Dev Kit configuration uses exactly this path.

### Protocol design: Microsoft.Testing.Platform

MTP is a newer, distinct platform embedded in the test application. Its [overview][mtp-overview] and [server-mode documentation][mtp-server] describe both direct application execution and tool-driven JSON-RPC server mode. It does not require a separate `vstest.console` application.

The inspectable [server-mode schema at MTP 2.5.2][mtp-schema] specifies:

| Purpose | Contract |
| --- | --- |
| Handshake | `initialize`; client/server information, capabilities, and protocol-version negotiation |
| Discovery | `testing/discoverTests`, with a `runId` and optional selected nodes/filter |
| Execution | `testing/runTests`, also associated with a `runId` |
| Streaming updates | `testing/testUpdates/tests`; changes identify parents and nodes with a separate `uid` and `display-name` |
| Results | Node execution state, source location, duration, optional retry information |
| Other output | Attachment updates and `client/log` |
| Cancellation/shutdown | `$/cancelRequest` and `exit` |

The negotiated protocol version is independent of the NuGet package version. The canonical source-only client was introduced in MTP 2.4. Its current launch models use loopback TCP, whether the application is launched in a child process or through an in-process callback.

The nullable **stateful-client capability**, added in 2.4.1, is especially relevant: retaining a test tree is different from merely consuming result events. It is also independent of server multi-request support. A persistent connection does not imply that overlapping executions are safe.

MTP has a broad node-state model, including `timed-out`, `error`, and `canceled`, rather than treating every unsuccessful operation as an assertion failure. Its wire schema gives non-C# clients a concrete contract even though the canonical client is C# source.

### Other architecture aspects and tradeoffs

The older VSTest model relies on discoverer/executor adapters and test-host infrastructure. MTP favors compile-time extension registration and a hostable test application. Both preserve framework-specific semantics while giving an IDE a common representation.

**What to borrow for Elm:** explicit identity versus labels; discovery separate from execution; structured errors; streamed results; capability/version negotiation; and cancellation that has a terminal outcome.

**What not to borrow:** a general-purpose .NET adapter ecosystem or transport stack merely because Pine itself is written in C#. Pine can expose its own small contract over the already established LSP connection.

## 2. rust-analyzer: the closest language-server-led reference

### UX design

rust-analyzer provides local runnable actions and a native Test Explorer integration. Its semantic model discovers a hierarchy of local crates, modules, and `#[test]` functions, retaining navigation locations and runnable metadata.

The Test Explorer is **opt-in** in the inspected release: `rust-analyzer.testExplorer` defaults to `false` in the [extension manifest][ra-manifest]. It should not be represented as an unconditional default Rust experience.

The [VS Code adapter][ra-client] creates Run and Debug profiles, resolves items through the server, and mirrors result notifications into `TestRun`. Debugging uses runnable metadata and a separate debug-session path; the adapter restricts its debug selection to a single item with runnable information.

**Why this is the strongest productivity reference for Elm:** it reuses semantic knowledge already maintained for navigation and editing, makes a nearby test runnable without designing a second editor, and offers a workspace hierarchy without requiring the TypeScript client to understand Rust test syntax.

### Protocol design

The [release-pinned LSP extension specification][ra-protocol] contains two related but different contracts:

| Contract | Responsibility |
| --- | --- |
| `experimental/runnables` | Return runnable descriptors for a document/position. The **client** executes the technology-specific command. |
| `experimental/discoverTest` | Resolve top-level tests or children of a supplied `testId`. |
| `experimental/discoveredTests` | Server-pushed test-tree changes. |
| `experimental/runTest` | Ask the **server** to run an inclusion/exclusion selection. |
| `experimental/changeTestState` | Per-test queued/started/passed/failed/skipped updates. |
| `experimental/appendOutputToRunTest` | Run-level output. |
| `experimental/abortRunTest`, `experimental/endRunTest` | Stop the active execution and signal its completion. |

The client advertises the experimental `testExplorer` capability. Test items carry an ID, parent, label, kind, optional document/range, `canResolveChildren`, and optional runnable.

An unusually useful feature is **scoped replacement**. Discovery results specify either parent scopes or file scopes whose old children/items are replaced. This tells the client when to delete tests that disappeared, rather than only adding and updating nodes forever.

### Other architecture aspects and limitations

[Semantic discovery][ra-discovery] traverses the analysis database rather than parsing test names out of terminal output. Nevertheless, discovering a semantic `#[test]` function is not the same thing as proving the configured test binary builds or enumerating every custom harness's runtime cases.

Several concrete limitations prevent treating this protocol as an ideal contract to copy verbatim:

- **A single active run:** the client stores one `currentTestRun`, aborting and waiting for it before starting another. The test-state/output/end notifications have no run ID. Concurrent runs would require a different correlation contract.
- **Limited result richness:** the documented failure state carries a string, not structured expected/actual values, source-linked assertion detail, or per-test durations.
- **Lazy protocol, partly eager client:** the client has an explicit workaround that eagerly resolves roots after a top-level discovery response. A `canResolveChildren` field alone does not guarantee good discovery latency.
- **Identity compromises:** discovery uses local crate display names as roots and qualified names below them; the source itself notes that the root-ID choice is imperfect. Elm descriptions and generated suites make label-derived identity still more hazardous.
- **Unstable runner output dependency:** the [test runner][ra-runner] invokes Cargo/libtest with `-Z unstable-options --format=json` and sets `RUSTC_BOOTSTRAP=1`. This is a concrete implementation dependency, not a stable, general Rust testing protocol to inherit.

**What to borrow:** semantic ownership, a thin client, lazy resolution, scoped removal semantics, and local navigation.

**What to improve:** run correlation, durable identity, rich failures, explicit discovery completeness, and a supported machine-readable runner contract.

## 3. cargo-nextest: a complementary Rust runner design

### UX design

Nextest optimizes the terminal/CI feedback loop rather than providing an editor test tree. It reports individual tests, their output and durations, and supports controls such as retries and slow-test handling. Its productivity value is predictable, high-throughput execution across test binaries.

For editor design it is useful as the answer to a different question: **what should the backend make observable and controllable once the user clicks Run?**

### Protocol design

[Machine-readable discovery][nextest-list] supports:

```console
cargo nextest list --message-format json
cargo nextest list --list-type binaries-only --message-format json
```

The first returns test instances grouped by binaries, with package/binary identities, test names, ignore state, and filter-match information. The second avoids running test binaries to enumerate instances. `nextest-metadata::TestListSummary` is the shared typed representation; the inspected documentation says a JSON schema is not yet available.

Execution reporting is not equally mature: [the machine-readable format overview][nextest-formats] identifies JUnit XML as the main integration format and libtest-like JSON as experimental and not full-fidelity. [The latter][nextest-libtest] requires `NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1` and has its own format version, `0.1`.

Thus nextest is **not evidence that a stable live editor protocol automatically follows from a good JSON listing command**. JUnit XML is valuable for completed reports, but is not by itself a discovery/watch/cancel/live-results protocol.

### Other architecture aspects and tradeoffs

The [execution-model documentation][nextest-design] describes two phases: build and list all tests, then schedule individual tests in separate processes across binaries.

Compared with Cargo's sequential execution of test binaries, this reduces cross-binary scheduling bottlenecks and enables per-test isolation and retry policy. The price is process startup and a thicker contract with test harnesses; custom harnesses may need adaptation.

**For Pine:** borrow separate preparation/execution phases, exact selection, useful progress, durations, and reproducible reruns. Do not require process-per-test: Pine already has VM-backed execution, and per-run/worker VMs plus cooperative budgets can preserve its intended execution model without importing nextest's OS-process costs.

## 4. Vitest and its official VS Code extension

### UX design

The [extension README][vitest-readme] documents native tree and gutter actions for Run, Debug, Coverage, and **Continuous Run**, including continuous execution scoped to a test, file, directory, or the whole tree. Continuous run is not enabled by default; `watchOnStartup` defaults to `false`.

Additional productivity features include Run Related Tests, inline console output, and import-duration inspection. The inspected release documents import breakdowns with Vitest 4.1+ and browser Trace View with Vitest 5, rather than implying all features work with the extension's minimum supported runner version, 1.4.

![Official Vitest documentation screenshot showing native test states, nested suites, and the Testing toolbar](https://raw.githubusercontent.com/vitest-dev/vscode/v1.52.2/img/vitest-extension.png)

*Source: Vitest Team, [extension README at 1.52.2][vitest-readme]; [MIT license][vitest-license]. This documentation illustration does not identify its capture-time runner/editor versions.*

### Protocol design

This is **runner-owned RPC, not LSP**. The inspected [worker contract][vitest-contract] includes `getFiles`, `collectTests`, `runTests`, `cancelRun`, `watchTests`, file-change notifications, and snapshot updates. Reverse events include collection, task updates, console logs, and run start/end.

The extension uses [`birpc`][vitest-rpc] and launches an extension worker loading the project's Vitest. In the inspected [child-process path][vitest-spawn], a local WebSocket carries that RPC; the terminal launch mode also uses a WebSocket. Human-readable stdout/stderr remains separate from structured task events.

Task IDs correlate backend tasks with `TestItem`s. File items are also distinguished by project: the same source file can legitimately belong to multiple execution configurations.

The wire contract is internal to this integration. Its existence does not promise an independent, cross-language protocol or a stable public WebSocket endpoint for arbitrary editors.

### Other architecture aspects and tradeoffs

Discovery is staged: files become lazy tree nodes, and resolving one requests its tests. Importantly, the [current worker][vitest-collect] invokes `parseSpecifications` or its experimental predecessor; **do not describe modern Vitest discovery as necessarily executing every test module**. [Vitest 5's API][vitest-api] also says collection uses static analysis by default, with runtime collection as an option.

Static collection cannot make every computed name or generated case authoritative. The [tree adapter][vitest-tree] explicitly reconciles dynamic pattern nodes with runtime-collected cases, preserving generated children and removing disappeared tasks.

The persistent Vitest/Vite environment and dependency graph support fast related-test reruns. Costs include retained resources, lifecycle/cancellation handling, version-specific worker paths, and reliance on runner APIs whose experimental parts can change outside normal SemVer guarantees.

**For Elm:** continuous testing should be an explicit, scoped user choice, not a consequence of opening a project. Reuse backend dependency knowledge, show unknown/generated groups honestly, and reconcile authoritative evaluated cases rather than deriving their identity from display strings.

## 5. Jest and the community VS Code extension

### UX design

[vscode-jest 6.4.4][jest-readme] integrates with native Test Explorer while adding folder-level status, run-mode controls, output terminals, inline errors, coverage, and snapshot actions.

Its default is more proactive than Vitest's: when a runnable command is found, it automatically runs and monitors tests in watch mode. Developers can switch between watch, on-save, and on-demand modes, including a session-only mode change without rewriting project settings.

![Official Jest extension screenshot showing run-mode selection, gutter actions, output panels, and status bars](https://raw.githubusercontent.com/jest-community/vscode-jest/v6.4.4/images/interface-6.1.0.png)

*Source: vscode-jest maintainers, [README at 6.4.4][jest-readme]; image explicitly named `interface-6.1.0.png`, therefore a historical 6.1.0-era illustration rather than a fresh 6.4.4 capture. [MIT license, copyright Orta][jest-license].*

### Protocol design

Jest's adapter is principally **CLI/process integration with structured results**, not a language-server testing protocol.

[Process management][jest-process] invokes `jest-editor-support`'s runner with CLI selections such as `--runTestsByPath`, name patterns, related-test selection, watch flags, and custom reporters. It consumes structured `executableJSON` events as well as stderr/output/process events. The extension's [small reporter][jest-reporter] also emits textual lifecycle markers; it should not be mistaken for a complete per-test RPC contract.

The source contains a real version adaptation: Jest 30 uses `--testPathPatterns`, whereas the older command is `--testPathPattern`. A seemingly simple command wrapper still needs runner-version awareness.

### Other architecture aspects and tradeoffs

The [result provider][jest-results] combines source parsing with a `TestReconciler` that consumes runner assertions. It matches runtime results back to nested source blocks and ranges, handles multiple results at one range, and represents unmatched/unknown results.

**Benefit:** works with existing Jest commands and project configuration, including wrappers and monorepo/virtual-folder setups. Users can often reuse exactly the command that works in their terminal.

**Cost:** source-versus-runtime reconciliation is substantial logic. Parameterized cases, generated names, wrappers, syntax errors, command quoting, and CLI version changes complicate discovery and exact selection. Name-pattern execution can be less precise than runner-owned opaque test IDs.

**For Elm:** this is a plausible compatibility/fallback model, but not the preferred primary architecture when Pine already owns compilation and evaluation. Avoid porting Elm parsing or result-name matching into the VS Code extension just to imitate Jest's compatibility strategy.

## 6. Playwright Test and its official VS Code extension

### UX design

The [official extension][pw-readme] combines native testing actions with browser-specific workflows: choose projects/browsers, show the browser, inspect traces, select locators, record tests, and debug step by step.

Playwright projects are execution configurations, not necessarily separate source trees. By default the first project is selected; developers can enable others to run the same source test under multiple browser configurations.

![Official Playwright animated documentation illustration showing the test tree and browser/trace controls](https://github.com/microsoft/playwright-vscode/assets/13063165/959cb45c-7104-4607-b465-bf74099142c5)

*Source: Microsoft, animated illustration linked by [README at extension 1.1.20][pw-readme], under "Show trace viewer." Its first frame shows the Testing view and Playwright controls; the walkthrough is historical and does not identify the inspected runner version. [Apache-2.0 license][pw-license].*

**UX lesson:** the native tree is the common shell, while domain-specific investigation belongs alongside it. Elm's likely equivalent is fuzz replay/counterexample inspection, not browser controls.

### Protocol design

The extension's [test-server interface][pw-contract] explicitly separates:

| Operation | Role |
| --- | --- |
| `listFiles`, `listTests` | Cheap file/configuration enumeration versus detailed suite collection |
| `runTests` | Select projects, locations, and/or `testIds`, with execution options |
| `watch`, `findRelatedTestFiles` | Watch selected files and query dependency-based relevance |
| `stopTests`, `closeGracefully` | Execution cancellation and server lifecycle |
| `onReport`, `onStdio`, `onTestFilesChanged` | Structured reporter events, human output, and invalidation |

The [backend adapter][pw-server] uses a WebSocket test-server connection, reconstructing suites/results with `TeleReporterReceiver`. Debugging launches the server under VS Code's Node debugger. This is a private/shared Playwright runner protocol, not LSP or a general test-protocol standard.

Detailed discovery returns a sequence of reporter events representing the collected suite. Execution emits related reporter events live. Reusing the runner's representation avoids a second TypeScript parser trying to infer Playwright semantics independently.

### Other architecture aspects and tradeoffs

The [test model][pw-model] creates cheap file placeholders, batches detailed file discovery, tracks already listed files, responds to file creation/deletion, and maintains project-specific state. It treats collection errors differently from valid test suites.

Runtime collection loads test definitions; "list tests" does not mean that arbitrary user module initialization is impossible. Configuration/global setup/browser reuse also introduces real lifecycle concerns.

**Benefit:** authoritative runner identity and rich reporter events support parameterization, project selection, and domain-specific results. Source maps and step locations improve navigation.

**Cost:** tighter coupling to runner versions, a persistent service, resources that outlive one run, and synchronization among discovery, global hooks, browser processes, and debugging.

**For Elm:** staged discovery, exact backend IDs, explicit setup-versus-execution errors, and a structured failure-inspection action are useful. Browser-process lifecycle is not.

## Cross-tool comparison and tradeoffs

### UX choices

| Choice | References | Benefit | Cost / Elm implication |
| --- | --- | --- | --- |
| Native Test Explorer + gutter actions | C# Dev Kit, rust-analyzer, Vitest, Jest, Playwright | Familiar local and workspace workflows | Requires reliable source ranges and ID reconciliation; preferred Elm baseline. |
| Build/runtime-authoritative discovery | C# platforms; detailed Playwright collection; nextest | Correct runtime cases and configuration | Compilation/evaluation can be slow or fail; show discovery progress/errors rather than an unexplained empty tree. |
| Cheap semantic/static discovery before execution | rust-analyzer; modern Vitest | Immediate feedback while editing | Cannot fully enumerate arbitrary generated cases; label incomplete groups rather than inventing leaves. |
| Automatic watch by default | Jest | Immediate regression feedback | Unexpected work, resource usage, and distracting output; not the recommended Elm default. |
| Explicit scoped continuous run | Vitest, Playwright | Tight feedback loop with user control | Needs dependency invalidation and serialized/coalesced scheduling. |
| Domain-specific investigation | Playwright traces/locators; Jest snapshots; Vitest logs | Reduces time from failure to explanation | Add only relevant Elm actions: replay seed, inspect counterexample, copy reproduction command. |

### Protocol and ownership choices

| Design | Representative implementation | Strengths | Weaknesses |
| --- | --- | --- | --- |
| Language server discovers; client runs descriptors | rust-analyzer `runnables` | Thin server execution responsibility; convenient terminal/debug actions | Client still orchestrates results, processes, cancellation, and selection. |
| Language server discovers and orchestrates execution | rust-analyzer Test Explorer | Shared semantics and incremental source model; thin UI adapter | Custom protocol maintenance; long work must not block editing; current Rust protocol lacks run correlation. |
| CLI adapter + structured result reconciliation | Jest; nextest listings/reports | Easy CLI/CI interoperability; respects existing commands | Discovery, live output, cancellation, and source identity need additional contracts; do not scrape presentation text. |
| Persistent runner service with RPC | Vitest, Playwright, MTP server mode | Rich live events, caches, runtime-authoritative IDs | Lifecycle/version coupling and resource costs; a separate network transport is not inherently necessary. |
| Common platform with framework adapters | VSTest; MTP framework integration | Multiple frameworks share an IDE model | More abstraction and compatibility obligations than a single Elm runner presently needs. |

### Discovery accuracy, latency, and trust

There are at least four useful levels of discovery:

1. **Project/files:** identify manifests and likely test modules cheaply.
2. **Declarations:** semantic/static candidates and source navigation without executing assertions.
3. **Evaluated tree:** construct `Test` values and enumerate concrete nested/generated cases.
4. **Execution results:** resolve runtime outcomes, counterexamples, retries, durations, and errors.

A design need not choose exclusively between "regex" and "run the entire suite." It can progressively enrich the same tree. However, each level must say what is authoritative.

Static discovery does not automatically imply that the overall tool executes no project code: configuration, Rust build scripts/procedural macros, runtime collection, or test-value construction may still run. Workspace trust, offline/dependency policy, and cooperative execution limits belong to backend integration, not merely to button placement.

### IDs, selection, and freshness

Descriptions are for people; identities are for protocols. The same description can appear multiple times, one source test can produce many cases, and one file can run under several projects/configurations.

Scope test identities to the project and backend's discovery context. Keep IDs distinct from labels and source ranges. Prefer exact inclusion/exclusion of identified nodes over reconstructing a command-line name pattern. If old IDs cannot be safely resolved after edits, explicitly reject them and refresh instead of running a similarly named test.

Tree updates need deletion semantics, such as rust-analyzer's scoped replacement. A failed or partial discovery must not accidentally delete every previously known test by looking like a successful empty response.

A run's results must remain associated with the inputs it actually tested. Moving a source range or renaming a test during execution must not attach an old failure to unrelated new code.

### Failure, cancellation, and completeness

Assertion failure, compilation error, discovery/evaluation error, timeout, cancellation, deliberate skip, and filter exclusion are different states. A stopped run is not a successful run; an undiscovered test is not a skipped test; an excluded test was not executed.

Streaming events improve perceived responsiveness and allow targeted failure inspection before the suite finishes. They also require run correlation, ordering, a terminal event/response, and handling of transport disconnects.

For fuzz tests, replay needs more than a red icon: seed, iteration count, selected property, counterexample, and relevant implementation/dependency identity. Automatic retries or watch reruns should not hide the original failing evidence.

## Applying the findings to Pine

### Existing foundation, inspected locally

Local baseline: commit **`e6cb9b117de7d7d9911429190f63330d109a2f2d`**. The Pine extension's package version is **0.3.7**, with VS Code **`^1.75.0`**.

Relevant existing surfaces:

- [`ElmTestRunner.cs`](../../implement/Pine.Core/Elm/Testing/ElmTestRunner.cs) exposes `CompileAndRunTests`, optional `IPineVM` injection, list-only mode, selection callbacks, and structured `ElmTestRun.Listed` / `Completed` results.
- Discovery already compiles declaration roots, evaluates exposed zero-parameter declarations, constructs test values, and prepares their test trees. It is **not** just syntax enumeration. Compilation precedes filtering: selecting one test currently does not isolate compilation from other discovered test-module roots.
- [`TestCommand.cs`](../../implement/pine/CLI/Elm/TestCommand.cs) consumes the core runner and maps structured output styles to CLI presentation. Its terminal UI is not an appropriate editor wire format.
- The [CLI guide](../../guide/elm-test-command-line-interface.md) already documents listing, exact selectors, ordinal fallback for duplicate paths, watching, fuzz seed/count, budgets/timeouts, and incomplete `only`/`skip` runs.
- The [language-server guide](../../guide/language-server-design-and-implementation.md) describes unsaved source contents, document version tracking, supersession, compiler-backed queries, and cooperative cancellation.
- [`extension.ts`](../../implement/vscode/extension/pine/client/src/extension.ts) launches `pine lsp` over stdio. It does not currently create a `TestController`.

These are assets to reuse, **not a claim that the editor testing feature already exists**. In particular, the current listing/result types are not by themselves a complete incremental tree protocol with source ranges, exact opaque IDs, and live per-test events.

### Recommended UX

Use the native Testing view with **project -> module/file -> describe/group -> test/property**. Gutter actions should navigate/run the same backend item as the tree; do not maintain a second independent collection for CodeLens.

Discover project/file containers cheaply, then resolve evaluated children on expansion, explicit refresh, or Run. An open test file may receive inexpensive semantic declaration discovery earlier. If exact child-level source mapping is unavailable for a generated case, link to the enclosing declaration and state that limitation rather than fabricating an assertion location.

Start with **Run**, Refresh, Cancel, source navigation, failure details, and rerun selection. Make continuous run explicit and scoped. Save-triggered reruns should include relevant dependency changes, coalesce bursts, and never accumulate overlapping runs of obsolete inputs.

For fuzz failures, add **Replay with this seed** and **Copy reproduction command**, retaining the effective fuzz settings and counterexample. Keep TODO/skip/incomplete-suite information visible. Do not advertise Debug or Coverage profiles until actual runtime/debugger or coverage support exists.

### Recommended protocol direction

Use a **Pine-specific extension on the existing LSP connection**, negotiated through initialization capabilities. Keep the data model independent of VS Code classes so another editor can use it.

The following is a **proposed design sketch**, not existing methods or a finalized schema:

| Operation | Suggested contract responsibility |
| --- | --- |
| `pine/tests/discover` request | Project URI, optional parent ID; return items, scoped replacement boundaries, completeness, source-located diagnostics, and a transport-level tree revision. |
| `pine/tests/changed` notification | Publish complete scoped replacements after accepted source changes; identify the revision and affected scope. Partial/error updates must not masquerade as complete empty replacements. |
| `pine/tests/run` request | Client run ID, project URI, tree revision, include/exclude IDs, fuzz settings, and execution limits. Keep the request outstanding until its final summary. |
| `pine/tests/event` notification | Run ID, sequence number, phase/test state, duration, structured failures/output, effective replay settings; reject stale or unknown correlation. |
| `$/cancelRequest` | Cancel the outstanding discovery/run request through the existing RPC cancellation path; still return a terminal response. |

The tree revision here is **host/transport bookkeeping for freshness**, not a new compiler "frontend snapshot" or "program revision" abstraction. Capture the accepted source contents and document versions used for execution in the host, using existing compiler/core APIs.

Keeping Run outstanding makes standard request cancellation sufficient. If a future design instead returns an execution handle immediately, it must add explicit cancellation by run ID; `$/cancelRequest` cannot cancel an already completed start request.

Every item should have a project-scoped opaque ID, parent, label, kind, optional source location, and discovery completeness. Exact selection should operate on resolved IDs, with exclusions applied after inclusions and descendant semantics defined. CLI `#N` discovery ordinals are useful within one enumeration, but must not become durable cross-edit IDs.

Use a protocol version independent of the Pine executable/extension version. Negotiate optional features such as live results, source-linked failures, fuzz replay, and continuous run. An unsupported version or missing capability should produce an explicit error or unavailable action, not a silent terminal-output fallback.

Initially allow one active run per project and keep language-service requests responsive. Include run IDs from the start even if executions are serialized, so late notifications and independent projects cannot contaminate each other's results.

### Recommended architecture and implementation boundaries

The **core runner** should own Elm test semantics, exact selection, preparation, fuzzing, and execution events. The **language-server host** should own source synchronization, project resolution, scheduling/cancellation, and transport DTOs. The **VS Code adapter** should own `TestController`/`TestRun` state and presentation.

Separating reusable preparation/discovery from execution is preferable to repeatedly invoking today's combined `CompileAndRunTests` just to populate each tree node. The implementation should reuse its compiler/evaluation logic rather than create a competing discovery engine. Preserve current exposed-declaration, `only`/`skip`, fuzz-seed, and dependency behavior unless deliberately changing and documenting them.

Reuse existing parsing/compilation/VM caches where safe, but do not share mutable run state between editing and testing. VM-backed execution does not require another OS process or another WebSocket listener. A separately launched runner is a reasonable later isolation/deployment option, not a prerequisite for the UX.

For CLI and future non-editor integrations, expose the **same structured discovery/results model** through an explicitly versioned machine-readable format if needed. Keep presentation output separate. This captures nextest's interoperability benefit without tying editor correctness to CLI text.

### Acceptance questions for the eventual implementation

| Scenario | Required behavior |
| --- | --- |
| Duplicate descriptions or generated tests | Distinct selectable IDs; correct parent/source attribution; no accidental "run all tests with this name." |
| Unsaved editor changes | Discovery and execution follow an explicit accepted-input policy; never silently show results for different contents. |
| Compile/discovery failure | Useful diagnostics and incomplete/error state, not "zero tests" or a clean result. |
| Rename/delete while a run is active | Scoped tree cleanup; old events stay attached to the old run, not new unrelated items. |
| Cancel during resolution, preparation, fuzzing, or execution | Cooperative cleanup, explicit canceled outcome, and responsive editing. |
| `only`, `skip`, TODO, no matches, or budget exhaustion | Preserve current Elm/Pine semantics and suite completeness; distinguish selection from execution. |
| Fuzz replay after filtering or changing worker count | Preserve the existing seed-distribution behavior and effective settings; report dependency/source assumptions. |
| Multiple `elm.json` projects and dependency edits | Correct project ownership and coalesced invalidation; no duplicate subscriptions or runaway watch work. |

## Source index

All implementation links below are pinned to the inspected release or documentation commit. Release identity/date references are the official release pages corresponding to the tags in the version table. Live documentation is supplemental where a pinned implementation/schema is also provided.

[vs-api]: https://github.com/microsoft/vscode-docs/blob/5a54a294d553935c53374f43c668e37d08f0118e/api/extension-guides/testing.md
[lsp]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/
[lsp-current]: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/
[lsp-model]: https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.18/metaModel/metaModel.json
[cs-ux]: https://github.com/microsoft/vscode-docs/blob/5a54a294d553935c53374f43c668e37d08f0118e/docs/csharp/testing.md
[cs-faq]: https://github.com/microsoft/vscode-docs/blob/5a54a294d553935c53374f43c668e37d08f0118e/docs/csharp/cs-dev-kit-faq.md
[cs-build]: https://github.com/microsoft/vscode-dotnettools/issues/3718
[vscode-docs-license]: https://github.com/microsoft/vscode-docs/blob/5a54a294d553935c53374f43c668e37d08f0118e/LICENSE.md
[vstest-discoverer]: https://github.com/microsoft/vstest/blob/v18.11.0/src/Microsoft.TestPlatform.ObjectModel/Adapter/Interfaces/ITestDiscoverer.cs
[vstest-executor]: https://github.com/microsoft/vstest/blob/v18.11.0/src/Microsoft.TestPlatform.ObjectModel/Adapter/Interfaces/ITestExecutor.cs
[vstest-messages]: https://github.com/microsoft/vstest/blob/v18.11.0/src/Microsoft.TestPlatform.CommunicationUtilities/Messages/MessageType.cs
[mtp-versions]: https://github.com/microsoft/testfx/blob/v4.5.2/eng/Versions.props
[mtp-overview]: https://github.com/dotnet/docs/blob/1443883303ca8a9e515a13c1c39c7224bb2cfa33/docs/core/testing/microsoft-testing-platform-intro.md
[mtp-server]: https://github.com/dotnet/docs/blob/a8c01b57f9a39d471dd70541de42e5a3a72ce2c5/docs/core/testing/microsoft-testing-platform-server-mode.md
[mtp-schema]: https://github.com/microsoft/testfx/blob/v4.5.2/docs/mstest-runner-protocol/server-mode-1.0.schema.json
[ra-manifest]: https://github.com/rust-lang/rust-analyzer/blob/2026-10-26/editors/code/package.json
[ra-protocol]: https://github.com/rust-lang/rust-analyzer/blob/2026-10-26/docs/book/src/contributing/lsp-extensions.md
[ra-client]: https://github.com/rust-lang/rust-analyzer/blob/2026-10-26/editors/code/src/test_explorer.ts
[ra-discovery]: https://github.com/rust-lang/rust-analyzer/blob/2026-10-26/crates/ide/src/test_explorer.rs
[ra-runner]: https://github.com/rust-lang/rust-analyzer/blob/2026-10-26/crates/rust-analyzer/src/test_runner.rs
[nextest-design]: https://github.com/nextest-rs/nextest/blob/cargo-nextest-0.9.150/site/src/docs/design/how-it-works.md
[nextest-list]: https://github.com/nextest-rs/nextest/blob/cargo-nextest-0.9.150/site/src/docs/machine-readable/list.md
[nextest-formats]: https://github.com/nextest-rs/nextest/blob/cargo-nextest-0.9.150/site/src/docs/machine-readable/index.md
[nextest-libtest]: https://github.com/nextest-rs/nextest/blob/cargo-nextest-0.9.150/site/src/docs/machine-readable/libtest-json.md
[vitest-readme]: https://github.com/vitest-dev/vscode/blob/v1.52.2/README.md
[vitest-license]: https://github.com/vitest-dev/vscode/blob/v1.52.2/LICENSE
[vitest-contract]: https://github.com/vitest-dev/vscode/blob/v1.52.2/packages/shared/src/index.ts
[vitest-rpc]: https://github.com/vitest-dev/vscode/blob/v1.52.2/packages/extension/src/spawn/rpc.ts
[vitest-spawn]: https://github.com/vitest-dev/vscode/blob/v1.52.2/packages/extension/src/spawn/child_process.ts
[vitest-collect]: https://github.com/vitest-dev/vscode/blob/v1.52.2/packages/worker/src/runner.ts
[vitest-tree]: https://github.com/vitest-dev/vscode/blob/v1.52.2/packages/extension/src/testTree.ts
[vitest-api]: https://github.com/vitest-dev/vitest/blob/v5.0.5/docs/api/advanced/vitest.md
[jest-readme]: https://github.com/jest-community/vscode-jest/blob/v6.4.4/README.md
[jest-license]: https://github.com/jest-community/vscode-jest/blob/v6.4.4/LICENSE
[jest-process]: https://github.com/jest-community/vscode-jest/blob/v6.4.4/src/JestProcessManagement/JestProcess.ts
[jest-reporter]: https://github.com/jest-community/vscode-jest/blob/v6.4.4/src/reporter.ts
[jest-results]: https://github.com/jest-community/vscode-jest/blob/v6.4.4/src/TestResults/TestResultProvider.ts
[pw-readme]: https://github.com/microsoft/playwright-vscode/blob/v1.1.20/README.md
[pw-license]: https://github.com/microsoft/playwright-vscode/blob/v1.1.20/LICENSE
[pw-contract]: https://github.com/microsoft/playwright-vscode/blob/v1.1.20/src/upstream/testServerInterface.ts
[pw-server]: https://github.com/microsoft/playwright-vscode/blob/v1.1.20/src/playwrightTestServer.ts
[pw-model]: https://github.com/microsoft/playwright-vscode/blob/v1.1.20/src/testModel.ts
