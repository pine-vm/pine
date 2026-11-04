# Elm Test Command Line Interface

Run Elm tests using the [`elm-explorations/test` package](https://github.com/elm-explorations/test)

Test dependency resolution supports `elm-explorations/test` versions 1.2.2, 2.2.0,
and 2.2.1 using the bundled test engine. The legacy `Expect.true` and `Expect.false`
helpers are included for 1.2.2 projects. Substituted packages still resolve only
explicitly supported versions, without silently falling back to upstream kernels.

The 1.2.2 compatibility implementation also provides `Test.Html.Query`,
`Test.Html.Selector`, and `Test.Html.Event`.
Test-scoped replacements for `elm/html` 1.0.0 and `elm/virtual-dom` 1.0.3/1.0.5 build
inspectable pure Elm trees, including keyed children, deferred lazy views, attributes,
styles, and mapped event handlers. Queries use the upstream selector and failure
reporting logic. These replacements inspect views; they do not render a browser DOM.
`Basics.pi` and `Basics.degrees` work during discovery and compiled execution.
The existing 2.x non-HTML engine retains its dependency set, so existing application
manifests do not need additional HTML dependency pins.

`Bitwise` operations normalize operands to 32-bit two's complement before operating,
mask shift counts to five bits, and sign-extend arithmetic right shifts. This fixes
CSS hash differences caused by negative or overflowing intermediate values, without
changing Pine's arbitrary-precision arithmetic outside `Bitwise`.

The bundled `Json.Encode` and `Json.Decode` replacements expose all APIs from
`elm/json` 1.1.4, including `Encode.set`, `Encode.dict`, and `Decode.oneOrMore`.
Encoding supports indentation (clamped to 0-10 spaces), last-value-wins object
keys, and `null` for nonfinite floats. Decoding preserves structured field/index
errors and supports JSON fractional and exponent notation. Pine retains its
arbitrary-precision integer and rational-float representation rather than
JavaScript's IEEE 754 rounding. Invalid JSON errors retain the original input;
their parser-specific diagnostic text can differ from JavaScript engines.

## Core discovery and editor navigation

`Pine.Core.Elm.Testing.ElmTestDiscovery.DiscoverTests` provides syntax-interpreter
discovery shared by the CLI and editor integrations. It evaluates test construction (including helpers,
computed labels and generated suites), then uses the resolved Elm test library to enumerate
leaves. Assertion and fuzz callbacks are not run during discovery. Both `--list-tests` and ordinary
`elm test` use this path; execution remains on PineVM. The
[versioned language-server protocol and VS Code Test Explorer](elm-testing-language-server-protocol.md)
reuse this discovery and execution machinery.

`result.Items` is a complete inventory in parent-before-child order, including
`describe` groups, empty groups, skipped tests and tests excluded by `Test.only`.
Each entry contains an ID, optional parent ID, listing metadata, discovery-root identity,
semantic kind (`Group`, `Unit`, `Fuzz`, `Todo`), effective selection
(`Included`, `Skipped`, `ExcludedByOnly`), inherited `IsOnly`, empty-group flag,
fuzz run count when applicable, and a required source location. These fields let
editor hosts choose group/test/skipped icons without inferring semantics from labels.
Groups whose children are all skipped are also marked skipped. `result.HasOnly`
records effective focus selection across roots.

`result.Tests` is the backwards-compatible selected, nonempty leaf view,
not the complete inventory. CLI listings also include skipped leaves from `Items`,
with a ` (skipped)` suffix and dim styling when color is enabled. Each file and group
in the `--list-tests` tree shows the number of included descendant tests in parentheses,
including skipped leaves. When filtering, these counts exclude filtered-out tests,
which are reported separately under each file and group. Selected leaves also have
their existing runner discovery index and exact execution filter. Phase timings
are available on the result.

`result.GetSourceLocation(testId)` returns the original module/source provenance and a
one-based, end-exclusive Elm syntax range for any inventory item, including groups and
skipped tests. Unknown IDs throw. Source locations address the
innermost observed construction expression: a generated test normally navigates to its
factory body, so several IDs can share one location.

Discovery preserves range-free AST node paths and projects them onto the corresponding CST,
following [`ElmSyntax.Path`](../implement/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Path.elm).
Parentheses are transparent; record fields are addressed by name and occurrence, not their
sorted AST positions. Results refer to the source set used for discovery, not live documents.
IDs identify a project-relative root and structural leaf/group ordinal, not a persistent
semantic identity: inserting or reordering entries can change them. Skipped and excluded
leaves retain inventory IDs, so toggling a sibling's skip status does not renumber them.
Editor hosts must manage document freshness.

`result.GetExecutionFilter(testId)` translates an ID to the existing runner's exact path or
discovery-ordinal selector for a selected leaf; groups and unselected items are rejected.
It must be used with the same project sources.

`result.PrepareExecution(testIds)` compiles selected runnable closures and their captured values
into independent Pine evaluations: an expression plus its environment value for each test ID.
Omitting IDs selects all runnable leaves; TODOs have no executable entry. Suite construction is
not replayed on PineVM. The CLI uses `ElmTestRunner.DiscoverAndRunTests` to discover, filter,
compile these entry points and run them with the existing worker, fuzz and result-rendering machinery.
Closure compilation uses the source compiler's operator lowering, including pipelines and
short-circuit Boolean operators, before creating resolved compiler IR.
The older `CompileAndRunTests` API retains compiler/PineVM discovery for compatibility and comparisons.

`Debug.log` is currently silent during test discovery and execution: its value is preserved
without producing log output. This also applies to captured, partially applied and higher-order loggers.

`Debug.todo` is supported in compiled test code, including first-class function values.
It fails only if execution reaches the call. A call reached while running a unit test,
fuzz property, or fuzz generator is reported as a failure of that test, including the
`Debug.todo` message; other tests continue running. Unexecuted branches do not fail tests.

Every scenario under `Pine.Core.Tests/TestData/Elm/CommandElmTest` has an
`expected-discovery.json` expectation. The scenario test compares the complete semantic
inventory and navigation data, excluding only elapsed times, and checks selected-listing
parity with compiler/PineVM discovery.

See the [implementation and performance comparison](../explore/2026-11-01-elm-editor-test-discovery-and-execution/syntax-discovery.md).

## Command-line options

```console
Arguments:
  <source>  Elm project directory or GitHub/GitLab tree URL. Defaults to the current directory.

Options:
  --color <Always|Auto|Never>              Color output: auto, always, or never. Overrides PINE_TERM_COLOR.
  --filter <EXPRESSION>                    Run tests matching the expression.
                                           Matching is case-insensitive. A plain term matches a substring of a file, group, or test name.
                                           Paths use / or \ on any OS: directories/filename/group descriptions/test name.
                                           Path segments match consecutively anywhere in the test path; the filename extension may be omitted.
                                           * matches zero or more characters within one segment; ** matches zero or more whole segments.
                                           Quote expressions containing spaces or wildcards. Conjunctive filters are not supported.

                                           Examples:
                                             --filter "convert concrete"
                                             --filter "tests/ConvertConcreteToAbstractTests/convert*/converts every*"
                                             --filter "ConvertConcreteToAbstractTests/**/converts every*"
                                             --filter "tests/*Tests.elm/**/converts*drops documentation"
                                           No matches: show the closest existing test paths. Use --list-tests to explore tests without running them.
                                           Prefix a full path with '=' for exact, case-sensitive matching, including literal wildcard characters.
                                           Use '#N' for a discovery ordinal when duplicate test paths cannot distinguish a test.
  --list-tests                             List tests without running them.
  --watch                                  Rerun tests on file changes.
  --workers <workers>                      Number of worker threads. Defaults to 11.
  --report-durations                       Show detailed durations, including compilation and test execution.
  --seed <seed>                            Initial unsigned 32-bit random seed for fuzz tests. Defaults to a new random seed.
  --fuzz <fuzz>                            Number of iterations per fuzz test. Must be positive. Defaults to 100. [default: 100]
  --offline                                Resolve packages using only local registry metadata and sources; make no network requests.
  --dependency-report <dependency-report>  Write the full dependency resolution report as JSON, including rejected branches and conflicts.
  --budget <budget>                        Shortcut setting both invocation and loop budgets; explicit per-kind options override it.
  --invocation-budget <invocation-budget>  Command-wide VM invocation budget for test execution.
  --loop-budget <loop-budget>              Command-wide backward-jump iteration budget for test execution.
  --timeout <timeout>                      Cooperative wall-clock timeout in seconds, including preparation.
  --max-stack-depth <max-stack-depth>      VM stack-depth safety limit. [default: 100_000]
  -?, -h, --help                           Show help and usage information
  -v, --verbose                            Use verbose output

Commands:
  profile <source>  Profile selected Elm tests in isolation and see where computation time is spent.
```

## Project sources

The source argument accepts a local project directory or a GitHub/GitLab tree URL. For example,
list the tests in this pinned project without checking out the repository:

```console
pine  elm  test  --list-tests  https://github.com/pine-vm/pine/tree/cc2c94d4c96a2794806b43c416fc7716bf86ab36/implement/Pine.Core.Tests/TestData/Elm/CommandElmTest/multiple-top-level-tests/input-app
```

This lists two tests from `tests/Tests.elm`: `first test` and `second group/second test`.
Unexposed declarations and exposed non-test values are not included:

```console
Loaded 2 project files in 2.01 seconds. Resolving dependencies and compiling Elm tests.
Available tests (2)
└── tests/Tests.elm (2)
    ├── first test
    └── second group (1)
        └── second test
```

Prefer a commit-pinned URL over a mutable branch when sharing a reproducible invocation.

Branch resolution uses the same disk cache as other Pine commands loading Git files or
directories.
Full commit IDs bypass branch resolution.

Remote loading requires network access and is rejected with `--offline`; use a local project
directory for offline operation. URLs containing credentials, query strings or fragments are not
accepted. Only the selected project subtree is downloaded. Remote manifests must keep their source
directories within that subtree; use a local checkout for projects that reference parent or sibling
source directories.

## Duration reporting

Use `--report-durations` to show independently measured timings using the instrumentation's
phase labels:

- `compilation`: source loading, dependency resolution, and test-root identification.
- `preparation`: syntax-based construction/discovery, filtering, and compilation of the selected runtime closures.
- `execution`: running the selected tests, including worker setup and result processing.

Both `preparation` and `execution` include a `VM calls (sum)` breakdown, measured around each
VM evaluation. Preparation uses the syntax interpreter, not a PineVM, so its VM-call duration is zero.
Execution includes evaluating the selected test runners. These are summed call durations, not exclusive phase durations:
parallel execution calls overlap, so their sum can exceed the execution wall-clock duration.

## Watch mode

```console
pine  elm  test  .  --watch  --report-durations
```

`--watch` runs the tests immediately, then keeps the command running and reruns
after changes in the local project directory, including subdirectories.

Rapid changes are coalesced. Test runs are sequential; changes during a run
schedule a subsequent run rather than starting a competing one. A failed test
or compilation error does not stop watching, so fixing the source triggers
another attempt. Press Ctrl+C to stop.

With `--report-durations`, each completed run prints its own timings. They describe the latest run, not time spent waiting for edits or accumulated time since watch mode started.

Watch mode retains source/VM caches, a generational invocation-result cache, and a learned caching
policy across reruns. Successful test execution evaluations feed bounded
background learning within the same process. Learning uses isolated PineVM instances and separate
caches; it does not consume the test run's execution budget.

The invocation cache retains up to three generations, rotating after completed evaluations when the
newest generation reaches 5,000 entries. Hits promote older entries. Admission and lookup use an
expression policy learned from an initially empty set; policy changes keep existing entries, and
each evaluation finishes with its captured policy. These limits govern invocation results, not
source-parsing or compilation metadata.

Learning retains eight recent evaluations and up to sixteen pending samples, tries up to four
policy changes per round, and runs no more often than every thirty seconds with a two-minute
cooperative deadline. Policy-switch diagnostics include content identifiers, additions/removals,
estimated costs, and churn counters. Stopping watch mode cancels its learning; training errors do
not stop tests or replace the last successful policy. One-shot runs and diagnostic profiles do not
enable watch-session learning.

Watch mode requires a local project directory; remote tree URLs are not watchable. It is an ordinary `elm test` option, not a profiling mode.
`--dependency-report` cannot be combined with `--watch`, because writing reports could itself trigger repeated reruns. Other ordinary test options apply to each run; execution budgets and timeouts start fresh for each run. Use `--seed` to keep the fuzz seed fixed across reruns; without it each run chooses a fresh seed.

## Profiling and instrumentation

Use `elm test profile` to investigate hangs, unexpected work, or expensive computations.
Profiling executes directly on that command and accepts any number of selected tests.
Omit `--filter` to profile the whole suite, or use a filter to select one test or a group.

```console
pine  elm  test  profile  .  --budget 1000000  --loop-budget 10000  --include-inputs  --include-locals  --sort Loops  --output profile.json
```

Ordinary test options other than `--watch` are accepted, including source, filtering/listing, seed, fuzz count,
offline resolution, dependency reports, color and duration reporting. Unlike ordinary `elm test`,
profiling always executes selected tests serially, regardless of `--workers`. Each test gets a fresh
VM and fresh invocation, parsing, compilation, encoding and reduction caches, separate from both
preparation and other tests. This prevents cache warming and scheduling from changing evaluation
work counters. For fuzz tests, use a fixed `--seed` and `--fuzz` to reproduce the work.

Terminal statistics, expression rankings and the JSON report aggregate work across all selected
tests. Preparation is timed once per command, but VM work counters and invocation caching now
cover execution only. Execution counters sum the independently profiled tests. JSON `SelectedTests`
lists all selected runnable test paths in discovery order; `SelectedTest` remains populated only
for a single-test selection. Invocation/loop budgets and the timeout still apply to the entire
command's execution, not independently to each test. The cooperative wall-clock timeout also
covers source preparation and syntax discovery. Timeouts and cancellation can
stop at different points in the computation; elapsed time and periodic stack samples are not
deterministic evaluation work counters.

`Summary.Counters` and each phase's `Total` are `EvalWorkCounters`, excluding cached
metadata derivations. `Summary.CachedWorkCounters` aggregates the separate cached
work in each phase. Currently these count template recognition attempts and their
direct-interpreter work, not all parsing or compilation. With shared caches,
cached work attribution depends on cache state and worker races, so it is not a
deterministic per-VM execution measure.

Diagnostic options include:

| Option | Meaning |
| --- | --- |
| `--budget` | Shortcut for both invocation and loop budgets; specific options override it. Shared with ordinary execution. |
| `--invocation-budget`, `--loop-budget` | Command-wide VM invocation and backward-jump budgets. |
| `--timeout` | Cooperative wall-clock deadline in seconds. Resolution observes cancellation; synchronous compilation/native leaves stop at their next cancellation boundary. |
| `--interval` | Status and stack sampling interval in seconds; `0` disables periodic sampling. |
| `--max-stack-depth`, `--stack-depth` | VM safety limit and maximum recorded stack frames, respectively. |
| `--top`, `--sort` | Terminal ranking size and sort metric: `Invocations`, `Instructions`, `Loops`. JSON always contains all recorded expressions. |
| `--include-inputs`, `--include-locals` | Query and record input paths/values and VM locals in sampled or stopped frames. Lazy values are described but never forced. |
| `--expressions`, `--no-stacks` | Show expression descriptions or hide terminal stack traces; neither discards JSON details. |
| `--no-precompiled-leaves`, `--no-invocation-cache` | Inspect pure Pine work or uncached work. |
| `--no-tail-recursion`, `--no-reduction` | Disable tail-call frame replacement or VM expression reduction. Compiler-generated backward jumps can still occur. |
| `--output` | JSON report path. Default: a unique file under `elm-stuff/pine/test-profiles` in the local project, or in the invocation's working directory for a remote project. |

## Fuzz testing

`Test.fuzz`, `fuzz2`, `fuzz3` and `fuzzWith` run using the bundled Elm generation and shrinking
engine, including dependent fuzzers such as `Fuzz.andThen`. A failing generated example is simplified
by replaying and reducing its random-choice tape, which preserves the generator's constraints.
The output includes the final counterexample in Elm notation.

The controls match `elm-test-rs`:

```console
pine  elm  test  .  --seed 597517184  --fuzz 100
```

`--fuzz` defaults to 100 and accepts a positive unsigned 32-bit integer. `--seed` accepts an
unsigned 32-bit integer, including zero; omitting it selects a new seed once per invocation.
`Test.fuzzWith` can override the run count for one property. Distribution expectations may need
additional examples beyond the configured count.

Seed distribution happens before filtering or worker scheduling, so changing `--workers` or
isolating a property with `--filter` does not change its generated examples. Fuzz runs print a
reproduction command with the effective seed and count. Reproduction assumes unchanged source
ordering, package implementations and compiler; identical CLI flags do not promise identical
inputs between Pine and JavaScript runners.

`--list-tests` includes fuzz properties without running them. `Test.only` and `Test.skip` follow
upstream selection semantics and make a run incomplete, even when the selected properties pass.
Invalid generators and exhausted rejection filters produce explicit failures. VM evaluation errors,
including execution-budget exhaustion, fail the individual test; an interrupted engine is not
reported as having successfully completed shrinking.

The API accepts `ElmFuzzOptions` separately from dependency configuration. Completed runs retain
the effective settings and resolution report; each fuzz result includes the assigned seed state,
actual/requested counts, failing iteration, original and simplified inputs, random-choice tapes,
distribution metadata and any evaluation error. `completed.ToDebugJson()` exports these diagnostics.
Choice tapes are retained for failed examples; the full unsuccessful shrink-attempt history is not collected.

Test-root values are materialized with the stack-safe Pine VM. The float generator's fixed exponent
permutation uses an equivalent closed-form mapping rather than allocating and sorting a lookup table.

Bundled sources, licenses, upstream commit identities and documented adaptations are under
`implement/Pine.Core/Elm/Testing/elm-test` and `elm-random`. No network access or Node process is
needed to load or execute the test/fuzz replacements.
