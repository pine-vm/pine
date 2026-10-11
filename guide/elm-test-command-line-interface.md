# Elm Test Command Line Interface

Run Elm tests using the [`elm-explorations/test` package](https://github.com/elm-explorations/test)

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
  --invocation-budget <invocation-budget>  Command-wide VM invocation budget, including preparation.
  --loop-budget <loop-budget>              Command-wide backward-jump iteration budget, including preparation.
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
└── tests/Tests.elm
    ├── first test
    └── second group
        └── second test
```

Prefer a commit-pinned URL over a mutable branch when sharing a reproducible invocation.

Remote loading requires network access and is rejected with `--offline`; use a local project
directory for offline operation. URLs containing credentials, query strings or fragments are not
accepted. Only the selected project subtree is downloaded. Remote manifests must keep their source
directories within that subtree; use a local checkout for projects that reference parent or sibling
source directories.

## Duration reporting

Use `--report-durations` to show independently measured timings using the instrumentation's
phase labels:

- `compilation`: source loading, dependency resolution, and compilation of the test environment.
- `preparation`: constructing test values, preparing runners, discovering tests, and filtering.
- `execution`: running the selected tests, including worker setup and result processing.

Both `preparation` and `execution` include a `VM calls (sum)` breakdown, measured around each
VM evaluation. Preparation includes evaluating exposed zero-parameter declarations (even
non-test values) and calling the test bridge's `prepare` function. Execution includes evaluating
the selected test runners. These are summed call durations, not exclusive phase durations:
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
preparation and other tests. This prevents cache warming and scheduling from changing performance
counters. For fuzz tests, use a fixed `--seed` and `--fuzz` to reproduce the work.

Terminal statistics, expression rankings and the JSON report aggregate work across all selected
tests. Preparation is counted once per command and remains separate from execution in
`CountersByPhase`; execution counters sum the independently profiled tests. JSON `SelectedTests`
lists all selected runnable test paths in discovery order; `SelectedTest` remains populated only
for a single-test selection. Invocation/loop budgets and the timeout still apply to the entire
command, including preparation, not independently to each test. Timeouts and cancellation can
stop at different points in the computation; elapsed time and periodic stack samples are not
deterministic performance counters.

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
