# elm-review TaskTest: warm incremental rerun profile

Date: 2026-11-04.

## Current branch status

The branch now implements the discovery bookkeeping fixes, removal of settings-triggered
LSP rediscovery, operator/semantic transformation reuse and pruning, and
**syntax-preserving canonical/prepared-declaration caching**. Historical measurements
below deliberately retain their original baselines; descriptions of the original
`cache: null` path and canonicalization guard are not descriptions of the current code.

| Scope | Implementation status |
|---|---|
| Immutable discovery snapshot and linear selector index | Implemented; section D |
| LSP settings/preparation/empty-selection overhead | Implemented; language-service follow-up |
| Operator lowering and reachable semantic IR conversion | Implemented; memory defaults and opt-in file-store cascade |
| Syntax-preserving canonical/prepared declarations | Implemented; measured follow-up below |
| Canonical observed-input manifests on disk | Not implemented; memory entries validate observations |
| Prepared-declaration persistence | Implemented opt-in, using versioned SHA-256 and existing codecs |
| Root construction, captured executable graphs, SCC emission and test-result caches | Still future work |
| Retained-byte budgets, persistent reclamation, long-session p95 | Still future work / not established by these measurements |

Fresh three-edit comparisons against the preceding transformation-cache implementation
measured median core latency **14.69 -> 12.48 s (15.0%)**, and actual LSP
discover-plus-run **15.62 -> 13.51 s (13.5%)**. Median allocation fell
**6.29 -> 5.73 GB** and **6.92 -> 6.33 GB**, respectively. Core interpreter
preparation fell **1.749 -> 0.669 s (61.8%)**. Each edit published just one new
canonical entry and one new prepared entry. These are sequential, shared-host
measurements, not a long-session p95 or a randomized causal estimate.

The remaining high-value discovery opportunity is **root-construction reuse**, with
explicit construction-versus-execution dependencies and fresh source provenance.
Prepared syntax reuse does not cache test-tree evaluation or executable captures.

## Original profiling summary and recommended order (historical)

**Optimize discovery bookkeeping before optimizing assertion execution or adding a
large persistent cache.** Retained VM caches do not avoid rebuilding the discovery
program, evaluating candidate roots, generating selectors, or exporting closures.

In the original three unprofiled, same-process edited reruns, discovery took **33.1-38.2 seconds**
and allocated **27.83 GB per rerun**. Compiling the 79 selected runners then took
7.2-10.9 seconds and allocated another 5.58 GB. Execution took 4.9-8.6 seconds.
Every measured edit produced **78 passes and exactly one expected failure**.

Two particularly actionable causes emerged:

1. A mutable prepared-declaration dictionary is converted to a new immutable
   dictionary on every interpreter invocation. This defeats the existing
   dictionary-identity-based resolver-index cache.
2. Exact-selector generation scans all discovered leaves for each leaf and
   repeatedly reconstructs both full paths: quadratic work before filtering.

A temporary combined experiment tested an immutable program snapshot and a
single full-path frequency index. It reduced median edited-rerun latency from
**53.72 to 24.03 seconds (55.3%)** and median total allocation from **36.45 to
10.87 GB (70.2%)**, without adding a persistent or cross-run discovery cache.
The experimental patch was removed after profiling; the subsequent explicit
implementation request is covered in section D below. The report and
[reusable profiling tool](../2026-11-04-elm-review-warm-rerun/README.md) are retained
so interventions can be measured against the same workload later.

For incremental discovery caching, use **declaration-level artifacts and explicit
dependency manifests**, not a cache keyed by the complete project fingerprint.
Separate dependencies needed to **construct the inventory** from dependencies
needed to **execute captured callbacks**. Otherwise a productive-code edit either
invalidates too much or, worse, reuses stale executable closures.

## Workload, edit, and scope

Original profiling baseline, not the current branch:
`20b86958052e5fdf4f595d67154b8ab434ef3993`.
The workload is the pinned elm-review source at
`https://github.com/jfmengels/elm-review/tree/73dfa9a03352a19fde51ffe3e6ea02bdb0f7010f`.
Its checkout contains 370 tracked files.

The selected suite is `tests/Simplify/TaskTest.elm`, using `--filter=TaskTest.elm`,
seed 12345, and the default 100 fuzz runs. All 79 selected tests are run; the
other 4,606 leaves are excluded by the filter, not by dependency-based selection.

The mutation is in **`Simplify.listOfWrapperSequenceChecks`**, at approximately
line 16328 of `tests/Simplify.elm`:

```diff
- \res -> qualifiedToString (qualify wrapper.wrap.fn res) ++ " []"
+ \res -> qualifiedToString (qualify wrapper.wrap.fn res) ++ " [ ]"
```

This is the rule implementation exercised by the tests, **not an assertion,
expectation, test label, or test callback edit**. In this repository the example
Simplify rule itself lives under `tests/`; it is not a `src/Review/*.elm` edit.
This distinction matters when reproducing the workload.

The changed diagnostic makes this selected test fail:

```text
Task / Task.sequence / should replace Task.sequence [] by Task.succeed []
UNEXPECTED ERROR MESSAGE
expected: Task.sequence on [] will result in Task.succeed []
actual:   Task.sequence on [] will result in Task.succeed [ ]
```

Subsequent measurements use two and then three spaces inside the brackets.
These are **new source contents on every rerun**, rather than repeatedly running
an unchanged project or returning to an already cached source version.
The unfiltered suite was not executed: there may be additional affected tests
outside TaskTest.

Only edited runs after an initial successful run are analyzed. Initial runs
prime the process and caches; they are excluded from the performance tables.
Remote loading, process startup, prebuild, restore, and build are also excluded.

### Host and controls

| Property | Value |
|---|---|
| OS | Ubuntu 24.04, Linux x64 |
| CPU presented to process | 2 logical CPUs, AMD EPYC 9V74 |
| Memory | Approximately 7.5 GiB, no swap |
| SDK / runtime | .NET SDK 10.0.403 / runtime 10.0.14 |
| Build | Release |
| Test execution | One worker; also the CLI default on this two-CPU host |
| CLR GC configuration | No explicit server-GC override in the CLI runtimeconfig |
| Diagnostic tools | dotnet-trace and dotnet-counters 10.0.750501; TraceEvent 3.1.23 |

This is not the Windows machine from the question. Absolute durations and
parallel VM-duration sums must not be compared as cross-machine speedups.
The environment is shared; three repetitions show the scale of the problem,
not a statistically established latency distribution or a long-session p95.

## Measurements

Sections A-C describe the original baseline and temporary experiment. Section D
and the subsequent implemented follow-ups describe successive interventions, not
one pooled benchmark. The latest canonical/prepared-cache comparison has its own
fresh baseline and samples below.

### A. Actual CLI watch session, including its background learner

The stock Release CLI was started on the local pinned checkout:

```sh
implement/pine/bin/Release/net10.0/pine elm test /path/to/elm-review \
  --filter=TaskTest.elm --report-durations --seed=12345 --watch --color=never
```

After its initial passing run completed, external diagnostics were attached and
the rule declaration was edited. The same CLI process and watch-owned caches
were retained. There was one filesystem-triggered edited rerun.

| Reported phase | Edited warm rerun |
|---|---:|
| Total | 81,753 ms |
| Compilation | 1,538 ms |
| Preparation | 66,564 ms |
| Preparation VM calls, summed | 0 ms |
| Execution | 13,651 ms |
| Execution VM calls, summed | 13,041 ms |
| Outcome | 78 passed, 1 failed |

The watch cache uses generational invocation storage and continuous cache-policy
learning. Its log shows policies growing from zero to one, two, then four
expressions. Logged learning rounds took approximately **11.1, 12.5, and
10.5 seconds**. The first completed before the edit; the latter two overlapped
the edited run. Work from the priming run can therefore continue into a warm
rerun even when the foreground run itself has completed.

The diagnostic capture lasted 100 seconds and included the edited run plus
surrounding watch/learner activity. It did **not** include the initial foreground
run. Its aggregate counters are capture-window measurements, not phase timings:

| Counter | Capture-window observation |
|---|---:|
| Process CPU, user + system | 107.75 CPU-seconds |
| Allocated bytes, runtime counter | 47.28 GB |
| Allocation-tick weighted estimate | 47.36 GB |
| Runtime GC pause counter | 13.14 seconds |
| Gen0 / Gen1 / Gen2 counter increments | 2,146 / 363 / 14 |
| Working-set range | 1.67-3.16 GB |
| GC committed-memory range | 1.52-3.07 GB |
| JIT compilation counter | 2.72 seconds |
| Monitor contention counter | 0 |
| Thread-pool queue-length observations | 0 |

The diagnostics themselves have overhead. In particular, do not interpret the
81.8-second traced run as an uninstrumented benchmark or attribute its difference
from the following runs solely to learning.
The event trace records about 14.77 seconds of `SuspendOther` EE suspensions;
these are distinct from GC suspensions and include profiling effects. Summing
every suspend-to-restart interval would overstate GC pause time.

### B. Unprofiled, retained-cache phase measurements

A separate Release harness used the same public core pipeline, serial execution,
and one retained `ElmTestRunCacheSession(generationOptions: new())`.
It used **no background learner**, matching the testing service's cache policy
rather than the CLI watch policy. Source parsing was retained as in CLI watch,
and dependency resolution was offline against the already populated package
cache. Native leaves were the core session defaults, as in the testing service;
the CLI has an additional aggregate-leaf composition.

This isolates the cost of the shared pipeline. It is **not an end-to-end LSP
latency measurement**: snapshot capture, protocol work, and the LSP's additional
rediscovery described later are not included.

Each phase records `Stopwatch`, process CPU delta, `GC.GetTotalAllocatedBytes`,
collection-count deltas, `GC.GetTotalPauseDuration`, and end-of-phase working set.
No CPU trace or counter collector was attached to these measured reruns.

| Phase, milliseconds | Edit 1 | Edit 2 | Edit 3 |
|---|---:|---:|---:|
| Load, resolve, parse, build identities | 1,655 | 741 | 761 |
| Discover inventory | 37,942 | 38,154 | 33,145 |
| Compile selected closures to Pine | 7,723 | 10,923 | 7,156 |
| Execute 79 tests | 6,295 | 8,591 | 4,948 |
| Total, including harness bookkeeping | **53,716** | **58,439** | **46,020** |
| Expected outcome | 78 / 1 | 78 / 1 | 78 / 1 |

Median total: **53.72 seconds**. The discovery sub-timers distinguish preparation
of interpreter syntax from construction, seeding, and inventory projection:

| Discovery sub-timer, milliseconds | Edit 1 | Edit 2 | Edit 3 |
|---|---:|---:|---:|
| Interpreter preparation | 2,120 | 2,430 | 2,402 |
| Remaining discovery | 35,821 | 35,724 | 30,742 |

The second row includes repeated per-root interpreter setup and selector
generation; it is not all time spent executing Elm test constructors.

| Allocation / GC measurement | Edit 1 | Edit 2 | Edit 3 |
|---|---:|---:|---:|
| Load/resolve/parse allocation, GB | 0.416 | 0.414 | 0.414 |
| Discovery allocation, GB | **27.831** | **27.830** | **27.830** |
| Closure-compilation allocation, GB | 5.582 | 5.581 | 5.581 |
| Execution allocation, GB | 2.621 | 2.604 | 2.632 |
| Total allocation, GB | **36.450** | **36.429** | **36.457** |
| Discovery GC pause, seconds | 8.332 | 8.923 | 7.941 |
| Total GC pause, seconds | 10.849 | 12.210 | 9.869 |
| Total process CPU, seconds | 59.36 | 63.70 | 49.09 |

GB here means decimal bytes / 1,000,000,000. GC pause is already included in wall
time; it must not be added to it. Process CPU can exceed wall time because GC and
other runtime work can use another CPU.

For example, Edit 1's discovery caused **1,574 Gen0, 242 Gen1, and 3 Gen2**
collections. End-of-phase working set across the measured baseline phases ranged
from 1.80 to 2.52 GB. These samples are not a retained-object census or proof of a
memory leak.

### C. Temporary combined experiment

Only two changes were made to discovery for this experiment:

```csharp
// Before invoking any roots; reuse the same immutable dictionary for every call.
var program = new Interpreter.Prepared(prepared.ToImmutableDictionary());

// Before projecting the inventory; preserve ordinal fallback for duplicate paths.
var fullPathCounts =
    listedLeaves.GroupBy(test => test.FullPath, StringComparer.Ordinal)
    .ToDictionary(group => group.Key, group => group.Count(), StringComparer.Ordinal);
```

Selector projection then uses the count to choose `"=" + fullPath` or
`"#" + discoveryIndex`, instead of calling the full-inventory scanning helper.
No discovery-result cache, declaration cache, execution cache, filtering shortcut,
or changed worker count was introduced.

The experiment was rebuilt, primed in a fresh long-lived process, and subjected
to the same three successive productive edits, again without attached profilers.

| Phase, milliseconds | Edit 1 | Edit 2 | Edit 3 |
|---|---:|---:|---:|
| Load, resolve, parse, build identities | 2,056 | 707 | 629 |
| Discover inventory | **7,836** | **5,775** | **5,764** |
| Compile selected closures to Pine | 8,050 | 8,146 | 10,749 |
| Execute 79 tests | 5,943 | 4,832 | 7,537 |
| Total | **24,026** | **19,484** | **24,701** |
| Outcome | 78 / 1 | 78 / 1 | 78 / 1 |

| Median across three edited reruns | Baseline | Combined experiment | Reduction |
|---|---:|---:|---:|
| Total wall time | 53.72 s | 24.03 s | **55.3%** |
| Discovery wall time | 37.94 s | 5.77 s | **84.8%** |
| Total allocation | 36.45 GB | 10.87 GB | **70.2%** |
| Discovery allocation | 27.83 GB | 2.25 GB | **91.9%** |

Total GC pause per edited run fell from 9.87-12.21 seconds to 2.99-4.05
seconds. Discovery's GC pause fell to 1.16-1.47 seconds.
The inventory still contained 5,214 entries, and all 79 tests still executed.
The per-run shared-cache entry counts, rotations, promotions, and hit/miss
statistics matched the baseline, supporting that this removed discovery work
rather than increasing execution-cache reuse or reducing the executed suite.

The two processes were primed independently and run sequentially, not in an
alternating randomized benchmark. The large allocation reduction is particularly
strong evidence; smaller timing differences in compilation/execution should be
treated as run-to-run variation, not as effects attributable to this patch.

This is a combined experiment, not an attribution of independent speedups to
each change. It validates this fixture and mutation, not every possible selector,
concurrency, or discovery edge case. The patch was removed rather than silently
shipping an optimization as part of a profiling/report task.

### D. Production implementation follow-up

After the separate implementation request, discovery now creates one immutable
prepared-declaration snapshot and builds one ordinal, full-path frequency index
before projecting inventory entries. The runner's diagnostic/suggestion selectors
also reuse one frequency index instead of rescanning all discovered tests.
The existing public `ExactSelector` API remains available.

Frequency keys are the existing `ListedTest.FullPath` strings, not structural
`ListedTest` equality: normalized file separators and description segments containing
slashes can produce duplicate strings. Comparisons remain case-sensitive and literal.
Discovery counts the same executable-leaf population as before, including roots
excluded by global `Test.only`, but excluding skipped leaves and empty groups.
Selectors for ambiguous paths retain their original discovery ordinals.
No cross-run or persistent cache was added.

The retained profiler was rebuilt against this implementation and run without
attached diagnostics, using the same pinned fixture, retained generational session,
serial execution, offline dependency resolution, seed, fuzz count, excluded priming,
and three successive productive edits:

| Phase, milliseconds | Edit 1 | Edit 2 | Edit 3 |
|---|---:|---:|---:|
| Load, resolve, parse, build identities | 1,238 | 1,103 | 765 |
| Discover inventory | **6,292** | **5,509** | **5,783** |
| Compile selected closures to Pine | 7,256 | 7,778 | 7,188 |
| Execute 79 tests | 5,045 | 6,149 | 5,185 |
| Total | **19,860** | **20,561** | **18,947** |
| Outcome | 78 / 1 | 78 / 1 | 78 / 1 |

| Median across three edited reruns | Original baseline | Implementation | Reduction |
|---|---:|---:|---:|
| Total wall time | 53.72 s | 19.86 s | **63.0%** |
| Discovery wall time | 37.94 s | 5.78 s | **84.8%** |
| Total allocation | 36.45 GB | 10.87 GB | **70.2%** |
| Discovery allocation | 27.83 GB | 2.24 GB | **91.9%** |

The inventory remained 5,214 entries, with 1,009 roots and 422 modules.
Each edited run produced only the specified failing test. Declaration/reverse-tree
counts, execution-cache before/after snapshots, and reduced-expression entry counts
matched the original baseline. The fixture was restored byte-for-byte.
The host/runtime and GC mode were unchanged; these are sequential measurements,
not a randomized comparison. The additional wall-time improvement over section C
must not be attributed to the small implementation differences; discovery time and
allocation reproduce the experiment's main result.

Regression coverage checks normalized and colliding literal paths, case sensitivity,
duplicate ordinals, single-pass frequency construction, global-only exclusion,
skips, and empty groups. Existing discovery/navigation, compiler-vs-syntax fixture,
runner/instrumentation, and language-service testing coverage also passed.

## Where preparation time went in the original captured baseline

### Phase names conceal important work

In the syntax-discovery path of
[`ElmTestRunner`](../../implement/Pine.Core/Elm/Testing/ElmTestRunner.cs),
the reported "compilation" scope mostly covers project loading, test-root
enumeration, and `PrepareForDeclarationDemandAsync`.

The "preparation" scope includes **both** `ElmTestDiscovery.DiscoverTests` and
`syntaxDiscovery.PrepareExecution`, which exports captured callbacks and compiles
them to Pine. The source-to-Pine work is therefore not all in the displayed
"compilation" number.

Preparation VM duration is zero because discovery uses the **Elm syntax
interpreter**, not `IPineVM.EvaluateExpression`. Closure export and compilation
also run outside that VM timer. Zero is expected, not evidence that preparation
is waiting or cheap.

`elm test profile` is unsuitable for reproducing this warm scenario: diagnostic
profiles reject a supplied cache session and isolate test execution caches.
External attachment to an already warmed process preserves the real cache path.

### Sampled stack evidence

The stock watch trace identifies the following hot paths. Durations are
**sampled managed-stack coverage**, with recursive appearances unioned within
each stack interval; they are not instrumented method timings or invocation
counts. Nested rows must not be added.

| Hot path | Sampled coverage | Interpretation |
|---|---:|---|
| Default `BuildResolvers(prepared declarations)` | 23.16 s | Repeated program snapshot and resolver setup |
| `PreparedDeclarationResolverIndex` construction | 17.04 s | Nested largely within resolver setup |
| `BuildInfixOperatorMap` | 2.23 s | Repeated whole-program setup |
| Discovery inventory projection lambda | 20.79 s | Dominated by exact-selector path construction/scanning |
| Demand canonicalization | 1.14 s | Real work, but not the largest first target |
| Closure export/compilation | 9.97 s | Selected runner preparation |
| `BuiltinOperatorLowering` | 4.59 s | Nested in closure preparation |
| `CompilerModel.FromCanonicalSyntax` | 3.76 s | Nested in closure preparation |
| `CompilerModel.Compilation.Compile` | 0.91 s | Includes SCC scheduling and emission |
| AST-path-to-CST `Resolve` | 0.006 s | Source-path traversal itself is not a significant hotspot here |

The trace's synthetic `CPU_TIME` labels are **not a trustworthy process CPU
accounting source on every thread**. For example, the finalizer thread appears
to cover about 70 seconds, including runtime polling/array-pool callbacks.
Summing exported per-thread intervals would misleadingly produce about
172 CPU-seconds. Use the actual 107.75-second process CPU counter instead.
Waiting support threads and that finalizer artifact are not ranked as
application bottlenecks.

The learner accounts for approximately **25.76 seconds of sampled stack
coverage** in the capture. This includes activity beyond the foreground rerun
and overlaps foreground work; it must not be added to foreground wall time.
The logged replay-cost proxy improved from 24,445,689 to 24,119,346 across the
first three policy switches, about 1.3%. That is not a measured wall-time saving,
but it motivates recording learning cost and realized foreground benefit
together before spending more CPU on additional candidates.

### Allocation attribution

Allocation-tick events, attributed by their captured stacks, give this
approximate breakdown for the stock watch capture:

| Stack category | Weighted allocation estimate |
|---|---:|
| Inventory projection / selectors | **21.36 GB** |
| Resolver setup | **4.26 GB** |
| Other discovery | 2.28 GB |
| Selected closure compilation | 5.55 GB |
| Test execution | 5.25 GB |
| Background learner | 8.30 GB |
| Other | 0.36 GB |

These are allocation-volume estimates, not live sizes. Sampling charges an
allocation quantum to the observed allocation type/stack, so small differences
from runtime totals are expected.

Across all stacks, strings account for about **14.13 GB** of sampled allocations.
Inside inventory projection alone, strings account for **11.86 GB**;
append/concatenation iterators, string arrays, and enumerators account for much
of the remainder. Resolver setup allocates candidate arrays, resolver
dictionaries, immutable-map nodes, function candidates, and enumerators.

### Root causes in the original profiling baseline

These are the captured causes before the implemented follow-ups, with links to
the files that now contain their replacements.

**Resolver-cache identity is lost.**
[`BuildResolvers`](../../implement/Pine.Core/Elm/Syntax/Interpreter/ElmSyntaxInterpreter.cs)
uses an existing immutable dictionary, or converts the supplied dictionary with
`ToImmutableDictionary`. Discovery builds a mutable `Dictionary` and places it
in `Interpreter.Prepared`. Every root interpretation, and every bridge
interpretation for a Test-valued root, repeats that conversion.

The resolver-index cache in
[`ElmSyntaxInterpreter.DeclarationResolverIndex.cs`](../../implement/Pine.Core/Elm/Syntax/Interpreter/ElmSyntaxInterpreter.DeclarationResolverIndex.cs)
is a `ConditionalWeakTable` keyed by dictionary identity. A newly allocated
snapshot is a new key even when all declarations are equal. This is repeated
setup within **one** discovery, before considering reuse across edits.

The immutable-snapshot experiment fixes that identity mismatch. A further
prepared-program execution context could retain the resolver, builtin-alias
map, and infix map once per immutable program; do not retain cancellation state,
observers, or run-specific diagnostics in that context.

**Selector generation is quadratic.**
[`ElmTestFilter.ExactSelector`](../../implement/Pine.Core/Elm/Testing/ElmTestFilter.cs)
calls `discovered.Count(predicate)` for every leaf. The predicate reads both
`item.FullPath` and `test.FullPath`, and
[`ListedTest.FullPath`](../../implement/Pine.Core/Elm/Testing/ElmTestRunner.cs)
constructs the path on every access using concatenation/append enumeration.

With 4,685 leaves, this is **21,949,225 pair comparisons** and approximately
43.9 million full-path constructions, before the file filter discards 4,606
tests. A frequency index changes this to a linear inventory pass plus lookups,
without changing global duplicate handling.

**Whole-suite lowering precedes selected-closure reachability pruning.**
[`PineClosureCompiler.Compile`](../../implement/Pine.Core/Elm/Syntax/Interpreter/ElmSyntaxInterpreter.PineCompilation.cs)
combines the canonical source declarations with generated closures, lowers
operators, converts to compiler IR, and lambda-lifts before restricting the
program to reachable functions.

Consequently, a filter selecting 79 tests does not prevent transformation of
the larger canonical environment. The profile suggests optimizing/caching these
earlier transformations before expecting SCC-emission caching alone to remove
most of the remaining 7-11 seconds.

## What survived edits in the original baseline

The harness observed:

| Structural quantity | Value |
|---|---:|
| Resolved compiler modules | 422 |
| Exposed zero-argument candidate roots | 1,009 |
| Demanded canonical declarations | 6,345 |
| Direct canonical dependency edges | 37,087 |
| Inventory entries, including groups | 5,214 |
| Runnable leaves before the filter | 4,685 |
| Selected leaves | 79 |
| Source-parse cache entries, primed -> after three edits | 422 -> 425 |
| Session declaration-canonicalization cache entries | **0 throughout** |
| VM reduced-expression entries, primed -> after three edits | 280,197 -> 336,840 |

Candidate roots are not all Test values: exposed zero-argument helper values
are also evaluated to find out whether they are tests.

The source-text parsing cache reused unchanged sources. But original syntax discovery
explicitly called canonicalization with `cache: null`. Merely passing the then-existing
session declaration cache was not valid:
[`Canonicalization.Declarations.cs`](../../implement/Pine.Core/Elm/ElmCompilerInDotnet/Canonicalization.Declarations.cs)
rejected a cache when `preserveOperatorApplications` was true. The current branch
replaces that restriction with mode-aware keys and marker-budgeted discovery
caches; the zero-entry row above is strictly historical.

VM invocation statistics are execution statistics, **not discovery cache hits**:

| Per edited rerun | Edit 1 | Edit 2 | Edit 3 |
|---|---:|---:|---:|
| Full-key invocation lookups | 151,092 | 148,283 | 155,123 |
| Hits, including local-buffer/shared hits | 20,321 | 20,025 | 20,981 |
| Hit rate | 13.45% | 13.50% | 13.53% |
| Successful observed admissions | 19,273 | 18,737 | 19,379 |
| Shared unique entries after run | 10,430 | 12,068 | 14,062 |

The session uses up to three generations with rotation requested at 5,000 newest
entries. This is a rotation policy, not a hard maximum-entry cap. Cumulative
rotations went from 3 after priming to 13 after the three edits.
Do not confuse physical generation slots, unique shared entries, and VM-facing
admissions; they count different boundaries.

These measurements show useful VM reuse but not high-level incremental
discovery. They do not prove why each VM lookup missed. Record expression/input
identity changes, eviction, and admission decisions separately before changing
the cache policy.

### Extra language-service work in the original baseline

[`LanguageServerTestingService`](../../implement/Pine.Core/Elm/LanguageServer/LanguageServerTestingService.cs)
cached `Snapshot.Discovery` within a snapshot. A new edited snapshot did not
reuse another snapshot's discovery artifacts.

Moreover, the run path obtained that inventory and then called
`inventory.WithExecutionSettings(settings, token)`. Its implementation in
[`ElmTestDiscoveryResult`](../../implement/Pine.Core/Elm/Testing/ElmTestDiscovery.cs)
called `DiscoverTests(build, testRoots, token, settings)` again. It avoided source
resolution on this second pass, but repeated canonicalization, preparation, root
construction, seeding, and inventory projection.

Thus a newly discovered edited LSP snapshot could incur **two discovery passes**
before preparing execution. Even a previously discovered snapshot repeated the
settings pass when run. This was a code-path finding, not a separately measured
LSP duration. Avoid estimating end-to-end LSP time by simply copying the harness
numbers.

The implemented follow-up below retains execution settings and separates
seed/run-count-dependent preparation from settings-independent inventory.
Seed changes no longer replay canonicalization and construction of every root.

#### Implemented follow-up and language-service measurements

The preceding paragraphs describe the original code path. The follow-up
implementation removes that second discovery pass rather than merely caching
another complete discovery result for every seed:

- The constructed inventory retains its original runners and the Test values
  for fuzz-bearing roots. Settings changes do not repeat resolution,
  canonicalization, suite construction, source association, or selector projection.
- `WithExecutionSettings` returns the same result for identical settings and
  shares preparation code when settings differ. `PrepareExecution` also accepts
  per-run settings and a current-operation cancellation token directly; the LSP
  uses this path without constructing a second inventory view.
- Only roots containing selected fuzz leaves are reseeded. The complete root is
  passed to the existing test-library bridge so that sibling, skipped, and
  focused tests retain exactly the same random-seed distribution.
  Unit runners are independent of seed/run count and are reused.
- Explicit `Test.fuzzWith` counts remain unchanged. Configuration views and
  concurrent preparations do not mutate the retained inventory's runners.
  A per-inventory preparation gate protects interpreter-backed values.
- ID lookup is indexed, including `GetExecutionFilter`; preparation no longer
  scans every inventory entry for each requested ID.
- Discovery identity and converted protocol locations are computed once.
  Source text is decoded/split once per source file, instead of once for each
  inventory item and again for every result. Successful results reuse these locations.
- Empty selections bypass compilation and execution-cache initialization.
  Sequential execution uses one worker task, not a separate `Task.Run` per test.
  The immutable canonical declaration conversion is shared within the inventory;
  compiled evaluations are not cached across runs.

The retained profiler now supports `--language-server`, invoking the actual
`LanguageServerTestingService` discover/run APIs. It captures each edited source
version, discovers it, runs the same 79 selected TaskTest leaves, then requests an
empty selection on that inventory with a different seed. Both implementations
were primed independently and then measured on three successive productive edits.
The baseline is upstream `209856c` (including the earlier immutable-program and
selector improvements plus the pulled formatting commit); the same profiler
driver was built against that baseline in an isolated worktree.

| Phase, milliseconds | Baseline edit 1 / 2 / 3 | Follow-up edit 1 / 2 / 3 |
|---|---:|---:|
| Discover/capture version and protocol inventory | 14,030 / 13,374 / 10,868 | 11,867 / 8,028 / 8,846 |
| Run selected 79 tests on discovered inventory | 18,663 / 21,439 / 18,347 | 13,057 / 12,792 / 14,507 |
| Empty selection, changed seed, same inventory | 5,701 / 6,225 / 5,612 | 0.21 / 0.20 / 5.86 |

| Median across three edited reruns | Baseline LSP | Follow-up LSP |
|---|---:|---:|
| Discover + selected run | 32.69 s | **23.35 s** |
| Allocation: discover + selected run | 17.35 GB | **11.50 GB** |
| Discovery/protocol-inventory allocation | 6.87 GB | **3.30 GB** |
| Selected-run allocation | 10.48 GB | **8.20 GB** |
| Empty-selection duration | 5.70 s | **0.21 ms** |
| Empty-selection allocation | 2.26 GB | **8,200 bytes** |

Discover-plus-run median latency fell **28.6%**, and median allocation fell
**33.7%**, relative to the already optimized baseline. The empty-selection phase
is reported separately and is not included in those totals. These LSP results
must not be compared directly to the earlier core-only phase tables.

All six measured edited runs retained 5,214 inventory entries, selected all 79
tests, and produced the same expected 78-pass/1-failure outcome and failing ID.
Corresponding source-version hashes matched across implementations. Resolution
fingerprints and discovery tokens differ as expected because they include the
compiler assembly's module identity. Regression tests also compare reseeded fuzz
evaluations and metadata against fresh discovery, including focus/skip siblings,
explicit run-count overrides, concurrent settings, cancellation/retry, and reuse
of discovery tokens and UTF-16 locations.

The host, serial execution, seed 12345, 100 fuzz runs, and generational execution
cache defaults are unchanged. The service uses its normal dependency-resolution
configuration with already warm package caches and no background learner.
No profiler was attached. Protocol calls and event handling were in-process:
JSON-RPC transport, editor rendering, and real filesystem-watcher latency are
not measured. This remains a sequential three-edit comparison, not a long-session
p95 or a randomized benchmark. Remaining discovery, closure-compilation, and VM
work are real costs. Canonical/prepared reuse is now implemented below;
root-construction and compiled-callable cache designs remain future work.

### Implemented operator-lowering and semantic-IR reuse

The next follow-up retains pure source transformations without caching discovery
inventories across edits:

- Operator lowering caches each source declaration using its complete canonical
  syntax, qualified identity, both lowering switches, algorithm version, and a
  SHA-256 fingerprint of the type/name environment. That environment contains
  signatures, arities, aliases, constructors and all declaration names, not
  unrelated function bodies.
- Semantic lowering additionally fingerprints all inferred function types.
  An unannotated helper's body can change types used when lowering an unchanged
  caller, so syntax-only or signature-only keys would be unsound.
- Reachability now precedes semantic body conversion and lambda lifting.
  The traversal includes exported application bodies, generated closures,
  captured-function references, canonical identifiers, and implicit record-update
  receivers. Non-function source metadata is retained in full. Final resolved-IR
  pruning still removes unused local/lifted functions.
- A semantic entry retains the resolved function, binding-name provenance, and
  type evidence. Captured runtime values, interpreter closures, per-run
  cancellation and compiled application-specific bodies are not cached under
  source keys.

The same retained driver measured three further productive edits, without a
profiler attached. Core closure preparation and allocation were:

| Core phase | Edit 1 | Edit 2 | Edit 3 |
|---|---:|---:|---:|
| Closure preparation | 2.524 s | 2.404 s | 1.923 s |
| Closure-preparation allocation | 1.019 GB | 1.019 GB | 1.019 GB |
| Complete edited rerun | 15.536 s | 14.797 s | 12.911 s |
| Complete-run allocation | 6.306 GB | 6.274 GB | 6.305 GB |

Median closure allocation fell from the earlier **5.58 GB to 1.02 GB**
(approximately **81.8%**). The new core median is **14.80 s / 6.30 GB**;
earlier core three-edit bookkeeping-only results were **19.86 s / 10.87 GB**.
These are sequential measurements from separate profiling passes, not a randomized
paired benchmark. A later one-edit pre-transformation check also allocated
5.575 GB in closure preparation; allocation reduction is more stable than the
wall-clock comparison on this shared host.

The operator tier contained 7,270 entries after priming, then 7,271 / 7,272 /
7,273 after the edits. The semantic tier contained 3,970, then 3,971 / 3,972 /
3,973. Each edit introduced **one new exact input in each tier**, instead of
retransforming the unchanged declarations. These counts include generated syntax;
they are not discovery-root counts or VM invocation-cache statistics.

The actual language-server mode measured:

| LSP phase, milliseconds | Edit 1 | Edit 2 | Edit 3 |
|---|---:|---:|---:|
| Discover/capture version and protocol inventory | 9,273 | 8,338 | 7,859 |
| Run selected 79 tests | 7,463 | 7,196 | 8,045 |
| Empty selection on retained inventory | 0.24 | 0.18 | 0.15 |

Compared with the preceding language-service follow-up, median discover-plus-run
latency fell from **23.35 s to 15.90 s (31.9%)**, and allocation from
**11.50 GB to 6.93 GB (39.7%)**. Median selected-run allocation fell from
8.20 GB to 3.66 GB. Empty selections remained at 8,200 bytes. The baseline
is the preceding implementation with only subsequent formatting changes pulled;
this measures the transformation change on top of removal of rediscovery, not
against the original double-discovery path. As before, transport/editor/watcher
latency and long-session p95 are not measured.

All six measured edits retained the same inventory, selected 79 tests, and
produced the expected 78-pass/1-failure outcome. Source parsing, discovery and VM
execution remain material costs. Root construction and cross-edit discovery
inventories are not cached; the following intervention adds canonical/prepared
declaration reuse rather than caching live constructed tests.

#### Lifetime and persistence boundary

Both data models use separately configurable `ICache<TKey, TValue>` storage.
The optional `IMarkerCacheLifetime<TMarker>` interface is independent of model
storage, allowing synchronized or single-threaded stores and memory/disk cascades.
The supplied thread-safe memory implementation associates each entry with its
last lookup/publication marker and counts distinct entries per invocation.
Async cleanup accepts mandatory removal, mandatory retention, or ranked
budget competition, with higher ranks retained first. Application callbacks run
outside storage locks, and cleanup cannot remove entries touched or replaced
after its snapshot.

Watch and LSP preparation use increasing session markers and retain the memory
tiers across edits. Above three times a nonempty invocation's distinct working
set, a tier is trimmed asynchronously toward twice that set. All configuration
variants accessed during the invocation count toward the working set; no
one-configuration budget is imposed on a multi-configuration workload. Active
last-touch markers are pinned, and each tier has at most one scheduled cleanup.
Empty selections do not create or evict these caches. Cleanup is observed and
awaited on session disposal.

Persistent-cache prerequisites are implemented, but disk persistence is opt-in:
`SourceTransformationInput.ComputeHash()` derives a domain-separated SHA-256
address from syntax, identity, environment and complete configuration/version.
It weakly memoizes syntax digests and hashes shared environment data once per
pass. Lossless codecs retain syntax, structural IR identities, binding names,
and generic-access evidence. Evidence is encoded by deterministic expression
traversal position and rebound to the decoded expression instances rather than
deserialized as unrelated dictionary keys.

Generic `FileStoreCache<TKey, TValue>` uses `IFileStore`, accepts a caller-selected
subtree and codec, and verifies input and payload digests. `LayeredCache` promotes
backing hits into memory and writes the backing store before publishing a front
entry. `SourceCompilationCaches.CreateInMemory(store, directory)` composes these
layers for both models. File-store, corruption and codec errors are explicit;
atomic replacement and cross-process coordination follow the selected store's
guarantees. Algorithm/builtin-semantics/schema changes must bump the versioned
keys and encodings. Persistent retention policy is deliberately not prescribed.

The retained profiler's `cache-check` command verifies restart reuse independently
of the large benchmark. A writer process populated two operator and two semantic
entries; a separate reader process with `--require-hits` obtained all four from
disk with zero misses and identical SHA-256 addresses. Both processes compared
the restored program and evidence count against fresh uncached conversion.
Regression coverage also checks nested inferred-type codecs, binding/evidence
identity, payload corruption, storage failures, active-marker retention,
single-flight cleanup with a newer pending budget, and the exact 3x/2x boundary.

Conservative whole-type-environment invalidation is still a limitation:
signature/constructor/name-set changes, inferred-type changes, or a changed
generated-closure shape can invalidate otherwise reusable function results.
Future observed-input manifests could narrow this without weakening correctness.
The present body-only productive edit preserves this environment and gets the
intended reuse.

### Implemented syntax-preserving canonical and prepared-declaration reuse

This intervention caches the **declaration preparation** boundary, not constructed
test trees, executable closures or test outcomes. It is wired through
`ElmTestRunCacheSession.DiscoveryCaches`, CLI watch's retained session, the actual
`LanguageServerTestingService`, all `DiscoverTests` overloads and the retained
profiling driver. Hosts can also supply `ElmTestDiscoveryCaches` directly.

#### Models and invalidation

Canonicalization extends the existing observation-based `DeclarationCache` rather
than introducing a second dependency/version engine. Its key now includes
**syntax-preserving versus lowered mode**, module identity and range-bearing CST.
Entries still contain canonical syntax, direct dependencies, reference provenance,
diagnostics and the resolution queries actually observed. Hits validate those
queries, including negative/ambiguous results, then re-demand dependencies from
the current build. A body-only callee edit therefore reuses its unchanged callers,
including callers inside the static reverse tree, without reusing the old callee.
Mode-isolation and the existing changed-resolution matrix are exercised in both modes.

Prepared storage uses
`ICache<PreparedDeclarationInput, PreparedSourceDeclaration>`. Its exact input is
the qualified owner plus **range-free canonical abstract syntax**. A weak
CST-identity normalization memo avoids repeated conversion on canonical hits;
source movement can miss the canonical tier yet still hit prepared storage.
Including the owner is necessary: otherwise identical functions in different
modules could share prepared-expression identities and overwrite each other's
per-run navigation observers.

The prepared artifact retains abstract syntax for closure compilation, pure
**unreduced** interpreter syntax, and lazily derived structural expression paths.
It reuses the interpreter's existing preparation and codecs; contextual closed-expression
reduction and operator lowering are not part of this transformation. Closure
compilation now consumes the retained abstract syntax instead of converting the
canonical CST a second time.

Each discovery still builds its own immutable program, observers, source bodies,
provenance, constructed values, runners and cancellation configuration. Cached
expression paths are projected onto the current CST. Range-bearing canonical
artifacts invalidate on movement rather than retaining stale diagnostic locations;
range-free prepared artifacts can survive that movement. Persistent restoration
rebuilds paths from the decoded prepared expressions, preserving their reference
identities rather than deserializing unrelated observer-map keys.

#### Shared retention policy and persistence boundary

Both tiers use the existing marker-managed memory storage. Canonicalization keeps
its resolution-query secondary index, but eviction removes the corresponding
observation version and drops empty syntax buckets. Versions count individually,
so changing imports repeatedly cannot accumulate an unbudgeted list behind one key.
The common `CacheInvocationLifetime` controller is extracted from the transformation
session and reused for discovery: strictly above **3x the last nonempty working
set**, cleanup targets **2x**, pins active markers and coalesces newer budgets.
The workload uses 6,345 canonical and 6,352 prepared entries per invocation:
trigger counts are above 19,035 / 19,056 and target budgets are 12,690 / 12,704.

Memory hits update mutable lifetime metadata under the storage lock without
allocating replacement entry records. Cleanup snapshots the access sequence as
well as the entry identity, so later touches/replacements remain protected.
Usage accounting retains numeric entry identities, not syntax keys/results:
an old invocation's usage counter cannot keep evicted canonical artifacts alive.
Removal observers run outside the storage lock, avoiding lock inversion with the
canonical index. Existing storage/lifetime regressions and explicit release tests
cover these invariants.

Watch/LSP defaults remain memory-only. Prepared declarations additionally support
the existing `IFileStore` cascade, with a versioned SHA-256 key over owner and
normalized syntax, weakly memoized syntax digests and the interpreter's lossless
prepared/abstract codec. There are no omitted environment/reducer options: this
preparation stage is deliberately context-free. Algorithm/encoding/native
preparation-semantics changes require a version bump. **Canonical observation
manifests remain memory-only**; no disk codec or persistent retention policy is
claimed for them.

The extended `cache-check` was run in separate writer/reader processes. The reader
restored all **three prepared, two operator and two semantic entries**, with zero
misses, matching input hashes and outputs equal to fresh conversion. This checks
restart reuse, not a disk-cache benchmark.

#### Fresh three-edit comparison

The comparison baseline is `a81216eec7dcdf12e25cbf0e2b4d4fcf093de499`, with the
previous transformation caches already enabled. Baseline and final implementations
each received a new process, one excluded passing priming round and the same three
distinct productive edits. Settings, fixture, one-worker execution, package cache
and generational execution policy were unchanged. No profiler or forced GC was used.
Two upstream formatting/constant-naming-only commits arrived during implementation
and were integrated after sampling. All 319 core/watch regressions and the seven
persisted-model reads were rechecked on that merged tree; those validation runs
are not pooled with the performance samples. Prepared-model hashes also remained
stable across the subsequent formatted rebuild.

| Core phase, seconds / decimal GB | Baseline median | With declaration caches median |
|---|---:|---:|
| Canonical/interpreter preparation, seconds | 1.749 | 0.669 |
| Complete discovery, seconds | 5.858 | 4.744 |
| Discovery allocation, GB | 2.244 | 1.739 |
| Selected closure preparation, seconds | 2.220 | 1.804 |
| Selected closure allocation, GB | 1.006 | 0.954 |
| Execution, seconds | 5.627 | 5.402 |
| Complete edited rerun, seconds | **14.693** | **12.484** |
| Complete-run allocation, GB | **6.292** | **5.731** |
| Summed phase GC pause, seconds | 2.181 | 1.417 |

Core preparation improved **61.8%**; median total latency improved **15.0%** and
allocation **8.9%**. The principal repeatable allocation savings are approximately
**0.505 GB in discovery** and **0.053 GB in closure preparation** per edit.
The actual language-server comparison measured:

| LSP phase / metric | Baseline median | With declaration caches median |
|---|---:|---:|
| Capture/resolve/discover/protocol inventory, seconds | 8.696 | 6.510 |
| Selected run, seconds | 7.211 | 6.979 |
| Empty selection on retained inventory, milliseconds | 0.400 | 0.746 |
| Discover-plus-run, seconds | **15.617** | **13.507** |
| Complete measured allocation, GB | **6.920** | **6.326** |
| Summed phase GC pause, seconds | 2.793 | 2.005 |

Actual LSP median latency improved **13.5%**, allocation **8.6%**. Empty selections
remained sub-millisecond in the final three samples and used 0-8,200 measured bytes;
these tiny values include allocation-counter sampling noise and do not establish
an empty-selection speedup. They still bypass execution-cache initialization and
compilation/reseeding.

Per-edit totals, retained to make the variation visible:

| Mode / implementation | Edit 1, seconds / GB | Edit 2, seconds / GB | Edit 3, seconds / GB |
|---|---:|---:|---:|
| Core baseline | 14.693 / 6.293 | 13.502 / 6.264 | 16.906 / 6.292 |
| Core declaration caches | 12.484 / 5.736 | 12.292 / 5.703 | 15.398 / 5.731 |
| Actual LSP baseline | 19.421 / 6.920 | 15.275 / 6.910 | 15.617 / 6.927 |
| Actual LSP declaration caches | 18.094 / 6.326 | 13.507 / 6.310 | 12.638 / 6.336 |

Both modes primed **6,345 canonical / 6,352 prepared entries**; the seven extra
prepared entries are native Basics choice declarations. After each edit the
counts were **6,346 / 6,353**, **6,347 / 6,354**, **6,348 / 6,355**:
exactly **one new publication in each tier per edit**. Operator and semantic
tiers likewise still added just one entry each. Every measured edit preserved
422 modules, 1,009 roots, 5,214 inventory entries, 79 selected tests, and the
expected **78 passes / one specified failure**. A separate `--dependencies`
check reproduced 6,345 declarations, 37,087 edges, 248 statically affected
declarations, 37 affected roots and 972 unaffected roots.

Final regression runs passed **271 core tests and 48 actual watch tests**.
Coverage includes changed-resolution inputs in both modes, callee-body edits,
mode isolation, memory/disk reuse after source movement, identical functions in
different modules, codec variants and expression identities, error diagnostics,
fuzz settings, concurrent snapshots, cancellation/retry, exact cleanup thresholds,
active-marker retention, secondary-index eviction and released usage references.

These are sequential samples on the same shared two-CPU host, not randomized
pairs, a long-session p95, or a retained-byte census. End-of-execution managed
heap samples varied substantially; count budgets cannot be equated to byte caps.
Source resolution, module-context/index construction, fresh observers, root
interpretation/bridge seeding, and execution still run. The third cached core
sample spent 6.284 seconds after interpreter preparation, illustrating that
root construction remains a substantial uncached and variable cost.

## Dependency-aware discovery caches

### How much lies outside this edit's static dependency tree?

Starting with `Simplify.listOfWrapperSequenceChecks`, a reverse traversal of
`DeclarationCanonicalizationResult.Dependencies` reaches **248 of 6,345
declarations**, including the changed declaration. **6,097 declarations
(96.1%) lie outside that reverse dependency tree**.

Among the 1,009 candidate roots, 37 are reverse-reachable and **972 (96.3%) are
not**. The 37 include the Simplify test-suite roots and
`TestHelpers.ruleWithDefaults` / `ruleExpectingNaN`. They are not 37 individual
test cases, and the 972 unaffected candidates are not all runnable test roots.

This demonstrates a large opportunity for declaration reuse, but also a limit
of static root invalidation: the selected `Simplify.TaskTest.all` root depends
on the rule through callbacks, so the conservative static graph marks the
whole root affected even though only one assertion outcome changes.

### Recommended cache layers

The canonical/prepared rows are implemented. The remaining rows describe future
cross-edit/persistent artifacts, not the existing within-snapshot settings views.

| Layer | Reusable artifact | Key / validation inputs |
|---|---|---|
| Source parsing | CST plus parse result | Exact source content and parser/schema identity |
| Module scopes | Exports, imports, aliases, infixes, constructor/type interfaces | Semantic module header/interface and consulted imported interfaces |
| Canonical declaration | Unlowered qualified syntax, references, direct dependencies, diagnostics | Declaration syntax, canonicalization mode, and observed symbol-resolution inputs |
| Interpreter preparation | Prepared declaration and expression-path metadata | Canonical declaration artifact and interpreter-preparation version |
| Root construction | Test-tree recipe or validated non-Test classification | Root code plus dependencies actually needed to construct/inspect the value |
| Runner preparation | Seeded runner recipe, labels, fuzz metadata | Construction artifact, bridge/test-library implementation, seed, fuzz count |
| Execution preparation | Lowered IR / compiled callable / selected Pine evaluation | Callback code, captured data, code dependencies, compilation configuration |
| Result reuse, later | Test outcome and diagnostics | Complete executable artifact, inputs, fuzz settings, native semantics |

The first root-construction cache can use **conservative static dependencies**.
It need not solve dynamic dependency tracking before providing reuse outside
the changed reverse dependency tree; canonical/prepared reuse already does so.

### 1. Reuse canonical and prepared declarations (implemented)

The branch extends the existing observation-based
[`Canonicalization.DeclarationCache`](../../implement/Pine.Core/Elm/ElmCompilerInDotnet/Canonicalization.Cache.cs),
with a syntax-mode identity and bounded lifetime. Its
entries retain dependencies and reference provenance, and its
observations include imports, aliases, operators, symbols, and unavailable names.
The implementation reuses that mechanism rather than inventing invalidation based only on filenames.

The syntax-preserving mode has a distinct key:
normal canonicalization can lower operators, while discovery requires the
original AST shape for navigation. The old safety guard has been replaced by
that identity invariant, not simply removed without mode separation.

On a cache hit, the implementation retains the direct dependency list and demands those declarations
against the **current** build. Canonicalizing a caller resolves a callee's
identity; it need not reprocess the caller's body merely because the callee's
implementation changed. Execution artifacts depending on that implementation
are a different cache layer.

For unchanged declaration artifacts, it reuses `PreparedDeclaration` and structural
expression paths, assembling one immutable program from reused and changed
entries. An incremental module-scope index could avoid rebuilding all 422 module
contexts, but the measured canonicalization cost makes this secondary to the
large bookkeeping fixes.

### 2. Cache root construction, including non-Test results (future)

Maintain forward dependency edges and their reverse index. Detect changed
declarations using semantic content identities, then recompute only affected
SCCs and roots. Cache non-Test classifications as well: otherwise many of the
1,009 candidates continue to incur interpretation on every edit.

A safe first key is:

```text
root construction key =
  H(discovery semantics version,
    root semantic identity,
    transitive construction dependency/SCC identities,
    consulted resolution/interface inputs,
    test-library and native semantics)
```

Use the static dependency closure initially. Keep bridge seeding separate so
seed/fuzz-count changes can reuse construction when valid.

A cached result must include a **construction recipe and provenance**, not just
labels. It needs parent/child structure, raw skip/only markers, empty/invalid
groups, source declaration/expression paths, and callback/capture references.
Caching only the displayed inventory would still force reconstruction to obtain
executable runners.

### 3. Distinguish construction reads from callback code dependencies (future)

The larger opportunity for a productive-code edit is to avoid invalidating test
construction just because a callback mentions changed code.

Conceptually:

```elm
suite =
    Test.test "example" (\() -> productiveFunction input |> Expect.equal expected)
```

Constructing this Test needs the label, test constructor, closure recipe, and
capture values. It does not need to execute `productiveFunction`.

A future dependency recorder should distinguish:

```text
construction reads: declarations/values actually evaluated or inspected
deferred code refs: code identities stored in closures, not executed yet
capture data reads: values forced while constructing or serializing captures
execution deps: code and values required when running the callback
```

A productive function-body edit can invalidate execution artifacts while leaving
the construction artifact reusable. This requires observation of **lazy forcing
as well as function entry**: recording only direct calls is insufficient.
Native operations, pattern inspection, imported factories, dynamically selected
branches, and top-level values must also contribute the inputs they consult.

This narrower dynamic graph was **not collected in this profile**, so its exact
hit rate is not established. The 96% static counts must not be advertised as a
96% reduction in discovery time or a measured dynamic cache hit rate.

### 4. Never cache stale live closures (future capture-graph design)

Current `ElmClosureInProcess` values retain prepared function/lambda bodies and
captured bindings. They are opaque interpreter values, not ordinary fully
materializable Pine values. The exported closure graph also handles recursion
by object identity.

Returning an old `ElmTestDiscoveryResult` for a new build can retain old canonical
declarations and old `CompileExecution` closures. Reusing the shape while
executing old code would defeat the failing-edit scenario entirely.

Represent cached callbacks as explicit recipes:

```text
closure = declared-code reference OR (declaration identity, lambda path)
          + lexical binding/capture identities
          + captured immutable data/code-reference graph
          + already-applied arguments
```

Rebind code references to the current validated declaration graph before
execution preparation. If the artifact embeds an old function implementation,
its key must change with that implementation. A supposedly shape-only cache
must not hide executable code inside an unversioned captured object.

Do not share mutable materialization state, expression-observer dictionaries,
cancellation tokens, VMs, or snapshot-bound delegates as durable cache values.

### 5. Preserve global selection/navigation when adding root reuse (future integration)

Per-root reuse must still preserve whole-suite semantics:

1. Collect cached/recomputed root inventories in deterministic discovery order.
2. Recompute global `Test.only`, skip propagation, leaf ordinals, and group state.
3. Build the full-path frequency index once, including duplicate-path behavior.
4. Apply the user's filter or requested IDs without changing discovery identity.
5. Project range-free expression paths onto the current matching CST and module
   provenance; create a fresh snapshot/discovery identity for the host.

`Test.only` in an otherwise filtered-out root can change the selected suite.
Blindly discovering only TaskTest would therefore be a behavioral change.
Fast prefiltering is safe only when sufficient cached metadata from other roots
is validated, or when an explicitly different execution contract permits it.

Preserve the complete `Items` inventory, not merely `Tests`: skipped and
only-excluded leaves, groups, todos, empty groups, duplicate labels, and imported
factory locations all matter to the language-service UI.

### 6. Reuse execution transformations (lowering implemented; SCC cache future)

Memoize declaration-level operator lowering and semantic IR conversion over
their actual type/interface observations. Restrict transformations to reachable
code earlier where correctness permits; do not discard constructor/type metadata
that those transformations require.

The implemented follow-up above now caches both transformations using conservative
type-environment fingerprints and prunes semantic body conversion before lambda
lifting. Narrower observed-input manifests remain a possible refinement.

There is already a useful emission seam:
`CompilerModel.Compilation.Compile` accepts a memoized `compileScc` worker, and
[`SccCompilationInput`](../../implement/Pine.Core/Elm/ElmCompilerInDotnet/CompilerModel/Compilation.cs)
projects member implementations, referenced constructors, direct external
callables, and compilation flags.
[`StructuredIdentityTests`](../../implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/StructuredIdentityTests.cs)
already demonstrate misses only for a changed declaration and dependents.

Wire such a cache into closure compilation, but do not mistake it for a cache
of the expensive earlier lowering/conversion stages. The sampled SCC pipeline
was only about 0.91 seconds of the 9.97-second closure-compilation path.

Generated closure names currently use `$PineSyntaxClosure.valueN` with a
traversal-local counter. Persistent or cross-selection reuse needs stable
lambda/application identities or deterministic graph normalization; unrelated
test insertion or a changed selection must not rename every later artifact.

## Remaining persistence roadmap beyond implemented transformation stores

Versioned input hashes, operator/semantic/prepared codecs, the generic file-store
cascade and count-based memory cleanup are implemented above. Canonical observation
manifests, root recipes, executable captures, SCC artifacts, byte-based budgets
and persistent reclamation still need the broader contracts in this section.

Use one typed artifact/key model for in-process and persisted storage, with
in-memory dictionaries serving as the hot tier. Persist detached immutable
representations, not serialized CLR object graphs.

Important properties:

- **Stable content identities.** Use deterministic, versioned encoding and a
  cryptographic digest. CLR `GetHashCode`, reference identity, dictionary
  enumeration order, and process-local lambda names are not persistent keys.
- **SCC-aware hashing.** Canonicalize recursive components and hash member bodies,
  internal edges, and external dependency identities without recursively chasing
  a cycle forever.
- **Separate graph identity from provenance.** Reuse range-free semantics after
  comment/whitespace edits; refresh source paths/ranges from the current snapshot.
  Keep original-module/package provenance for navigation and diagnostics.
- **No whole-project key for every entry.** The current build resolution
  fingerprint includes `AppSources`; it changes for this one-line edit.
  Use per-artifact dependency keys, with separate identities for manifest/lock
  resolution, exact package sources, substitutions, compiler, and native ABI.
  The global fingerprint remains useful as snapshot identity, not as the sole
  key of all reusable declarations.
- **Explicit environment contracts.** Include relevant parser/canonicalizer/
  interpreter/compiler schema versions, implicit import policy, operator mode,
  bridge implementation, package/substitution content, and native semantics.
  Seed and fuzz count belong only to layers they actually affect.
- **Atomic, validated storage.** Write complete artifacts atomically, verify
  length/digest/schema and referenced objects on read, and handle corruption with
  explicit diagnostics, not a success-shaped empty inventory. The current file-store
  adapter propagates corruption/storage errors; a future recovery policy may explicitly
  invalidate and recompute after reporting them.
- **Bounded lifetime.** Budget retained bytes, not just entry counts. Pin artifacts
  for active snapshots/runs; use generational/LRU hot storage and reachability-based
  disk reclamation. Historical source texts, VM compilation caches, and
  interpreter captures also need accounting.
- **Concurrency and cancellation.** Deduplicate in-flight work per key. A cancelled
  request must not publish a partial inventory as complete, and cancellation of
  one waiter must not invalidate an artifact already completed for another.

Persisting the current monolithic discovery result would retain too much
snapshot-specific state and would be invalidated too broadly. Persisting
declaration, construction, capture-graph, and compilation artifacts provides the
reuse granularity needed by the intended long-running developer workflow.

## Prioritized improvement backlog

| Priority | Improvement | Evidence / expected benefit | Main correctness concern |
|---|---|---|---|
| P0, implemented | Linear exact-selector generation and reuse/precompute full paths | Historical 21.36 GB sampled allocation; eliminated quadratic projection | Preserve duplicate ordinals and exact path semantics |
| P0, implemented | One immutable prepared program and resolver index per discovery | Historical 23.16 s resolver setup; stable dictionary identity now reused | Program immutability; isolate observers/cancellation |
| P1, implemented | Avoid LSP's full settings-triggered rediscovery | Inventory/runners retained; only selected fuzz roots reseeded | Seed/fuzz settings and frozen snapshot identity |
| P1, implemented | Cache/prune operator lowering and semantic IR conversion | Closure allocation 5.58 -> 1.02 GB; actual LSP median 23.35 -> 15.90 s | Complete type/name context, short-circuit/pipe semantics, constructor metadata |
| P1, implemented | Syntax-preserving canonical/prepared-declaration caches | Core preparation 1.749 -> 0.669 s; one new entry per tier per edit | Observed resolution inputs, owner identity and current CST navigation |
| P1/P2 | Root-construction and non-Test caches, initially static | 972 unaffected candidate roots; persistent-friendly reuse boundary | Retain recipes and provenance, not stale live closures |
| P2 | Dynamic construction-vs-execution dependency manifests | Static graph unnecessarily invalidates callback-containing roots | Lazy forcing and captured code/data dependencies |
| P2 | SCC compiled-callable cache and stable exported closure identities | Existing pure memoization seam and structural-identity tests | Whole closure graph/capture identity; not just declaration name |
| P2 | Foreground-aware learning schedule, budget, and benefit telemetry | 25.76 s sampled learner coverage; 8.30 GB sampled allocation in capture | Do not remove learning benefits based on one workload |
| P2 | Improve execution hit-rate diagnostics and result-level reuse | Warm invocation hit rate about 13.5%; latest core execution median 5.40 s | Dependency/input changes versus eviction/admission |
| P2 | Stream outcomes and prioritize affected/previously failing tests | Developer feedback need not wait for final suite rendering | Preserve requested coverage and cancel/stale-run identity |
| P3 | Reuse unchanged package/module resolution and scope artifacts | Latest warm core source stage allocates about 412 MB, usually under 1 s | Manifest/lock/package refresh and substitution identity |

The implemented first two changes required no disk cache and removed large amounts of
work rather than retaining their allocations. More workers, lock replacement,
filesystem-watcher tuning, and remote-download optimization are not supported
as the leading priorities by this warm-run evidence.

## Follow-up measurement and correctness plan

The retained driver already records coarse phase metrics and retained/new entry
counts. Add finer low-overhead events and hit/miss-reason counters for: source loading/resolution,
canonicalization hits/misses, prepared declarations, resolver/index creation,
root construction, bridge seeding, inventory projection/selectors, operator
lowering, semantic IR conversion, lambda lifting, SCC emission, and execution.
Keep foreground and learner CPU/allocation accounting separate.

Record cache hit/miss **reasons** and retained bytes per layer; distinguish
construction-read invalidation from callback-code invalidation. Measure first
failure notification as well as total suite completion: the current CLI only
announces execution after discovery and closure compilation finish.

For a long-session benchmark, use at least dozens of distinct edits, include
undo/redo, and report warm p50/p95, first-result latency, allocations, GC pauses,
retained memory, artifact bytes, and per-layer hit rates. Also test a larger
machine and a real LSP client. This report's three edits do not establish those
long-session characteristics.

For future persistent root/construction/SCC/result caches, require differential
tests against a fresh uncached run for the cases below. Existing canonical,
prepared and lowering cache regressions cover their respective boundaries;
they do not establish correctness of those unimplemented higher-level caches.

- A productive function body changing one assertion outcome while labels remain
  unchanged; the failing result must not be incorrectly reused as a pass.
- Construction-time changes to labels, branch counts, imported factories,
  `Test.only`, skip/todo/empty groups, and generated or duplicate tests.
- Module imports/exports, missing-to-present symbols, ambiguities, constructors,
  operator precedence, type interfaces, and package/substitution changes.
- Source movement and formatting-only edits, with exact current CST navigation.
- Fuzz seed/count changes, reruns after cancellation, and concurrent snapshots.
- Cache eviction, process restart, artifact corruption, and incompatible schema/
  compiler/native versions.

## Reproduction and artifacts

The committed tool lives in
[`explore/2026-11-04-elm-review-warm-rerun`](../2026-11-04-elm-review-warm-rerun/README.md).
Its README gives complete build/run/trace commands. It removes the original
session-specific paths, records environment/build provenance, validates each
edited run's exact failing test, and restores the workload even on cooperative
cancellation or exceptions.

The reusable harness adds safety and artifact-handling checks around the same
measured core boundaries. It now supplies the ordinary retained discovery and
compilation caches; their production watch/LSP wiring lives in Pine.Core.
It does not depend on the temporary optimization from section C.

As an independent original-baseline reproducibility check, the retained project was built and run
through successful priming and two distinct edited reruns. Both edits produced
the specified 78/1 outcome and the same inventory and dependency counts.
Their totals were 55.82 and 54.32 seconds; these additional samples are not pooled
with the original three-run baseline. The retained trace analyzers reproduced
the original capture's allocation totals, collection counts, and allocation-stack
categories.

Build the embedded artifacts and CLI before starting the watch process:

```sh
cd implement
dotnet run --project prebuild/prebuild.csproj --configuration Release
dotnet build pine/pine.csproj --configuration Release
```

Clone the workload, check out the pinned commit, run the watch command shown
earlier, and wait for its initial 79-pass completion. Attach diagnostics **only
then**, and apply the one-declaration mutation:

```sh
dotnet-trace collect --process-id <watch-pid> \
  --profile dotnet-sampled-thread-time,gc-verbose \
  --duration 00:01:40 --buffersize 128 \
  --output warm-edit-1.nettrace --format Speedscope

# In a separate terminal, over the same warm window:
dotnet-counters collect --process-id <watch-pid> \
  --counters System.Runtime --format csv --duration 00:01:40 \
  --output warm-edit-1-counters.csv
```

For unprofiled phase replication, retain one session and call these boundaries
in order for priming and then each new edited tree:

```csharp
using var session = new ElmTestRunCacheSession(generationOptions: new());
var settings = new ElmTestExecutionSettings(12345, 100);

// Load files and exposed zero-argument roots exactly as ElmTestRunner does.
// Retain session.SourceParsing during PrepareForDeclarationDemandAsync.
// Resolve offline after the package cache has been primed.
var discovery = ElmTestDiscovery.DiscoverTests(
    build, roots, executionSettings: settings, caches: session.DiscoveryCaches);
var filter = new ElmTestFilter("TaskTest.elm");
var selected = discovery.Items.Where(item =>
    item.Selection == ElmTestDiscoverySelection.Included &&
    item.Kind is ElmTestDiscoveryItemKind.Unit or ElmTestDiscoveryItemKind.Fuzz &&
    filter.Matches(item.ListedTest, item.DiscoveryIndex ?? 0)).ToArray();
IReadOnlyDictionary<string, ElmTestExecutionEntry> entries;
using (session.BeginCompilationInvocation())
    entries = discovery.PrepareExecution(
        selected.Select(item => item.Id), compilationCaches: session.CompilationCaches);
var results = selected.Select(item =>
    ElmTestRunner.ExecuteDiscoveredTest(item, entries[item.Id], session)).ToArray();
```

Measure each boundary without forcing a GC between runs. Assert 79 results,
zero failures when priming, and exactly the specified failure after each edit.
Use deltas of `session.GetCacheStatisticsSnapshot()`, not cumulative totals, for
per-run hit rates. Dependency-tree counts come from the canonical result's
`Dependencies` graph, not merely module imports.

Raw artifacts and the complete temporary harness/analyzers were collected under
the session's `files/` directory:

```text
/root/.copilot/session-state/2c11ad1574684b2aba14ef71d1b59e6e/files/
  watch.log
  warm-edit-1.nettrace
  warm-edit-1.speedscope.json
  warm-edit-1-counters.csv
  warm-edit-1-counter-summary.json
  warm-edit-1-gc.json
  warm-edit-1-allocation-stacks.json
  warm-edit-1-stacks.json
  selected-stack-coverage.json
  harness.log
  harness-results.json
  warm-harness/{warm-harness.csproj,Program.cs}
  trace-analyzer/{trace-analyzer.csproj,Program.cs}
  analyze-speedscope.py
  combined-experiment/{harness.log,harness-results.json}
  implemented-fast-path/{metadata.json,harness-results.json}
  implemented-fast-path.log
  lsp-baseline-runs/{metadata.json,harness-results.json}
  lsp-optimized-runs/{metadata.json,harness-results.json}
  lsp-baseline-runs.log
  lsp-optimized-runs.log
  transform-core-runs/{metadata.json,harness-results.json}
  transform-lsp-runs/{metadata.json,harness-results.json}
  syntax-baseline-core/{metadata.json,harness-results.json}
  syntax-baseline-lsp/{metadata.json,harness-results.json}
  syntax-final-core/{metadata.json,harness-results.json}
  syntax-final-lsp/{metadata.json,harness-results.json}
  syntax-dependencies-check/{metadata.json,harness-results.json}
  persistent-syntax-final-write.json
  persistent-syntax-final-read.json
  persistent-syntax-merged-read.json
```

The raw NetTrace is about 109 MiB and the speedscope export about 158 MiB;
they are intentionally not committed. Session-local artifacts are not guaranteed
to remain available indefinitely. The measurements, interpretation, mutation,
pipeline boundaries, and cache design needed for prioritization are retained in
this versioned report.

Baseline artifact SHA-256:

```text
warm-edit-1.nettrace
36b40d2fa7579e670fbbc2ca2f5a7a104b1b08bc6172f62074748c595fa32703

harness-results.json
09bb78a179bcdf96fc2970bdf30fb3078c304c41cf4d3c890a6043bc17601d83

watch.log
c285056a50ce57b406d2517894474039898b302df7497993ba380549c85ef901

combined-experiment/harness-results.json
0d0a30c2b33cd57ddb7f60eb6d2f41ec5fddbe9cc7f8c16316114ff13682363e

implemented-fast-path/harness-results.json
0eca0248ce49544210d2d0187b385c36d327ba6900c04e5d6294434d6c304d5c

implemented-fast-path/metadata.json
cd19976aaa4fbd8786e2a6b89d265c765890c534e609043c323f5f6d5376988b

lsp-baseline-runs/harness-results.json
3a0c96581d125a2fcf5f4ba5b8baa5e10464656c2f050b014276380c8c7cec80

lsp-optimized-runs/harness-results.json
4043f809ef4412b88031636d44e25be542f6a32402357d6ba15c7c274caf9329

syntax-baseline-core/harness-results.json
2987ab712d565b7a4d2ee2878db9ab5aa685d733435649195cf536c4f7f5fbb0

syntax-baseline-lsp/harness-results.json
5fca83f29d9c1e89320dd392cd8891b4065fd22a02eda52ba029ef5797c67942

syntax-final-core/harness-results.json
840430ba57a38b58694a6a70f49dc18cdae9cb7f1ee0533956f16ccadd92af96

syntax-final-lsp/harness-results.json
a3c9fd6bd37fb533e3da5bf94ce8a3af411f8eb8d793c5bb3a5dec576788ab4e
```
