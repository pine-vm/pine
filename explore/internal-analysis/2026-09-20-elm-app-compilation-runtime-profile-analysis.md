# Elm App Compilation Runtime Profile Analysis

## Executive summary

The profile captures an approximately 10.86-second, almost entirely CPU-bound run of
`ElmAppCompilationSnapshotTests.ElmAppCompilation_Run_all_snapshots`. The useful application
work is concentrated on one thread. On that thread, the Elm syntax interpreter accounts for
essentially the complete observed interval.

The largest measured opportunities are:

1. **Replace linear declaration resolution with indexed or preparation-time resolution.**
   `ResolveAgainstDeclarations` covers 4,003.5 ms, or 36.87% of the profiled interval, and
   3,945.7 ms is directly attributed to that method rather than to a visible child call.
2. **Remove dictionary growth and general-purpose dictionary insertion from binding-heavy
   interpreter paths.** A direct `Dictionary.TryInsert` child of `RunTrampoline` covers
   1,715.3 ms (15.80%), while `Dictionary.Resize` below `BindPattern` covers another
   1,071.2 ms (9.87%). These intervals are in separate parts of the sampled stack and therefore
   represent a combined upper bound of 25.66%.
3. **Reduce per-call dispatch and object construction after fixing lookup and bindings.**
   `ApplyResolvedCall` has 1,815.1 ms (16.72%) directly attributed to it, excluding its visible
   resolver, binding, builtin, hash, and recursion-check children.
4. **Optimize value materialization and individual builtins only after the dispatch fixes.**
   The entire Pine builtin resolver path is 958.9 ms (8.83%); `PineValueInProcess.Evaluate`
   is 316.2 ms (2.91%), and binary concatenation is 170.1 ms (1.57%).

The profile does **not** support prioritizing test-runner, pipe, lock, or thread-pool work for
compiler throughput. Those frames belong mainly to auxiliary threads that are waiting while the
single application thread is busy.

There is also an important provenance limitation: the filename timestamp is
`2026-09-20 14:15:17`, while commit `ab9d7d28c` ("Fold closed Elm expressions during
preparation") is timestamped `2026-09-20 14:23:35 +0000`. The speedscope file does not embed a
Git commit. Assuming the clocks are comparable, this profile predates that optimization by about
eight minutes. It should therefore be treated as the baseline after the prepared-syntax and
linked-environment changes, but before closed-expression folding. Reprofiling the current
revision is the first required measurement step.

## Profile and workload

Profile:

- File: `Pine.IntegrationTests.exe_20260920_141517.speedscope.json`
- Format: speedscope evented profile
- Exporter: `Microsoft.Diagnostics.Tracing.TraceEvent@3.1.16.0`
- Time unit: milliseconds
- Profiles: 13 thread profiles
- Shared frames: 211
- Active application thread: `Thread (5520)`
- Application-thread interval: 10,858.8969 ms
- Events on the application thread: 807,174
- `CPU_TIME` intervals: 4,214
- `CPU_TIME`: 10,842.6321 ms, or 99.85% of the application-thread interval
- `UNMANAGED_CODE_TIME` on the application thread: 16.2647 ms, or 0.15%
- Median represented CPU interval: 2.0145 ms
- Mean represented CPU interval: 2.5730 ms

The profiled test is
`ElmAppCompilationSnapshotTests.ElmAppCompilation_Run_all_snapshots`. It runs two snapshot
cases serially:

| Case | Source files | Source bytes |
|---|---:|---:|
| `demo-backend-state` | 10 | 26,209 |
| `read-source-file-0` | 4 | 1,027 |

The larger case is likely responsible for most work, but the trace does not mark case boundaries.
The profile therefore cannot assign time to either case independently.

## How the numbers were normalized

This is an evented speedscope export built from sampled stacks. It uses synthetic leaf frames
named `CPU_TIME` and `UNMANAGED_CODE_TIME`.

Three consequences matter:

1. The duration of a method is an estimate based on sampled intervals, not instrumented elapsed
   time inside that method.
2. The number of open/close event segments is **not** the number of method calls. For example,
   3,990 segments mentioning an interpreter frame do not mean that method was invoked exactly
   3,990 times.
3. Raw inclusive duration can exceed the 10.86-second wall-clock span when the same frame appears
   recursively or multiple times in one stack. The recursive
   `AsCompletelyLoweredElmApp` overload has 21,717.8 ms of raw activation duration but only
   10,858.9 ms after overlapping intervals are merged. The latter is the meaningful coverage.

All percentages below use the 10,858.8969 ms application-thread span as the denominator. For
each frame, overlapping and nested occurrences of that same frame were merged before computing
coverage.

The 13 thread spans sum to 141,166.8 ms, but that sum is not process wall time: it counts
simultaneously existing threads repeatedly. Most non-application threads are represented under
`UNMANAGED_CODE_TIME`, `WaitHandle`, thread-pool polling, or test log pipe pumping. Those frames
describe waiting support threads, not 130 seconds of compiler work.

## Observed hot path

The dominant stack is:

```text
xUnit runner
  -> ElmAppCompilationSnapshotTests.Compile
  -> ElmAppCompilation.AsCompletelyLoweredElmApp
  -> ElmAppCompilation.CachedElmAppCompilationIteration
  -> ElmSyntaxInterpreter.Interpret
  -> ElmSyntaxInterpreter.RunTrampoline
  -> ElmSyntaxInterpreter.ApplyResolvedCall
```

`RunTrampoline` covers effectively the entire useful interval. This means outer compilation
caching, source-file packaging, result parsing, and snapshot comparison are not visible as
material CPU consumers in this capture. The profile is fundamentally an interpreter profile.

### Inclusive frame coverage

| Frame | Merged interval | Share of application thread | Interpretation |
|---|---:|---:|---|
| `RunTrampoline` | 10,858.9 ms | 100.00% | Entire sampled workload is interpreted execution |
| `ApplyResolvedCall` | 8,259.3 ms | 76.06% | Named-call resolution and entry dominate |
| `ResolveAgainstDeclarations` | 4,003.5 ms | 36.87% | Largest isolated optimization target |
| `BindPattern` | 1,119.4 ms | 10.31% | Mostly dictionary capacity growth |
| `PineBuiltinResolver` | 958.9 ms | 8.83% | Builtin dispatch and execution |
| `ApplyPineBuiltinFunction` | 800.8 ms | 7.37% | Actual builtin application path |
| `PineValueInProcess.Evaluate` | 316.2 ms | 2.91% | Forced materialization |
| `DeclQualifiedName.GetHashCode` | 245.1 ms | 2.26% | Repeated compound-key hashing |
| `PineValueInProcess.ConcatBinary` | 170.1 ms | 1.57% | Binary/list concatenation |
| `BuiltinFunctionSpecialized.concat` | 110.4 ms | 1.02% | Concrete concatenation |
| `CheckForInfiniteRecursion` | 83.8 ms | 0.77% | Periodic continuation-stack scan |
| `PineValueInProcess.Skip` | 62.6 ms | 0.58% | Slice operation |
| `ApplyFunctionValue` | 56.2 ms | 0.52% | Closure/function-value dispatch |
| `PineValueInProcess.Take` | 49.7 ms | 0.46% | Slice operation |
| `PrepareLetGroupAndSortNonFunctionDecls` | 30.3 ms | 0.28% | Cached let-plan retrieval/runtime setup |
| `LocalBindingEnvironment.TryGetValue` | 27.1 ms | 0.25% | Linked-scope lookup |
| `StringEncoding.ValueFromString` | 12.2 ms | 0.11% | Runtime string/tag encoding |

Inclusive rows overlap. They must not be added together.

### Direct child breakdown of `RunTrampoline`

The following edges are mutually exclusive at a sampled instant and better expose where the
trampoline sends its time:

| Direct child of `RunTrampoline` | Interval | Share |
|---|---:|---:|
| `ApplyResolvedCall` | 8,259.3 ms | 76.06% |
| `Dictionary.TryInsert` | 1,715.3 ms | 15.80% |
| Direct `CPU_TIME` leaf | 556.2 ms | 5.12% |
| `ApplyFunctionValue` | 56.2 ms | 0.52% |
| `PrepareLetGroupAndSortNonFunctionDecls` | 30.3 ms | 0.28% |
| `LocalBindingEnvironment.TryGetValue` | 27.1 ms | 0.25% |

This distribution shows that the main interpreter loop and local lookup are not currently the
primary bottlenecks. Named calls and dictionary mutation are.

### Direct child breakdown of `ApplyResolvedCall`

| Direct child of `ApplyResolvedCall` | Interval | Share |
|---|---:|---:|
| `ResolveAgainstDeclarations` | 4,003.5 ms | 36.87% |
| Direct `CPU_TIME` leaf | 1,815.1 ms | 16.72% |
| `BindPattern` | 1,111.3 ms | 10.23% |
| `PineBuiltinResolver` | 958.9 ms | 8.83% |
| `DeclQualifiedName.GetHashCode` | 245.1 ms | 2.26% |
| `CheckForInfiniteRecursion` | 81.7 ms | 0.75% |

Within `ResolveAgainstDeclarations`, 3,945.7 ms (36.34% of the complete run) is directly
attributed to the method. Only 51.6 ms is visibly below it in `SpanHelpers.SequenceEqual`.
This strongly indicates that iteration, type tests, name checks, and branch control in the
resolver are expensive in aggregate; namespace comparison alone is not the central issue.

Within `BindPattern`, `Dictionary.Resize` covers 1,071.2 ms (9.87% of the complete run), or
approximately 96% of the `BindPattern` interval. This is unusually clear evidence that
zero-capacity dictionaries are being created and grown repeatedly on the call path.

## Source-level interpretation

### 1. Declaration resolution is linear

`ElmSyntaxInterpreter.ResolveAgainstDeclarations` iterates over every prepared declaration for
each unresolved named application. For unqualified names, `UserDefinedResolver` may do a
same-module pass and then a module-agnostic fallback pass. The prepared program already stores
declarations in a dictionary keyed by `DeclQualifiedName`, but the hot resolver does not use a
direct lookup for the common qualified and same-module cases.

Relevant code:

- `implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.cs:4216-4240`
- `implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.cs:4249-4330`
- `implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.Modules.cs:30-52`
- `implement/Pine.Core/CodeAnalysis/DeclQualifiedName.cs:9-69`

The dedicated prepared syntax model still represents an identifier only as a
`DeclQualifiedName`; it does not retain a direct link to its resolved declaration or builtin.
Consequently, canonicalized source names are resolved repeatedly at runtime.

### 2. Binding uses fresh general-purpose dictionaries

Each saturated declared-function call creates:

```csharp
var bindings = new Dictionary<string, PineValueInProcess>();
```

It then recursively inserts pattern-bound arguments. No capacity is supplied, so the first
insert allocates the initial bucket/entry arrays and larger patterns can trigger further growth.
The same pattern exists for let scopes, case arms, and destructuring continuations.

Relevant allocation/mutation sites:

- `ElmSyntaxInterpreter.cs:1329` - mutable let layer
- `ElmSyntaxInterpreter.cs:1854-1893` - completed let bindings
- `ElmSyntaxInterpreter.cs:1973` - case-pattern bindings
- `ElmSyntaxInterpreter.cs:2031` - record-access path creates an apparently unused dictionary
- `ElmSyntaxInterpreter.cs:2394-2405` - function-argument bindings
- `ElmSyntaxInterpreter.cs:2691` and `:2723` - closure/function-value bindings
- `ElmSyntaxInterpreter.cs:3684-3768` and `:3842-3948` - pattern insertions

The linked `LocalBindingEnvironment` introduced immediately before this profile avoids copying
parent scopes, and its lookup itself is only 0.25%. That change appears directionally successful.
The remaining cost is constructing and populating each new local layer, not walking parent
layers.

### 3. Call dispatch allocates and hashes on every named call

`ApplyFunctionOrValue` constructs an `Application`, passes it through a chain of resolver
delegates, and `ApplyResolvedCall` returns newly allocated result/outcome records. Saturated calls
also create a bindings dictionary, an `ApplicationContext`, a `Kont.CallFrame`, and sometimes
argument arrays.

`DeclQualifiedName.GetHashCode` recomputes a hash by adding the declaration name and every
namespace string on each call. The type already stores `FullName`, but it does not cache its hash.
This is measurable at 2.26%, although much of the hash activity may disappear automatically if
runtime resolver lookups are eliminated or changed.

Relevant code:

- `ElmSyntaxInterpreter.cs:2284-2304`
- `ElmSyntaxInterpreter.cs:2306-2458`
- `DeclQualifiedName.cs:55-69`

### 4. Tail calls retain continuation frames

Every user-defined call pushes a new `Kont.CallFrame`, including self-recursive tail calls.
The source explicitly notes that tail recursion therefore uses memory proportional to call depth.
Every 1,000 user calls, `CheckForInfiniteRecursion` scans the continuation stack.

The visible recursion-check cost is only 0.75%, so this is not a first-order CPU target in the
current profile. However, tail-frame retention likely contributes to allocation and cache
pressure hidden inside the 16.72% direct `ApplyResolvedCall` bucket. It also limits scalability
for larger compiler inputs.

Relevant code:

- `ElmSyntaxInterpreter.cs:2421-2447`
- `ElmSyntaxInterpreter.cs:824-1012`

### 5. Builtins force some values to concrete `PineValue` objects

The builtin path covers 8.83% of the run. Its visible components include:

| Builtin/value operation | Interval | Share |
|---|---:|---:|
| `ApplyPineBuiltinFunction` | 800.8 ms | 7.37% |
| `PineValueInProcess.Evaluate` | 316.2 ms | 2.91% |
| `PineValue.List` below `Evaluate` | 281.2 ms | 2.59% |
| `BuiltinFunction.ApplyFunctionGeneric` | 182.3 ms | 1.68% |
| `ConcatBinary` | 170.1 ms | 1.57% |
| Specialized binary `concat` | 110.4 ms | 1.02% |
| `Skip` | 62.6 ms | 0.58% |
| `Take` | 49.7 ms | 0.46% |
| `Reverse` | 37.9 ms | 0.35% |

`Evaluate -> PineValue.List` shows that deferred in-process lists are still being materialized
on this workload. Nevertheless, no single builtin is large enough to justify optimizing it
before declaration lookup and binding allocation.

### 6. Whole-compilation caching is not the current answer

`AsCompletelyLoweredElmApp` hashes the full source tree and interface configuration, then caches
the final result. That can make exact repeat compilations cheap, but these snapshot cases are
different source trees and are each run once. The profile stays inside the interpreter for the
full captured interval, indicating no useful whole-result cache hit during this capture.

The compiler modules are already parsed and prepared behind a static `Lazy<Prepared>`.
`ElmCompilerCache` is currently created per public compilation call, which can matter during
result decoding or dependency iterations, but those operations are not visible as major sampled
costs here.

## Prioritized optimization plan

### Priority 0: Reproduce and improve measurement

Before changing another hot path:

1. Re-run the exact snapshot test on current `HEAD`, because closed-expression folding likely
   changes instruction count and hotspot proportions.
2. Record the Git SHA, build configuration, runtime version, machine, warmup policy, and case
   name alongside every profile.
3. Separate `demo-backend-state` and `read-source-file-0` into independently measurable cases.
4. Run each case enough times to report median and distribution, while separating cold module
   preparation from warm interpretation.
5. Use the existing `BeginInstrumentationScope` and
   `FunctionStepCountingInvocationLogger` to capture:
   - trampoline instruction count,
   - direct function applications,
   - function-value applications,
   - Pine builtin invocations,
   - top Elm functions by direct application count.
6. Add temporary counters for declaration entries examined, same-module fallback count,
   parameter-binding count, let/case binding count, and maximum continuation depth.
7. Capture an allocation profile in addition to CPU samples. This trace contains no GC or
   allocation-stack frames, so it cannot quantify bytes allocated or collection pauses.

The repository-specific test invocation should use the Microsoft Testing Platform form:

```powershell
Set-Location implement\Pine.IntegrationTests
dotnet run -c Release -- --filter-method="*ElmAppCompilation_Run_all_snapshots*"
```

Do not compare a warm run against this profile without explicitly labeling the difference:
static lazy preparation and whole-result caching can materially change what is measured.

### Priority 1: Index or pre-resolve declarations

#### Implementation status

Implemented on 2026-09-20:

- `PreparedDeclarationResolverIndex` enumerates each prepared declaration dictionary once and
  builds separate indexes for qualified callable names and unqualified simple names.
- Resolver indexes are cached per declaration-dictionary instance with
  `ConditionalWeakTable`; dictionaries must be treated as immutable after first use.
- Qualified calls use an exact `(namespaces, callable name)` lookup.
- Unqualified calls retain the existing two-stage semantics: first inspect candidates in the
  caller's module, then fall back to candidates with the same simple name in original
  declaration order.
- Function targets, record-alias field metadata, and individual choice constructors are stored
  directly in index candidates. Runtime resolution therefore no longer scans the complete
  declaration set, rebuilds record field lists, or re-scans all constructors of a choice type.
- Normal prepared execution, concrete-declaration compatibility execution, direct
  `UserDefinedResolver` calls, and instrumented execution all use the same cached index.
- A regression test wraps the declaration dictionary with an enumeration counter and verifies
  that building the resolver enumerates it once while ten subsequent resolutions perform no
  further enumeration. The test also pins same-module selection to `P.helper`.

Validation completed:

- 1,037 focused `ElmSyntaxInterpreter` tests passed.
- The Release `ElmAppCompilation_Run_all_snapshots` integration test passed.

The integration command's elapsed time includes process startup, Release build work, compiler
module preparation, and the newer closed-expression-folding pass, so it is not directly
comparable to the attached 10.86-second profile interval. A new profile is still required to
measure the realized speedup and locate the migrated hotspot.

#### Low-risk first implementation

Build immutable lookup structures once in `ElmSyntaxInterpreter.Prepared`:

- exact `DeclQualifiedName -> PreparedDeclaration`;
- same-module `(namespace, simple name) -> callable declaration`;
- unqualified simple-name candidates for the fallback semantics;
- constructor and record-alias constructor metadata by callable name.

Then make qualified and same-module calls direct lookups. Preserve the current fallback behavior
for ambiguous/unqualified cases and add tests covering same-name declarations in multiple
modules.

#### Higher-payoff implementation

During preparation, classify every non-local identifier and application target as one of:

- local binding lookup;
- known builtin;
- known declared function;
- known choice constructor;
- known record-alias constructor;
- unresolved/dynamic fallback.

Store the resolved target directly in the prepared node. This removes repeated resolver-chain
dispatch as well as declaration scans and some `DeclQualifiedName` hashing.

#### Expected impact

- Measured upper bound from `ResolveAgainstDeclarations`: 36.87%.
- Impossible best case if the entire bucket vanished: about 1.58x faster.
- A 75% reduction in this bucket alone would imply about 1.38x overall.
- A 90% reduction would imply about 1.50x overall.

These are Amdahl-style ceilings, not forecasts. The higher-level `ApplyResolvedCall` bucket
overlaps the resolver bucket and must not be added to it.

### Priority 2: Replace binding dictionaries on prepared call paths

#### Immediate experiment

Precompute the maximum number of names bound by every function argument list and pattern. Use
that count as dictionary capacity. This is a small semantic change and directly tests whether
capacity growth explains the measured `Dictionary.Resize` time.

Also remove the unused `patternBindings` allocation in the `Kont.AccessRecordField` branch at
`ElmSyntaxInterpreter.cs:2031`.

#### Preferred design

Compile prepared patterns to binding plans with stable slots:

- simple variable parameter: bind the argument directly to one slot;
- wildcard/literal parameter: no dictionary;
- tuple/list/constructor destructuring: precomputed extraction operations and target slots;
- environment layer: compact name/slot metadata shared by the prepared function plus a
  per-call value array.

Keep a dictionary fallback only for dynamic or compatibility entry points. Let groups need
special handling because recursive closures observe a mutable layer, but their names and slot
count are also static and can be prepared.

#### Expected impact

- Direct `RunTrampoline -> Dictionary.TryInsert`: 15.80%.
- `BindPattern -> Dictionary.Resize`: 9.87%.
- Combined measured upper bound: 25.66%.
- Impossible best case if both buckets vanished: about 1.35x faster.
- Removing 75% of both buckets would imply about 1.24x overall.

The expected result from capacity pre-sizing alone is lower than that upper bound because
`TryInsert` still has hashing and collision work. Slot-based bindings are required to approach
the ceiling.

### Priority 3: Specialize the prepared call instruction

After target resolution and binding plans exist, avoid constructing a general `Application`,
resolver result, and `ApplyCallOutcome` for the common saturated named-call path. A prepared call
node can carry:

- target kind and direct target reference;
- expected arity;
- precompiled binding plan;
- resolved top-level name;
- whether over/under-application handling is possible.

Keep the existing general path for partial application, function values, custom resolvers, and
public compatibility APIs.

The direct `CPU_TIME` leaf under `ApplyResolvedCall` is 16.72%, so this area has a meaningful
ceiling after its separately visible resolver, binding, builtin, hashing, and recursion children
are excluded. Some of that 16.72% may be inlined runtime work rather than removable allocation;
an allocation trace and before/after benchmark are required.

### Priority 4: Reduce forced materialization at builtin boundaries

Use the existing in-process specializations as the pattern:

1. Rank Elm and Pine builtins by dynamic count with
   `FunctionStepCountingInvocationLogger`.
2. For the top functions, record how often arguments are already concrete versus backed by
   `_list`, `_concatBuilder`, slice builders, integers, or closures.
3. Add direct `PineValueInProcess` implementations only where they avoid a measured
   `Evaluate`.
4. Preserve lazy structure across `concat`, `take`, `skip`, and `reverse` chains where doing so
   does not create pathologically deep builders.

The entire visible materialization bucket is 2.91%, so isolated work here cannot match the
resolver or binding improvements. Binary concatenation has only a 1.57% measured ceiling in this
run.

### Priority 5: Tail-call frame replacement

Detect when a call is in tail position and replace the current call frame instead of pushing a
new one. Preserve enough logical call information for errors and recursion detection, possibly
using a compact cycle detector separate from the evaluation continuation stack.

This is primarily a scalability and allocation improvement. The directly measured recursion
check is only 0.75%, so tail-call work should follow the first three priorities unless allocation
profiling reveals a larger hidden cost.

### Priority 6: Parallelize independent snapshot cases only for batch throughput

The application computation is single-threaded and 99.85% CPU-bound while the other threads are
mostly waiting. The two snapshot cases are independent, so running them concurrently could
reduce this test's batch wall time on a multi-core machine.

This does not improve single-compilation latency, can increase memory pressure, and may obscure
regressions in the interpreter itself. It should therefore be treated as a test-throughput
change, not as a substitute for optimizing the interpreter.

## Changes not justified by this profile

- **Optimizing `TestLogFileWriter.PumpWindowsPipe`:** it appears on a waiting support thread, not
  on the CPU-bound application path.
- **Tuning thread-pool waits or `WaitHandle`:** these frames describe idle/waiting threads.
- **Adding broad parallelism inside one interpretation:** there is no evidence that the current
  interpreter data structures or evaluation order can safely exploit it, and coordination could
  cost more than it saves.
- **Optimizing `LocalBindingEnvironment.TryGetValue` first:** it is only 0.25% after the linked
  environment change.
- **Further optimizing let dependency planning first:** the cached
  `PrepareLetGroupAndSortNonFunctionDecls` path is only 0.28%.
- **Focusing on `StringEncoding.ValueFromString`:** it is only 0.11% in this capture.
- **Assuming GC is irrelevant:** no GC frame is visible, but this is not an allocation profile.
  Allocation rate still needs a dedicated measurement.

## Validation matrix

For each optimization, record both elapsed time and semantic equivalence:

| Measurement | Why |
|---|---|
| Median wall time per snapshot case | Primary user-visible latency |
| CPU time per case | Distinguishes less work from scheduling noise |
| Interpreter instruction count | Detects reductions from closed folding or new builtins |
| Direct/function-value/builtin application counts | Shows which dispatch paths changed |
| Declaration entries examined per application | Verifies indexed resolution |
| Binding dictionaries, insertions, and resize count | Verifies the binding fix |
| Allocated bytes and Gen0/Gen1/Gen2 collections | Detects improvements hidden by CPU samples |
| Maximum and average continuation depth | Verifies tail-call behavior |
| Output snapshot equality | Guards compiler semantics |

Use at least:

1. the large `demo-backend-state` case;
2. the small `read-source-file-0` case;
3. a warm repeat of an identical source tree to measure cache behavior;
4. a near-identical source tree with one changed file to measure incremental behavior;
5. focused interpreter tests for ambiguous names, constructors, destructuring, recursion, partial
   application, and over-application.

## Recommended sequence

1. Reprofile current `HEAD` with each snapshot case isolated.
2. Add counters for resolver probes and binding construction.
3. Implement direct dictionary lookup/indexing for qualified and same-module declaration
   resolution.
4. Pre-size binding dictionaries as a low-risk confirmation experiment.
5. Replace common prepared-function bindings with precomputed slots.
6. Specialize prepared named calls to bypass the generic resolver/result pipeline.
7. Re-rank builtins, then optimize only the remaining materialization-heavy operations.
8. Evaluate tail-call frame replacement using allocation and continuation-depth data.

If indexed resolution and slot-based bindings remove 80% of their non-overlapping measured
buckets, the profile implies roughly a 2.0x overall speedup ceiling for this workload. That is an
optimistic model: hotspot migration, remaining dispatch work, JIT effects, and the unprofiled
closed-expression-folding change will reduce or redistribute the realized gain. The practical
target should be set only after the current revision is reprofiled.

## Limitations

- The profile represents one run of one test method on one machine.
- The binary's Git revision is not recorded in the profile.
- The filename timestamp suggests the binary predates the latest commit in the checkout.
- The profile combines two snapshot cases without markers.
- Evented sampled stacks estimate time; they do not provide exact invocation counts.
- Synthetic `CPU_TIME` leaves prevent conventional method self-time reporting. Direct
  parent-to-`CPU_TIME` intervals are the closest available exclusive estimate.
- Raw inclusive totals are distorted by recursion and repeated stack frames.
- No allocation stacks, allocation sizes, heap snapshots, or GC pause events are present.
- No hardware-counter data is available for cache misses or branch misprediction.
- Framework and async frames dominate process-wide inclusive totals but do not identify
  application optimization targets.
- A single warmup state may mix JIT, static initialization, cache population, and steady-state
  execution even when those activities do not appear as separately named frames.
