# Fixed-code proof reuse: implementation and measured results

Date: 2026-11-01. This implements intervention 2 from the
[original performance investigation](../2026-11-01-elm-type-inference-test-performance/README.md).
The baseline is Pine revision `f77eb7b4895f5dbe9a8a2005c6f73dfe95b02940`.

**Combined result:** median three-phase time falls **128.225 -> 69.192 s (-46.04%)**,
execution falls **108.548 -> 51.972 s (-52.12%)**, and allocated traffic falls
**35.593 -> 32.718 GB (-8.08%)**. An isolated constructor-proof run improves total
time by 21.32%; most additional savings come from the complementary bounded-proof
tier. The [ablation comparison](#isolating-the-two-proof-tiers) distinguishes these
mechanisms and their overlapping costs.

## 1. What changed

Previously, structural plan lookup was keyed by the **entire encoded template value**.
Changing a captured argument changed that key, even when the code and construction
grammar were unchanged. A cache miss repeated symbolic constructor recognition and
walked the terminal encoded value to establish the recognizer's size/depth bounds.
A warm expression parse cache did not eliminate that walk.

The implementation introduces two complementary, parse-cache-scoped proof tiers:

1. **Parameterized constructor proofs.** A canonical constructor shape retains fixed
   code and encoding tags, but replaces eligible captured literal data with opaque
   references to paths in the current input template. Symbolic recognition establishes
   a reusable plan for that shape. Each successful application binds those references
   to the *actual current template*, shifts the supplied-environment slots back to their
   ordinary numbering, and preserves the original template value for escaped results.
2. **Exact fixed-code bounded proofs.** The first encounter with a terminal encoded
   value runs the existing bounded walker. A successful proof records its expanded
   node count after checking depth; an unsuccessful proof records that the value cannot
   fit the recognizer's maximum budget. Later encounters reuse that proof and charge
   the current recognizer's remaining budget without repeating the tree walk.

The second tier also benefits exact-value fallback when a constructor shape is
uncertain. It overlaps the repeated-terminal-check opportunity described in
intervention 3 of the original report. The measured production change includes both
tiers; their savings must not be treated as independent or added to the old hypotheses.

This is **not** a tag-only cache key, a different hash/encoding, a smaller fuzz run,
an unchecked aggregate-size shortcut, a new IR lowering, or a change to invocation-result
cache policy. Structural `InvokeTemplateDirectly` can still execute IR/native code.
The simple `DirectEvalIfSimpleTemplate -> EvalDirect` shortcut is a separate path.

### Safety and exact-value preservation

- Fixed tags and code-looking literal payloads remain part of the shape key. Canonical
  keys compare complete Pine values, not just hashes.
- Nested literal encodings can expose captured data as opaque root paths. The symbolic
  recognizer cannot treat those paths as invariant code. If a shape cannot prove a
  complete fixed-code plan, it falls back to the existing exact-value recognizer;
  an uncertain shape is **not** a negative result for every possible bound value.
- Code-looking *data* may conservatively remain fixed. That can reduce reuse but cannot
  freeze a changing terminal expression. Tests cover such values and differing bodies.
- Binding uses a new per-application memo dictionary and immutable recipes. It never
  stores a mutable environment or an example capture in a shared positive proof.
  Intermediate materialization and zero-argument materialization retain exact encodings.
- The bounded proof is keyed by the exact terminal value. Its first walk still checks
  expanded DAG multiplicity, width, depth, and the 16,384-node/128-depth limits.
  A caller with less remaining budget still rejects a proof whose node count does not fit.
- Both caches follow `PineVMParseCache` lifetime, so compiler and VM recognition share
  them through the existing `TryParseTemplate` entry point. The existing full-value
  runtime plan cache and its lifetime remain unchanged.
- Concurrent factories may duplicate initial proof work, as with `ConcurrentDictionary`
  generally. Published proofs are immutable, and binding state is not shared.
- Warm lookups still observe the work profile's cancellation checkpoint.

### Implementation surfaces

| Surface | Responsibility |
| --- | --- |
| `ExpressionTemplateProofCache.cs` | Shape canonicalization, shared symbolic proof lookup, exact rebinding, bounded fixed-code proof lookup |
| `ExpressionTemplatePlan.cs` | Shared entry point, unchanged exact fallback, supplied-environment offset during proof construction, validated budget application |
| `PineVMParseCache.cs` | Shared proof-cache ownership and final entry-count exports |
| `PineVMWorkProfile.cs` | Physical shape/proof/binding counters, distinct from logical VM work |
| Profiling runner `Program.cs` | Exports both new cache counts with the ordinary scenario summaries |
| `ExpressionTemplateProofCacheTests.cs` | Capture rebinding, exact escaped results, code/data separation, budget equivalence, concurrent binding, cancellation |

## 2. Experiment design

The pinned project is `Janiczek/elm-syntax-type-inference` at
`aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f`, with seed **1050281084**, **100 fuzz runs**
and **one worker**. The ordinary shared-cache profiling runner from the original
investigation is used for both revisions.

Source-content fingerprint:
`07b4ec08dcb4ff7d44988740247a0a419b7e38dcaa4f1a69447c39088f728eb4`.

The baseline resolution fingerprint is
`10ad8e0bc4536c36c899a6d05c7f0a6a3232a5b3c8a527933f4a5dc0923e6ab2`;
the optimized fingerprint is
`f6bf6781f8fd3fcb6c8b06eae2f97ed42e6d7622687b02665190e66d64e30841`.
They differ because `Configuration.CompilerImplementationId` includes the
**Pine.Core assembly MVID**, which changes when its implementation is rebuilt
(`ElmDependencyResolutionModels.cs:111-113`). This is an intentional compiler-cache
identity change, not different Elm dependencies.

The comparison script verifies exact equality of manifests, requirements, resolved
package records, substitution source fingerprints and configuration after excluding
only that compiler build identity. It separately requires each revision's raw
resolution fingerprint to remain stable. Nondeterministic selection ordering in the
diagnostic `Trace` list is not a resolved input. The raw manifests are retained;
the fingerprints are not rewritten or falsely reported as equal.

The host is the same shared Ubuntu 24.04 x64 environment with two exposed logical
AMD EPYC 9V74 CPUs, approximately 7.5 GiB RAM, no swap, .NET SDK 10.0.403 and runtime
10.0.14. These are not dedicated-machine or Windows screenshot timings.

### Run boundaries and controls

- Baseline and optimized Release binary directories are frozen separately. The retained
  `binary-sha256.txt` identifies the actual executable and Pine.Core assemblies.
- Three new baseline/optimized pairs are **interleaved**, each in a fresh process.
  No tests, builds, trace analysis, or other benchmark processes run concurrently.
- Ordinary runs use `baseline` mode: no work profile or diagnostic instrumentation.
  Process counters exclude initial native-leaf setup, consistently on both sides.
- After the ordinary pairs, both revisions run in `sharing` observer mode with ordinary
  shared-cache boundaries. Observed times and allocations include measurement overhead;
  they are not substituted for unobserved performance numbers.
- Allocation/GC EventPipe captures and execution-only Linux CPU captures are separate
  fresh processes. Kernel capture waits for the runner's callback after preparation and
  selects the **exact target PID**, never every process on the shared host.
- Every run must produce 559 passes, zero failures/incomplete results, identical source
  fingerprints and identical resolved dependency inputs, and eight fuzz properties each reporting 100 requested
  and elapsed runs. VM logical work, cache activity and direct-template counters are
  checked separately rather than assuming every physical recognition counter should stay equal.

An earlier exploratory capture used the initial implementation before additional
allocation counters were added. It completed in 68.093 s and allocated 32.712 GB.
It is **not** one of the final interleaved samples.

## 3. Quantitative results

### 3.1 Ordinary runtime, CPU, allocations and pauses

| Metric | Baseline median | Optimized median | Reduction |
| --- | ---: | ---: | ---: |
| End-to-end three-phase wall time | **128.225 s** | **69.192 s** | **59.033 s / 46.04%** |
| Compilation | 7.739 s | 7.571 s | 2.17% |
| Preparation | 11.557 s | 9.649 s | 16.51% |
| Execution | **108.548 s** | **51.972 s** | **52.12%** |
| Process CPU | **140.205 CPU-s** | **77.601 CPU-s** | **44.65%** |
| Process-window allocated bytes | **35.593 GB** | **32.718 GB** | **2.875 GB / 8.08%** |
| GC paused time | 12.330 s | 11.090 s | 10.06% |

GB means decimal bytes; CPU-seconds can exceed wall-seconds because GC and other
process threads contribute. The reported phase total excludes source-loading/startup
outside those phases. Summed VM-call time is not added to wall time.

| Interleaved pair | Baseline total | Optimized total | Paired wall reduction |
| --- | ---: | ---: | ---: |
| 1 | 128.709 s | 69.192 s | 46.24% |
| 2 | 128.225 s | 71.223 s | 44.45% |
| 3 | 125.602 s | 68.639 s | 45.35% |

The baseline range is 125.602-128.709 s; the optimized range is 68.639-71.223 s.
Execution ranges are 106.305-110.678 s and 48.655-54.126 s, respectively. All three
paired comparisons improve execution by 51.10-54.23% and allocations by 7.98-8.10%.
This is a large, repeated effect on this host, not a precise cross-machine forecast
or a confidence interval from three samples.

### Isolating the two proof tiers

Two additional **single-run exploratory ablations** separate parameterized constructor
proof reuse from exact bounded-code proof reuse. The saved patches disable one tier
without adding production flags. Both preserve every logical/cached VM counter and
the resolved package/substitution inputs.

| Configuration | Unobserved total | Execution | CPU | Allocated bytes | Observed recognizer visits |
| --- | ---: | ---: | ---: | ---: | ---: |
| Neither tier, main baseline median | 128.225 s | 108.548 s | 140.205 CPU-s | 35.593 GB | 7,968,192,007 |
| Constructor shapes only, one run | **100.887 s** | 83.156 s | 110.654 CPU-s | 34.368 GB | 4,525,423,184 |
| Exact bounded proofs only, one run | **69.994 s** | 52.465 s | 78.190 CPU-s | 32.502 GB | 17,105,752 |
| Both tiers, main optimized median | **69.192 s** | 51.972 s | 77.601 CPU-s | 32.718 GB | 14,470,973 |

The parameterized-constructor tier alone removes about **43.2% of recognizer visits**
and its exploratory total is **21.32% below** the main baseline median, consistent
with the original intervention-2 hypothesis. Fixed-code bound reuse is responsible for
most of the remaining repeated-walk elimination, especially for negative/uncertain
shapes that still use exact recognition.

These rows are not a four-way, three-sample factorial timing study. In particular, the
approximately 0.8 s difference between bounded-only and combined totals is within the
observed run variation and does **not** establish a precise incremental wall speedup
for shape reuse after bound reuse. Combined allocation is about **216 MB higher**
than the bounded-only exploratory run, consistent with its key/binding allocation tax.
The two mechanisms overlap strongly; adding their individual speedups would be wrong.

The third optimized compilation takes 9.789 s versus approximately 7.5 s in the other
two runs. Preparation also varies substantially on the baseline. Do not attribute
small compilation/preparation differences confidently to proof reuse. The decisive
change is execution CPU and wall time. GC pauses fall by only about 1.24 s at the median,
so less GC suspension cannot explain the 59 s total improvement.

### 3.2 Recognition work and proof reuse

These are serial matched-observer measurements, not timing samples.

| Existing recognition counter | Baseline | Optimized |
| --- | ---: | ---: |
| Physical recognizer visits | **7,968,192,007** | **14,470,973** |
| Recognizer stages | 935,659 | 473,976 |
| Constructor stages | 95,816 | 114,899 |
| Recognition recipe child arrays | 1,248,447 | 857,641 |
| Recognition recipe child slots | 3,806,871 | 2,660,353 |
| Budget-limit encounters | 265,156 | 276,138 |
| Runtime factory results rejected by budget | **253,281** | **253,281** |
| Malformed/unsupported runtime factory results | **139** | **139** |
| Terminal parse failures | **0** | **0** |

The apparent 99.82% reduction in the existing visit counter is not the complete
denominator: new shape scanning and binding have separate counters. Adding **6,092,557
shape visits** and **2,067,837 binding visits** gives **22,631,367** physical
recognition/selection/binding visits, still **99.716% below** the old expanded walk
count. The optimized recognizer counter already includes the cold fixed-code walks;
their 11,178,771 visits must not be added again.

| New proof work | Optimized count |
| --- | ---: |
| Parameterized shape probes / hits | **801,643 / 774,469** |
| Shape factories / final entries | **27,174 / 27,174** |
| Successfully bound shared plans | **507,940** |
| Uncertain-shape fallbacks to exact recognition | 293,703 |
| Canonical shape child arrays / slots | 1,975,037 / 7,139,462 |
| Binding recipe child arrays / slots | 523,033 / 1,572,846 |
| Binding recipe visits | 2,067,837 |
| Exact bounded fixed-code probes / hits | **287,908 / 286,658** |
| Bounded-proof factories / final entries | **1,250 / 1,250** |
| Cold fixed-code walk visits | **11,178,771** |
| Bounded-proof budget rejections | 276,138 |

**Shape lookup reuse is 96.61%**, including uncertain shapes.
**Bounded fixed-code proof reuse is 99.566%**. Positive shared plans account for 507,940
applications. Even pessimistically treating every one of the 27,174 shape factories
as a distinct positive proof, at least **94.65%** of those applications are repeat
proof uses; the true positive-only rate is higher because the cache includes uncertain
shapes. Do not equate this with invocation-result hits or the simple-direct template census.

Constructor stages and budget encounters increase because an uncertain abstract
shape can fail before exact recognition is retried. The extra 10,982 budget encounters
are not additional failed tests or additional rejected runtime templates: the runtime
budget-negative and malformed counts remain identical.

The cache reduces symbolic *proof establishment*, not every full-value plan entry.
Runtime plan probes/hits/misses remain **3,617,077 / 2,855,926 / 761,151**; factory
executions remain 761,151 and negative resolutions remain 1,394,972. The full-value
plan cache still contains **761,151 entries**. It would be incorrect to claim that the
runtime plan cache shrank to 27,174 entries.

There is a real new allocation tax: shape selection builds almost two million
canonical child arrays, and binding adds 523k recipe arrays. Recognition-only array
counts fall, but recognition plus binding arrays total 1,380,674 versus 1,248,447
previously. Whole-process allocated bytes nevertheless fall by 2.875 GB because
repeated large bounded-walk storage is eliminated. Further reduction of key/binding
allocation remains an opportunity; these arrays are not free just because proof lookup hits.

The shape child-array counter specifically instruments `List`/`Eval` key construction;
it does not count literal-key arrays or cold `Parameterize` path/recipe arrays.
Binding-array counts exclude the outer intermediate-recipe array and memo dictionary.
These physical counters explain mechanisms but are not a complete heap-object census.
Whole-process allocation totals and EventPipe sampling include those uncounted costs.

### 3.3 Behavior, cache activity and simple direct eval

The complete observer `Counters`, `CachedWorkCounters`, and `CountersByPhase`
objects are **identical** between revisions. In particular:

| Logical/runtime work | Both revisions |
| --- | ---: |
| Runtime invocations | 7,036,828 |
| Loop iterations | 1,471,117 |
| VM instructions | 162,678,119 |
| Logical list builds / slots | 10,582,211 / 35,442,356 |
| Structural terminal invocations | 2,224,454 |
| Consolidated eval stages | 2,505,845 |
| Deferred allocations / materializations | 4,961 / 169 |
| IR compilations / parsed entries | 13,467 / 723,412 |
| Encoded / reduced expression entries | 255,340 / 660,092 |

Invocation-cache statistics are also exactly equal: 1,542,385 lookups, 56,856 hits,
258,191 admission attempts, 255,033 admissions and 3,158 rejected admissions. The
3.686% result-cache hit rate is unchanged. No reduced fuzz work, extra result-cache hits,
changed logical accounting, or different cache policy explains the speedup.

The requested simple direct-eval path from the original investigation is unchanged:
36,133 recognition probes, 35,539 applications, 3,450 applied identities, 707,181
logical instructions, 274,666 logical list builds, and 671,642 logical slots.
Its associated root parse probes/hits remain 39,987 / 36,537. Both matched captures
allocate **108,834,392 bytes** in direct-template application windows. The earlier
memoization inefficiency is not fixed by this intervention.

All profiled parser root probes decrease from **3,180,578 to 2,684,404**; hits decrease
by the same 496,174, while misses remain **256,270** and final parsed entries remain
723,412. Shared positive proofs avoid redundant warm root parsing, not necessary new
expression parsing.

### 3.4 Where the improvement is concentrated

Both observers recover all **559 test windows**. Their process-wide resource deltas
include observer work and background process activity.

| Observer phase | Baseline wall / allocated traffic | Optimized wall / allocated traffic |
| --- | ---: | ---: |
| Compilation | 7.897 s / 2.140 GB | 7.320 s / 2.135 GB |
| Preparation | 10.841 s / 4.047 GB | 10.248 s / 4.066 GB |
| Execution | **150.253 s / 30.269 GB** | **69.234 s / 27.427 GB** |

Preparation allocation slightly increases: proof caching is not an across-the-board
allocation reduction. Almost all net allocation savings and observed CPU reduction
come from execution.

| Matched expensive test/property | Baseline observer wall / bytes | Optimized observer wall / bytes |
| --- | ---: | ---: |
| Nested-list `fastConcatMap` equivalence fuzz property | **58.339 s / 6.532 GB** | **15.049 s / 4.993 GB** |
| Large list of large records inference test | 18.581 s / 3.441 GB | 13.396 s / 3.036 GB |
| `findLastMap` equivalence fuzz property | 5.697 s / 0.648 GB | 1.336 s / 0.469 GB |
| Large list literal inference test | 4.264 s / 0.935 GB | 2.285 s / 0.853 GB |
| SCC maximality property | 2.526 s / 0.593 GB | 2.091 s / 0.539 GB |

The nested-list property loses about **43.29 s** of observer wall time and **1.538 GB**
of allocation traffic. This is consistent with repeatedly changing fuzz values while
the fixed terminal code stays reusable. These windows include generation, comparison
implementations and the property body; they do not isolate one Elm function's cost.

### 3.5 Trace evidence and remaining hotspots

Both complete EventPipe captures and both execution-only kernel captures report
**zero lost events**. The compact speedscope files have valid frame/sample indices
and equal sample weights.

The kernel baseline contains **130,372 CPU samples**, with a 121.780 s event span;
the optimized capture contains **67,467**, with a 60.912 s span. Their corresponding
execution phases are 121.674 s and 60.646 s. These are separate traced processes,
not the unobserved timing samples. They cover approximately the entire execution
after the preparation callback, including small collection/termination boundaries.

| On-CPU method/subsystem | Baseline share | Optimized share |
| --- | ---: | ---: |
| `RecognitionBudget.CheckValue`, exclusive | **31.49%** | **0.089%** |
| `CheckValue`, inclusive | **37.50%** | **0.092%** |
| Structural `TryParseTemplate`, inclusive | **41.27%** | **1.23%** |
| Shape construction `BuildShape`, inclusive | Not present | 1.07% |
| Shared-plan recipe binding, inclusive | Not present | 0.45% |
| IR entry setup `ExpressionEntryLessCache`, inclusive | 14.85% | 22.13% |
| Deterministic content hashing, inclusive | 9.75% | 16.06% |
| Binary value encoding, inclusive | 8.13% | 13.27% |
| Concrete materialization, inclusive | 5.30% | 8.92% |
| All generic `EvalDirect` users, inclusive | 0.63% | 1.17% |
| `__tls_get_addr`, exclusive | 2.65% | 0.66% |
| Unresolved native/JIT leaf frames, exclusive | **30.92%** | **44.99%** |

Inclusive rows overlap and must not be added. New shape/binding work may be inlined
under broader recognizer frames. Native unwinds are incomplete, so unresolved frames
remain unattributed rather than being labeled GC or application code.

The original investigation's independent earlier capture found 40.07% exclusive
`CheckValue`; the new paired baseline finds 31.49%. Different scheduling, trace coverage,
unresolved frames and runtime overhead change a sampled denominator. The before/after
conclusion is robust: the bounded walk falls below 0.1% in the optimized capture, and
exact counters independently show that almost all repeated walks disappear.

Remaining subsystem *percentages* rise because total CPU shrinks. The unchanged 13,467
IR compilation entries and logical VM work do not mean hashing or generic direct
evaluation became more expensive. Hashing/encoding and ordinary frames are now
larger relative targets; this intervention did not optimize them.

### 3.6 Allocation sites, GC and retained-memory limits

EventPipe samples **36.677 GB / 311,552 ticks** before and **33.797 GB / 284,603 ticks**
after. These are traffic estimates across whole-process trace windows, including
startup; they are not exact per-type byte/object counts. The ordinary runner's
allocation medians are the primary total-allocation comparison.

| Sampled family/site | Baseline | Optimized |
| --- | ---: | ---: |
| Stacks containing `CheckValue` | **2.966 GB** | **0.00564 GB** |
| Byte arrays | 4.350 GB | 4.373 GB |
| Ordinary stack frames | 3.218 GB | 3.181 GB |
| In-process value wrappers | 3.176 GB | 3.174 GB |
| Serializer `Dictionary<ListValue,long>` entry arrays | 2.194 GB | 2.105 GB |
| In-process child arrays | 1.629 GB | 1.617 GB |
| `int[]` | 1.429 GB | 1.517 GB |
| Concrete `PineValue[]` | **1.280 GB** | **1.485 GB** |
| Concrete list objects | 0.683 GB | 0.748 GB |
| Stacks containing deterministic encoding | 5.758 GB | 5.711 GB |
| Stacks containing IR entry setup | 13.436 GB | 13.087 GB |

The bounded walk's sampled allocation attribution falls **99.81%**. Previously each
repeat built traversal storage for encoded values; now only the first exact code proof
does so. More concrete child-array/list traffic is consistent with canonical shape
keys and rebound constant recipes. Serializer, frame and wrapper traffic remain large.
Stack-attributed rows overlap and use the analyzer's visible top-eight allocation
frames; do not sum them as disjoint allocations or assume complete ancestor coverage.

Paired GC suspensions sum to **12.824 s before / 12.075 s after**; collection starts
are 1,953 / 1,782. Gen-2 start-to-stop time is partly concurrent and is not added to
paused time. Allocation reduction does not make GC suspension disappear.

The maximum reported GC heap is **3.278 GB before / 3.319 GB after**. This experiment
therefore **does not establish a reduction in peak or retained heap**, despite lower
allocation traffic. The old full-value caches remain populated and the new caches add
27,174 shapes plus 1,250 bounded proofs. Root graphs can be shared across caches;
entry counts cannot be multiplied by assumed payload sizes to claim retained savings.
A retained-heap census would still be needed.

### 3.7 Verification and interpretation

The final implementation passes **661 IntermediateVM tests**, including the new
proof-rebinding/budget/concurrency/cancellation regressions. The profile's existing
cancellation checkpoint is throttled to every 4,096 calls; the warm-hit test respects
that contract rather than imposing immediate cancellation on one lookup.

All six ordinary interleaved processes, both matched observers, and all four traced
scenario processes complete with 559 passes and all eight fuzz properties at 100 runs.
Both ordinary ablation runs and both ablation observers also satisfy those checks.
The normalized resolved-input comparison is exact, and all logical/cached VM accounting,
result-cache statistics and full-value plan-cache outcomes match.

The production assembly rebuilt after isolated experiment preparation is byte-for-byte
identical to the frozen assembly used for the main captures:
`1fe58c1a1ac0f32c44492a26a8c444f295f90bf67ea77bd058538d9f1037d837`
(SHA-256 of `Pine.Core.dll`). No experimental toggle remains in the shipped code.

**Result:** the combined fixed-code proof reuse change makes this pinned serial scenario
approximately **1.85x faster**, removes most bounded-recognition CPU, and reduces allocated
traffic by approximately **2.875 GB**. It does not shrink the existing full-value plan cache,
eliminate ordinary VM/hashing allocation, or fix simple direct-constructor memoization.
Results from this two-vCPU host must not be applied directly to the original parallel
Windows screenshot, and overlapping intervention hypotheses must not be added.

## 4. Reproduction and retained support

Build the profiling runner for each revision after repository prebuild, and copy the
entire Release output directory before switching/rebuilding. Do not point both
variants at a build output that can be overwritten.

```sh
cd implement
dotnet run --project prebuild/prebuild.csproj --configuration Release
dotnet build ../explore/2026-11-01-elm-type-inference-test-performance/ProfileScenario.csproj -c Release
```

From the repository root, run the saved comparison driver using separate frozen binaries:

```sh
bash explore/2026-11-01-fixed-code-proof-reuse/measure-comparison.sh \
  /absolute/baseline/ProfileScenario.dll /absolute/optimized/ProfileScenario.dll \
  /absolute/pinned-project /absolute/new-comparison-directory
python explore/2026-11-01-fixed-code-proof-reuse/compare.py \
  /absolute/new-comparison-directory /absolute/comparison.json
```

For paired allocation and true-CPU evidence:

```sh
bash explore/2026-11-01-fixed-code-proof-reuse/trace-comparison.sh \
  /absolute/dotnet-trace /absolute/baseline/ProfileScenario.dll \
  /absolute/optimized/ProfileScenario.dll /absolute/pinned-project \
  /absolute/new-trace-directory
```

The trace driver uses `dotnet-trace` 10.0.750501 and the retained TraceEvent analyzer.
Linux CPU sampling requires perf support/permissions. EventPipe exports official
speedscope profiles; the analyzer also produces compact managed-stack and kernel-CPU
profiles. Compact units are sample counts (`none`), not fabricated elapsed seconds.
Native unwinds can be incomplete, so unresolved samples remain in the denominator.

Raw traces, ETLX files, complete NDJSON streams, copied binaries, and downloaded
third-party source are not committed. Compact evidence and numeric summaries are
retained under `measurements/`; the programs regenerate the full captures.

### Reproducing component ablations

Apply `ablation-constructor-only.patch` or `ablation-bounded-only.patch` in a **separate
working tree at the implementation revision**, then build and freeze its entire runner
output as above. The first patch bypasses only bounded-proof reuse; the second bypasses
only constructor-shape lookup. Do not apply both or overwrite the production binaries.
Run the comparison driver with an optional final repetition count of `1` for an
exploratory component experiment; pass the same count to `compare.py`.

In the retained ablation capture, the driver's `baseline` label means
**constructor-only** and `optimized` means **bounded-only**. The separate
`ablation-comparison.json` preserves that labeling; it is not another baseline-to-full
comparison. Both observers retain the exact main-run logical counters and runtime
plan-cache outcomes.

### Evidence inventory

| Retained files under `measurements/` | Contents |
| --- | --- |
| `comparison.json`, `baseline-{1,2,3}.json`, `optimized-{1,2,3}.json` | All six ordinary summaries, distributions, paired deltas, exact logical comparisons and all 559 matched test/resource windows |
| `{baseline,optimized}-observed.json.gz` | Complete observer summaries, including site counters and direct-template census |
| `{baseline,optimized}-dependencies.json.gz` | Actual manifests, requirements, resolved package/substitution records and compiler identities |
| `{baseline,optimized}-summary.json.gz`, `*-kernel-summary.json.gz` | Managed/allocation and target-process CPU trace summaries |
| `{baseline,optimized}-summary.speedscope.json.gz`, `*-kernel-summary.speedscope.json.gz` | Four compact, valid speedscope profiles, with unresolved frames preserved |
| `{baseline,optimized}-derived.json.gz` | Derived GC intervals, allocation tables and stack/subsystem attribution |
| `{baseline,optimized}-{traced,kernel-run}.json` | Result/settings summaries for the four traced scenario runs |
| `{constructor-only,bounded-only}.json`, `*-observed.json.gz`, `*-dependencies.json.gz`, `ablation-comparison.json` | Component timing samples, counters, logical comparisons and resolved inputs |
| `binary-sha256.txt`, `ablation-binary-sha256.txt`, `dotnet-info.txt`, `trace-version.txt`, timestamps and `*.log` | Binary/tool provenance, capture logs and the final passing test log |
| `SHA256SUMS` | Checksums for every retained evidence file |

Compressed files use deterministic gzip headers and were byte-verified after
decompression against the originals. To inspect a profile, decompress its `.json.gz`
to a file outside the worktree and load the resulting JSON in a local speedscope viewer.
The complete ordinary/observer raw streams are deliberately omitted; resource-window
derivations remain available in the comparison JSON, and the saved programs recreate them.
