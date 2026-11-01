# Syntax-based Elm test discovery and source navigation

This implements the preparation step for the editor integration discussed in
[the UX/protocol analysis](analysis.md), not a language-server protocol or VS Code
Testing API integration.

## Implementation

The public API is `Pine.Core.Elm.Testing.ElmTestDiscovery.DiscoverTests`, with
filesystem, in-memory project and already-resolved-build overloads.

Discovery follows the existing runner's exposed zero-argument roots under
`tests/`, excluding nested projects. It reuses dependency resolution, package
substitutions, compiler module identities and original module provenance.
Demand canonicalization processes the test roots and their dependencies, rather
than evaluating every body in every module. The interpreter also receives Pine's
bundled source implementation of `Basics`, which the compiler normally supplies
natively.

The syntax interpreter constructs actual `Test` values. This handles aliases,
helpers, computed descriptions, conditionals, records and `List.map`-generated
suites, without approximating them by recognizing calls named `Test.test`.
The existing `PineTestBridge.prepare` and `Test.Runner` enumerate actual leaves,
preserving validation, `only`, `skip`, unit/fuzz/todo and empty-group behavior.
The result is the current runnable listing, not an inventory of skipped or
excluded tests. Empty groups are omitted, matching compiler-based listing.

Assertion and fuzz callbacks remain closures and are not executed. Discovery
still evaluates arbitrary suite-construction code: it can fail, hit interpreter
quotas or be canceled. It is not a purely static scan and does not perform the
compiler's complete type checking. Compilation remains the execution gate.

### Mapping range-free syntax to source

`ElmSyntaxExpressionPath` implements the expression-addressing subset of
[`ElmSyntax.Path`](../../implement/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Path.elm).
Paths are relative to a named top-level declaration's body; module/declaration
identity is stored separately. Its CST lookup mirrors
[`ElmSyntax.Concrete.SourceLookup`](../../implement/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Concrete/SourceLookup.elm):

- Parentheses disappear from the AST and are transparent during lookup.
- Application/list/tuple/conditional children retain their ordered indices.
- Lambda/let bodies and case branches have dedicated structural edges.
- Record setters use field name plus occurrence, followed by their value child,
  because AST fields are sorted while CST fields retain source order.
- Literal spelling, such as hexadecimal integers, need not match AST normalization.

Syntax-query canonicalization qualifies operator targets while keeping their
binary-node shape. Ordinary compiler canonicalization still lowers them to
function applications. Interpreter preparation for discovery disables
shape-changing lowering and closed-expression reduction. Prepared case dispatch
groups constant branches but preserves their order, so paths still correspond
to the original branches.

Opt-in, reference-keyed expression-result observers associate actual constructed
test values with their originating AST paths. Tracking is outside value equality
and does not attach editor metadata to runtime values. Standard library
implementation details do not replace the user's construction site.

`result.GetSourceLocation(id)` returns original source/module ownership, declaration,
node path and a one-based, end-exclusive range projected from the corresponding
CST. A generated test normally navigates to its factory body. Several distinct
IDs can therefore share one source location. Unknown IDs and mismatched paths
fail explicitly; no location is guessed.

### Identity and execution boundary

IDs encode the project-relative root file, root declaration and leaf ordinal,
independently of labels. They distinguish duplicate labels and generated cases.
They are discovery-result identities, not persistent semantic identities across
arbitrary edits: adding/reordering leaves can change them. Editor hosts must
manage document freshness and resolve navigation on the relevant discovery result.

`GetExecutionFilter(id)` translates the selected leaf into the existing runner's
exact path or `#N` selector. It preserves discovery ordinals assigned *before*
global `Test.only` filtering. Execution recompiles the same sources and continues
on PineVM; interpreter closures are not transferred between engines.

Two interpreter/runtime issues exposed by this path were fixed: `length` must
inspect closure-bearing lists without materializing their children, and error
rendering must not materialize a nested opaque value while probing a type tag.
Fresh-process benchmarking also exposed recursive `ElmValue`/reused-cache
initialization; singleton values are now constructed without entering that cache.

## Measured comparison

Measurements use the implementation on top of baseline commit
`c5efc2d` recorded in full in [the raw samples](benchmark/results.json).
The baseline is `ElmTestRunner.CompileAndRunTests(..., listTests: true)`, not
assertion execution. The new path includes source-location association.

Environment: Release build, .NET SDK **10.0.403**, runtime **10.0.14**, Ubuntu
**24.04.3 LTS**, x64, AMD EPYC **9V74**, **2 allocated logical CPUs**.
The substituted test library is Pine's `elm-explorations/test` **2.2.1** implementation
(`pine-elm-test-2.2.1-fuzz-v2`); the project uses `elm/core` **1.0.5** and
`elm/random` **1.0.0**.

Each engine/shape/size runs in a separate fresh process, sequentially, with five
invocations. "First use" is invocation zero; "warm" is the median of invocations
one through four. Each invocation creates fresh discovery/compiler execution
state, with process-global caches naturally warm after the first call.
Fixture creation, process launch and forced pre-sample GC are outside the timer.
Project loading, resolution, parsing and discovery are inside it.

| Suite | Leaves | First-use syntax / compiler | Warm syntax / compiler | Warm speedup |
|---|---:|---:|---:|---:|
| Direct test expressions | 100 | 5,950.9 / 8,468.1 ms | 231.7 / 571.6 ms | 2.47x |
| Direct test expressions | 1,000 | 5,802.1 / 10,917.3 ms | 512.5 / 1,526.2 ms | 2.98x |
| `List.map` factory | 100 | 5,583.0 / 8,353.1 ms | 218.2 / 553.1 ms | 2.53x |
| `List.map` factory | 1,000 | 5,651.9 / 11,038.6 ms | 404.9 / 989.1 ms | 2.44x |

| Suite | Leaves | Warm allocation, syntax / compiler |
|---|---:|---:|
| Direct | 100 | 68.4 / 172.7 MiB |
| Direct | 1,000 | 168.2 / 443.3 MiB |
| Generated | 100 | 66.3 / 164.8 MiB |
| Generated | 1,000 | 147.3 / 263.6 MiB |

Allocations are medians of process-wide `GC.GetTotalAllocatedBytes` deltas,
not peak memory or retained heap sizes. Both paths had no embedded prebuilt
runtime artifacts in this checkout and built their reused-value caches on first
use. That multi-second initialization dominates first-use times; these are
not predictions for an already-running language server or a packaged build.

Warm syntax phase medians, in milliseconds:

| Suite | Leaves | Source preparation | Interpreter preparation | Discovery/navigation |
|---|---:|---:|---:|---:|
| Direct | 100 | 136.9 | 55.0 | 27.7 |
| Direct | 1,000 | 216.6 | 88.8 | 194.9 |
| Generated | 100 | 128.9 | 61.6 | 28.1 |
| Generated | 1,000 | 149.7 | 51.5 | 205.2 |

Phase medians need not add to the median total. The source phase remains a
substantial cost; avoiding compiler work alone does not make project-wide
rediscovery free. A future editor host can reuse already-resolved inputs, but
these measurements do not claim incremental-cache performance.

Every sample verifies the **complete expected file/group/test path sequence**,
not just a count. The syntax path additionally resolves every returned ID and
checks its source file. Results favor syntax discovery by **2.44-2.98x** in these
warm fixtures, with lower allocation. This is a measured fixture result, not a
universal guarantee for arbitrary project construction code or dependency graphs.

### Reproduction

Build once from the repository root:

```sh
dotnet build explore/2026-11-01-elm-editor-test-discovery-and-execution/benchmark/benchmark.csproj --configuration Release
```

Run each engine in a fresh process; vary `100`/`1000` and `direct`/`generated`:

```sh
dotnet explore/2026-11-01-elm-editor-test-discovery-and-execution/benchmark/bin/Release/net10.0/benchmark.dll 100 direct 5 syntax
dotnet explore/2026-11-01-elm-editor-test-discovery-and-execution/benchmark/bin/Release/net10.0/benchmark.dll 100 direct 5 compiler
```

The persistent harness emits JSON samples and throws on incorrect discovery or
navigation. Automated C# regressions in `ElmTestDiscoveryTests` check exact source
expressions, parentheses, record order/updates, let/case/lambda paths, custom
operators, imported/generated factories, duplicate IDs, PineVM selection,
`only`/`skip` parity, unknown IDs, errors and cancellation.
