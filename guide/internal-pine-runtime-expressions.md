# Internal Pine Runtime Expressions

`Pine.Core.Runtime.InternalExpression` is the production intermediate VM's
release-private retained model. Dynamic parsing, compilation, reduction,
inlining, environment substitution, frame roots and template terminals use it.
Pine's language, canonical value encoding and public evaluation APIs remain
unchanged; this implementation and its constructor programs may change every release.

## Retained model and conversions

`Expression` has `Literal`, `List`, `Eval`, `Builtin`, `Conditional`,
`Environment` and `Label`, corresponding to every canonical variant
(`Literal` corresponds to canonical `Litral`). `BuiltinFunction` enumerates
all 17 Pine primitives. `BuiltinFunctions.FromName` rejects unknown,
case-mismatched and internal-only function names; `Builtin` rejects undefined
enum values. `Label` retains the arbitrary label value, not an eagerly
computed display string.

Additional variants:

| Retained variant | Exact canonical expansion | Retained savings |
| --- | --- | --- |
| `BuiltinBinary(function, left, right)` | `Builtin(name(function), List([left, right]))` | No input-list node or child array; checked enum and two expression references |
| `Length_Equal_Binary(sequence, other, lengthOnLeft)` | `Builtin("equal", List([Builtin("length", sequence), other]))`, or the reversed operand order | No length node, equality input-list node or child array; two expression references and an inline ordering flag |
| `HeadSkip(count, input)` | `Builtin("head", Builtin("skip", List([Litral(count), input])))` | No skip node, input-list node/array or literal-count reference; count is an inline Int32 |

These are structural fusions, not new arithmetic/sequence semantics. In
particular, **any** builtin whose input is statically a two-item expression
list can use `BuiltinBinary`, including normally unary or variadic functions.
The name describes the input syntax, not the builtin's semantic arity.
Other input shapes stay `Builtin(function, input)`; both retained operands
may evaluate to arbitrary blobs/lists. Binary length equality and literal
head/skip take priority over the more general fusion.
Head/skip operates with Pine semantics, not platform integer
overflow. Signed Int32 is a storage optimization suitable for x86/WASM;
Pine integer semantics do not become fixed-width.

`ExpressionConversion.FromCanonical` fuses the popular shapes by default.
For head/skip it requires a **byte-exact canonical signed Int32 literal**.
Negative and boundary counts are retained exactly. Larger integers,
noncanonical encodings, invalid counts and dynamic counts retain general
head/skip syntax. No literal or label payload is silently regenerated.
`ToCanonical` expands the fused wrappers back into their exact original
canonical syntax.

`Length_Equal_Binary` accepts arbitrary expressions for both the measured sequence
and the other operand, not a precomputed length or constant-only comparison.
It fuses only binary equalities with a direct length operand. Either side can
contribute the length; the flag preserves the original evaluation order and
syntax. If both operands are length calls, the left one is fused and the
right remains a general builtin. Generic `Head` and `Equal` retained variants
were removed: their small savings did not justify extra reduction branches.

Both conversions preserve shared child identity within one conversion.
`fusePopularShapes: false` preserves the general variant layout. Explicit
general `Builtin(function, List([left, right]))` and binary builtin nodes can represent the same
canonical syntax; canonical conversion with fusion normalizes the former.
Therefore:

- Canonical -> internal -> canonical is exact structural equality.
- Internal -> bytes -> internal preserves the exact retained variants.
- Internal -> canonical -> internal is exact for normalized inputs; use
  `fusePopularShapes: false` for general-only inputs, or compare against the
  explicitly normalized representation.

Collections are immutable/owned, structural hashes are computed at
construction, and fields have no mutable/init setters. Reconstruct a changed
node instead of mutating it. Conversions use iterative postorder traversal.
Binary encoding/decoding limit recursive nesting to 512 and also check available
execution stack; this is a current codec bound, not a limit of the Pine language.

## Canonical Pine value encoding

`ExpressionValueEncoding.EncodeExpressionAsValue` emits exactly the standard
compact 2026 expression value representation, expanding fused nodes directly
as immutable Pine values. `ParseExpressionFromValue` reads compact 2026
values directly into the retained model, with the same optional
`fusePopularShapes` setting as canonical-model conversion. It also accepts the
compact `Condition` alias. Invalid shapes and unknown builtin names return
explicit `Result` errors.

Neither path allocates an intermediate canonical `Expression` tree.
Traversal is iterative; shared children and arbitrary compact literal/label
payloads retain their identity. Deprecated 2024 wrappers are not recognized,
including inside a candidate fusion. Fused decoding visits only retained children, not
the discarded builtin/list/count wrappers. Scratch traversal and identity-map
storage still allocate and are measured separately.

Tests compare direct value encoding against canonical expansion followed by
the existing encoder, and direct parsing against the existing parser followed
by canonical-to-internal conversion. Re-encoding and canonical expansion
close the symmetry diagrams for every variant, both ordering flags, all
builtins and both fusion settings.

## Production pipeline and compatibility boundaries

`PineVMParseCache.ParseInternalExpression` decodes canonical Pine values directly
on misses, fusing directly without allocating discarded syntax wrappers.
Both its internal and public canonical parser accept only the 2026 format.
Literal and label payloads remain arbitrary Pine values, not nested expressions
to validate. Historical `ExpressionEncoding2024` utilities remain available
outside this production parser; there is no legacy cache or fallback.

The runtime uses `InternalExpressionCompilation` and `InternalPineIRCompiler`.
Reduction, inlining, substitution and compiler-local structural analyses
operate on fused retained syntax, without canonical roundtrips between passes.
Cached canonical conversion also fuses directly. Smart builtin construction
normalizes changed nodes, while unchanged nodes retain their identity.
There is no separate fusion pass at IR entry. An allocation-free builtin-input
view is shared by reduction and IR compilation, avoiding synthetic input lists.
Arithmetic flattening/constant collection, tag cancellation, conditional
proofs, length/sequence reductions and slice recognizers accept fused forms.
Length equality and constant head/skip lower directly, including fused local reads.
Closed fused calls fold directly rather than depending on a discarded list
node's constant reduction. Nested takes compose using the minimum count.
Cached metrics describe the **expanded canonical tree**, so established
inlining, recursion and code-size heuristics do not change because of fusion.

The public `ExpressionCompilation`, `PineIRCompiler`, `ReducePineExpression`
and `ExpressionSubstituteEnvironment` methods are adapters to these native
implementations, not separately maintained copies of their algorithms.
Compilation, encoding and standard shared reduction caches have retained keys.
Immutable/frozen policy dictionaries with the default structural comparer
are indexed once by native keys. Mutable dictionaries and custom comparers
retain live canonical lookup semantics, including replacement and removal.
Custom compilation/policy delegates still work through adapters. With no
configured policies the compiler uses a static no-op check, and the default
Elm test host no longer supplies a canonical no-op predicate.
Compatible compilation configuration is still
required when sharing caches.

Public canonical roots cross a weak, identity-memoized boundary. Child mappings
preserve unchanged canonical siblings when a transformed ancestor is returned.
Frames, direct calls and template plans retain internal roots and expose
canonical views only when requested. Original encoded callee values remain
the identity for invocation caches, native leaves, template proofs and tail guards.
Canonical metadata hashes and persisted `pine-ir 1` references are not renamed.
Canonical flat hashing streams the existing deterministic bytes into buffered
SHA-256 updates, without retaining or copying the complete binary encoding.
CFG successors use a non-boxing struct enumerator. Operation rewrites reuse
unchanged blocks and arrays, allocating builders only after an actual edit;
redundant-local-write analysis uses entry initialization and first-write
positions instead of per-operation initialization-set snapshots.
Local initialization and liveness use inline/bounded bitsets with a sparse
fallback for large slot IDs. Value-flow facts retain exact values, list length,
and sorted sparse element proofs directly. The production CFG pass chain uses
a compilation-local context to validate each changed snapshot, reuse analyses
only when their dependencies are unchanged, and skip ineligible or unchanged
passes without changing their established order. Public standalone validation
still performs a full check; no mutable analysis state participates in graph
record equality or survives the compilation.
Stateless return and loop-transfer nodes are shared. Structured operation nodes
reuse wrappers for the existing fixed instruction instances by reference
identity, without conflating customized instructions of the same kind.
Appending an empty fragment or replacing an operation with the same instruction
returns the original fragment.

Remaining conversion costs are intentional:

- Elm compilation, static-program/code-generation backends and public APIs
  still produce or consume canonical syntax.
- Enabled canonical observers, arbitrary cache delegates, skip-inlining
  predicates and precompiled selectors request canonical views. Use a null
  skip-inlining predicate when no policy is needed, rather than a no-op delegate.
- Instruction serialization and diagnostic rendering materialize canonical
  references. Canonical config/metadata hashing still occurs at distinct
  compilation entries; it has not been replaced by the internal binary hash.
- Instrumented direct evaluation uses the canonical reference evaluator when
  `PineVMWorkProfile` is enabled. Normal compiler-time/direct evaluation uses
  retained variants and enum dispatch. EventPipe profiling does not enable
  that canonical observer path.
- Reverse views are weakly cached, but are not indiscriminately registered as
  forward conversions. Canonical builtin helper APIs dispatch through the
  general native entry points instead of casting a fused root to `Builtin`.

There is no new persisted compilation-result cache or eager conversion of the
reused-value bundle. The binary codec below remains a separate artifact facility.
See the [production implementation log](../explore/2026-11-15-internal-expression/production-implementation-log.md)
for the complete-workload comparison and allocation/CPU attribution.
The [binary-builtin follow-up](../explore/2026-11-15-internal-expression/builtin-binary-implementation-log.md)
explains why late fusion reduces node counts without automatically improving
the earlier reduction/inlining passes in that historical revision.
The [early-fusion follow-up](../explore/2026-11-15-internal-expression/early-fusion-implementation-log.md)
records the recognizer port and its matched workload measurements.

## Binary constructor contract

`ExpressionBinary` uses the existing
`PineTreeBinaryEncoding.TreeEncoder<object>` /
`TreeDecoder<object>` facility. Each expression kind has a real, canonical
Pine constructor program `C`:

```text
Eval(External256(treeHash(canonicalExpressionEncoding(C))),
     List(retainedField0, retainedField1, ...))
```

Within one scope, only the first occurrence of a constructor hash is external.
Later occurrences use an eight-byte backward reference to that hash entry,
saving 28 bytes each. The shared decoder resolves either form directly to the
same specialized constructor dispatch, without canonical expression objects.

The program produces the **canonical expression-as-Pine-value** represented
by those fields. Examples: the list constructor prepends `"List"` to the
environment; binary builtins construct the canonical builtin/input-list
encoding; head/skip constructs the literal count and general builtin layers.
`Length_Equal_Binary` writes sequence, other operand and an inline integer ordering
flag (0 or 1); its constructor chooses the matching operand order.
`Builtin` and `BuiltinBinary` constructor programs map their numeric enum to
the canonical function name. Their hashes consequently capture that mapping.
`BuiltinBinary` writes the enum, left operand and right operand in that order;
its constructor hash is
`68554a13960f8add4c7761d7a78f720b44d563142b120da0d829693c137490d1`.
It replaces the former equality-only constructor, so its internal bytes/hashes
change. Previous equality-only artifacts require their previous constructor
profile; no version header or canonical-language change is introduced.

There is **no version header or version field**. The constructor expression
hashes specify which programs must be available to decode the payload.
Change a constructor contract by changing its Pine program/hash. Precomputed
hash bytes avoid creating canonical constructor programs on the hot path;
`GetConstructorExpressions` and `ResolveConstructorExpression` initialize
those programs only for regeneration or general-Pine decoding.

Lists flatten into the environment; expression input-list wrappers omitted
by fusion are not encoded/decoded as retained objects. Int32 enum/count
fields and the ordering flag are written as inline Pine signed-integer blobs and decoded from spans
without allocating `PineValue` scalar wrappers. Literal/label payloads still
retain their ordinary Pine blob/list objects. Shared completed expressions
and payload nodes can use relative references.

The specialized decoder consumes constructor fields directly, allocates
only retained nodes and their owned storage, and never constructs canonical
`Expression` objects or canonical expression value trees. The reference index
scan/pool can still allocate scratch backing arrays. Constructor hash entries
are referenceable only in constructor positions; inline environments and raw
scalar fields are **not** referenceable. Unknown hashes, invalid enums/counts, wrong types/arities,
truncations and invalid references are rejected. Sessions share references
across roots; standalone `Encode`/`Decode` establish independent scopes and
report consumed extent for embedding.
Unshared-hash bytes using the current constructor profile remain readable,
but normalized re-encoding and internal binary hashes change. Sharing
references alone does not change constructor hashes. Replacing a constructor
contract, as with `BuiltinBinary`, does; canonical expression values stay unchanged.

`ComputeHashUncached` streams the exact standalone encoded bytes into buffered
SHA-256. `ComputeHash` adds weak identity memoization. Buffering avoids a
cryptographic update for every tiny header; bounded encoder-reference
dictionaries and rented hash buffers still have first-use/concurrent growth
costs. SHA-256 results identify this **internal binary representation**, not
canonical Pine content hashes. Different general/fused representations of
the same canonical expression can have different hashes. Normalize before
using one identity domain for persisted compiler lookup; cache settings and
delegate policy identity remain separate requirements.

`InternalExpressionTests` snapshot the retained forms and assert native
roundtrip/re-encoding, canonical structural roundtrip, deterministic hash,
payload/reference sharing and an independent ordinary-Pine oracle. The
oracle decodes the same bytes through the general binary decoder, resolves
constructor hashes, executes them with `DirectInterpreter`, and compares the
result with canonical encoding of the reconstructed expression.

Regenerate/check constructor hashes:

```sh
dotnet run --project explore/2026-11-15-internal-expression/measure.csproj \
  -c Release -- constructors
```

Measure conversion, decoding, hashing and pure traversal for a corpus cycling
through all builtins with statically two-item inputs:

```sh
dotnet run --project explore/2026-11-15-internal-expression/measure.csproj \
  -c Release -- --builtins
```

The node-occurrence and traversal figures are synthetic retained-tree
measurements, not substitutes for the full Elm-test execution comparison.

The programs follow the retained-tree codec design from
`231ef524ea68a5cd90b39dc24f2844f2122fbb85` and its ASCII refinement
`07079ea1619bf877987718d82c01e9a7846d1d3f`.
No new string codec is necessary: builtin names are static constructor data,
and arbitrary labels are already retained Pine values.

## Pre-migration subsystem inventory (historical)

The following groups record the initial migration plan, its producers,
consumers and external boundaries. A census of `PineLanguage` imports also
finds many **value-only** users; they need no expression conversion. Do not
mechanically replace every import. This table records the required adaptation
or explicit decision to leave a subsystem canonical. Pending-action wording
in this historical table is superseded by the production choices above.

| Area / current code | Required adaptation or boundary decision | Additional conversion exposure |
| --- | --- | --- |
| Public evaluation interfaces: `Runtime/IPineVM.cs`, `EvalExprDelegate.cs`, `LockingPineVM.cs`, `IntermediateVM/PineVM.cs` constructors/evaluation overloads | Preserve canonical API; add an internal execution path and a boundary adapter | Convert canonical roots once per identity/session, not every evaluation; retain/weakly cache mappings with a defined lifetime |
| Dynamic Eval parsing: `CodeAnalysis/PineVMParseCache.cs`, `Encodings/ExpressionEncoding{,2024,2026}.cs` | Integrate `ExpressionValueEncoding`'s direct **canonical-PineValue -> internal** parser with session caches and public parse-error contracts | Canonical value parsing is now implemented separately from the binary artifact decoder. Calling the old parser then converting retains extra trees; use the direct path on misses |
| Expression entry and caches: `IntermediateVM/PineVM.cs:479-588`, `ExpressionCompilationCache.cs`, `PineVMSharedCaches.cs`, `PineVMExpressionEncodingCache.cs` | Key compiler/reduction/encoding caches by normalized internal syntax under compatible configuration; retain external identity separately | Existing canonical override/specialization dictionaries and external cache callbacks require adapters or once-per-session catalog conversion |
| Compiler entry/inlining/reduction: `ExpressionCompilation.cs`, `CodeAnalysis/ReducePineExpression.cs` | Implement every general/fused variant directly; port rewrite caches and expansion/recursion guards | Converting back before each pass defeats this change. Existing depth, ancestor/root forms and environment constraints remain part of reuse decisions |
| Structural analysis: `CodeAnalysis.cs`, `StaticFunctionInterface.cs`, `ExprMappedToParentEnv.cs`, `ExpressionSubstituteEnvironment.cs`, `ExpressionEnvClass.cs`, `StaticExpression*.cs`, `CalleeRecognition.cs` | Teach paths, substitution, free-environment recognition and metrics about fused nodes | The new model has no cached canonical `EvalCount`, `BuiltinCount`, `ReferencesEnvironment`, `SubexpressionCount` or `MaxDepth` yet; define canonical-expanded vs retained counts before porting heuristics |
| IR and direct calls: `PineIRCompiler.cs`, `StackInstruction.cs` (`DirectInvocation`), `StackFrame.cs` | Compile enum/specialized forms directly; carry internal roots through frames/linked calls | Tail-loop guard generation currently canonically encodes root forms; preserve original encoded callee values when available rather than rebuilding them |
| CFG operations: `PineControlFlowFragment.cs`, `PineControlFlowGraph*.cs`, `StackFrameInstructions.cs` | Mostly instruction/value based, not expression based; adapt expression-bearing invocation/template metadata | A model change does not itself eliminate CFG dataflow, validation or liveness allocation. Do not introduce conversions in otherwise expression-free passes |
| Template plans/proofs: `ExpressionTemplatePlan.cs`, `ExpressionTemplateProofCache.cs`, `PineVMExpressionTemplatePlanCache.cs`, `PreparedTemplateInput.cs`, `TemplateValueRecipe.cs` | Retain internal terminal expressions; preserve exact encoded terminal values and constructor recognition | Bounded recognizers currently parse canonical values. Convert/cache once, or add a direct decoder; recipes and `PineValueInProcess` still represent stable Pine values |
| Direct interpreter and compiler-time evaluation: `Runtime/DirectInterpreter.cs`, `CodeAnalysis/StaticExpressionInterpreter.cs`, `Internal/BuiltinFunction{Specialized,Fused}.cs` | Either implement an internal interpreter/enum dispatch or deliberately retain a canonical reference interpreter | Repeated canonical expansion for constant folding is a hot-path penalty. Keeping a canonical interpreter only for tests/oracles adds no production conversion |
| Builtin/precompiled catalogs: `IntermediateVM/SetupVM.cs`, `ElmCompilerInDotnet/PrecompiledLeaves/*`, `PineVM.cs` selection/skip callbacks | Separate native leaf identity from internal syntax; preserve canonical encoded-expression value lookup and policy semantics | Converting each catalog entry lazily may add cold misses; batch/session conversion can amortize. Never replace native leaf content hashes with internal representation hashes |
| Invocation/result caching and learning: `InvocationCache*.cs`, `GenerationalInvocationCache.cs`, `ContinuousInvocationCacheLearning.cs`, `InvocationCachePolicy*.cs`, `StackFrameInputHash.cs` | Leave canonical expression-value/environment result keys stable, or explicitly introduce a separate migration | Internal compilation hashes cannot silently rename existing result-cache files. Use already supplied canonical `ExprValue`; synthesize it only if an internal-only caller reaches a value-keyed boundary |
| Optimization config: `PineVM/OptimizationParametersSerial.cs`, `PopularExpression.cs` | Keep canonical hash lookup/catalog contracts or design a versioned translation layer | Entry currently eagerly computes a canonical hash for config/metadata. Make that work conditional/lazy, but preserve the external hash contract |
| Diagnostics/reporting: `EvaluationError.cs`, `ExecutionErrorReport.cs`, `ExpressionCompiled.cs`, `EvaluationEvent.cs`, `ExecutedStackInstruction.cs`, `InvocationCountReport.cs`, `PineVMWorkProfile.cs` | Retain cheap internal frame snapshots, and keep public canonical reports compatible through lazy views | Convert/hash only when a report is rendered or a canonical callback actually runs. Histograms can aggregate internal identities and convert once per distinct key, not once per invocation |
| Instruction serialization/debug views: `StackInstructionTextFormat.cs`, `StackInstructionTraceRenderer.cs`, `StackInstructionMetadataEncoding.cs`, `Json/EncodePineExpressionAsJson.cs` | Keep canonical external JSON/text contracts; adapt direct-invocation expression and template metadata | Existing `pine-ir 1` persists canonical expression references/linked frames. Convert at serialization or evolve an explicit artifact schema; value tree hashes and internal binary hashes are different domains |
| Elm producers: `ElmCompilerInDotnet/{ExpressionCompiler*,PatternCompiler,OperatorCompiler,CompilerModel/*,CoreLibraryModule/*}`, `Elm/Syntax/Interpreter/ElmSyntaxInterpreter.PineCompilation.cs`, `ElmTestDiscovery.cs`, `ElmTestRunner.cs` | Continue producing canonical expressions for compatibility, with conversion after compilation, or later add an internal backend | Converting only final roots amortizes compiler output, but internally retained subgraphs may coexist with canonical compiler caches; count both lifetimes |
| Language-service/interactive/platform hosts: `Elm/LanguageServer/*`, `implement/pine/ElmInteractive/*`, `Elm/Platform/*`, `CLI/{Run*,Make*,CompileInteractiveEnv*}`, `IntermediateVM/SetupVM.cs` | Session-owned adapters/cache composition should preserve public APIs and compilation compatibility | Short-lived VMs must share compatible internal conversions; do not rebuild a whole environment for each request/test |
| Alternate .NET/codegen paths: `CodeAnalysis/StaticProgram*.cs`, `FunctionRecord.cs`, `CodeGen/Pine/*`, `DotNet/*`, `implement/pine/Pine/CompilePineToDotNet/*` | Can remain canonical as alternate backends; port only if sharing internal analyses intentionally | Crossing from internal VM output to canonical static-program/C# APIs costs expansion. Avoid roundtrips if both paths stay independent |
| Reuse/bootstrap/bundles: `Optimization/ReusedInstances.cs`, `PineLanguage/Expression.cs` factories, `Bundle/*`, `Elm/BundledElmEnvironments.cs`, prebuild artifacts | Decide which representation is interned/embedded; preserve canonical bootstrap and reference identity | Converting the entire reused bundle eagerly risks cold-start allocation and duplicate trees. Lazy bounded conversion of actually used roots is the first comparison |
| Tests/profiling/explore programs | Maintain canonical oracle/API tests; add internal reduction/IR equivalence, artifact compatibility and workload measurements before switching | Diagnostic instrumentation must distinguish decoder/conversion/compile/encoding/report phases and avoid making observation eager |

Concrete inspection anchors include `IPineVM.cs:12-38`,
`PineVM.cs:479-588`, `PineVMParseCache.cs:23-102`,
`StaticFunctionInterface.cs:70-88,126-193`,
`PineIRCompiler.cs:145-185`, `StackFrame.cs:13-22`,
`StackInstruction.cs:520-540`, `ExpressionTemplatePlan.cs:14-18,137-145`,
`InvocationCache.cs:346-382`, `EvaluationError.cs:63-84,142-149`,
`ExecutionErrorReport.cs:12-16,31-39`, and
`ReusedInstances.cs:95-107`.

## Conversion amortization and measurements

1. Keep canonical external API roots; convert at the VM/session entry with
   bounded or weak identity memoization. Do not assume canonical/internal
   equality is the same identity domain.
2. For dynamic Eval, retain the original expression `PineValue` and directly
   decode to the internal model using `ExpressionValueEncoding`; session
   caching and production parse-error compatibility are implemented.
3. Reduction, compiler-local analysis and IR construction are ported together
   to keep the hot pipeline internal. A canonical fallback per pass would
   erase the expected savings.
4. Keep canonical hashes/encodings lazy and cached by distinct root when
   required by leaf lookup, external config, public reports or result caches.
   Internal compilation-artifact hashing can remain separate.
5. Measure conversion cache misses/hits, canonical mapping calls, duplicate
   live representations, reference-dictionary growth, byte buffers and
   retained node counts. Include first-use and concurrent capacity growth.

The reusable microbenchmark in
`explore/2026-11-15-internal-expression/` separately measures conversion,
direct retained decoding, canonical value-plus-expression decoding, encoding
and hashing of a deliberately fusion-heavy 256-comparison corpus. `--length`
selects length equalities with alternating operand order; added operations
compare direct canonical-value encoding/parsing with canonical-model adapters.
It is
**not** the full Elm test scenario and cannot establish an execution speedup.
`--single` performs one invocation after input preparation; it is **not** a
true cold-start measurement because preparation already uses factories and
the codec. Normal runs warm each operation five times and measure 100 calls.
The program reports per-operation allocations, process CPU and GC counts.
See its [implementation log](../explore/2026-11-15-internal-expression/implementation-log.md)
for exact revision, results and tradeoffs.
