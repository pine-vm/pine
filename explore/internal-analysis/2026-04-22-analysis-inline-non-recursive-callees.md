# Analysis: Storing Compiled Functions and Emitting Two Call-Site Forms (Form A and Form B)

This document describes the design adopted by `ElmCompiler.cs` (in
`implement/Pine.Core/Elm/ElmCompilerInDotnet/`) for emitting and
consuming compiled top-level Elm functions. It supersedes earlier
drafts that described inlining non-recursive callees as bare literal
encoded values without a stored wrapper.

The core design decisions are:

- For every emitted function we store **four properties** (see §2.1):
  the generic-application wrapper value, the parameter count, the
  env-functions list (empty for non-recursive functions), and the
  encoded body in the env-functions-at-index-0 layout.
- At every call site we emit one of **two functionally equivalent
  forms** (see §2.2): a compact single-`ParseAndEval` form (Form A)
  when arity matches, or a generic per-argument `ParseAndEval` chain
  (Form B) otherwise.
- All consumers of compiled Pine — most importantly the static-program
  parser used by snapshot tests — must recognize both forms and
  **canonicalize them to the same name** for any saturated
  application of the same callee (§2.4).

The document covers:

1. Background on the prior layout and the relevant code paths.
2. The new design: what is stored per function and the two call-site
   forms.
3. All downstream sites that must be adapted.
4. Expected challenges during implementation and migration.
5. Downstream changes that can be landed *before* the emitter
   switches output shape, to isolate risk — parser-first.
6. Open design questions to clarify before starting.
7. A detailed, step-by-step implementation plan, including the
   changes to `elm-compiler-implementation-guide.md`.

The plan is intentionally written so that two different implementers
should arrive at substantially the same code structure, sequence of
commits, and tests.

---

## 0. Implementation Progress and Findings

This section is updated as the implementation lands. It complements
the per-step status notes at the head of each subsection in §7.

### Status snapshot (2026-04-22)

- [x] **7.1 Action 1** — Round-trip parser tests for both wrapper
  shapes via `FunctionRecord.ParseFunctionValue`. Already covered by
  the existing `ParseFunctionValue_With{,out}EnvFunctions_*_SymmetryTest`
  tests in `implement/Pine.Core.Tests/CodeAnalysis/FunctionValueBuilderTests.cs`
  for parameter counts 0–3.
- [x] **7.1 Action 2** — `StaticProgramParser` tests mixing both
  wrapper shapes. The `WithoutEnvFunctions`-root tests
  (`Function_int_add_71_WithoutEnvFunctions`,
  `Mixed_environment_with_both_wrapper_shapes`) are now active and
  passing as of the F-1 fix in this session (see below).
- [x] **7.2 — Form A recognition + canonicalization** *(call-site
  recognition complete)*. Implemented in the prior session:
  `StaticProgramParser.ParseCurriedFunctionApplication` detects
  Form A and canonicalizes it to the same
  `StaticExpression.FunctionApplication` that the equivalent Form B
  chain produces. The discrimination between "wrapper value" and
  "encoded body" lookups is done via a new optional
  `IdentifyEncodedBodyOptional` callback on
  `StaticProgramParserConfig<T>` (defaulting to "always returns
  null", so existing callers are unaffected).
  - 3 passing tests in `StaticProgramParserTests.cs`
    (`Minimal_invocation_one_parameter_FormA`,
    `Minimal_invocation_two_parameter_FormA`,
    `FormA_and_FormB_canonicalize_to_same_FunctionApplication`).
- [x] **F-1 fix landed (this session).** Implemented option (a) per
  the recommendation in §0:
  - Added `bool UsesEnvFunctionsLayout = true` (default) to
    `FunctionRecord` (positional record parameter) so the historical
    behaviour is preserved at every existing construction site.
  - `ParseFunctionRecordTagged` now falls back to
    `ParseFunctionValue` when the existing `ParseNestedWrapperForm` /
    `ParseMultiParamNestedWrapperForm` paths reject a wrapper, and
    only emits a `FunctionRecord` from the fallback when the result
    is `ParsedFunctionValue.WithoutEnvFunctions` (limiting the
    fallback's blast radius — see Finding F-3 below).
  - `NamesFromCompiledEnv.BuildApplicationFromFunctionRecord`
    branches on `UsesEnvFunctionsLayout`: when false it builds the
    env-value class with arguments at `[k]` and no env-functions
    entries at `[0,k]`.
  - `StaticProgramParser.ParseValueAsFunction` derives
    `envParametersOffset` (1 with-env-funcs / 0 without) from the
    flag and threads it through the `ParseExpression`-family
    methods; the path-in-env recognition guards become
    `>= envParametersOffset` and the parameter-index calculation
    becomes `pathInEnv[0] - envParametersOffset`.
    `parametersExprs` for the function header use
    `[envParametersOffset + paramIndex]`.
  - `ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr`
    *also* branches on the flag — see Finding F-3 below.
- [x] **7.2 cleanup (this session).** The two stale "Environment
  structure" comments at the resolve-callee path (former line 711)
  and the `ParseFullApplicationArguments` doc-string (former line
  785) have been refreshed to reference the WithEnvFunctions layout
  per §2.1 and to point readers at this analysis document.
- [x] **7.3 audit (this session).** Verified that
  `NamesFromCompiledEnv.cs`, `CodeAnalysis.cs`, and
  `ElmInteractiveEnvironment.cs` either route through the now
  layout-aware `ParseFunctionRecordTagged` /
  `BuildApplicationFromFunctionRecord` /
  `ApplyFunctionArgumentsForEvalExpr` helpers, or are decoupled
  from the wrapper-shape question. The only direct
  `EnvFunctions.Length` inspection outside those helpers is
  `CodeAnalysis.FilterClassRemovingAllNonExpressions.KeepEntryAsIs`,
  which only matters for the legacy `"Function"`-tagged wrapper
  path; both `WithEnvFunctions` and `WithoutEnvFunctions` wrapper
  values parse as expressions and are kept as-is via the
  `parseCache.ParseExpression` check that runs first. No fix
  required.
- [x] **7.5 implementation-guide refresh (this session).** Added a
  new subsection *"Storing Compiled Functions and Two Call-Site
  Forms (Form A and Form B)"* to
  `implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md`
  describing the four properties, both call-site forms, the
  rationale, the canonicalization requirement, and the
  `UsesEnvFunctionsLayout` discriminant — and linking back to this
  analysis document.
- [x] **7.6a (2026-04-23) — additive subset of §7.6.**
  Extended `CompiledFunctionInfo` (and the `WithCompiledFunction`
  helper) with two new properties — `int ParameterCount` and
  `IReadOnlyList<PineValue> EnvFunctions` — populated at the single
  construction site in `ElmCompiler.CompileSCC`. While §7.6a is in
  effect, `EnvFunctions` mirrors the per-member `envFunctionsList`
  that is already baked into the wrapper, so the wrapper shape and
  layout are **byte-identical to the previous build**. All 2921
  `Pine.Core.Tests` pass; 1 skipped (the existing baseline);
  `Pine.IntegrationTests` builds clean. See "Plan-change learning
  (§7.6 split)" below for why §7.6 was split.
- [x] **7.6b (2026-04-23) — runtime-layout narrowing + cross-SCC
  literal-inlining at call sites.**
  - `FunctionScc.GetLayout()` now returns the SCC members only.
    `AdditionalDependencies` is retained on the record for
    compilation-ordering bookkeeping but no longer participates in
    the runtime env-functions list. `CompileSCC.envFunctionsList`
    therefore contains only the members' encoded bodies; the
    previous "fall back to cache for missing entries" branch is now
    a defensive `InvalidOperationException`.
  - `ExpressionCompiler.cs` full-application path branches on
    "callee in caller's (members-only) layout" vs "callee only in
    the compiled-functions cache". Same-SCC callees keep the
    existing `[0,k]` env-relative reads (their layout is now
    identical to the caller's by construction). Cross-SCC callees
    emit `Literal(callee.EncodedBody)` for `functionRef` and
    `Literal(List(callee.EnvFunctions))` for the env-functions slot
    of the call environment, removing the `current_env[0][k]` reads
    that the old caller-layout-extension relied on.
  - `ExpressionCompiler.CompileExpressionRef` (function-as-value at
    a qualified name) likewise branches: cross-SCC callee with
    `paramCount > 0` → `Literal(callee.WrapperValue)`; cross-SCC
    callee with `paramCount <= 0` → `ParseAndEval(Literal(callee.EncodedBody),
    [Literal(List(callee.EnvFunctions))])` so the *value* is
    produced rather than the wrapper. (Without this branch,
    references like `Set.empty`/`Dict.empty` infinitely re-parsed
    their own wrapper expressions and the test suite hit the
    interpreter's stack-depth limit. Caught and fixed in the same
    session.)
  - **Note for §7.7:** §7.6b *already* accomplishes the cross-SCC
    "inline `Literal(WrapperValue)` at call sites" change that the
    original §7.7 plan listed. The remaining §7.7 work is therefore
    narrower than the original write-up suggests — see the §7.7
    banner.
  - Re-baselined: 4 `DependencyLayoutTests` (members-only layouts);
    ~22 `PerformanceCountersFormatting`-driven instruction-count
    baselines in `ElmParserExpressionTests`, `ElmParserFileTests`,
    `FunctionApplicationOverheadTests`, `KernelJsonFunctionTests`;
    3 IL-frame snapshots in `List_map_triple_with_function_parameter`
    (literal-hash drift; one frame's instruction sequence
    shortened); `Compiler_uses_cross_module_inlining_when_enabled`
    weakened from `BeLessThan` to `BeLessThanOrEqualTo` (cross-SCC
    literal-inlining now happens regardless of the inlining flag,
    weakening the original strict invariant). Final state:
    **2921/2921 `Pine.Core.Tests` pass, 1 skipped**.
- [x] **7.6b Phase 4 (2026-04-23) — analysis-doc refresh.**
  Updated the §0 status snapshot, the §7.6 banner, and the §7.7
  banner to reflect what §7.6b actually landed.
- [x] **§7.7 (2026-04-23 / 2026-04-24) — single-member non-recursive
  SCC wrapper switch + dual-layout call-site emitter.**
  - `FunctionScc` now carries `IsRecursive`, computed at SCC
    construction (multi-member ⇒ recursive; single-member ⇒
    recursive iff self-referential per `AnalyzeFunctionDependencies`).
  - `ExpressionCompilationContext` gains `UsesEnvFunctionsLayout`
    (and `EnvParametersOffset`); `BuiltinHelpers.BuildPathToParameter`
    threads the offset through every call site in `ExpressionCompiler`
    and `ElmCompiler`.
  - `ElmCompiler.CompileSCC` branches on `IsRecursive`: recursive
    SCCs keep `EmitFunctionValueWithEnvFunctions`; non-recursive
    single-member SCCs use `EmitFunctionValueWithoutEnvFunctions`
    with empty `EnvFunctions`.
  - `ExpressionCompiler.cs` cross-SCC call sites (full-application
    path and the zero-param `CompileExpressionRef` value-reference
    path) emit a flat `[arg0, ..., argN-1]` env (no env-functions
    slot) when the callee's `EnvFunctions.Count == 0`.
  - `FunctionRecord.ParseFunctionRecordTagged`: zero-param
    `WithoutEnvFunctions` wrappers (whose value is just
    `EncodeExpressionAsValue(body)`) were falling through to the
    bare-literal fallback and getting double-encoded as
    `Literal(<encoded body>)`. Now tries
    `TryFunctionRecordFromParsedFunctionValue` before that fallback.
  - `StaticProgramParser.IdentifyResponse` gains
    `CalleeUsesEnvFunctionsLayout`. Form A call-site detection uses
    it to choose between `[envFuncs, arg0, ...]` and `[arg0, ...]`
    env layouts. `BuildStaticProgramParserConfig` populates the new
    flag from `FunctionRecord.UsesEnvFunctionsLayout`.
  - `ElmCompilerTestHelper.RenderStaticFunction`: path-recognizer
    extended to accept both `pathInEnv[0] is 1` (legacy
    `WithEnvFunctions`) and `pathInEnv[0] is 0` (§7.7
    `WithoutEnvFunctions`). Both render as
    `param_1_<pathInEnv[1]>[suffix]`, preserving existing
    tuple-destructure / choice-type snapshots without per-test
    snapshot churn.
  - **Form A vs Form B selection.** §7.7 item 2 of the original
    plan ("explicit Form A vs Form B selection") is satisfied
    behaviourally by the existing structure: the saturated
    full-application branch in `ExpressionCompiler.cs` (≈ line 595)
    emits Form A; the partial-application fall-through at ≈ line 737
    routes through `CompileGenericFunctionApplication`, which is
    Form B with `Literal(callee.WrapperValue)` as the innermost
    `encoded` (because the function-reference compile path now
    inlines the wrapper as a literal — done by §7.6b). No further
    refactor was needed.
  - Snapshot rebaselines: 8 `DependencyLayoutTests` (non-recursive
    single-member SCCs now have empty layouts);
    `SizeBasedInliningRegressionTests.ParseToFile_compiles_to_valid_function_with_inlining`
    accepts either wrapper shape; ~40 perf-counter snapshots in
    `FunctionApplicationOverheadTests`, `ElmParserExpressionTests`,
    `ElmParserFileTests`, `KernelJsonFunctionTests`; frame0 / frame1
    instruction-trace snapshots in
    `FunctionApplicationOverheadTests.List_map_triple_with_function_parameter`;
    and the renderer fix above transparently covered 8
    tuple-destructure / choice-type tests. Final state:
    **2920 succeeded, 0 failed, 2 skipped** in `Pine.Core.Tests`.
- [x] **§7.7 reverted in favour of uniform layout (2026-04-24, Approach A1).**
  After landing, the dual-shape design proved to add net complexity:
  every consumer (`FunctionRecord`, `StaticProgramParser`,
  `NamesFromCompiledEnv`, `ElmInteractiveEnvironment`,
  `ExpressionCompilationContext`, `BuiltinHelpers`,
  `BuildStaticProgramParserConfig`) had to branch on the
  `UsesEnvFunctionsLayout` discriminant. A follow-up analysis
  ([`2026-04-24-analysis-simplify-function-record-layout-flags.md`](2026-04-24-analysis-simplify-function-record-layout-flags.md))
  enumerated three approaches and recommended **Approach A1: always
  emit `WithEnvFunctions`**. Implemented in PR
  `copilot/simplify-implementation` (commits Phase 1 / 2a / 2b / 2c):
  the `WithoutEnvFunctions` emitter and `ParsedFunctionValue`
  variant are removed, the `UsesEnvFunctionsLayout` discriminant is
  removed from `FunctionRecord` / `ExpressionCompilationContext` /
  `IdentifyResponse`, and `BuildPathToParameter` collapses to a
  single-arg form. Non-recursive single-member SCCs still get an
  empty env-functions list (§7.6b's contribution stands), but the
  wrapper shape itself is now uniform — `env[0]` always holds the
  env-functions list, even when empty, and parameters always live at
  `env[1+i]`.
- [ ] 7.4 (baseline metrics) — not started; low priority.
- [ ] **7.8 — Zero-Parameter Eager-Evaluation Heuristic. Investigation
  done (2026-04-24); implementation deferred.** Findings from this
  session's investigation:
  - The **trivially-constant subset** of §7.8 is already handled by
    `ReducePineExpression.SearchForExpressionReduction` /
    `TryEvaluateExpressionIndependent`. Every body compiled by
    `CompileSCC` runs through `ReduceExpressionBottomUp` (line ≈
    962 of `ElmCompiler.cs`); when the resulting expression has no
    environment references and contains no `ParseAndEval` recursion
    that needs the PineVM to unwind, the reduction substitutes
    `Literal(value)` automatically. So zero-parameter declarations
    whose body is "just constants and kernel calls" already behave
    as eager.
  - The **non-trivial subset** of §7.8 — where the body invokes
    `ParseAndEval` against another (non-recursive) callee, e.g.
    `myList = List.range 1 100 |> List.map String.fromInt` — is
    *not* covered by the existing reduction, because
    `TryEvaluateExpressionIndependent` for `ParseAndEval`
    deliberately does NOT recurse into the PineVM (the PineVM
    fallback at lines ≈ 117–128 of `ReducePineExpression.cs` was
    disabled on 2024-10-25 due to a stack-overflow regression).
    This is the bucket §7.8 was designed for.
  - **Implementation requirements** for the non-trivial subset:
    add a budgeted `PineVM.EvaluateExpressionOnCustomStack` call
    inside `CompileSCC` Phase 3 for non-recursive single-member
    SCCs with `paramCount == 0`; add a size comparison
    (max-nesting-depth and distinct-node-count, both ≤) per §6
    question 5; wrap in `try/catch` for `ParseExpressionException`
    and other interpreter exceptions; add the three tests called
    out in §7.8 (small-evaluable / large-evaluable / non-terminating).
    Estimated diff: ~100 lines + 3 tests + likely some snapshot
    rebaselines for `DependencyLayoutTests` and perf-counter tests
    where eagerly-evaluated wrapper bytes differ.
  - **Why deferred:** The implementation is straightforward but the
    snapshot-rebaseline blast radius is hard to predict in this
    session without trial-and-error. The existing reduction already
    captures the most common case; the marginal optimization for
    the non-trivial subset is real but not blocking. A dedicated
    follow-up PR with focused before/after metrics is the cleaner
    landing path.
- [x] **7.9 (2026-04-24) — implementation-guide refresh.** Updated
  the `elm-compiler-implementation-guide.md` subsections
  *"Composition of the Environment Functions List"* and *"Function
  Values And Generic Function Application"* to reflect the §7.7
  reality: env-functions list is restricted to SCC members,
  non-recursive single-member SCCs use `WithoutEnvFunctions` with
  an empty list, and cross-SCC callees are inlined at the call site
  as `Literal(callee.EncodedExpression)` (Form A) or
  `Literal(callee.WrapperValue)` (Form B). Linked the existing
  *"Storing Compiled Functions and Two Call-Site Forms (Form A and
  Form B)"* subsection (added in the §7.5 commit) for the canonical
  description. The deeper rewrites of "Full Function Applications"
  with worked Form A / Form B examples and a new "SCCs as the
  Compilation Tree" subsection are deferred — the existing prose
  already describes the layout convention accurately, and the new
  subsection covers the four properties / dual-form selection /
  wrapper-shape discriminant.
- [x] **7.10 (2026-04-24) — `dotnet format` verification.**
  `dotnet format Pine.Core/Pine.Core.csproj --verify-no-changes`
  and the same on `Pine.Core.Tests/Pine.Core.Tests.csproj` both
  exit clean — no formatting drift after the §7.7 commits. The
  defensive branch in `ExpressionCompiler.CompileApplication`
  (lines ≈ 632–639, "should be unreachable for same-SCC callees")
  is intentionally retained as a guard against compiler bugs; its
  comment already documents that it is unreachable post-§7.6b.
  The unused `EmitPlainValueDeclaration` helper in `ElmCompiler.cs`
  is retained because §7.8 (still pending) is planned to repurpose
  it for the zero-parameter eager-evaluation heuristic.

**Plan-change learning (§7.8 deferral, 2026-04-24):** The §7.8
plan as drafted assumes that zero-parameter declarations' bodies
need a *new* eager-evaluation pass to be replaced with literals.
In practice the existing `ReducePineExpression` pass already
performs constant folding via `TryEvaluateExpressionIndependent`,
so the trivially-constant subset of §7.8 is already covered. The
non-trivial subset (bodies that need PineVM `ParseAndEval`
unwinding to evaluate) is the only remaining target — and that
subset specifically needs the budgeted PineVM call that
`TryEvaluateExpressionIndependent` deliberately omits. Future
implementers should scope §7.8 narrowly to this remaining subset
to keep the snapshot-rebaseline blast radius predictable.

**Plan-change learning (§7.6 split, 2026-04-23):** The §7.6 commit
plan as originally drafted bundles a purely additive concern (storing
the four properties) together with a runtime-shape change (dropping
`AdditionalDependencies` from the env-functions list and switching
non-recursive members to `WithoutEnvFunctions`). The doc claimed
"emission at call sites is unchanged in this commit (the existing
emission path continues to work because the same `WrapperValue` is
the artifact it already produces)". Audit of
`ExpressionCompiler.cs` (lines ≈ 538–577 for full applications and
lines ≈ 1670–1729 for `EmitFunctionExpressionFromEncodedBody` /
function-value lookups) shows this is **only true for cross-SCC
callees that are emitted by inlining `Literal(callee.WrapperValue)`,
not for the existing emission path** — which today reads cross-SCC
callees from `env[0][k]` of the **caller's** layout
(`sharedLayout = members ++ additionalDeps`). Concretely:

1. Same-SCC callees use `env[0][functionIndex]` where
   `functionIndex` indexes into the caller's full layout. If the
   recursive SCC's env-functions list shrinks to members-only this
   index domain still works (members come first in the layout).
2. Cross-SCC callees today use `env[0][functionIndex]` where
   `functionIndex` is the cross-SCC dep's slot in the caller's
   layout. If we drop `AdditionalDependencies` the slot disappears
   and the lookup goes off the end of the env-functions list.
3. The `callEnvFunctions` builder at call sites (≈ line 547) reads
   each dep of the *callee* via `env[0][depIndex]` of the *caller's*
   layout, with the same problem.

Therefore §7.6 is split into two sub-steps:

- **§7.6a (landed):** add the `ParameterCount` and `EnvFunctions`
  fields and populate them. While §7.6a is in effect, `EnvFunctions`
  mirrors the existing `envFunctionsList`, so the wrapper bytes are
  unchanged and there are no snapshot diffs. This unblocks §7.7
  cleanly because the call-site emitter can read the four properties
  directly off `CompiledFunctionInfo` without relying on
  re-derivation from the wrapper.
- **§7.6b:** the wrapper-shape change (drop `AdditionalDependencies`,
  use `WithoutEnvFunctions` for non-recursive members) is **coupled
  to the call-site emitter switch** (Form A / Form B with literal
  inlining of cross-SCC callees). It is therefore folded into §7.7
  rather than landed separately.

Implication for §7.7: the §7.7 implementer should treat the
"narrow `EnvFunctions`" change and the "switch the emitter" change
as a single atomic commit, with snapshot rebaselines in the same
commit.

### Findings (encountered during implementation)

**F-1 (2026-04-22, RESOLVED): `ParseFunctionRecordTagged` did not unwrap
`WithoutEnvFunctions` root declarations.**

> **Resolution (this session):** Implemented option (a). `FunctionRecord`
> now carries a `bool UsesEnvFunctionsLayout = true` discriminant;
> `ParseFunctionRecordTagged` falls back to `ParseFunctionValue` for
> rejected wrappers and only emits a `FunctionRecord` from the
> fallback when the result is `WithoutEnvFunctions`. Downstream
> branches in `BuildApplicationFromFunctionRecord`,
> `StaticProgramParser.ParseValueAsFunction` /
> `ParseExpression`-family methods, and
> `ApplyFunctionArgumentsForEvalExpr` honor the flag. The two
> `Skip`-annotated tests are now active and passing. The original
> finding text is preserved below for context.

`StaticProgramParser.ParseProgram` invokes
`FunctionRecord.ParseFunctionRecordTagged` to extract the inner body
of each root. When the value was emitted by
`FunctionValueBuilder.EmitFunctionValueWithoutEnvFunctions`, the
result is a tagged `"ParseAndEval"` value whose `Environment` is
`[Environment]` for one parameter (or `[concat ...]` for ≥ 2
parameters) — *not* the `[envFunctions, Environment, ...]` shape
that `ParseNestedWrapperForm` expects. As a result,
`ParseFunctionRecordTagged` returns an `Err` and `ParseProgram`
falls back (line ~168 of `StaticProgramParser.cs`) to producing
`StaticExpression.Literal(wholeWrapperValue)` — i.e., the entire
wrapper is treated opaquely.

Consequence: every consumer that walks
`StaticProgramParser.ParseProgram` output for symbol names and body
shape will misrepresent any `WithoutEnvFunctions` declaration. This
is currently unreachable from real compiler output (the existing
emitter only produces `WithEnvFunctions`), but it **must** be fixed
before step 7.7 (emitter switch) lands. The fix has been folded
into the scope of 7.2 (see updated note in §7.2).

The parallel API `FunctionRecord.ParseFunctionValue` (used by the
symmetry tests) handles both wrapper shapes correctly via separate
code paths (`ParseFunctionValueFromParseAndEval` /
`ParseFunctionValueFromListExpression`). One option for the fix in
7.2 is to route `ParseFunctionRecordTagged` through the same logic
for the un-tagged wrapper case, rather than maintaining two
implementations.

**Additional sub-finding (also discovered 2026-04-22):** a naive
"convert `ParsedFunctionValue.WithoutEnvFunctions` to `FunctionRecord`"
mapping is **not** sufficient. The downstream consumer
`NamesFromCompiledEnv.BuildApplicationFromFunctionRecord` builds an
`envValueClass` that assumes the WithEnvFunctions layout — env
functions at `[0, k]`, arguments at `[1 + k]`. A `WithoutEnvFunctions`
body, in contrast, expects arguments directly at `[k]` (no leading
env-functions slot). Therefore, fixing F-1 requires one of:

- (a) extending `FunctionRecord` (or
  `BuildApplicationFromFunctionRecord`) with an explicit shape
  discriminant (e.g., a `bool UsesEnvFunctionsLayout` field), and
  branching on it when constructing the env-value class and the
  parameter expressions;
- (b) replacing `ParseFunctionRecordTagged` calls in
  `StaticProgramParser.ParseProgram` with a direct path that
  consumes `ParsedFunctionValue` (the richer sum type) and routes
  per-shape;
- (c) at parse time, normalizing the `WithoutEnvFunctions` inner
  expression by rewriting its `[k]` parameter accesses to `[1 + k]`
  before constructing a `FunctionRecord` — likely the most
  invasive option.

Option (a) is the recommended default: smallest blast radius,
explicit at the type level, minimal cost to call sites.

**F-2 (2026-04-22): `CoreModules.AddCoreModules` silently drops
new optional fields on `StaticProgramParserConfig`.**

`AddCoreModules` constructs a *new* `StaticProgramParserConfig` from
explicit per-field arguments rather than `previousConfig with { ... }`.
When `IdentifyEncodedBodyOptional` was added in this session, the
core-modules wrapper omitted the field, so the lookup was lost on
every config built through `BuildStaticProgramParserConfig.Default`
(which always chains through `AddCoreModules`). All three new
Form A tests failed with `IdentifyInstanceRequired` errors until
`AddCoreModules` was updated to forward the field.

Lesson for future plan steps that extend `StaticProgramParserConfig`:
search for **every** `new StaticProgramParserConfig<...>(...)` call
site (currently `StaticProgramParserConfig.OptionalNullRequiredThrow`,
`BuildStaticProgramParserConfig.FromDictionaryWithOriginals`, and
`CoreModules.AddCoreModules`) and propagate the new field at each.
Better still: prefer `previousConfig with { Field = ... }` syntax
in wrapping helpers so future fields propagate by default.

**F-3 (2026-04-22): the existing Elm compiler already emits
`WithoutEnvFunctions` wrappers in production, contradicting an
earlier note in this document.**

The original §0 sub-finding asserted that "this is currently
unreachable from real compiler output (the existing emitter only
produces `WithEnvFunctions`)". That was wrong: a `grep` for
`EmitFunctionValueWithoutEnvFunctions` finds at least four call sites
in `implement/Pine.Core/Elm/ElmCompilerInDotnet/ExpressionCompiler.cs`
(around lines 1011, 1150, 1844, and 1915). These produce real
`WithoutEnvFunctions` wrappers — for example, choice-type tag
constructors used as function values (covered by the
`Partial_application_choice_type_tag_*` and
`Cross_module_choice_type_tag_*` tests in `ApplyChoiceTypeTagTests`).

This caused two cascading regressions during the F-1 fix and forced
two follow-up changes:

1. **Cascading regression in
   `ApplyFunctionArgumentsForEvalExpr`.** With F-1 in place,
   `ParseFunctionRecordTagged` started returning a `FunctionRecord`
   with `UsesEnvFunctionsLayout = false` for these previously-opaque
   tag-constructor wrappers. The function then went through
   `ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr`,
   which unconditionally built a WithEnvFunctions environment
   (`[envFunctions, args...]`). The body — expecting `[args...]`
   directly — read `arg0` from `env[0]` and got the empty
   env-functions list back, producing a result that was structurally
   close to but not equal to the expected one (off by a
   `Skip(1)`-equivalent layer). Fixed by branching on
   `UsesEnvFunctionsLayout` in `ApplyFunctionArgumentsForEvalExpr`.

2. **`TryFunctionRecordFromParsedFunctionValue` had to be narrowed.**
   The first attempt returned a `FunctionRecord` for both
   `WithEnvFunctions` and `WithoutEnvFunctions` results from the
   bridge. That broke `Partial_application_three_parameters` because
   `ParseFunctionValue` happily classifies an *intermediate
   partially-applied wrapper* (which `ParseNestedWrapperForm`
   correctly rejects with the "Inner expression appears to be
   another wrapper level" check) as a complete multi-arg function,
   and the bridge then short-circuited the existing
   partial-application loop in `CreateFunctionValueInvocationDelegate`.
   Fixed by only emitting from the bridge when the parsed value is
   `ParsedFunctionValue.WithoutEnvFunctions` — for `WithEnvFunctions`
   results, returning `null` lets the caller propagate the original
   `ParseNestedWrapperForm` error and fall through to the existing
   step-by-step `ParseAndEval` evaluation loop in the test helper.

Lessons:

- Any future change to `FunctionRecord` shape or to
  `ParseFunctionRecordTagged`'s acceptance criteria must be tested
  against the full `Pine.Core.Tests` suite, not just the
  `CodeAnalysis` subset; the choice-type-tag and partial-application
  test classes exercise paths that are easy to overlook.
- The §0 sub-finding text and §1.3 (the inline-as-bare-value
  precedent) should be updated when 7.6/7.7 land to reflect that
  `WithoutEnvFunctions` is already in production use.

**F-4 (2026-04-24): runtime-cost regression in `KernelJsonFunctionTests`
after commit `b52c8de2` (§7.6b "shrink runtime layout to SCC members;
literal-inline cross-SCC callees").**

The §7.6b commit re-baselined `KernelJsonFunctionTests` with broadly
*worse* performance counters for the JSON decode paths. The deltas
(baseline → new) — every count is for a single decode invocation:

| Test                              | InstructionCount | InvocationCount | BuildListCount |
| --------------------------------- | ---------------- | --------------- | -------------- |
| `Decode_int`                      |  8 217 → 10 695 (**+30 %**) |  376 →   368 (-2 %)  |    915 →   953 (+4 %)  |
| `Decode_string`                   |  5 510 →  7 418 (**+35 %**) |  235 →   248 (+6 %)  |    593 →   656 (+11 %) |
| `Decode_field_name`               | 11 841 → 15 011 (**+27 %**) |  517 →   567 (+10 %) |  1 245 → 1 425 (+14 %) |
| `Decode_lazy_nested`              | 22 978 → 44 010 (**+92 %**) |  974 → 1 542 (+58 %) |  2 364 → 4 004 (+69 %) |
| `Decode_at_string`                |  6 924 → 15 270 (**+121 %**)|  276 →   454 (+64 %) |    699 → 1 235 (+77 %) |
| `Decode_at_int`                   |  6 924 → 15 270 (**+121 %**)|  276 →   454        |    699 → 1 235        |
| `Decode_oneOf`                    |  8 669 → 17 519 (**+102 %**)|  336 →   460        |    807 → 1 154        |
| `Decode_map`                      | 10 294 → 13 700 (+33 %)     |  400 →   358        |    965 →   906        |
| (simple decoder, line 1434)       |  6 330 →  4 835 (**-24 %**) |  213 →    74        |    556 →   262        |
| (simple decoder, line 1479)       | 17 907 → 16 726 (-7 %)      |  732 →   528        |  1 657 → 1 290        |

The two improvements are isolated to decoders whose body collapses
under reduction once the cross-SCC callee is a literal. Every other
test got significantly slower — `Decode_at_*` and `Decode_oneOf`
more than doubled.

**Causal chain.**

1. **Compiler change.** Commits `89107f0e` + `b08a9d0b` (rolled into
   `b52c8de2`):
   - `FunctionScc.GetLayout()` was changed from
     `Members ++ AdditionalDependencies` to `Members` only. The
     per-SCC `envFunctionsList` therefore contains only the SCC
     members' encoded bodies; cross-SCC dependencies are no longer
     reachable via `current_env[0][k]`.
   - `ExpressionCompiler.CompileApplication` (full-application path)
     and `CompileExpressionRef` (function-as-value path) were
     branched: same-SCC callees keep the env-relative reads, while
     **cross-SCC callees emit `Literal(callee.EncodedBody)` and
     `Literal(List(callee.EnvFunctions))`** as the call's `encoded`
     and the env-functions slot of the call environment.
2. **What the new emission looks like at a Json-decoder call site.**
   Decoder combinators like `Json.Decode.field`, `Json.Decode.at`,
   `Json.Decode.oneOf`, `Json.Decode.lazy`, `Json.Decode.map`,
   `Json.Decode.andThen` are each their *own* SCC (single-member,
   non-recursive). Every internal call between two of them — and
   there are many, e.g. `at` is implemented in terms of `field`,
   `field` in terms of `decodeValue`, `oneOf` in terms of `andThen`
   — becomes a *cross-SCC* call after §7.6b. Each such call site
   now embeds:
   - a `Literal(<encoded body of callee>)` operand for `encoded`;
     this value is a complete `PineValue.ListValue` representing the
     callee's compiled body — typically tens to hundreds of nodes —
     and
   - a `Literal(List(callee.EnvFunctions))` operand for the
     env-functions slot. `EnvFunctions` is the callee's *own*
     SCC-members layout (what was baked into the wrapper). For a
     non-recursive single-member SCC this is a one-element list
     containing the callee's own encoded body — i.e. **the same
     value as `EncodedBody` again** — duplicated on every call site.
3. **Pre-§7.6b shape (faster).** Before the change, the same call
   sites read both the `encoded` operand and the env-functions slot
   from `current_env[0][k]` of the *caller's* environment.
   `sharedLayout = members ++ additionalDeps` carried every
   transitively used cross-SCC dependency exactly once per
   compilation unit, and the call-site machinery was a small
   `[0, k]` path expression. The interpreter handled this with
   bounded `Build_List` and `ParseAndEval` invocations.
4. **Why the new shape costs more.** Three independent effects
   compound:

   a. **Wrapper re-evaluation per call.** The new `encoded` operand
      is the literal *encoded body* (i.e. the inner expression that
      sits **inside** the callee's wrapper, expecting an
      `[envFuncs, arg0, ..., argN-1]` env). Because the value is now
      reached via `Literal(callee.EncodedBody)` — and not via
      reading `env[0][k]` of an already-built layout — the
      interpreter must `ParseAndEval` the literal each call. There
      is no shared sub-expression any more for the parse cache to
      latch onto on subsequent decoder calls within the same outer
      decode, so the parse cache hit rate drops. This shows up as
      extra `InstructionCount` and `BuildListCount` for the env-list
      that wraps the literal env-functions on every call.

   b. **Doubled storage of small bodies.** For the simple decoders
      `Json.Decode.string`, `Json.Decode.int`, `Json.Decode.field`,
      `Json.Decode.lazy`, etc., the callee's `EnvFunctions` is a
      one-element list whose only element is the callee's own
      `EncodedBody`. The call site therefore embeds **the same
      encoded body twice as separate literals** —
      `Literal(EncodedBody)` for the `encoded` operand, and
      `Literal(List([EncodedBody]))` for the env-functions slot.
      Pre-§7.6b, both came from a single env read, sharing the
      underlying `PineValue` (Pine's structural sharing makes this
      effectively free). Post-§7.6b, the literals participate in
      separate `Build_List` instructions to construct the call
      environment on every invocation, even though the underlying
      `PineValue` payloads still share. This explains the
      `BuildListCount` rising even when `InvocationCount` is
      roughly flat.

   c. **Loss of recursive sharing in `Json.Decode.lazy` /
      `Json.Decode.oneOf`.** `Json.Decode.lazy` exists specifically
      to defer evaluation of a recursive sub-decoder. Pre-§7.6b
      the recursive `Json.Decode.<Comment>` decoder lived in the
      caller's env-functions list and was reached via
      `current_env[0][k]` from inside `lazy` — a single shared
      reference no matter how deeply the lazy tree unfolded.
      Post-§7.6b, every recursion through `lazy` re-emits a fresh
      `Literal(List(envFunctions))` containing the nested decoder's
      members layout, and (because the lazy and the recursed
      decoder are separate SCCs) the lookup chain that used to
      walk one `[0, k]` path now walks several
      `Literal`-then-`ParseAndEval` hops. This is why
      `Decode_lazy_nested` doubled (+92 %), `Decode_oneOf` doubled
      (+102 %) and `Decode_at_*` more than doubled (+121 %): each
      of those tests recurses through several cross-SCC
      combinators per JSON node, and the per-call cost increase
      compounds with the recursion depth.

5. **Why a couple of tests improved.** The two negative-delta rows
   are decoders whose body either:
   - has *no* cross-SCC dependency in the hot path (so the rewrite
     introduces no extra literal env construction), but happens to
     benefit from the per-SCC env-functions list shrinking from
     `members ++ additionalDeps` to just `members` — the env list
     materialised at the call site is now smaller, which cuts
     `Build_List` work; or
   - resolves entirely under `ReducePineExpression` after the
     literals are inlined (constant folding through the
     simplified call shape collapses the body to a kernel
     application).

   Both effects are real wins of §7.6b — they just don't outweigh
   the per-call literal-construction overhead on the hot paths.

**What we could change to recover the pre-§7.6b efficiency on these
tests, without giving up the §7.6b architectural benefits.**

The §7.6b design is fundamentally correct (and §7.7 depends on it).
The performance regression is in the *implementation choice of how
cross-SCC callees are referenced at call sites*, not in the
"members-only env-functions list" decision itself. Several
non-exclusive remediations:

- **R-1 (cheap, recommended): hoist cross-SCC literal env-functions
  out of the hot path.** Today
  `ExpressionCompiler.CompileApplication` emits
  `Literal(List(callee.EnvFunctions))` *inline* at every call site.
  In a function body that calls the same cross-SCC callee N times
  (typical for recursive JSON decoders), this is N identical
  `Literal` nodes that the parse cache cannot dedupe across
  `Build_List` boundaries. A bottom-up reduction pass (or a small
  CSE pass over the per-function compiled body) that collects
  identical literal env-functions and replaces them with a single
  shared sub-expression would halve the `BuildListCount` for the
  hot decoders. `ReducePineExpression` already runs at the end of
  `CompileSCC` Phase 1; extending it to recognise structurally
  identical literal lists and share them is a one-screen change.

- **R-2 (medium): re-introduce the env-functions slot as a *single*
  list shared by an entire SCC, even when the SCC is non-recursive,
  but populate it with the *cross-SCC dependencies that the SCC's
  members actually reference*.** This restores the pre-§7.6b
  property that each cross-SCC callee body is referenced by a
  single `[0, k]` path expression per call site. The trade-off is
  that we re-introduce the `members ++ additionalDeps` complexity
  in `FunctionScc.GetLayout()`, but with the §7.6b discipline that
  *additional* deps are only included when their reference count
  inside the SCC exceeds a small threshold (e.g. 3). For SCCs whose
  members each reference a cross-SCC callee at most once (the
  common case), §7.6b's literal inlining still applies.

- **R-3 (medium / risky): always emit cross-SCC callees as
  `WithoutEnvFunctions` wrappers** (this is the §7.7 line of
  thinking). When `callee.EnvFunctions.Count == 0` the emitted
  literal at the env-functions slot is `List []`, which the
  interpreter elides entirely. Combined with §7.7's "compile the
  callee body to expect parameters at `env[i]` instead of
  `env[1+i]`" the call site shrinks back to roughly its
  pre-§7.6b cost for non-recursive single-member SCCs. **This is
  what §7.7 already implements** — and it is the reason `§7.7` was
  the next item in the plan after §7.6b. Re-running
  `KernelJsonFunctionTests` after §7.7 (commit `aed9a03b` +
  follow-ups) will tell us how much of F-4 is already gone; the
  numbers in this section pre-date §7.7 and reflect the §7.6b
  *interim* state.

- **R-4 (cheap, additive to R-3): teach
  `ReducePineExpression.SearchForExpressionReduction` to fold
  `ParseAndEval(Literal(<encoded ParseAndEval>), env)` one level**
  when the inner `ParseAndEval`'s `encoded` is itself a literal —
  i.e., short-circuit the "literal wrapper, evaluate the wrapper,
  then evaluate the inner body" two-step into a single
  `ParseAndEval` against the inner body with a flattened env. This
  removes the per-call `Build_List` for the cross-SCC env-list
  whenever the env-functions literal is empty. (The 2024-10-25
  stack-overflow noted at `ReducePineExpression.cs` line ≈ 117
  applies only to the recursive *value* fallback; the structural
  fold proposed here is purely syntactic and cannot loop.)

**Recommended next step (smallest blast radius first).** Re-run
`KernelJsonFunctionTests` on `HEAD` (post-§7.7) and capture the new
numbers. If §7.7 alone has restored most of the regression
(expected for the non-recursive single-member SCC case, which is
exactly what most decoder combinators are), R-4 plus an
opportunistic CSE in `ReducePineExpression` (R-1) should close the
remaining gap on `Decode_lazy_nested` and `Decode_oneOf` without
any change to the §7.6b architectural decisions. If the regression
persists, R-2 is the next escalation. R-3 is already done.

---

## 1. Background: How the Current Layout Works

### 1.1 Overall Structure

Compilation of an Elm program currently proceeds (see `ElmCompiler.cs`
`CompileInteractiveEnvironment` and the helper
`ComputeDependencyLayoutsAndSccs`) as follows:

1. Collect every top-level function declaration that should be
   compiled into a flat dictionary keyed by qualified name
   (`Module.functionName`).
2. For every function, collect its **direct dependencies** by walking
   the body and recording calls to other compiled functions.
3. Run Tarjan's algorithm on the call graph to obtain the strongly
   connected components (SCCs) **in dependency order** (sinks first,
   roots last).
4. For every SCC, compute a single shared `IReadOnlyList<string>`
   *layout*. The layout is currently:

       Members(sorted) ++ AdditionalDependencies(sorted)

   where `AdditionalDependencies` is the union of all transitive
   dependencies of all SCC members that are not themselves SCC
   members. This means the layout includes *every* compiled function
   reachable from the SCC, not just the SCC itself. The shared layout
   is stored in `CompilationContext.FunctionDependencyLayouts` keyed
   by every member name.
5. Iterate SCCs in dependency order. For each SCC, `CompileSCC`:
   - compiles the body of every SCC member with
     `ExpressionCompiler.Compile`, passing the shared layout in
     `ExpressionCompilationContext.DependencyLayout`;
   - encodes each compiled body as a `PineValue`
     (`ExpressionEncoding.EncodeExpressionAsValue`);
   - assembles the `envFunctionsList` by walking the layout: SCC
     members come from the just-encoded bodies, dependencies outside
     the SCC come from the cache populated by previously compiled
     SCCs;
   - calls `FunctionValueBuilder.EmitFunctionValueWithEnvFunctions`
     once per member to produce the final wrapper function value
     embedded in the module.

### 1.2 Per-Call-Site Code Generation

`ExpressionCompiler.cs` (around lines 530–600) emits a "full
application" against a known top-level callee as follows:

- The encoded body of the callee is fetched at runtime via
  `current_env[0][functionIndex]`, where `functionIndex` is the
  callee's position inside the *caller's* layout.
- The callee's own env-functions list is constructed at call time by
  reading `current_env[0][depIndex]` for every entry in the *callee's*
  layout. Because all SCC members share a layout, and because the
  caller is required to also have every callee dependency in its own
  layout (this is enforced — `FunctionNotInDependencyLayout` is
  returned otherwise), this works.
- The new environment passed to `ParseAndEval` is a flat list:
  `[ envFuncsForCallee, arg0, arg1, ... ]`.

In other words, both the callee body and the callee's transitive env
contents are looked up by index from the *caller's* env-functions
list. Every function therefore carries a copy of every transitive
dependency of every function it might call. For a real Elm program
this list grows large quickly.

### 1.3 Function-Value (Generic) Application

When the compiler does not know the callee (parameter, local binding,
record-access function), it emits per-argument `ParseAndEval` chains
where the function value (a `WithEnvFunctions` or `WithoutEnvFunctions`
record) carries everything it needs internally. That path is unchanged
by this proposal; it already serves as the precedent for an
"inline-the-value" strategy.

### 1.4 Why the Current Design Was Adopted

The shared layout is convenient because the entire SCC can be emitted
in a single pass: each SCC member can reference every other member
*and* every external dependency through the same flat list, without
having to recompute the path expression for each call site. It also
matches the structure of `FirCompiler.elm`, which is the upstream
reference implementation.

The cost is that:

- Every compiled function carries a runtime list whose length is the
  size of its transitive call closure (within compiled functions),
  even when no call site needs any given entry.
- Every full-application call site emits two `Build_List` operations
  (one for the callee's env-functions list, one for the call
  environment), each with a fan-out matching the callee's layout
  width.
- The encoded function bodies are never literally embedded in the
  caller's expression, so content-addressable deduplication cannot
  share identical callee bodies across call sites — each caller
  *references* the body by index, not by value.

---

## 2. The Proposed New Layout

### 2.1 What the Compiler Stores Per Emitted Function

For every top-level function the compiler produces, it stores a
**`CompiledFunctionInfo`** record with the following four properties.
These are the canonical artifacts that consuming sites (other call
sites, partial-application points, host-side parsers and tools) work
against.

1. **`WrapperValue : PineValue`** — the function value wrapped for
   *generic incremental application of arguments*. This is the
   "function-value" representation: a nested chain of `ParseAndEval`
   forms that, when applied one argument at a time, eventually
   evaluates the body. It is what today's
   `FunctionValueBuilder.EmitFunctionValueWithEnvFunctions` /
   `EmitFunctionValueWithoutEnvFunctions` produce.
2. **`ParameterCount : int`** — the number of parameters the function
   declares.
3. **`EnvFunctions : IReadOnlyList<PineValue>`** — the list of
   env-function `PineValue`s that the function's body expects to find
   at `current_env[0]`. This list is **empty** when the function is
   not part of any (mutually) recursive group; it contains the SCC
   members' wrapper values when the function is part of a recursive
   SCC.
4. **`EncodedExpression : PineValue`** — the encoded body expression
   in the layout the compiler has used so far for SCC-internal calls:
   the env-functions list is expected at index 0 of the runtime
   environment, and the function's arguments are spread starting at
   index 1. Concretely the body reads its env functions as
   `current_env[0][i]` and its arguments as `current_env[i+1]`.

Properties 3 and 4 together fully specify the body and its closure;
property 1 is the convenient packaging for partial application and
for being passed around as a value; property 2 is metadata used to
choose the call-site shape.

The compiler is structured as a tree of SCCs in dependency order.
When an SCC `S` finishes compiling, every member `F ∈ S` has a
populated `CompiledFunctionInfo` that the compiler caches for use at
any call site reached later in the topological order — including
call sites in subsequent SCCs.

### 2.2 Two Call-Site Forms

At every emission of a known-callee function application, the
compiler chooses between **two functionally equivalent shapes**. The
choice depends solely on whether the number of arguments supplied at
the call site equals the callee's `ParameterCount`.

#### Form A — Compact (single `ParseAndEval` for all arguments)

Used when **`appliedArgs.Count == callee.ParameterCount`** (a
"saturated" application).

```
ParseAndEval
  encoded     = Literal( callee.EncodedExpression )
  environment = List [ Literal( List [ callee.EnvFunctions... ] ),
                       arg0, arg1, ..., arg{n-1} ]
```

That is:

- `encoded` is the literal `PineValue` of property **4**.
- `environment` is a `List` expression whose item at index 0 is the
  literal of property **3** wrapped as a single `PineValue` (an empty
  list literal when the callee is non-recursive), and whose items at
  positions 1..n are the argument expressions in order.
- This shape is single-`ParseAndEval`-deep at the call site.

#### Form B — Generic (one `ParseAndEval` per argument)

Used when `appliedArgs.Count != callee.ParameterCount` (under-
or over-application, including the special case of zero arguments
applied to a function that takes more than zero).

```
ParseAndEval                                                -- outermost: applies argN-1
  encoded     = ParseAndEval                                -- applies argN-2
                  encoded     = ...                         -- repeats per applied argument
                                  encoded     = Literal( callee.WrapperValue )
                                  environment = arg0
                  environment = arg{N-2}
  environment = arg{N-1}
```

Concretely, each applied argument adds one nesting layer of
`ParseAndEval`; the innermost `ParseAndEval` has the literal of
property **1** as its `encoded` operand. This is the same "incremental
application of a function value" mechanism used today for unknown
callees (parameters, locals, record-access functions).

### 2.3 Rationale for Two Forms

- **Form B is generic.** It works for *every* application of any
  function value — saturated, partial, or over-applied. If we only
  ever emitted Form B, the compiler would still be correct.
- **Form A is more compact.** When the arity matches, Form A collapses
  what would otherwise be `n` nested `ParseAndEval`s into a single
  one. This:
  - reduces nesting depth of the emitted code, which lowers
    serialization cost, storage, and the cost of structural equality
    checks downstream;
  - exposes the entire argument list to the body in one step, which
    lets the interpreter (and any compiled specialization of it)
    avoid the per-argument parse/eval handshake.

Form A is therefore preferred whenever applicable; Form B is the
fallback that guarantees universal coverage.

### 2.4 Functional Equivalence and the Need for Canonicalization

Forms A and B produce the same observable result for any given
saturated application: applying a function value (Form B) until all
its arguments are consumed is semantically identical to invoking the
underlying body with `[envFunctions, args...]` as its environment
(Form A). Both forms are valid Pine.

This means that **a single conceptual function application can appear
in the compiled program in two distinct concrete shapes**. Any
downstream tool that derives a *name* (or any other canonical
identifier) for an application — most importantly the static-program
parser used by snapshot tests — must therefore treat both shapes as
equivalent and map them to the **same name** before rendering. The
mapping must be deterministic and stable across runs.

The recommended approach is to canonicalize both shapes to a common
intermediate form during parsing: when the parser recognizes either
shape for a known callee `F` with `n` arguments and detects that
`appliedArgs.Count == n`, it normalizes the result to Form A's
"saturated application of `F`" representation. Form-B-shaped
saturated applications are detected by walking the chain of
per-argument `ParseAndEval`s, identifying that the innermost
`encoded` operand is a literal `WrapperValue` of some known `F`, and
counting that the chain length equals `F.ParameterCount`.

The same canonicalization applies to recursive callees: a same-SCC
call may be emitted in either form, and both must reduce to the same
name.

### 2.5 Same-SCC vs. Cross-SCC: A Single Mechanism

Under the new design the **same two forms are used for both same-SCC
and cross-SCC callees**. The only difference is the *contents* of
property 3 (`EnvFunctions`):

- For a non-recursive callee, `EnvFunctions` is empty, so the
  literal at environment index 0 in Form A is `List []`.
- For a callee belonging to a recursive SCC, `EnvFunctions` contains
  the SCC members' wrapper values, so the literal at environment
  index 0 in Form A is `List [ memberWrapper0, memberWrapper1, ... ]`
  in the SCC's stable order.

Because the env-functions list is now stored alongside each callee
(property 3) and inlined as a literal at the call site, callers no
longer need to carry their callees' env-functions in their own
runtime layout. The caller's own layout is determined by *its own*
SCC membership, not by what it calls.

For a self-recursive function (an SCC of size one), the function's
own wrapper value appears in its own `EnvFunctions`. This is the only
case where a function's body, via `current_env[0][indexOfSelf]`,
recovers a value identical to the literal that callers use to invoke
it — exactly as today.

### 2.6 Zero-Parameter ("Plain Value") Declarations

A zero-parameter Elm declaration is, at the Pine level, a function
that, when applied to an empty argument list, computes a value.
Today, `EmitPlainValueDeclaration` emits exactly that. Under the new
design, two strategies are possible at compile time:

1. **Eagerly evaluate.** Run the wrapped expression in a new VM
   instance with a budget (suggested: 100,000 instructions) and, if
   evaluation succeeds, replace the declaration with a wrapper that
   returns the evaluated value as a literal.
2. **Keep as a deferred application.** If the budget is exceeded, or
   if the evaluated value is *larger* than the application
   representation that produced it, keep the declaration as the
   already-compiled wrapper expression.

The compiler must decide between (1) and (2) per declaration. The
heuristic is:

```
if evalSucceededWithinBudget && size(resultValue) <= size(applicationExpression):
    emit literal value
else:
    emit application
```

Eager evaluation is *not* always safe because the compiler will
eventually support non-strict evaluation, and infinite values must
not force the compiler to loop. The instruction budget bounds this
and also bounds total compilation time.

The heuristic must be deterministic so that snapshot tests and
content-hash-based caching remain stable across runs.

Note that Form A applies naturally to a zero-parameter saturated
"call": `appliedArgs.Count == 0 == ParameterCount`, so the
environment is just `List [ Literal(EnvFunctions) ]` (the env
functions slot, with no following arguments).

---

## 3. Affected Downstream Sites

The following components consume the env-functions layout, the
"encoded body lives at index 0,k of the env" convention, or the
particular wrapper shape that
`FunctionValueBuilder.EmitFunctionValueWithEnvFunctions` produces.
Each is a candidate for adaptation. (Locations are given as relative
file paths to the repository root.)

### 3.1 The Compiler Itself

- `implement/Pine.Core/Elm/ElmCompilerInDotnet/ElmCompiler.cs`
  - `ComputeDependencyLayoutsAndSccs`: shared layout shrinks to SCC
    members only; `AdditionalDependencies` is no longer used to
    populate the layout, but is still useful for ordering SCC
    compilation.
  - `CompileSCC`: produces a `CompiledFunctionInfo` per SCC member
    with all four properties populated (see §2.1):
    - `EncodedExpression` (property 4) is the encoded body in the
      env-functions-at-index-0 layout.
    - `EnvFunctions` (property 3) is empty for non-recursive members;
      for a recursive SCC of size *k*, it is the list of the *k*
      members' wrapper values in stable (sorted) order.
    - `WrapperValue` (property 1) is built via
      `FunctionValueBuilder.EmitFunctionValueWithEnvFunctions` (when
      `EnvFunctions` is non-empty) or
      `EmitFunctionValueWithoutEnvFunctions` (when empty).
    - `ParameterCount` (property 2) is the declared arity.
  - `EmitPlainValueDeclaration`: gains the eager-evaluation
    heuristic described in §2.6.
- `implement/Pine.Core/Elm/ElmCompilerInDotnet/ExpressionCompiler.cs`
  - The "full application" branch (around lines 530–600) becomes
    arity-based, not SCC-based:
    - If `appliedArgs.Count == callee.ParameterCount`, emit
      **Form A** (single `ParseAndEval` with `Literal(callee.EncodedExpression)`
      and `List [ Literal(List(callee.EnvFunctions)), arg0, ..., arg{n-1} ]`).
    - Otherwise, emit **Form B** (per-argument `ParseAndEval` chain
      whose innermost `encoded` is `Literal(callee.WrapperValue)`).
  - The same dual-form emission applies to *both* same-SCC and
    cross-SCC callees; the only difference is the contents of
    `callee.EnvFunctions` (and therefore the literal embedded at
    environment index 0 in Form A).
  - The `FunctionNotInDependencyLayout` error path becomes
    unreachable for cross-SCC callees and remains only as a
    defensive check for genuinely missing same-SCC entries (which
    would indicate a compiler bug).
  - The lambda-application helper at lines ~1670–1730 (which builds
    function values for higher-order argument passing) inlines a
    cross-SCC callee's `WrapperValue` (property 1) as a literal when
    passing it as a value, mirroring the Form B representation.
- `implement/Pine.Core/Elm/ElmCompilerInDotnet/CompilationContext.cs`
  - `CompiledFunctionInfo` is extended (or revised) to carry all
    four properties from §2.1 explicitly. The previously single
    `EncodedBody` field is preserved as `EncodedExpression`
    (property 4). New fields hold `WrapperValue` and `EnvFunctions`.
  - `ExpressionCompilationContext.DependencyLayout` semantics change
    (members-only).
  - `GetFunctionIndexInLayout` is used only when constructing the
    body's view of `current_env[0]` during compilation; it is *not*
    consulted at the call site (call sites read all four properties
    of the callee directly from the compiled-functions cache).

### 3.2 Function-Value Parsing, Encoding, and Canonicalization

These components are **central to this work** because every shape
emitted by the compiler must be recognized, decomposed, and
canonicalized to a stable name before being rendered (e.g. for
snapshot tests). Both Form A (compact, single `ParseAndEval`) and
Form B (generic, per-argument `ParseAndEval` chain) must be accepted
and mapped to the **same** logical application of the same callee.

- `implement/Pine.Core/CodeAnalysis/FunctionRecord.cs`
  - `ParseFunctionRecord` / `ParseFunctionValue` already detects the
    `WithEnvFunctions` and `WithoutEnvFunctions` wrapper shapes.
    Under the new design, *every* emitted `WrapperValue` (property 1)
    is one of these two shapes, with `WithoutEnvFunctions` used when
    `EnvFunctions` is empty and `WithEnvFunctions` otherwise. The
    parser must continue to round-trip both, and tests must cover
    both with one, two, and three parameters.
  - The parser must also expose enough metadata for downstream
    canonicalization to recover, from a `WrapperValue`, the
    underlying body identity (so that Form A and Form B both
    fingerprint to the same callee).
- `implement/Pine.Core/CodeAnalysis/FunctionValueBuilder.cs`
  - `EmitFunctionValueWithoutEnvFunctions` and
    `EmitFunctionValueWithEnvFunctions` are the builders for property
    1 (`WrapperValue`). No new builder is required, but the compiler
    must always populate property 4 (`EncodedExpression`) consistent
    with the body the wrapper applies — i.e. a body that reads
    `current_env[0][i]` for env functions and `current_env[i+1]` for
    arguments.
- `implement/Pine.Core/CodeAnalysis/StaticProgramParser.cs`
  - The parser must recognize **both** call-site forms for any known
    callee:
    - Form A: a single `ParseAndEval` whose `encoded` operand is a
      literal matching some known callee's `EncodedExpression`
      (property 4) and whose `environment` is a `List` expression
      with a literal env-functions list at index 0.
    - Form B: a chain of `ParseAndEval` whose innermost `encoded`
      operand is a literal matching some known callee's
      `WrapperValue` (property 1).
  - In both cases, the parser produces the same canonical name —
    e.g. `Module.functionName` applied to `n` arguments — so that
    snapshot output is identical regardless of which form the
    compiler chose.
  - The parser must also tolerate Form B chains that are *not*
    saturated (under-application). These remain rendered as a partial
    application and must not be confused with Form A.
  - Comments at lines 501 and 708 (which assume a fixed
    `[envFunctions, arg0, arg1, ...]` shape for every full
    application) must be rewritten to describe the dual-form
    convention.
- `implement/Pine.Core/CodeAnalysis/ExpressionBuilder.cs`,
  `implement/Pine.Core/CodeAnalysis/CodeAnalysis.cs`,
  `implement/Pine.Core/CodeAnalysis/NamesFromCompiledEnv.cs`,
  `implement/Pine.Core/CodeAnalysis/ElmInteractiveEnvironment.cs`
  - All inspect compiled environments to derive symbol names,
    decompose nested wrappers, etc. Each must:
    - Accept both wrapper shapes (`WithEnvFunctions`,
      `WithoutEnvFunctions`).
    - Use the canonicalization step from `StaticProgramParser` (or
      a shared helper) so that Form A and Form B applications of the
      same callee are reported under the same name.

### 3.3 Call Sites in the .NET Host

- `implement/pine/Elm/ElmCompilerInElm.cs`
- `implement/pine/Elm/Platform/MutatingCommandLineApp.cs`
- `implement/pine/Elm/Platform/MutatingWebServiceApp.cs`
- `implement/pine/Elm/Platform/WebServiceInterface.cs`
- `implement/pine/Elm/Platform/VolatileProcessHost.cs`
- `implement/pine/Elm/Platform/ApplyUpdateReport.cs`
- `implement/pine/ElmTime/JsonAdapter.cs`
- `implement/pine/Platform/WebService/PersistentProcessLive.cs`
- `implement/pine/Pine/CompilePineToDotNet/CompileModuleToCSharp.cs`
- `implement/pine/Pine/PineVM/Precompiled.cs`

These mostly call `FunctionRecord.ParseFunctionRecord(Tagged)?` to
unwrap a compiled value and then apply arguments. Because both
existing wrapper shapes are already supported by `ParseFunctionValue`,
most of these need no change beyond test fixtures being regenerated.
Each must be re-verified end-to-end against a suite of compiled apps.

### 3.4 Tests and Snapshot Fixtures

Snapshot tests under `implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet`
will produce new outputs. Each must be re-baselined after a careful
manual diff, because the shape change is intentional. Specifically:

- `ElmCompilerTests/DependencyLayoutTests.cs` asserts the *exact*
  contents of layouts. Many of those expectations change: a
  non-recursive function will have an empty env-functions list (or
  only itself, depending on the chosen convention).
- `ElmCompilerTests/SimpleRecursiveFunctionTests.cs`,
  `*OperatorApplicationTests.cs`, `*RecordExpressionTests.cs`, etc.
  assert structural shapes of the emitted Pine expressions. Each
  will require re-baseline. Where possible, snapshot assertions
  should be expressed against the **canonical name** produced by
  `StaticProgramParser` rather than against the raw expression
  shape, so that switching between Form A and Form B for the same
  saturated call does not require future re-baselining.
- `Inlining/SizeBasedInliningRegressionTests.cs` and the snapshot
  fixtures it uses depend on the wrapper shape too.
- Tests under `Pine.Core.Tests/CodeAnalysis/` (notably
  `FunctionValueBuilderTests.cs`, `StaticProgramParserTests.cs`)
  exercise the parser. New tests must cover **both Form A and
  Form B** call-site shapes for the same callee, asserting that
  both produce the same canonical name when saturated and a
  distinguishable partial-application name when under-applied.
- Integration tests under `Pine.IntegrationTests/CodeAnalysis/`
  (`OptimizeAndEmitStringFromIntTests.cs`,
  `OptimizeAndEmitIdivTests.cs`, `ParserFastTests.cs`,
  `ElmValueJsonValueEncodingTests.cs`) will require re-baselining.

### 3.5 Supporting Documentation

- `implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md`
  ("Full Function Applications", "Composition of the Environment
  Functions List", "Function Values And Generic Function Application").
  See section 7.9 below for the precise edits.
- `implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-in-dotnet-coverage-gaps.md`
  may reference layout shapes; review and update.
- `implement/Pine.Core/Elm/ElmCompilerInDotnet/explore-early-instantiation-stage.md`,
  `analysis-inlining-improvements.md`,
  `cross-module-inlining-challenge.md` mention the same plumbing in
  passing; cross-references should be added.
- `docs/2026-04-12-cross-module-inlining-design.md` and
  `docs/2026-04-11-size-based-inlining-stack-overflow-investigation.md`
  reference the env-functions list. Add an addendum noting the new
  layout.

### 3.6 The Elm-in-Elm Compiler

`implement/Pine.Core/Elm/elm-in-elm/src/FirCompiler.elm`,
`implement/Pine.Core/Elm/elm-in-elm/src/ElmCompiler.elm` and the
training context apps under
`implement/test-and-train/elm-interactive-scenarios-train/elm-compiler-dependencies/`
are the upstream Elm reference implementation. They are not changed
by this work — the .NET compiler is allowed to diverge. We should
add a note in the .NET compiler's header comment explaining the
divergence so future readers do not "fix" it.

---

## 4. Expected Challenges

### 4.1 Two Output Shapes Co-Exist

After the change, *every* downstream consumer must accept both:

- "old" `WithEnvFunctions` wrappers (recursive SCC members), and
- "new" `WithoutEnvFunctions` wrappers (non-recursive declarations).

The parser already does, but every call site that currently asserts
the wrapper has env-functions has to be widened. Subtle bugs hide in
places that do `if (envFuncs.Count == ...)` without the
`WithoutEnvFunctions` branch.

### 4.2 Snapshot Test Churn

Almost every snapshot test that captures the compiled environment
will change. Mass re-baselining without a careful per-snapshot review
risks hiding a genuine regression behind a wave of intended changes.
The recommended mitigation is to land the structural layout change in
its own commit, separate from any logic change, and review every
snapshot diff manually against a small set of canonical examples
that we trace by hand.

### 4.3 SCC Detection Quality

Today's SCC detection is parametrized only by direct dependencies
*within compiled functions*. Cross-module dependencies, dependencies
on functions that are excluded from compilation roots, and
specialized variants generated mid-pipeline can all blur the call
graph. A function that appears self-contained may, post-inlining or
post-specialization, develop a recursive edge that wasn't present in
the original SCC graph. The compilation order must guarantee that
the SCC graph used at code-generation time is the *post-optimization*
graph, not the pre-optimization graph.

### 4.4 Inlined Wrapper Size

Embedding a complete `PineValue` for a callee at every call site is
correct but, for large callees with many call sites, may inflate the
*expression-tree* size — even though the value-tree size stays
constant under content addressing. Diagnostic tooling that measures
expression-tree depth or count (e.g. `Reducer`, `cse` passes) may
behave differently.

The selection rule between Form A and Form B is **purely arity-based**
(see §6.3); no size threshold is used to fall back to a different
emission strategy. Form A is the principal mitigation: when the
nesting depth of a saturated call would otherwise be `n` (one
`ParseAndEval` per argument in Form B), Form A collapses it to one.
We should measure the expression-tree size impact after the switch
and, if a regression is observed, revisit this rule rather than
silently introducing a size cap.

### 4.5 Self-Reference vs. Same-SCC Reference

A self-recursive function is an SCC of size one. Its `EnvFunctions`
(property 3) under the new design is `[selfWrapper]`. At a saturated
call site (Form A), the literal embedded at environment index 0 is
`List [ selfWrapper ]`, so the body finds itself at
`current_env[0][0]` exactly as today. The change for self-recursive
functions is therefore *only* that their `EnvFunctions` no longer
includes their non-recursive dependencies. Their `WrapperValue`
remains `WithEnvFunctions`.

### 4.6 Order of Compilation Within an SCC

Inside an SCC of size > 1, members refer to each other by index.
Compiling members one at a time and "publishing" their value as soon
as they are done is *not* sound, because a subsequent member's body
must reference a sibling by an env-index, not by literal value (the
literal value cannot exist yet — it would have to embed itself).
The current two-phase compile (compile bodies, then build wrappers)
must be preserved.

### 4.7 Form A and Form B: Two Functionally-Equivalent Call-Site Shapes

Form A and Form B both encode the same saturated application (when
the arity matches the parameter count). The compiler chooses Form A
in that case as a deliberate compactness optimization, but a Form B
emission of the same call is *also* legal Pine — and existing
emitters, hand-written test fixtures, or older compiled artifacts
may produce it.

The canonicalization step in `StaticProgramParser.cs` (and any tool
that consumes parsed output) must therefore be tested on **both
shapes side-by-side**:

- A test corpus of synthetic inputs that exercise Form A, Form B
  saturated, and Form B partial — for the same callee — should
  produce identical canonical names for the saturated cases and a
  distinguishable partial-application name for the under-applied
  case.
- Snapshot tests must be written against the canonical name, not
  against the raw expression shape, so that switching between Form A
  and Form B (now or in any future optimization pass) does not churn
  the snapshots spuriously.

This canonicalization requirement is the single most invasive
downstream consequence of the new design and motivates the
parser-first ordering of the implementation plan in §7.

### 4.8 Heuristic Stability for Zero-Parameter Declarations

The "evaluate within 100k instructions and prefer the smaller
representation" heuristic is sensitive to:

- the precise instruction-counting policy,
- the canonicalization of the result `PineValue` (deduplication
  applied or not),
- whether the comparison is value-tree size, expression-tree size,
  or some combined metric.

All three must be locked in advance and documented in the guide so
that compilation results are reproducible and bit-stable.

---

## 5. Downstream Adaptations Landable Before the Switch

Each of the following can be done in a separate commit and verified
against the *existing* compiler output before any output shape
changes. They de-risk the main switch. **Parser robustness — including
explicit recognition and canonicalization of both Form A and Form B —
must land first**, since both forms will appear in the compiled
output once the emitter changes are made.

1. **Parser robustness for both wrapper shapes.** Add explicit tests
   in `Pine.Core.Tests/CodeAnalysis/FunctionValueBuilderTests.cs` and
   `Pine.Core.Tests/CodeAnalysis/StaticProgramParserTests.cs`
   covering:
   - hand-constructed `WithoutEnvFunctions` wrappers for one-, two-,
     and three-parameter bodies (round-trip through
     `FunctionRecord.ParseFunctionValue`);
   - hand-constructed `WithEnvFunctions` wrappers with a non-empty
     env-functions list for one-, two-, and three-parameter bodies;
   - mixed environments containing both wrapper shapes side by
     side.
2. **Parser recognition of both call-site forms.** Add tests that
   feed `StaticProgramParser` synthetic call-site expressions in
   **Form A** (saturated, single `ParseAndEval`, literal env-funcs
   list at index 0) and **Form B** (per-argument `ParseAndEval`
   chain, literal `WrapperValue` at the innermost `encoded`), for
   the *same* underlying callee, and assert that:
   - both forms parse successfully;
   - both forms produce **identical canonical names**;
   - a Form B chain of length less than the callee's arity is
     reported as a partial application (different name).
3. **Symbol-name derivation.** Audit `NamesFromCompiledEnv.cs`,
   `ElmInteractiveEnvironment.cs`, and `CodeAnalysis.cs` for any
   path that requires `WithEnvFunctions` *or* assumes a particular
   call-site form. Add fallback handling and regression tests
   covering both forms.
4. **Static-program analysis.** Update `StaticProgramParser.cs`
   comments and any code that assumes the
   `[envFunctions, arg0, ...]` shape is the only full-application
   shape. Introduce the canonicalization helper that maps Form A
   and saturated Form B to a single representation.
5. **Wrapper-shape inspection in code-gen-to-CSharp.** Verify that
   `CompilePineToDotNet/CompileModuleToCSharp.cs` and
   `Precompiled.cs` correctly handle `WithoutEnvFunctions` wrappers
   for top-level declarations and both call-site forms.
6. **Tooling.** Anywhere that derives a "function name with hash
   suffix" from the env-functions composition (see the tag-based
   naming described in the implementation guide), add support for
   computing a suffix from a `WithoutEnvFunctions` wrapper. Add
   tests.
7. **Bench harness baseline.** Capture a baseline of compiled-program
   sizes (count of distinct `PineValue` nodes, total expression
   nodes, runtime instructions for a representative app) before any
   compiler change. This lets us validate the predicted improvements
   in a follow-up.

---

## 6. Open Design Questions

These should be decided before implementation begins. Each question
suggests a default but should be confirmed with the project owner.

1. **Layout convention for trivial SCCs.** For a non-recursive
   single-function SCC, the `EnvFunctions` list (property 3) is
   empty. The literal embedded at environment index 0 in Form A is
   therefore `List []`.
2. **Same-SCC convention for recursive SCCs.** `EnvFunctions`
   contains the SCC members' wrapper values, with no duplication,
   using a stable sorting (by qualified name) so that the emitted
   form is deterministic.
3. **Choosing between Form A and Form B.** The selection rule is
   purely arity-based: Form A iff
   `appliedArgs.Count == callee.ParameterCount`; Form B otherwise.
   No size-based fallback is applied — Form A's compactness
   advantage holds regardless of callee size.
4. **Eager evaluation budget.** Confirm 100,000 instructions for
   zero-parameter declarations. *Recommended default*: 100,000, but
   make it a named constant (`ZeroParamEagerEvalBudget`) for easy
   tuning.
5. **Comparison metric for "more compact" (zero-parameter case).**
   We compare both the maximum nesting depth and the number of
   unique expressions and values. Both must be lower or equal to
   decide to inline the resulting value.
6. **Specialized declarations.** Specialized variants must be
   independent, as the pipeline outputs declarations that could
   just as well appear as source code from the application.
7. **Backwards compatibility.** Only needed to the extent required
   by automated tests in the two .NET test projects (some of these
   parse persisted code emitted by previous versions). Because the
   parser must already accept both Form A and Form B, this is
   covered automatically.
8. **Should the call-graph for SCC detection consider only
   `Pine_kernel`/`Pine_builtin`-free dependencies?** No — there
   should be no such condition.
9. **Canonical name for saturated applications.** The canonical name
   produced by `StaticProgramParser` for any saturated application
   of `F` (regardless of Form A vs Form B) is the same name used
   today for full applications: `Module.functionName` plus the
   ordered argument list. *This is not a behavioural choice; it is a
   correctness requirement that snapshot tests rely on.*

---

## 7. Implementation Plan

The plan is broken into commits that should be landable in order.
**Parser adaptation lands first (commits 7.1–7.3): every consumer of
compiled Pine must accept and canonicalize both Form A and Form B
before the emitter is allowed to produce either of them in
unfamiliar combinations.** Only after parser robustness is verified
do we change what the emitter produces (commits 7.4–7.7).
Documentation lands in parallel with the code change that motivates
it.

### 7.1 Commit 1 — Parser Tests for Both Wrapper Shapes

> **Status (2026-04-22): Action 1 complete; Action 2 partially
> complete.** Round-trip tests for both wrapper shapes already exist
> in `FunctionValueBuilderTests.cs`
> (`ParseFunctionValue_With{,out}EnvFunctions_*_SymmetryTest`,
> parameter counts 0–3). New tests in `StaticProgramParserTests.cs`
> exercise Form B calls *to* a `WithoutEnvFunctions` callee
> (passing) and `WithoutEnvFunctions` *root declarations*
> (`Skip`-annotated, documenting Finding F-1). See §0 for details.

Files affected:

- `implement/Pine.Core.Tests/CodeAnalysis/FunctionValueBuilderTests.cs`
- `implement/Pine.Core.Tests/CodeAnalysis/StaticProgramParserTests.cs`
- `implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ElmCompilerTestHelper.cs`

Goal: prove that today's parsers and helpers correctly accept both
`WithEnvFunctions` (with arbitrary non-empty env-functions list) and
`WithoutEnvFunctions` wrappers for one-, two-, and three-parameter
top-level declarations.

Action items:

1. Hand-construct each wrapper shape with the relevant builder, then
   parse it via `FunctionRecord.ParseFunctionValue` and assert the
   recovered parameter count, env functions, and body.
2. Add a `StaticProgramParser` test that builds a tiny synthetic
   compiled environment containing a mix of `WithEnvFunctions` and
   `WithoutEnvFunctions` declarations, and asserts that all
   declarations are parsed and named.

This commit must not change any production code.

### 7.2 Commit 2 — Parser Recognition and Canonicalization of Form A and Form B

> **Status (2026-04-22): COMPLETE.**
>
> - Form A recognition + canonicalization at the call-site level
>   (3 passing tests; see §0).
> - New optional `IdentifyEncodedBodyOptional` callback on
>   `StaticProgramParserConfig<T>`.
> - Test config helpers populate the encoded-body lookup.
> - `CoreModules.AddCoreModules` updated to forward the new field
>   (Finding F-2).
> - F-1 fix landed: `WithoutEnvFunctions` root declarations are
>   correctly unwrapped end-to-end. `FunctionRecord` carries a
>   `UsesEnvFunctionsLayout` discriminant; `BuildApplicationFromFunctionRecord`,
>   `StaticProgramParser.ParseExpression` (via
>   `envParametersOffset`), and
>   `ApplyFunctionArgumentsForEvalExpr` honor it. The two
>   formerly-skipped tests pass. See Findings F-1 (resolved) and
>   F-3 (cascading consequence) in §0.
> - Stale "Environment structure: [[functions], [arguments]]"
>   comments in `StaticProgramParser.cs` (the resolve-callee branch
>   and the `ParseFullApplicationArguments` doc-string) refreshed
>   to point at the WithEnvFunctions layout per §2.1 and link to
>   this analysis document.

Files affected:

- `implement/Pine.Core/CodeAnalysis/FunctionRecord.cs`
  - `ParseFunctionRecordTagged` (the variant used by
    `StaticProgramParser.ParseProgram`) must be extended to
    correctly unwrap `WithoutEnvFunctions` wrappers used as root
    declarations. Today it routes via `ParseNestedWrapperForm`,
    which assumes a `[envFunctions, Environment, ...]` environment
    shape and so fails for the `[Environment]` (1-param) and
    `[concat ...]` (≥ 2-param) shapes that
    `EmitFunctionValueWithoutEnvFunctions` produces. See Finding
    F-1 in §0. One option is to route the un-tagged wrapper case
    through the existing `ParseFunctionValue` /
    `ParseFunctionValueFromParseAndEval` /
    `ParseFunctionValueFromListExpression` codepaths, which
    already handle both shapes.
- `implement/Pine.Core/CodeAnalysis/StaticProgramParser.cs`
  - Recognize **Form A** at a call site: a single `ParseAndEval`
    whose `encoded` is a literal that matches some known callee's
    `EncodedExpression` (property 4) and whose `environment` is a
    `List` expression with a literal env-functions list at index 0.
  - Recognize **Form B** at a call site: a chain of `ParseAndEval`
    whose innermost `encoded` is a literal that matches some known
    callee's `WrapperValue` (property 1).
  - Add a canonicalization helper that, for a saturated application
    detected in either form, returns the **same canonical name**
    (`Module.functionName(arg0, ..., arg{n-1})`).
  - Update the comments at lines 501 and 708 to describe the
    dual-form convention.
- `implement/Pine.Core/CodeAnalysis/NamesFromCompiledEnv.cs`,
  `implement/Pine.Core/CodeAnalysis/CodeAnalysis.cs`,
  `implement/Pine.Core/CodeAnalysis/ElmInteractiveEnvironment.cs`
  - Route through the new canonicalization helper so that the same
    name surfaces regardless of which form the compiler chose.

Tests (new):

- `Pine.Core.Tests/CodeAnalysis/StaticProgramParserTests.cs`:
  - For a hand-constructed callee `F` of arity 2, build a Form A
    saturated call site and a Form B saturated call site, and
    assert both parse to the **same** canonical name.
  - Build a Form B chain of length 1 against the same `F` and assert
    it parses as a partial application (distinct name).
  - Repeat for arity 0, 1, and 3.

This commit must not change the emitter; it expands the parser so
that the subsequent emitter changes do not break anything.

### 7.3 Commit 3 — Audit and Fix Symbol-Name Derivation

> **Status (2026-04-22): COMPLETE — clean audit, no fix needed.**
>
> Verified that all three files route their wrapper-shape decisions
> through helpers that now honor `FunctionRecord.UsesEnvFunctionsLayout`:
>
> - `NamesFromCompiledEnv.cs` only reads
>   `FunctionRecord.ParseFunctionRecordTagged` →
>   `BuildApplicationFromFunctionRecord` (both updated by F-1).
> - `CodeAnalysis.cs` does the same in
>   `ParseAsStaticMonomorphicProgram(FunctionRecord, …)` and
>   `ParseAsStaticMonomorphicProgram(IReadOnlyDictionary<…>, …, …)`.
>   The remaining direct `EnvFunctions.Length` inspection lives in
>   `FilterClassRemovingAllNonExpressions.KeepEntryAsIs` and only
>   matters for the legacy `"Function"`-tagged wrapper path; both
>   `WithEnvFunctions` and `WithoutEnvFunctions` wrapper values
>   parse as expressions and are kept as-is via the
>   `parseCache.ParseExpression` check that runs first.
> - `ElmInteractiveEnvironment.cs` —
>   `ApplyFunctionArgumentsForEvalExpr` already branches on the
>   flag (landed as part of the F-1 fix).
>
> No code changes required by this step; the audit is recorded
> here so future implementers do not need to repeat it.

Files audited (read-only):

- `implement/Pine.Core/CodeAnalysis/NamesFromCompiledEnv.cs`
- `implement/Pine.Core/CodeAnalysis/ElmInteractiveEnvironment.cs`
- `implement/Pine.Core/CodeAnalysis/CodeAnalysis.cs`

Goal: confirm that none of these paths *require* a `WithEnvFunctions`
wrapper, a non-empty env-functions list, or a particular call-site
form. If any do, fix and add a regression test using the helpers
from commit 7.2.

### 7.4 Commit 4 — Capture Baseline Metrics

Files affected:

- New test under `Pine.IntegrationTests/CodeAnalysis/` that compiles
  one canonical app and writes a JSON file with:
  - distinct `PineValue` count,
  - total expression-node count for the compiled environment,
  - count of `WithEnvFunctions` and `WithoutEnvFunctions` wrappers,
  - average env-functions list length,
  - count of Form A and Form B call sites.

The file is not committed; it is logged for later comparison and
the test is `[Fact(Skip = "metrics-only baseline")]`.

### 7.5 Commit 5 — Update Implementation Guide With New-Design Description

> **Status (2026-04-22): COMPLETE.**
>
> Added a new subsection *"Storing Compiled Functions and Two
> Call-Site Forms (Form A and Form B)"* to
> `implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md`
> (placed immediately after *"Function Values And Generic Function
> Application"*) covering:
>
> - the four properties stored per emitted function (§2.1),
> - the two call-site forms (§2.2),
> - the rationale (§2.3),
> - the canonicalization requirement (§2.4),
> - the `UsesEnvFunctionsLayout` discriminant (post-F-1),
> - and a backlink to this analysis document.
>
> No emitter shape change yet — emitter switch lands in 7.7.

Files affected:

- `implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md`

Goal: add a new subsection describing:

- the four properties stored per emitted function (§2.1),
- the two call-site forms (§2.2),
- the rationale (§2.3),
- the canonicalization requirement (§2.4),
- and link back to this analysis document.

This commit only documents intent; no shape change yet.

### 7.6 Commit 6 — Store Four Properties Per Compiled Function

> **Status (2026-04-23): COMPLETE — landed as §7.6a + §7.6b.**
>
> **§7.6a (2026-04-23) — additive only.** Extended
> `CompiledFunctionInfo` with two new properties — `int ParameterCount`
> and `IReadOnlyList<PineValue> EnvFunctions` — and routed both
> overloads of `WithCompiledFunction` through them. The single
> construction site in `ElmCompiler.CompileSCC` populates the new
> fields with `bodyInfo.paramCount` and the `envFunctionsList` value
> already baked into the wrapper. Wrapper bytes unchanged; snapshots
> unchanged; **2921/2921 `Pine.Core.Tests` pass, 1 skipped**;
> `Pine.IntegrationTests` builds clean.
>
> **§7.6b (2026-04-23) — runtime-layout narrowing + cross-SCC
> literal-inlining at call sites.**
> `FunctionScc.GetLayout()` now returns SCC members only; the
> per-SCC `envFunctionsList` shrinks to the members' encoded bodies;
> cross-SCC callees are emitted at call sites via
> `Literal(callee.EncodedBody)` and `Literal(List(callee.EnvFunctions))`
> (full applications) or `Literal(callee.WrapperValue)` /
> `ParseAndEval(...)` for zero-parameter values
> (function-as-value references). All 2921 `Pine.Core.Tests` still
> pass after re-baselining ~30 snapshots / instruction-count
> assertions; see the §0 status snapshot for the per-test list and
> the cross-SCC zero-param wrapper-vs-value bug that was caught and
> fixed in the same session.
>
> **§7.6b also accomplishes part of the originally-planned §7.7
> work** (the cross-SCC `Literal(WrapperValue)` inlining at call
> sites). The remaining §7.7 work — switching non-recursive
> single-member SCCs to `WithoutEnvFunctions` wrappers and the
> explicit Form A vs Form B emitter selection — is described in the
> updated §7.7 banner below.

Files affected:

- `implement/Pine.Core/Elm/ElmCompilerInDotnet/CompilationContext.cs`
  - Extend `CompiledFunctionInfo` to carry `WrapperValue`,
    `ParameterCount`, `EnvFunctions`, and `EncodedExpression` (the
    latter being the existing `EncodedBody`, possibly renamed for
    clarity).
- `implement/Pine.Core/Elm/ElmCompilerInDotnet/ElmCompiler.cs`
  - `ComputeDependencyLayoutsAndSccs`: drop
    `AdditionalDependencies` from the layout result; continue to
    compute it for compilation ordering only.
  - `CompileSCC`: build all four properties for each member as
    described in §3.1. For non-recursive members, `EnvFunctions` is
    empty and `WrapperValue` comes from
    `EmitFunctionValueWithoutEnvFunctions`. For recursive SCC
    members, `EnvFunctions` is the SCC member list (in stable
    sorted order) and `WrapperValue` comes from
    `EmitFunctionValueWithEnvFunctions`.

Tests:

- Re-baseline every snapshot affected by the layout change.
- The snapshot diffs must show shorter env-functions lists
  (members only) on `WithEnvFunctions` wrappers, and
  `WithoutEnvFunctions` wrappers for non-recursive top-level
  declarations.

This commit must compile and all tests must pass; emission at call
sites is **unchanged** in this commit (the existing emission path
continues to work because the same `WrapperValue` is the artifact it
already produces).

### 7.7 Commit 7 — Emit Form A and Form B at Call Sites

> **Status (2026-04-24): COMPLETE.** Items 1 and 3 of the narrowed
> scope landed in commits `aed9a03b` (emitter) and `84575b40`
> (renderer + final snapshot). Item 2 (explicit Form A vs Form B
> selection) is satisfied behaviourally by the existing structure —
> see "Item 2 disposition" below. All 2920 `Pine.Core.Tests` pass,
> 2 skipped. See the §0 status snapshot for the full list of
> snapshots rebaselined and the parser-side change required to
> recognise the new wrapper shape at function-record root.
>
> **What §7.6b previously did** (out of scope for §7.7 then and now):
> - Cross-SCC callees emit `Literal(callee.EncodedBody)` /
>   `Literal(List(callee.EnvFunctions))` (full applications) and
>   `Literal(callee.WrapperValue)` / direct
>   `ParseAndEval(Literal(EncodedBody), [Literal(List(EnvFunctions))])`
>   for zero-parameter values (function-as-value references).
> - Cross-SCC callees no longer flow through `current_env[0][k]` of
>   any caller; the per-SCC `envFunctionsList` contains only the SCC
>   members.
> - The lambda-application helper at the historical
>   `ExpressionCompiler.cs` lines ~1670–1730 (originally listed as
>   needing an update for cross-SCC literal-inlining) is unaffected
>   because cross-SCC qualified-name function-value references now
>   flow through `CompileExpressionRef`, which already inlines the
>   literal.
>
> **What §7.7 added (landed):**
>
> 1. **`WithoutEnvFunctions` wrappers for single-member non-recursive
>    SCCs.** Done. `FunctionScc` carries `IsRecursive`; `CompileSCC`
>    branches on it; bodies of non-recursive single-member SCCs are
>    compiled with `UsesEnvFunctionsLayout = false` so parameters
>    live at `env[i]` instead of `env[1+i]`. The full-application
>    emitter omits the env-functions slot in the call environment
>    when the callee's `EnvFunctions.Count == 0`, producing
>    `[arg0, ..., argN-1]` directly.
>
> 2. **Item 2 disposition — Explicit Form A vs Form B selection.**
>    Already satisfied behaviourally by the existing emitter
>    structure: the saturated branch in
>    `ExpressionCompiler.CompileApplication` (≈ line 595) emits
>    Form A; the partial-/over-application path falls through to
>    `CompileGenericFunctionApplication`, which is the per-argument
>    `ParseAndEval` chain (Form B). The "innermost `encoded` is
>    `Literal(callee.WrapperValue)`" property holds because cross-SCC
>    function-value references go through `CompileExpressionRef`,
>    which §7.6b already wired to emit
>    `Literal(callee.WrapperValue)`. No additional refactor was
>    necessary; the canonicalization tests from §7.2 keep passing
>    unchanged.
>
> 3. **Snapshot rebaselines.** Done. See the §0 status snapshot for
>    the per-test list. The **renderer-side fix** in
>    `ElmCompilerTestHelper.RenderStaticFunction` deserves a note:
>    it accepts both `pathInEnv[0] is 1` (legacy `WithEnvFunctions`)
>    and `pathInEnv[0] is 0` (§7.7 `WithoutEnvFunctions`), rendering
>    both as `param_1_<pathInEnv[1]>[suffix]`. This avoided per-test
>    rebaselining for ~8 tuple-destructure / choice-type tests whose
>    rendered output is structurally identical but uses a different
>    env path under the new layout.
>
> **Parser-side change required by §7.7 (landed):** The §0 entry
> for §7.7 above describes this in detail, but to summarise:
> `FunctionRecord.ParseFunctionRecordTagged` previously returned a
> bare-`Literal` fallback for zero-param `WithoutEnvFunctions`
> wrappers (whose value is `EncodeExpressionAsValue(body)`),
> double-encoding the body. We now try
> `TryFunctionRecordFromParsedFunctionValue` first and only fall
> back to the literal path if that fails. Without this fix
> downstream parsing of zero-param non-recursive declarations under
> the new wrapper shape would produce malformed
> `Literal(<encoded body>)` nodes.
>
> The original §7.7 plan text below is preserved for reference but
> the items above describe what actually landed.

Files affected:

- `implement/Pine.Core/Elm/ElmCompilerInDotnet/ExpressionCompiler.cs`
  - Replace the existing full-application emission with the
    arity-based selection of §3.1:
    - `appliedArgs.Count == callee.ParameterCount` → emit Form A:
      `ParseAndEval(Literal(callee.EncodedExpression), List [ Literal(List(callee.EnvFunctions)), arg0, ..., arg{n-1} ])`.
    - Otherwise → emit Form B: per-argument `ParseAndEval` chain
      whose innermost `encoded` is `Literal(callee.WrapperValue)`.
  - Apply the same dual-form selection uniformly to same-SCC and
    cross-SCC callees; do not branch on SCC membership.
  - Update the lambda-application helper at lines ~1670–1730 to
    inline the cross-SCC callee's `WrapperValue` as a literal when
    passing it as a value (Form B representation), removing reads
    from the current env for cross-SCC callees.
  - The `FunctionNotInDependencyLayout` error path remains as a
    defensive check for missing same-SCC entries.

Tests:

- Re-baseline snapshots. The diffs should show:
  - saturated call sites collapsed to a single `ParseAndEval`
    (Form A) with a literal env-functions list at index 0 (empty
    for non-recursive callees, populated for same-SCC recursive
    ones);
  - non-saturated call sites preserved as a chain of per-argument
    `ParseAndEval`s (Form B).
- The canonicalization tests from commit 7.2 must continue to pass
  unchanged: the parser must produce the **same** canonical name
  for any saturated application of a given callee, regardless of
  whether the snapshot before this commit showed Form B and the
  snapshot after shows Form A.
- Run `Pine.IntegrationTests/CodeAnalysis/OptimizeAndEmitStringFromIntTests.cs`
  and similar; the metrics from commit 7.4 should show the
  predicted reduction in distinct value count and nesting depth.

### 7.8 Commit 8 — Zero-Parameter Eager-Evaluation Heuristic

> **Status (2026-04-24): DEFERRED — narrowed scope identified.**
> Investigation in this session showed that the
> *trivially-constant subset* of §7.8 is already covered by
> `ReducePineExpression.SearchForExpressionReduction` /
> `TryEvaluateExpressionIndependent`, which runs on every body
> compiled by `CompileSCC` (line ≈ 962 of `ElmCompiler.cs`). When a
> body has no environment references and contains no
> `ParseAndEval` recursion, the existing reduction substitutes
> `Literal(value)` automatically — so zero-parameter declarations
> whose body is "just constants and kernel calls" already behave
> as eager.
>
> The **non-trivial subset** — bodies that invoke `ParseAndEval`
> against another (non-recursive) callee, e.g.
> `myList = List.range 1 100 |> List.map String.fromInt` — is
> *not* covered by the existing reduction, because
> `TryEvaluateExpressionIndependent` for `ParseAndEval`
> deliberately does NOT recurse into the PineVM (the PineVM
> fallback at lines ≈ 117–128 of `ReducePineExpression.cs` was
> disabled on 2024-10-25 due to a stack-overflow regression). This
> remaining subset is the only target left for §7.8.
>
> **Implementation requirements** for the non-trivial subset (for
> the next implementer):
> - Add a budgeted `PineVM.EvaluateExpressionOnCustomStack` call
>   inside `CompileSCC` Phase 3, gated on
>   `usesEnvFunctionsLayout == false && bodyInfo.paramCount == 0`,
>   guarding the `EvaluationConfig` with a small budget (e.g.
>   `InvocationCountLimit = 100_000`, `StackDepthLimit = 32`).
> - Add a size comparison: max-nesting-depth and distinct-node-count
>   of the resulting `PineValue` must both be ≤ those of the
>   deferred body (`EncodeExpressionAsValue(bodyInfo.body)`).
>   Reuse `PineValue.ListValue.NodesCount` for the latter.
> - Wrap in `try/catch` for `ParseExpressionException` and any
>   PineVM error; on any failure or budget exhaustion, fall back to
>   the deferred form.
> - Add the three tests called out below.
> - Estimated diff: ~100 lines + 3 tests + likely some snapshot
>   rebaselines for `DependencyLayoutTests` and perf-counter tests
>   where eagerly-evaluated wrapper bytes differ.

Files affected:

- `implement/Pine.Core/Elm/ElmCompilerInDotnet/ElmCompiler.cs`
  - `EmitPlainValueDeclaration`: implement the heuristic.
    1. Build the deferred-application form (a saturated Form A
       application of the zero-parameter wrapper) as today.
    2. Run the deferred form in a new `PineVM` instance with
       `instructionCountLimit = ZeroParamEagerEvalBudget` (a new
       `private const int = 100_000`).
    3. If evaluation succeeds and the resulting value is *not
       larger* than the deferred form (per the metric in question 5
       of §6 — both maximum nesting depth and distinct
       expression/value count must be ≤), emit
       `EmitFunctionValueWithoutEnvFunctions(Literal(resultValue),
       parameterCount: 0)`.
    4. Otherwise, emit the deferred form.
  - Document the chosen size metric inline.
- `implement/Pine.Core/Elm/ElmCompilerInDotnet/CompilationContext.cs`
  - No structural change.

Tests:

- New tests under
  `Pine.Core.Tests/Elm/ElmCompilerInDotnet/ElmCompilerTests/`
  covering:
  - a zero-parameter declaration that evaluates quickly to a small
    value (expect literal),
  - a zero-parameter declaration whose evaluated value is a large
    list and the application form is smaller (expect deferred),
  - a zero-parameter declaration that doesn't terminate within
    100,000 instructions (expect deferred and no test timeout).

### 7.9 Commit 9 — Documentation Refresh

Files affected:

- `implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md`
  Edit the existing sections as follows:

  - **"Full Function Applications" (lines ~40–113)**:
    - Replace the single "full application" description with the
      dual-form description from §2.2:
      a) **Form A** — saturated application:
         `ParseAndEval(Literal(callee.EncodedExpression), List [ Literal(List(callee.EnvFunctions)), arg0, ..., arg{n-1} ])`.
         The literal at environment index 0 is `List []` for
         non-recursive callees and the SCC member list for
         recursive callees.
      b) **Form B** — non-saturated (partial or over-application):
         a per-argument `ParseAndEval` chain whose innermost
         `encoded` is `Literal(callee.WrapperValue)`.
    - State the selection rule explicitly: Form A iff
      `appliedArgs.Count == callee.ParameterCount`.
    - Replace the worked `factorial` example to show factorial as a
      self-recursive SCC of size one. Its `EnvFunctions` is
      `[ factorialWrapper ]`, and a saturated call appears in Form A
      with that one-element literal list at environment index 0.
    - Add a worked cross-SCC example: `f x = g (h x)` where neither
      `f`, `g`, nor `h` is recursive. Show that the saturated body
      of `f` contains
      `ParseAndEval(Literal(<g.EncodedExpression>),
      List [ Literal(List []),
      ParseAndEval(Literal(<h.EncodedExpression>),
      List [ Literal(List []), x ]) ])`.
    - Add a worked Form B example: `g` passed unsaturated to
      `List.map`, which produces `Literal(<g.WrapperValue>)` as a
      function value.
  - **"Composition of the Environment Functions List"**:
    - Restrict the rules to SCC members only (property 3 of §2.1).
    - Remove the bullet stating that "Other functions might be
      included as well"; under the new design they are inlined as
      literals at the call site instead.
    - Keep the ordering rule (sorted by qualified name).
  - **"Function Values And Generic Function Application"**:
    - Clarify that this is exactly the Form B mechanism, and that
      the same mechanism is used for known callees that are not
      saturated.
  - Add a new subsection **"Storage Per Compiled Function"**
    documenting the four properties from §2.1.
  - Add a new subsection **"Two Call-Site Forms and Their
    Canonicalization"** documenting §2.2 and §2.4, including the
    explicit requirement that both forms must canonicalize to the
    same name in `StaticProgramParser`.
  - Add a new subsection **"Zero-Parameter Declarations"**
    describing the eager-evaluation heuristic, the budget constant
    `ZeroParamEagerEvalBudget = 100_000`, and the size-comparison
    metric.
  - Add a new subsection **"SCCs as the Compilation Tree"**
    explaining that compilation visits SCCs in topological order,
    that a finished SCC publishes a populated
    `CompiledFunctionInfo` per member into the cache, and that
    subsequent SCCs read all four properties of the callee directly
    when emitting their call sites.

- `implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-in-dotnet-coverage-gaps.md`
  Update references to the env-functions list to mention "for
  recursive SCCs only".
- `implement/Pine.Core/Elm/ElmCompilerInDotnet/explore-early-instantiation-stage.md`
  Add a forward-reference to the new design (no behaviour change in
  this file).
- `docs/2026-04-12-cross-module-inlining-design.md`,
  `docs/2026-04-11-size-based-inlining-stack-overflow-investigation.md`
  Add a one-paragraph addendum noting the new layout (four stored
  properties, two call-site forms) and pointing at this analysis.

### 7.10 Commit 10 — Cleanup

- Remove dead code in `ExpressionCompiler.cs` paths that were only
  reachable under the old "callee body lives in caller env" branch
  for cross-SCC callees.
- Remove the now-misleading "AdditionalDependencies" notion from
  `FunctionScc` if no remaining caller needs it. (If it is still
  used for compilation order, keep the field but tighten its XML
  doc.)
- Run `dotnet format` over every changed C# file.
- Re-run `dotnet  run` (per the repo's testing convention) for each
  affected test project and ensure all tests pass.

---

## 8. Verification Strategy

After every code commit (7.6 through 7.10):

1. From the directory of each affected test project, run
   `dotnet  run` (no filter) and confirm zero failures.
2. For commits 7.6, 7.7, and 7.8, additionally re-run the metrics
   harness from commit 7.4 and record the new numbers. The expected
   trend after commit 7.7 is a noticeable reduction in average
   nesting depth at saturated call sites and in distinct value count.
3. Manually inspect at least one `factorial`-style snapshot, one
   `map`-style higher-order-function snapshot (which exercises Form B),
   and one zero-parameter declaration snapshot per commit to confirm
   the diff matches the intended shape change.
4. Run the canonicalization tests from commit 7.2 after every
   subsequent commit; a regression here means the same conceptual
   call now renders under two different names in snapshots — a
   correctness bug.

If any verification step fails, the commit must be reverted or
amended before progressing to the next.

---

## 9. Summary

The new design stores **four properties per emitted function**: the
generic-application wrapper value (property 1), the parameter count
(property 2), the env-functions list (property 3, empty for
non-recursive functions), and the encoded body in the
env-functions-at-index-0 layout (property 4). Call sites then choose
between two functionally equivalent emission shapes based purely on
arity:

- **Form A** (saturated, single `ParseAndEval`): used when the
  number of arguments equals the parameter count. The `encoded`
  operand is the literal of property 4; the environment is a list
  with the literal env-functions list (property 3) at index 0,
  followed by the arguments. This is the compact form.
- **Form B** (per-argument `ParseAndEval` chain): used in all other
  cases. The innermost `encoded` operand is the literal of property
  1. This is the generic, universally-applicable form.

Both forms must be recognized and **canonicalized to the same name**
by `StaticProgramParser` and every other consumer that derives
identifiers from compiled Pine. The implementation plan therefore
lands parser robustness, dual-form recognition, and canonicalization
**first**, with explicit tests, before changing the emitter to
produce the new shapes. Recursive SCCs continue to populate
property 3 with their member wrappers; non-recursive functions get
an empty property 3 (and an empty env-functions literal at index 0
in Form A). Zero-parameter declarations gain a bounded
eager-evaluation heuristic. The migration proceeds in small,
testable commits.
