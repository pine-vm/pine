# Why `Hover_on_let_bound_name_in_case_branch_does_not_loop_forever` fails when `runSpecializationBeforeLambdaLifting = true`

Date: 2026-05-30

## TL;DR

* The regression test
  `Pine.Core.Tests…ApplicationTests.LanguageServiceProvideHoverRegressionTests.Hover_on_let_bound_name_in_case_branch_does_not_loop_forever`
  **passes** with the current default
  `ElmCompiler.CompileInteractiveEnvironment(runSpecializationBeforeLambdaLifting: false, inlineLetDestructureThunks: true)`
  and **fails** when `runSpecializationBeforeLambdaLifting` is flipped to `true`.
* Under `true`, evaluating `Probe.addWorkspaceFile` (which parses the synthetic
  `Main.elm` through the bundled `Elm.Parser`) on the test PineVM does not
  terminate within the 10,000,000-invocation limit. This is the same
  user-observable "loop forever" / "tail-call cycle" symptom reported for the
  full `ParserFast.elm` baseline.
* Root cause is in the **syntax-transformation pipeline that runs *before*
  lambda lifting** — the `SpecializeAndInlineDeclarations` (specializing-first)
  pass invoked from `ElmCompiler.RunStandardLoweringPipeline` when
  `runSpecializationBeforeLambdaLifting` is on. It emits monomorphized
  `__specialized__N` variants of the recursive `ParserFast` parser loops
  (e.g. `Elm.Parser.Expose.skipWhileWithoutLinebreakHelp__specialized__2`,
  `Elm.Parser.Expression.loopWhileSucceedsHelp__specialized__5__specialized__1`)
  that change the runtime behaviour of the parser relative to the lift-first
  ordering.
* The Elm **syntax interpreter** route, once fixed, serves as an oracle for both
  orderings: it *does not* reproduce the loop on the specialize-first output (it
  terminates with a correct-looking `WorkspaceSummaryResponse`), and its earlier
  *failure* on the lift-first output (`map2__lifted__lambda1` pattern-match
  error) was an interpreter bug — **module-agnostic resolution of unqualified
  references** — not a defect of the lift-first program. With that resolution bug
  fixed (see §4) the interpreter agrees with the PineVM on the lift-first output,
  so the divergence that remains is solely the specialize-first runtime-loop
  behaviour.

## How to reproduce

The investigation tests live in
`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/`:

* `TmpHoverRepro.cs`
  * `Repro_false` — compiles the LanguageService + synthetic `Probe`/`Main`
    fixture with `runSpecializationBeforeLambdaLifting: false` and runs the
    hover scenario on the PineVM. **Passes.**
  * `Repro_true` — identical but with `runSpecializationBeforeLambdaLifting:
    true`. **Fails:** `addWorkspaceFile` exceeds the 10M invocation limit.
  * `Repro_true_profile` — instruments the failing run, maps each PineVM
    invocation back to its declaration name, and writes the hottest functions to
    `/tmp/hover_profile_top.txt`.
  * `Repro_true_highlimit` — re-runs the `true` scenario with a 200M
    invocation/loop limit to distinguish "infinite loop" from "merely slow".
* `TmpHoverDiff.cs`
  * `Diff_decls_between_orderings` — lowers the same fixture under both
    orderings (lowering only, no Pine emission, ~15 s) and writes a structural
    diff plus rendered bodies of the looping function family to `/tmp/`.
  * `Interpreter_route_true` / `Interpreter_route_false` — run the
    **Elm syntax interpreter** (`ElmSyntaxInterpreter.Interpret`) directly on
    the post-lowering `ModulesForCompilation` for each ordering and record the
    result/error to `/tmp/hover_interpreter_{true,false}.txt`.

> The `Tmp…` prefix marks these as investigation scaffolding. They are not
> intended as permanent CI assertions (the `true` path is slow); fold the
> relevant pieces into a focused regression test once the defect is fixed.

## Evidence

### 1. The PineVM path loops only under `true`

`Repro_false` completes the full parse + hover well within the 10M limit
(~31 s). `Repro_true` aborts with:

```
Failed eval for 'addWorkspaceFile': Invocation count limit exceeded: 10_000_000
… stack frames: 36 … invocations: 10_000_001 …
```

The failure happens during `addWorkspaceFile`, i.e. while the bundled
`Elm.Parser` parses the synthetic `Main.elm` — not during `provideHover`.

### 2. The hot functions are specialize-first artefacts

`Repro_true_profile` (`/tmp/hover_profile_top.txt`) shows the work is dominated
by monomorphized parser-loop variants that **only exist under `true`**:

```
total invocations: 5,402,007 (before counting cap)
  900,783  Elm.Parser.Expose.skipWhileWithoutLinebreakHelp__specialized__2
  797,123  Elm.Parser.Tokens.isNotReserved
  302,217  ParserFast.pStepCommit
  135,397  Elm.Parser.Expression.loopWhileSucceedsHelp__specialized__5__specialized__1
  …
```

`Diff_decls_between_orderings` (`/tmp/hover_diff_summary.txt`) confirms the
ordering change is far from a no-op on this fixture:

```
decls(false)=1334  decls(true)=1713
onlyFalse=112  onlyTrue=491  differing=606
```

i.e. the specialize-first pass adds 491 new declarations (the `__specialized__N`
families, plus extra `__lifted__lambdaN` and `__stripped` siblings) and changes
the body of 606 existing declarations.

### 3. The specialized `skipWhile…` bodies are individually well-formed

The rendered bodies of the `skipWhileWithoutLinebreakHelp__specialized__N`
family (`/tmp/hover_focus_skipWhile.txt`) are structurally identical to the
generic `ParserFast.skipWhileWithoutLinebreakHelp`, just with the `isGood`
higher-order parameter replaced by a concrete predicate
(`Elm.Parser.Tokens.isAlphaNumOrUnderscore`, `Char.isHexDigit`, …). Each still
advances `offset` by 4 per recursion and terminates at end-of-input, so the
defect is **not** a single obviously-broken `__specialized__` body. The looping
is an emergent runtime behaviour of the specialize-first program as a whole
(a non-progressing outer parser combinator that keeps invoking these loops),
consistent with the "tail-call cycle, length 1" the PineVM reports on the full
`ParserFast.elm` baseline.

> Open question left for the fix: `Repro_true_highlimit` (200M limit) was started
> to confirm whether the `true` PineVM run is a true non-terminating cycle or
> "merely" a large multiplicative slowdown; it had not finished at the time of
> writing. Re-run it to nail this down. The full-file baseline's explicit
> `Cycle length: 1` detection argues for a genuine cycle.

### 4. Elm syntax interpreter route (as requested)

Running the **Elm syntax interpreter** on the post-lowering
`ModulesForCompilation` (the same input the Pine-emission backend consumes —
see `CompareInterpreterWithIntermediateVM.BuildInterpreterDeclarations` and the
"Elm Syntax Interpreter for Verification" section of
`Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md`):

* **`true` (specialize-first):** `addWorkspaceFile` **succeeds** and returns a
  well-formed `WorkspaceSummaryResponse` (`/tmp/hover_interpreter_true.txt`).
  The interpreter therefore does **not** reproduce the PineVM loop on the
  specialize-first syntax.
* **`false` (lift-first):** `addWorkspaceFile` **fails** in the interpreter with

  ```
  Case expression did not match any arm.
  Scrutinee value: Good Nothing (PState (<pine_blob …>) 0 1 1 1)
    at map2__lifted__lambda1 (<function>, <function>, <function>) (PState … )
    at map4__lifted__lambda1 [ <function>, … ] (PState … )
    at ParserFast.run (Parser <function>) "module Main exposing (..)…"
    at Elm.Parser.parseToFile …
    at LanguageService.addFile …
  ```

  i.e. the `map2__lifted__lambda1` helper's `case` fails to match a `Good …`
  scrutinee.

**Root cause (confirmed).** The failure is **not** a generic gap in handling
lambda-lifted *tuple-parameter* closures — hand-written reproductions of simple
and nested tuple-capture closures (including optional parsers returning
`Good Nothing`) interpret correctly. The real defect is **module-agnostic
resolution of unqualified references** in
`ElmSyntaxInterpreter.UserDefinedResolver`:

* Lambda lifting (`LambdaLifting.CreateLiftedFunctionCall`) emits calls to its
  generated helpers (e.g. `map2__lifted__lambda1`) as **unqualified**
  `FunctionOrValue` references, relying on a later qualification step that the
  interpreter's input (built without canonicalization) does not apply.
* Several modules independently define a helper with the **same simple name**
  but different behaviour. In particular `ParserFast.map2__lifted__lambda1`
  matches a 2-argument `Good a s1` constructor while
  `Parser.Advanced.map2__lifted__lambda1` matches a 3-argument `Good p1 a s1`.
* The interpreter resolved an unqualified reference by scanning **all**
  declarations and returning the first one whose simple name matched, regardless
  of module. So an unqualified `map2__lifted__lambda1` call from `ParserFast`
  could resolve to `Parser.Advanced`'s helper. A value built by one module's
  `Good` constructor then reaches the other module's `case`, which has a
  different arity → *"Case expression did not match any arm."*
* The same context-loss compounds one level deeper: once the wrong (or even the
  right) helper is entered via an **unqualified** call, the interpreter set the
  body's "current top-level" to that unqualified name, dropping the module. Any
  unqualified constructor reference inside the body (e.g. `Good`) then also
  resolved module-agnostically, picking whichever module's `Good` appears first.

**Fix.** Two coordinated changes in `ElmSyntaxInterpreter`:

1. `UserDefinedResolver` now resolves an unqualified reference with a
   **preference for the caller's own module**: it first restricts the search to
   declarations whose namespaces equal the calling context's module
   (`Application.Context.CurrentTopLevel.Namespaces`) and only falls back to the
   module-agnostic search when no same-module declaration matches.
2. `ApplicationResolution.ContinueWithFunction` now carries the **resolved
   declaration's full module-qualified name** (`ResolvedName`). When the
   interpreter enters the body it sets the "current top-level" to that resolved
   module (rather than the possibly-unqualified call-site name), so nested
   unqualified references — sibling helpers and constructors — resolve against
   the home module too.

With both changes the interpreter resolves lambda-lifted helpers and their
constructors to the correct module and serves as a sound oracle for the
lift-first output. Simple/nested tuple-parameter closures were never the
problem; the earlier "tuple-parameter closure gap" framing was a red herring.

The practical conclusion for the requested "interpreter route" check: after this
fix the syntax interpreter resolves the lift-first program the same way the
PineVM does, so it can be trusted as an oracle for **both** orderings. The
`Interpreter_route_{true,false}` tests remain wired up to re-confirm, and a
focused unit regression
(`UnqualifiedReferenceModuleResolutionTests`) pins the cross-module
resolution behaviour without the slow full-lowering path.

## Is this a defect in the compiler's syntax transformations? Where?

**Yes — the trigger is the specialization-before-lambda-lifting transformation.**

* The knob is `runSpecializationBeforeLambdaLifting` on
  `ElmCompiler.CompileInteractiveEnvironment` /
  `LowerToElmSyntaxForCompilation`. When `true`,
  `ElmCompiler.RunStandardLoweringPipeline` runs a convergence-bounded loop of
  `ElmSyntaxOptimization.SpecializeAndInlineDeclarations(…,
  RewriteConfig.Combined)` on the **canonicalized, not-yet-lambda-lifted** flat
  declaration dictionary, *before* the standard `LambdaLifting.LiftLambdas`
  pass (see `ElmCompiler.cs`, the `if (runSpecializationBeforeLambdaLifting)`
  branch around lines 682–721).
* This is the only pipeline difference that affects this test: the test uses
  `disableInlining: true`, so the post-lift optimization pipeline — and with it
  the `inlineLetDestructureThunks` cleanup — does **not** run. The
  `inlineLetDestructureThunks` default flip in the same commit is therefore
  irrelevant to this test; only `runSpecializationBeforeLambdaLifting` matters.
* The specializing-first pass produces the `__specialized__N` /
  `__specialized__N__specialized__M` parser-loop families and reshapes 600+
  declarations. The resulting program is accepted by the backend but changes the
  parser's runtime behaviour so that the PineVM no longer terminates on the
  `Elm.Parser` parse of `Main.elm`.

So the defect lives in the **SpecializingFirst pipeline applied pre-lambda-lift**
(`Pine.Core/Elm/ElmCompilerInDotnet/SpecializingFirst/*` driven from
`RunStandardLoweringPipeline`), not in lambda lifting per se and not in the
later inlining/cleanup passes. The exact mis-specialized combinator still needs
to be isolated to a single `__specialized__` family (the profile points the
finger at the `skipWhileWithoutLinebreakHelp__specialized__*` and
`loopWhileSucceedsHelp__specialized__*__specialized__*` loops and their callers);
the rendered-body diff in `/tmp/hover_differing_bodies.txt` is the next place to
bisect.

## Why the commit set the default to `false`

Commit `fcf6b14` ("find setup compatible with
`Hover_on_let_bound_name_in_case_branch_does_not_loop_forever`") set
`runSpecializationBeforeLambdaLifting = false` (and
`inlineLetDestructureThunks = true`) precisely to keep this test green: it
selects the pipeline ordering under which the LanguageService parse terminates.
It is a *workaround that avoids the trigger*, not a fix for the underlying
specialization defect. Re-enabling specialize-first must wait until the
mis-specialized parser-loop combinator is corrected.

## Suggested next steps

1. Finish the infinite-vs-slow determination with `Repro_true_highlimit`.
2. Bisect the specialize-first output: interpret/emit each
   `OptimizationIterations` sub-stage snapshot
   (`AfterSpecialization`, `AfterHigherOrderInlining`) to find which transform
   first introduces the non-terminating shape (technique documented in
   `elm-compiler-implementation-guide.md` §"Using the Interpreter to Diagnose
   Optimizer Defects").
3. Shrink to a single `__specialized__` family by trimming the synthetic
   `Main.elm` and the participating `ParserFast`/`Elm.Parser` combinators until
   one specialized loop reproduces the PineVM non-termination in isolation.
4. **Done (this change):** closed the interpreter's unqualified-reference
   resolution gap exposed by `Interpreter_route_false` — see §4 "Root cause" and
   "Fix" — so the interpreter resolves lambda-lifted helpers and their
   constructors to the correct module and can serve as an oracle for both
   orderings. Pinned by `UnqualifiedReferenceModuleResolutionTests`.
