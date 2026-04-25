# 2026-04-25 `maxOptimizationRounds = 2` corrupts `loopUntilHelp__specialized__2`

## TL;DR

After commit 8e06cbb (which raised the default for
`ElmCompiler.CompileInteractiveEnvironment(...).maxOptimizationRounds` from
`1` to `2`), parsing any input that contains a string literal through
`Elm.Parser.Expression.expression` corrupts the specialized recursive
helper `Elm.Parser.Tokens.loopUntilHelp__specialized__2`. The corruption
is introduced by the **specialization sub-stage of the second optimization
iteration** (the iteration with `Round = 1`); the higher-order-inlining and
size-based-inlining sub-stages of the same iteration neither introduce nor
mitigate the defect.

The compile-to-PineVM path surfaces the corruption as opaque
"infinite recursion at expression `3e207948`". The Elm syntax interpreter
surfaces it as the much more diagnostic
`Cannot bind named pattern 'PState' to value of type ElmValue+ElmFunction`,
together with a named Elm call stack that pinpoints the offending function
and the wrong-typed argument.

This document records what we observe, NOT a proposed fix. The defect is
left in place for a follow-up that owns the specialization stage.

## How to reproduce

`OptimizationRoundTwoInfiniteRecursionRegressionTests` in
`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/`
contains five `[Fact]`s built from one ~30-line Elm test module that does
nothing more than call `ParserFast.run Elm.Parser.Expression.expression`
on a `String`:

| Test                                                                                  | Pipeline reach                                  | Status |
| ------------------------------------------------------------------------------------- | ----------------------------------------------- | ------ |
| `..._with_maxOptimizationRounds_1_succeeds`                                           | PineVM, after round 0                           | pass   |
| `..._via_syntax_interpreter_after_round_one_succeeds`                                 | Syntax interpreter, after round 0 (`Inlined`)   | pass   |
| `..._with_maxOptimizationRounds_2_does_not_infinitely_recurse`                        | PineVM, after round 1                           | fail   |
| `..._via_syntax_interpreter_after_round_two_does_not_infinitely_recurse`              | Syntax interpreter, after round 1 (`Inlined`)   | fail   |
| `..._via_syntax_interpreter_after_iteration1_specialization`                          | Syntax interpreter, mid-round-1, after spec.    | fail   |
| `..._via_syntax_interpreter_after_iteration1_higher_order_inlining`                   | Syntax interpreter, mid-round-1, after HOI      | fail   |
| `..._via_syntax_interpreter_after_iteration1_size_based_inlining`                     | Syntax interpreter, end of round 1              | fail   |

## What the PineVM path reports

```
Failed eval: Detected infinite recursion. Cycle length: 1 invocation (stack-growing cycle).
Cycle expressions (one entry per invocation in the cycle):
  [0] 3e207948
Stack trace ending where the cycle entered (innermost first, 101 frames):
  3e207948
  3e207948
  ... (100 identical hashes)
```

`3e207948` is an opaque truncated SHA prefix of a Pine expression. There
is no built-in way to map it back to its originating Elm declaration
without instrumentation, which is exactly the kind of brittleness the new
requirement to also exercise the syntax interpreter is designed to avoid.

## What the syntax-interpreter path reports

`ElmSyntaxInterpreter.Interpret(parseStringLiteral, ["\"hello world\""], post-pipeline-decls)`
returns:

```
Cannot bind named pattern 'PState' to value of type Pine.Core.Elm.ElmValue+ElmFunction.
Elm call stack (innermost first):
  at Elm.Parser.Tokens.loopUntilHelp__specialized__2 True (Parser <function>) <function> "hello world" <function> <function>
  at Elm.Parser.Tokens.loopUntilHelp__specialized__2 False (Parser <function>) <function> "" <function> (PState (<pine_blob 52 bytes>) 4 1 1 2)
  at Elm.Parser.Tokens.singleQuotedStringLiteralAfterDoubleQuote__lifted__lambda3 (PState (<pine_blob 52 bytes>) 4 1 1 2)
  at ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn__lifted__lambda1 [ <function>, <function>, <function>, <function> ] (PState (<pine_blob 52 bytes>) 4 1 1 2)
  at ParserFast.symbolFollowedByParser "\"" (Parser <function>) (PState (<pine_blob 52 bytes>) 0 1 1 1)
  at ParserFast.offsetSourceAndThen__lifted__lambda1 <function> (PState (<pine_blob 52 bytes>) 0 1 1 1)
  at ParserFast.lazy__lifted__lambda1 <function> (PState (<pine_blob 52 bytes>) 0 1 1 1)
  at ParserFast.map3__lifted__lambda1 [ <function>, <function>, <function>, <function> ] (PState (<pine_blob 52 bytes>) 0 1 1 1)
  at ParserFast.loopWhileSucceedsOntoResultFromParser__lifted__lambda1 [ Parser <function>, <function>, <function>, <function> ] (PState (<pine_blob 52 bytes>) 0 1 1 1)
  at TestModule.parseExpression "\"hello world\""
  at TestModule.parseStringLiteral "\"hello world\""
```

Two things are immediately apparent that the PineVM trace did not show:

1. **The offending function is named.** It is
   `Elm.Parser.Tokens.loopUntilHelp__specialized__2` — a specialized
   variant of the `loopUntilHelp` recursive helper from `ParserFast.elm`
   (the helper that drives `ParserFast.loopUntil`, used by string-literal
   parsing in `Elm.Parser.Tokens.singleQuotedStringLiteralAfterDoubleQuote`).
2. **The corruption is in the recursive call's argument list, not the
   pattern-matching logic of the body.** Compare the two
   `loopUntilHelp__specialized__2` frames:

   | Frame                                | Argument 1     | Argument 2          | Argument 3   | Argument 4      | Argument 5    | Argument 6           |
   | ------------------------------------ | -------------- | ------------------- | ------------ | --------------- | ------------- | -------------------- |
   | Outer (correct, called from caller)  | `False`        | `(Parser <function>)` | `<function>` | `""`           | `<function>`  | `(PState ... 4 1 1 2)` |
   | Recursive (wrong, called from body)  | `True`         | `(Parser <function>)` | `<function>` | `"hello world"` | `<function>`  | `<function>`           |

   The 6th argument should be the new parser state `s1` produced by the
   element parser. In the well-formed outer call it is a `PState`
   record. In the corrupted recursive call it is a closure value (an
   Elm function). The body's first action is
   `case parseEnd s0 of Good () s1 -> ...`, which destructures `s0` with
   a `PState` constructor pattern; binding `PState` against a function
   value is precisely the error the interpreter reports.

   In the dynamically typed Pine VM, no such pattern check exists: the
   list-shaped projection that `case` lowers to simply pulls a
   sub-expression out of the function record and feeds it back into the
   body, which projects again, and so on — visible to the VM's cycle
   detector as "the same body expression invoked over and over with the
   same Pine encoding shape". Hence "cycle length 1, hash `3e207948`".

In short: the PineVM and the interpreter are looking at the same
underlying defect from different sides. The interpreter's pattern
checker turns the silent type-confusion into a self-describing error
that names the offending Elm function and points at the offending
argument slot.

## What changed between the working and broken pipeline outputs

Both `..._after_round_one_succeeds` and
`..._with_maxOptimizationRounds_1_succeeds` interpret/run the
`pipelineStageResults.Inlined` dictionary produced when the pipeline runs
exactly **one** iteration. Both pass. So `loopUntilHelp__specialized__2`,
as emitted by the first iteration's specialization sub-stage and then
processed by that iteration's higher-order-inlining and size-based-inlining
sub-stages, is well-formed.

The bug is introduced when the optimization pipeline runs a **second**
iteration. Indexing into `pipelineStageResults.OptimizationIterations` for
`Round == 1`, the three bisection tests reveal:

| Snapshot                        | Status |
| ------------------------------- | ------ |
| `iter1.AfterSpecialization`     | fail   |
| `iter1.AfterHigherOrderInlining`| fail   |
| `iter1.AfterSizeBasedInlining`  | fail   |

The first failing snapshot is `iter1.AfterSpecialization`. The two later
sub-stages neither fix nor compound the failure (they all reach the
interpreter with the same call stack and the same wrong 6th argument on
the recursive call). The defect is therefore introduced by the
**specialization phase of the second iteration**.

## Where to look

Concretely, in `implement/Pine.Core/Elm/ElmCompilerInDotnet/ElmCompiler.cs`,
the relevant call is at lines 1400-1411 (the `Phase 1: Specialization`
block inside `ApplyOptimizationPipelineWithStageResults`):

```csharp
{
    var result =
        ElmSyntaxSpecialization.Apply(currentDecls, Inlining.Config.OnlyFunctions);

    if (result.IsErrOrNull() is { } specErr)
        return $"Specialization (round {round}) failed: " + specErr;
    ...
    currentDecls = specDict;
}
```

When `round == 1`, `currentDecls` already contains
`Elm.Parser.Tokens.loopUntilHelp__specialized__2` — emitted in good shape
by `round == 0`'s pass through the same code. The `round == 1` invocation
of `ElmSyntaxSpecialization.Apply` rewrites it (or rewrites the call sites
that flow into its recursive call) such that the recursive call's argument
list is no longer well-formed.

Likely directions for the follow-up that owns the fix:

- Inspect what `ElmSyntaxSpecialization.Apply` does to a function whose
  name already contains the `__specialized__N` suffix — i.e., a function
  that *was* the output of a previous specialization round. The current
  pipeline appears to assume specialization is idempotent on its own
  output, but the empirical evidence here contradicts that assumption.
- Inspect specifically how recursive calls inside an
  already-specialized function get rewritten when the surrounding
  specialization run also rewrites the call's caller — the wrong
  argument in slot 6 of the recursive call has the same *position* as a
  value that the caller passes through, suggesting an off-by-one in
  argument-list reconstruction (e.g., dropping the new `s1` because it
  is matched by the original `s0` parameter name and the previous
  specialization renamed parameters).
- Add validation downstream of each specialization pass (analogous to
  the recently introduced `LambdaLiftingValidator`) that asserts every
  recursive call site's argument list matches the callee's parameter
  arity and shapes — this would have caught the bug at compile time
  rather than at PineVM-evaluation time.

## Fix

The bug was in `TrySpecializeRecursiveCallWithCombinedSingleChoiceTagAndHigherOrder`
in `implement/Pine.Core/Elm/ElmCompilerInDotnet/Inlining.cs`, around line 3466.

**Root cause:** The function combines two specialization transforms:
1. `RewriteRecursiveCallsForSingleChoiceTagSpecialization` — rewrites recursive
   calls from the original function name to the specialized name, and restructures
   arguments for single-choice-tag unwrapping.
2. `RewriteGroupCallsInExpression` — strips invariant higher-order arguments from
   recursive calls.

The bug was that step (2) was searching for calls to the **original** function
name (`funcImpl.Name.Value`), but step (1) had already renamed those calls to
use `specializedName`. As a result, step (2) found no matching calls and did
NOT strip the higher-order arguments. The recursive calls retained their
original 7-argument form even though the specialized function only had 6
parameters, causing argument misalignment (a function closure landed in the
parser-state slot).

**Fix:** Changed the mapping in `RewriteGroupCallsInExpression` from:
```csharp
[(funcImpl.Name.Value, funcInfo.ModuleName, specializedModuleName, specializedName)]
```
to:
```csharp
[(specializedName, specializedModuleName, specializedModuleName, specializedName)]
```

This makes step (2) correctly target the calls that step (1) has already
rewritten, ensuring higher-order arguments are stripped as intended.

## Why the syntax interpreter is the right oracle here

The PineVM is dynamically typed at the value layer (lists and blobs);
the Elm syntax interpreter retains the surface-language pattern-match
discipline. When an optimizer rewrites a recursive call to pass a value
of the wrong Elm type, the PineVM sees only a list and silently keeps
projecting; the syntax interpreter raises a structured error with the
Elm-level call stack the moment a `case` would have to bind that
ill-typed value to a constructor pattern. The two oracles disagree on
the *visible symptom* (cycle vs. type error) but agree on the underlying
defect, and the interpreter's error report contains everything a
maintainer needs to localise the rewrite mistake — without any extra
instrumentation.

The bisection test class therefore serves two roles: (a) regression
coverage that the bug is fixed, and (b) a worked example of using the
syntax interpreter as a rich-error oracle on intermediate pipeline
snapshots, applicable any time a future PineVM evaluation surfaces an
opaque hash-only error.
