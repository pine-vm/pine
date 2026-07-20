# Tail-call lowering and loop-carried locals

## Summary

`ElmParserFileTests.File_matches_language_service_scenario_ModuleA` fails when
tail-recursion optimization is enabled because the compiler records tail calls
by structural `Expression.ParseAndEval` equality rather than by their position
in the expression tree.

The same `ParseAndEval` expression occurs in both tail and non-tail positions
in one parser function. `EnumerateTailCalls` finds the tail occurrence and adds
the expression to `CompilationContext.TailCallElimination`. Later,
`CompileParseAndEval` looks up every structurally equal occurrence in that
dictionary. It therefore lowers a non-tail occurrence to a backward jump too.

The non-tail occurrence has two pending operands on the evaluation stack. The
generated guarded loop branch replaces local 0 and jumps to instruction 0
without consuming those operands. `StackFrameInstructions.ComputeMaxStackUsage`
correctly rejects the result:

```text
Inconsistent stack depth at 0 (0 vs 2)
```

Diagnostic output placed the malformed backedge at instruction 243. Immediately
before the guarded call, two arithmetic operands had already been pushed. The
generic branch evaluated the call and continued with arithmetic instructions;
the loop branch instead jumped to the frame entry with those two operands still
live. This confirms that the optimized occurrence was not in tail position.

This is not caused by environment specialization. The exception is raised while
constructing the generic `StackFrameInstructions` in
`ExpressionCompilation.CompileExpression`.

## Compiled-expression-count regression in `6aaa46e`

### What the counter measures

`CompiledExpressionCount` is the number of distinct frame expressions that were
actually entered, not the number of instruction variants generated for one
expression. The report's source says:

> “The number of entries is the number of distinct compiled expressions invoked
> across the evaluation task(s) that produced the report.”

([`InvocationCountReport.cs` at `6aaa46e`, lines 25–28](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/InvocationCountReport.cs#L25-L28))

The implementation confirms that definition:

> `public int CompiledExpressionCount =>`
> `    InvocationCountPerExpression.Count;`

([`InvocationCountReport.cs` at `6aaa46e`, lines 39–44](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/InvocationCountReport.cs#L39-L44))

Each frame expression is also the key of the VM compilation cache. A miss calls
the compiler and stores the result:

> `if (_expressionCompilationDict.TryGetValue(rootExpression, out var cachedCompilation))`
> `    return cachedCompilation;`
>
> `var compilation = ExpressionEntryLessCache(rootExpression);`
>
> `_expressionCompilationDict[rootExpression] = compilation;`

([`PineVM.cs` at `6aaa46e`, lines 259–269](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/PineVM.cs#L259-L269))

Therefore, more distinct entered frame expressions mean more cache misses and
more calls to `CompileExpression` in this workload. The call site is:

> `var compilation =`
> `    ExpressionCompilation.CompileExpression(`
> `        rootExpression,`
> `        ...`
> `        enableTailRecursionOptimization: _enableTailRecursionOptimization,`

([`PineVM.cs` at `6aaa46e`, lines 320–331](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/PineVM.cs#L320-L331))

### Size and shape of the regression

The commit changes the shared language-service VM from the one-argument helper
call to explicitly enabling tail recursion:

> `ElmCompilerTestHelper.PineVMForProfiling(`
> `    reportFunctionApplication: _ => { },`
> `    enableTailRecursionOptimization: true);`

([`ElmLanguageServiceTests.cs` at `6aaa46e`, lines 270–273](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs#L270-L273))

All three affected snapshots gain exactly 26 distinct compiled expressions:

| Scenario | Before | After | Increase |
| --- | ---: | ---: | ---: |
| references | 193 | 219 | 26 (13.5%) |
| challenging references | 226 | 252 | 26 (11.5%) |
| challenging rename | 227 | 253 | 26 (11.5%) |

The source evidence for the “before” values is:

> `CompiledExpressionCount: 193`
>
> `CompiledExpressionCount: 226`
>
> `CompiledExpressionCount: 227`

([`ElmLanguageServiceTests.cs` at parent `3bc61d1`, lines 522–530](https://github.com/Viir/super-duper-disco/blob/3bc61d1337922997f6e0f81c5549293afac89fd2/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs#L522-L530),
 [lines 860–868](https://github.com/Viir/super-duper-disco/blob/3bc61d1337922997f6e0f81c5549293afac89fd2/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs#L860-L868),
 [lines 991–999](https://github.com/Viir/super-duper-disco/blob/3bc61d1337922997f6e0f81c5549293afac89fd2/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs#L991-L999)).
The corresponding “after” source says:

> `CompiledExpressionCount: 219`
>
> `CompiledExpressionCount: 252`
>
> `CompiledExpressionCount: 253`

([`ElmLanguageServiceTests.cs` at `6aaa46e`, lines 524–532](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs#L524-L532),
 [lines 862–870](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs#L862-L870),
 [lines 993–1001](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs#L993-L1001)).

### Root cause

The immediate cause of the count increase is a bundled policy in
`InstructionsFromExpressionTransitive`: enabling tail-recursion optimization
also disables the entire first static-inlining and reduction pass:

> `var inlinedStaticInvocations =`
> `    disableReduction || enableTailRecursionOptimization`
> `    ?`
> `    rootExpression`
> `    :`
> `    InlineStaticInvocationsAndReduceRecursive(`

([`ExpressionCompilation.cs` at `6aaa46e`, lines 184–200](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/ExpressionCompilation.cs#L184-L200))

That first pass is specifically able to turn a statically encoded
`ParseAndEval` target into its parsed body. It rejects environment-dependent
targets, independently evaluates the encoded expression, parses the resulting
value, and only then applies recursion and skip-inlining guards:

> `if (parseAndEvalExpr.Encoded.ReferencesEnvironment)`
> `    return null;`
>
> `if (ReducePineExpression.TryEvaluateExpressionIndependent(`
> `    parseAndEvalExpr.Encoded, parseCache).IsOkOrNull() is not { } exprValue)`
> `    return null;`
>
> `if (parseCache.ParseExpression(exprValue).IsOkOrNull() is not { } parseOk)`
> `    return null;`

([`ExpressionCompilation.cs` at `6aaa46e`, lines 360–387](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/ExpressionCompilation.cs#L360-L387))

When that pass is skipped, calls it would have absorbed can remain
`ParseAndEval` frame boundaries. Entering those residual boundaries adds keys
to the histogram because the builder executes:

> `var expression = enteredStackFrame.FrameExpression;`
>
> `if (_counts.TryGetValue(expression, out var existing))`
> `    _counts[expression] = existing + 1;`
> `else`
> `    _counts[expression] = 1;`

([`InvocationCountReport.cs` at `6aaa46e`, lines 265–276](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/InvocationCountReport.cs#L265-L276))

A second reduce-and-inline pass still runs, but it is not a substitute for the
disabled pass. The pipeline invokes it separately:

> `var reducedExpression =`
> `    disableReduction`
> `    ? expressionWithEnvConstraint`
> `    : ReduceExpressionAndInlineRecursive(`

([`ExpressionCompilation.cs` at `6aaa46e`, lines 215–233](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/ExpressionCompilation.cs#L215-L233))

It has different admission rules, including a 500-subexpression limit on an
inlined body both before and after recursive processing:

> `if (500 < inlinedExprReduced.SubexpressionCount)`
> `    return null;`
>
> `...`
>
> `if (500 < inlinedFinal.SubexpressionCount)`
> `    return null;`

([`ExpressionCompilation.cs` at `6aaa46e`, lines 900–939](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/ExpressionCompilation.cs#L900-L939))

A manual controlled run of
`References_request_finds_usage_across_modules` kept
`enableTailRecursionOptimization: true` and changed only the first condition
from `disableReduction || enableTailRecursionOptimization` to
`disableReduction`. `CompiledExpressionCount` returned from 219 to 193. The
linked condition and the committed before/after snapshots above make this
single-variable check straightforward to repeat.
Leaving the first pass disabled while only raising the second pass's
500-subexpression limits did not restore the baseline (it produced 212), which
also rules out treating the two passes as interchangeable.

The feature flag therefore activates loop lowering and, at the same time,
suppresses a broader optimization that previously removed 26 dynamically
reached frame expressions. The measured count increase is attributable to the
suppressed pass; the experiment does not by itself prove that re-enabling the
pass preserves every loop-lowering opportunity. Commit `6aaa46e` exposes this
pre-existing coupling by enabling the flag in this test workload.

## Why a local patch is insufficient

The current API separates tail-position discovery from instruction emission:

- `EnumerateTailCalls` returns expression values, losing occurrence and parent
  context.
- `TailCallElimination` is keyed by structurally equal `ParseAndEval` values.
- `CompileParseAndEval` cannot tell whether the occurrence currently being
  emitted is in tail position.
- `NodeCompilationResult` is a linear instruction accumulator. It does not
  represent basic blocks, block inputs, stack state at edges, or loop-carried
  values.

Keying by reference identity would only hide this example. Expression nodes may
be shared, interned, reduced, or reconstructed, and one node instance can still
legitimately appear in multiple positions. Clearing the evaluation stack before
the jump would also be wrong: a non-tail call must return so the pending
computation can continue.

A narrow repair could thread an `isTailPosition` flag through every recursive
compiler method and require it in `CompileParseAndEval`. That would restore
correctness, but it would duplicate implicit control-flow rules throughout the
existing stack emitter and would not provide a good basis for split
loop-carried locals.

## Proposed compilation design

### 0. Preserve the existing optimization pipeline

Tail-call recognition must not globally disable static inlining, reduction, or
environment specialization. These transformations should remain controlled by
their own profitability and safety policies; enabling loop lowering should only
change the lowering of calls proven to be self calls in tail position.

This separation is needed because the current feature flag controls both the
first inliner and root environment substitution:

> `disableReduction || enableTailRecursionOptimization`
>
> `...`
>
> `envConstraintId is null || enableTailRecursionOptimization`

([`ExpressionCompilation.cs` at `6aaa46e`, lines 184–209](https://github.com/Viir/super-duper-disco/blob/6aaa46ebe57a4c9a3b6d67e0530ac241b09c9115/implement/Pine.Core/Interpreter/IntermediateVM/ExpressionCompilation.cs#L184-L209))

Run the established optimization pipeline before control-flow lowering, while
preserving enough origin metadata to identify the root function through
rewrites. In particular, represent function identity separately from structural
equality of the optimized expression. A tail-call candidate should carry a
stable callee identity plus its lowering occurrence; optimization may replace
the candidate, but must not force all other static calls to remain as frame
boundaries merely to make recognition easier.

This ordering is a design requirement, not yet a validated local patch. Before
adopting it, verify that enabling the first inliner together with loop lowering
preserves correctness, still emits the intended loops, and retains the
invocation and instruction-count improvements.

### 1. Preserve occurrence context while lowering

Lower Pine expressions into a small control-flow IR before producing stack
instructions. Tail position should be a property of the lowering context, not a
property stored against an expression value.

The context starts in tail position at the function root and propagates as
follows:

- Both result branches of a tail-position conditional are tail positions.
- A conditional condition is never a tail position.
- List items, kernel inputs, tags, and `ParseAndEval` operands are not tail
  positions.
- A child whose result is consumed by any later operation is not a tail
  position.

Only a `ParseAndEval` lowered in tail context may become a loop backedge.
Structural expression equality can still be used for CSE, but never for
classifying control-flow occurrences.

### 2. Introduce explicit basic blocks and terminators

Use a sequential IR composed of basic blocks. Each block contains ordinary
operations and ends in exactly one terminator:

- return a value;
- jump to a block with arguments;
- conditional jump to two blocks with arguments;
- invoke and continue in a return block;
- tail-invoke another function/frame.

The function entry is also the loop header for self-tail calls. A self-tail call
becomes a jump to that header only after the target expression has been proven
to identify the same function. A guarded dynamic call branches to either:

- the loop edge, when the encoded target matches; or
- a normal invoke block, when it does not.

This representation makes it impossible to place a loop jump in the middle of
an expression that still needs the call result: such an occurrence necessarily
has a continuation block and is therefore not tail.

### 3. Model loop-carried values as block parameters

Give the entry/loop-header block one parameter per logical function input.
Initially, the external function interface can unpack its single Pine
environment list into those parameters. A self-tail edge computes the next
values and passes them directly to the header.

This directly supports the intended optimization: subsequent iterations keep
inputs spread over locals rather than rebuilding the list-shaped function
environment. The list representation is then needed only at boundaries that
actually require it, such as:

- entry from the generic function-call interface;
- a dynamic call that cannot use a known static interface;
- an operation that observes the complete environment value.

Environment paths should map to typed IR values or local IDs in the header,
instead of relying on the magic convention that local 0 contains the whole
environment.

### 4. Separate values from physical locals

Use virtual value IDs during control-flow lowering. Run liveness analysis and
assign physical VM locals only after the CFG is complete.

For a loop edge, compute all next-iteration values before overwriting current
locals. Lower the edge arguments to a parallel copy. Resolve copy cycles with a
temporary local when necessary. This avoids clobbering when, for example, the
next values swap two current inputs.

Values invariant across iterations can remain in dedicated locals and need not
be copied on the backedge. Values derived from loop parameters must either be
recomputed or explicitly carried; CSE must not reuse a value from a previous
iteration without proving invariance.

### 5. Lower the CFG to stack instructions last

Schedule each basic block independently with an empty evaluation stack at block
entry and exit, except for an explicit return value. Materialize cross-block
values in locals. Then resolve labels to instruction offsets.

This gives the stack verifier a simple invariant: every edge to a block has the
same stack depth, normally zero. Jump offsets and maximum stack use become
properties of the final lowering step rather than concerns mixed into recursive
expression compilation.

## Suggested migration

1. Add a control-flow IR with virtual values, block parameters, labels, and
   explicit terminators.
2. Decouple static inlining, reduction, and environment specialization from the
   tail-loop feature flag; preserve stable function identity through those
   transformations.
3. Validate the decoupled pipeline against optimized-versus-unoptimized
   evaluation, loop counts, invocation counts, instruction counts, and compiled
   expression counts.
4. Lower conditionals and calls into the control-flow IR while propagating tail
   context and origin identity.
5. Convert self-tail calls into guarded or direct jumps to the entry block.
6. Add validation before stack lowering:
   - every block is terminated;
   - edge argument counts match block parameters;
   - only tail occurrences produce tail terminators;
   - virtual values dominate their uses.
7. Allocate locals and lower blocks to the existing `StackInstruction` format.
8. First preserve the current one-local environment representation, then split
   the loop-header parameter into individual input locals.
9. Remove `CompilationContext.TailCallElimination`,
   `JumpToLoop.EnvironmentLocalIndex`, and expression-keyed tail-call discovery
   after the new path covers the existing cases.

## Regression coverage

Add a focused compiler test where one structurally equal `ParseAndEval` appears
once as a function result and once as an operand whose result is consumed. The
test should verify that:

- only the tail occurrence becomes a backedge;
- the non-tail occurrence remains a normal invocation;
- every backedge reaches its target with stack depth zero;
- evaluation with optimization enabled equals evaluation with it disabled.

Keep `File_matches_language_service_scenario_ModuleA` as the end-to-end
regression. Add loop tests with multiple carried values, including swapping
values and deriving one next value from another current value, before enabling
the split-local representation.

Also retain paired performance coverage with tail-loop lowering disabled and
enabled for the same language-service workloads. Enabling it must not increase
the set of distinct entered frame expressions unless a separately reviewed
inlining-policy change intentionally accepts that trade-off. At minimum, assert
that the three `CompiledExpressionCount` values do not regress from 193, 226,
and 227 respectively; those baselines are quoted and linked in “Size and shape
of the regression” above.
