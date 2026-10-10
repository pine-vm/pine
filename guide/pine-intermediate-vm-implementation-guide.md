# Pine Intermediate VM Implementation Guide

## Evaluation termination

The intermediate VM does not attempt to detect infinite recursion or other
infinite cycles. General nontermination cannot be detected completely, and
heuristic cycle detection adds work and allocations to the evaluation hot path.
Instead, callers control termination with explicit quotas and cooperative
cancellation.

### Quotas

`PineVM.EvaluationConfig` supports independent limits for:

- invocations, including eval and direct stack-frame invocations;
- loop iterations, counted when the VM takes a backward jump;
- live stack depth.

A `null` limit disables that quota. `EvaluationConfig.Default`, used by the
ordinary `EvaluateExpression` entry point unless the VM has a custom default,
allows 10,000,000 invocations, 10,000,000 loop iterations, and 100,000 live
stack frames. `EvaluationConfig.Unbounded` explicitly disables all limits.
Evaluation returns an
`EvaluationErrorReason.QuotaExhausted` after the corresponding counter or stack
depth exceeds the configured limit. Counter totals imported from direct
evaluation shortcuts are checked immediately after they are added.

Quota exhaustion means only that a configured budget was consumed. It is not
proof that the evaluated program would never terminate. Callers can inspect the
returned stack frames and inputs to look for repeated states when diagnosing
possible recursion.

### Cooperative cancellation

`EvaluateExpressionOnCustomStack` accepts a `CancellationToken` and returns an
`EvaluationErrorReason.CancellationRequested` when cancellation is observed.
The VM checks the token:

- before evaluation starts;
- before an invocation;
- whenever any jump instruction is executed.

Cancellation is cooperative. Native or precompiled work that does not observe
the token cannot be interrupted while it is running.

## Structured evaluation errors

`EvaluationError` contains a reason, performance counters, and a bounded stack
trace ordered from the innermost frame to the outermost. Each stack-trace entry
retains the expression, frame input, selected instructions, and instruction
pointer. These objects are retained by reference, avoiding expression hashing,
input materialization, or display formatting on the evaluation path.

Consumers that need human-readable output can call
`EvaluationError.RenderDisplayString`. Rendering performs potentially expensive
derivations such as expression encoding and hashing only on demand.

## Evaluation work counters

`PerformanceCountersFormatting.FormatCounts` and `FormatAllCounts` both include
all counters. `DirectInterpreter.Counters` records external entries separately from
recursive expression visits, with per-kind counters except for trivial labels.
Label visits remain included in the aggregate expression count.
`BuildListItemCount` counts immediate list slots, including literal prefixes and
direct evaluation, rather than sizes of referenced subtrees. It is rendered
immediately after `BuildListCount` in both text and JSON. Performance-counter snapshots use the test
helper `ShouldBeWithDiff` to display bounded, multi-line difference hunks.

VM reports, errors and live evaluation events expose `CountersByOrigin` /
`LoadCountersByOrigin`. The disjoint origins separate `VirtualMachine` work from
`ExpressionTemplatePlanParsing` and `DeferredTemplateValueMaterialization`. Frame baselines use the
same counter snapshot and subtraction operations so new fields propagate to
per-frame reports as well as evaluation-wide totals.

`EvaluationConfig.MaterializeResult` defaults to `false`, preserving lazy root
results. Consumers that will materialize the result immediately, such as Elm test
instrumentation, set it to `true` so the final snapshot includes that work.
Counters exclude compilation and diagnostic serialization; cache-dependent work
should be compared under a controlled cache policy.

## Compilation Units and Specializations

When compiling Pine expressions to sequential representations, the most common specialization targets a subset of `Environment` values. Under such constraints, expressions that depend on `Environment` can be reduced and simplified at compile time.

In common configurations for a minimal number of specializations, the VM uses separate constraints on `Environment` values only to fix the environment components which encode functions in a mutually recursive group to concrete values.

Perhaps the most important use case for constraints on `Environment` values is recognizing recursive and mutually recursive functions.

Recognizing recursive functions as such at compile time is important for thorough optimization. For this reason, the VM compiles each strongly connected group of expressions as a single unit.

> Note: SCC compilation unit not yet implemented in PineVM, remains TODO. Will probably not arrive before CFG implementation cleanup.

## Performance Optimizations

### Optimized Invocation Interfaces

In canonical representations of programs, we package all arguments into a single value and pass it to the `Eval` environment. Since `Environment` is the only kind of reference in the Pine language, the arguments packed in there must also include program code at least when using recursive functions.

An early implementation of the VM stayed close to the canonical representation for inputs to and outputs from `Eval`: One value in and one value out.

Most of the lists used to package eval inputs and outputs are short-lived because they are immediately deconstructed on the other side.

And since every list creation adds significant runtime overhead, avoiding them is an important lever for improving runtime efficiency.

> Note: specialized interface currently only implemented for the input side, output side remains TODO.

### Expression Templates and Deferred Values

An expression template constructs an encoded Pine expression from an environment.
A recognized chain of these templates eventually reaches a terminal expression.
Frontend compilers can emit such chains to implement incremental function
invocation, but the VM optimization operates on the encoded-expression structure.

An intermediate `Eval` yields an encoded-expression value containing earlier
environments as literals. These short-lived values often do not justify compilation,
so the VM can evaluate templates directly.

`ExpressionTemplatePlan` recognizes and validates the chain and describes how to
build the terminal environment. `DeferredTemplateValue` retains that plan and the
supplied environments without constructing the full intermediate Pine value.
Materialization reconstructs that value only when a structural operation requires it;
this is deferred value construction, not general-purpose deferred execution.

The counters are `ExpressionTemplatePlanParseCount`,
`DeferredTemplateValueAllocationCount`, `TemplateDirectInvocationCount`, and
`DeferredTemplateValueMaterializationCount`. The direct-invocation counter measures
terminal expression invocations that bypass intermediate encoded-expression
construction, not every direct invocation in the VM.


### Inlining Using the Control Flow Graph

As part of compiling expressions into sequential representations, we inline some invocations based on the control flow graph.

That inlining is not limited to a single level.
For example, Pine code compiled from the frontend code shown below can be inlined so far that the representation of `skipIdentifier` contains no invocations at all (the recursion in `skipToIdentifierEnd` is represented as a loop).

```Elm

skipIdentifier : String -> Int -> Int
skipIdentifier source offset =
    if isIdentifierStart (String.left 1 (String.dropLeft offset source)) then
        skipToIdentifierEnd source (offset + 1)

    else
        offset


skipToIdentifierEnd : String -> Int -> Int
skipToIdentifierEnd source offset =
    if isIdentifierChar (String.left 1 (String.dropLeft offset source)) then
        skipToIdentifierEnd source (offset + 1)

    else
        offset


isIdentifierStart : String -> Bool
isIdentifierStart character =
    case character of
        "_" ->
            True

        "a" ->
            True

        "b" ->
            True

        _ ->
            False


isIdentifierChar : String -> Bool
isIdentifierChar character =
    case character of
        "_" ->
            True

        "0" ->
            True

        "a" ->
            True

        "b" ->
            True

        _ ->
            False

```

While inlining decisions can be predicated on code size, the Pine expression node count is not part of such a gating.

Instead, size-based gates use counts closer to the number of instructions in the final representation, which can differ significantly.

For example, when compiling the Elm function `isIdentifierChar` from the example above, we emit a Pine condition node for each of the case arms.

However, since the patterns are constant values and contain only a literal expression, all these arms end up as a single map instruction in the sequential IR.

Adding more case arms increases the Pine expression size, but the sequential IR size remains constant.

But inlining does not stop there. For example, multiple instances of `skipIdentifier` can be inlined in another function, resulting in multiple local loops.
