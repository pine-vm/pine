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

`EvaluationError` contains a reason, evaluation work counters, and a bounded stack
trace ordered from the innermost frame to the outermost. Each stack-trace entry
retains the expression, frame input, selected instructions, and instruction
pointer. These objects are retained by reference, avoiding expression hashing,
input materialization, or display formatting on the evaluation path.

Consumers that need human-readable output can call
`EvaluationError.RenderDisplayString`. Rendering performs potentially expensive
derivations such as expression encoding and hashing only on demand.

## Evaluation work counters

`EvalWorkCounters` contains execution work, excluding cached metadata derivations.
`EvalWorkCountersFormatting.FormatCounts` and `FormatAllCounts` both include
all evaluation counters. `DirectInterpreter.Counters` records external entries separately from
recursive expression visits, with per-kind counters except for trivial labels.
Label visits remain included in the aggregate expression count.
`BuildListItemCount` counts immediate list slots, including literal prefixes and
direct evaluation, rather than sizes of referenced subtrees. It is rendered
immediately after `BuildListCount` in both text and JSON. Evaluation-work-counter snapshots use the test
helper `ShouldBeWithDiff` to display bounded, multi-line difference hunks.

VM reports, errors and live evaluation events expose `CountersByOrigin` /
`LoadCountersByOrigin`. `EvalWorkCountersByOrigin.Total` sums only `VirtualMachine`
and `DeferredTemplateValueMaterialization`. Allocating and reconstructing deferred
runtime values is evaluation work, not shared metadata derivation work.
The snapshot's separate `CachedWorkCounters` contains template recognition attempts
(`ExpressionTemplatePlanParseCount`) and their direct-interpreter work
(`ExpressionTemplatePlanParsing`). Reports and errors also expose it directly.
Frame baselines subtract both categories independently.

`CachedWorkCounters` is a reusable value record, independent of the collector:
an evaluation or a cache container can populate and aggregate snapshots.
Only template recognition is currently instrumented here; expression parsing,
encoding, reduction and sequential-IR compilation are not yet counted.
Cached derivation work depends on cache state and is **not deterministic per VM
or request** when workers share caches: attribution depends on who wins a race,
and concurrent cache factories may perform redundant work. Do not equate attempts
with distinct cache entries. `CachedWorkCountersFormatting` renders this work
separately; phase reports and JSON preserve both categories without adding cached
work to evaluation totals or invocation/loop budgets.

`EvaluationConfig.MaterializeResult` defaults to `false`, preserving lazy root
results. Consumers that will materialize the result immediately, such as Elm test
instrumentation, set it to `true` so the final snapshot includes that work.
Evaluation counters exclude compilation and diagnostic serialization. Even execution
work comparisons require a controlled invocation-cache policy and configuration.

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

### Expression Templates and Deferred Evaluation Chains

An expression template constructs an encoded Pine expression from an environment.

The optimization described in this section is motivated by the frequented occurrence of these forms, driven by frontend compilers often using it to represent curried functions and partial application.

Frontend compilers use such templates to represent curried functions that escape as values:
incremental application of a function of unknown origin is represented by nesting each
argument in another `Eval` expression. Intermediate results contain earlier environments
as literals in the encoded expression for the next application.

The optimization replaces repeated construction, parsing, and evaluation of these
intermediate encoded expressions with a shared structural plan and collected environments.
It defers **value construction**, not execution of arbitrary computations. Upon reaching
the terminal expression, the VM forwards its arguments directly where possible, rather
than constructing and then deconstructing its canonical environment.

#### Recognition and semantic contract

Initially, intermediate template expressions support only `List`, `Litral`, and
`Environment`. `Environment` may occur in any list item, including multiple times per
level. Recognition uses the current compact 2026 expression encoding only; legacy 2024
constructors use ordinary evaluation instead. The terminal expression may contain arbitrary Pine computations.

Constructor-only syntax is necessary for this initial scope, but is not sufficient to
prove a recognized chain. Recognition tracks each stage's environment symbolically and
establishes a finite sequence of stages, invariant terminal code, and recipes for the
values constructed from collected environments.

For example, in schematic expression notation,
`List([Litral("Litral"), Environment])` constructs an encoded literal whose payload
depends on the supplied environment. A recognizer cannot cache one fixed terminal AST
for all applications of this template, so this case is outside the initial fixed-terminal
scope.
Putting `Environment` in an encoding tag or another code-structural position can make
the expression kind or validity dynamic. Recognition must prove that environment
references preserve the chain's code structure and can be represented by recipes
referencing the collected environments. It falls back at the first stage where this
cannot be established.

For a recognized chain requiring `n` further applications, let `E_1, ..., E_n` be the
successive supplied environments, `B` the invariant terminal code, `R_k` the exact
intermediate-value recipe after `k` applications, and `G` the terminal-environment recipe.
Constants already present in the entry value are part of these recipes. The contract is:

```text
Before saturation (0 <= k < n):
    ordinary application result = instantiate(R_k, E_1, ..., E_k)

At saturation:
    ordinary application result = evaluate(B, instantiate(G, E_1, ..., E_n))

Optimized terminal invocation:
    supply projections of G directly, without constructing G
```

Recipes describe value construction and references to collected environments, not suspended arbitrary
expressions. An underapplication returns the corresponding deferred intermediate value.
With overapplication, the VM executes `B` and then applies remaining environments to
its result. "Terminal" is relative to this plan, not necessarily to the enclosing
application chain.

Failure to recognize a chain is not a program error. In particular, recognition must
not report an error merely because a future stage would construct malformed code:
constructing that value can succeed, and the parse error belongs to the later `Eval`
that attempts to execute it. Recognition of unfamiliar values has a bounded-work policy
and falls back to ordinary evaluation when its budget is exhausted.

#### Shared plans and immutable intermediate values

The shared structural plan contains terminal code, stage transitions, and recipes for
intermediate values and the terminal environment, including constants already present
in the entry value. A shared-plan dictionary is keyed by that concrete entry value;
factoring different entry values into a common structural identity is not required.

Each intermediate `PineValueInProcess` holds a plan reference and the environments
supplied by successive `Eval`s. These collected environments are not part of the
dictionary key or cached plan, so varying them does not create new dictionary entries.
Continuing a recognized chain reuses the plan reference without materializing the
intermediate value for another cache lookup. The applicable intermediate-value recipe
follows from the chain description and the number of supplied environments.

Every intermediate value is represented as an immutable `PineValueInProcess`, just as
other in-process values. Allocating a small instance for each encountered `Eval` is
acceptable; avoiding these allocations is not a primary goal. Applying a partial value
produces a new value without mutating the original, so the same partial application can
be independently reused with different arguments. Internal memoization must preserve
these immutable value semantics.

When compilation establishes an intermediate template expression's shape, its instructions can
create this representation directly. They need not first construct a concrete encoded
Pine value and then rediscover its structure. Runtime recognition and caching remain
available for externally supplied or otherwise unknown values; this optimization does
not require a new frontend encoding or a new observable Pine value kind.

Recipes share common structure rather than expanding an independent tree for every
nesting level. Intermediate recipes and their derived metadata can be prepared on
demand. Extending a chain must not repeatedly copy the complete prefix of collected
environments. Multiple references to one environment share the value and reusable
derivations rather than duplicating the value. The storage representation is not
prescribed, but chain bookkeeping should be linear in the number of applications,
excluding recognition and work required by consumers.

#### Direct terminal argument and guard projection

For the parameter layout inferred for the terminal expression's unconstrained
compilation, derive an argument recipe for each parameter path `p`:

```text
ArgumentRecipe[p] = project(G, p)
```

Simplify these projections before invocation. Prefer a constant, a collected environment,
or a path within that environment over building enclosing lists. Construct a list only when
the requested argument actually includes that list. Projection must preserve Pine's
path semantics, including missing fields and list/blob distinctions.

For example, if `G = [constantFunctions, E_1, [E_2, E_1]]`, a parameter at `[2, 1]`
receives `E_1` directly: neither the inner pair nor the enclosing environment is built.
This also allows unrelated environment components to remain unmaterialized.

Here, "unconstrained interface" means the inferred `StaticFunctionInterface` of the
generic compilation, not `StaticFunctionInterface.Generic`, which forwards the whole
environment as one parameter. If terminal code genuinely consumes the whole
environment, its recipe remains available as an in-process value.

Specialization selection also uses projections of `G`. Guard paths may descend below
a forwarded parameter or inspect components not independently exposed as parameters.
They must not require canonical environment reconstruction just to select instructions.
Keep structural recipes separate from compilation-specific entry-point adapters, so
recompilation and future parameter layouts do not constrain the value representation.
Native or other consumers that require a concrete environment can use the same `G`
recipe as a fallback.

After projection, an invocation should not retain the entire chain or unnecessary
environment roots merely through its adapter or backing storage. Retain only data
needed by the selected invocation and its resulting values.

#### Demand-driven observation and materialization

Using an intermediate value outside another `Eval` is not inherently a reason to fully
materialize it. Returning it, storing it in a list, or forwarding it to another function
can preserve the same deferred representation. Representation-aware consumers resolve
only what they demand: known kind or length from recipe metadata, an individual field
from its projection, or relevant components for an equality comparison.

Full materialization is a fallback when a consumer requires a concrete Pine value and
has no representation-aware implementation. Reaching `B` is an execution boundary,
not a requirement to materialize intermediate encoded expressions or `G`. Materializing
an intermediate value need not discard its plan and collected environments if later invocations can
still reuse them; their retention must be weighed against memory lifetime.

Materialization reconstructs the exact Pine value, not merely an equivalent executable
expression. This matters when different accepted expression encodings decode to the
same AST. Equality and hashing must agree with concrete value semantics; shortcuts
based on plans and collected environments are valid only when they prove that agreement.

#### Evaluation order, limits, and accounting

Bypassed logical `Eval` invocations are not counted in `InvocationCount`. We introduce a new work counter specifically for the `Eval` operations saved by this consolidation mechanism, and another new work counter for non-terminal materialization of intermediate values.

`TemplateConsolidatedEvalCount` includes bypassed nonterminal applications and any elided
terminal `Eval` wrapper. An invariant constructor terminal has no such wrapper.
Materialization performed during an evaluation is charged to that evaluation, even when
the intermediate value originated in an earlier evaluation.

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
