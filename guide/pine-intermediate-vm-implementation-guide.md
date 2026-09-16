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

