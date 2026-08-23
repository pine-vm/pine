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
