# Removing infinite-recursion detection from the intermediate Pine VM

## Decision

The intermediate VM uses configured quotas and cooperative cancellation as its
termination policy. It no longer attempts to classify executions as infinite
recursion or infinite loops.

A complete detector for arbitrary Pine programs is impossible without imposing
a finite state space: a recursive computation can continue producing distinct
states forever. The removed detector recognized only bounded periodic patterns,
while adding bookkeeping, allocation, value materialization, and periodic
latency to normal evaluation.

## Runtime safeguards

`PineVM.EvaluationConfig` supplies nullable limits for invocation count,
backward-jump count, and live stack depth. A non-null limit bounds the
corresponding kind of VM work; `null` leaves that dimension unbounded. The
ordinary evaluation API uses finite defaults of 10,000,000 invocations,
10,000,000 loop iterations, and 100,000 live frames unless the VM is constructed
with a different policy.

`EvaluateExpressionOnCustomStack` also accepts a `CancellationToken`.
Cancellation is checked before evaluation starts, before invocations, and on
all jump instructions. This cadence lets a host respond to deadlines, user
actions, shutdown, or external resource monitoring without adding a check to
every VM instruction.

These safeguards are complementary:

- Quotas provide deterministic bounds for the operations they count.
- Cancellation lets policy remain outside the VM.
- Neither can interrupt a long-running native or precompiled operation that
  does not cooperate.
- Neither directly bounds allocation or the size of an individual value.

Quota exhaustion does not prove nontermination. Valid computations can exceed a
budget, and suitable limits vary with inputs, compiler optimizations, caching,
and precompiled implementations.

## Diagnostics

Both quota exhaustion and requested cancellation return `EvaluationError`.
The error reason is a structured variant rather than a pre-rendered message.
Quota errors identify the exhausted quota and configured limit.

The error also contains performance counters and up to 100 live stack-frame
snapshots, ordered innermost first. Each snapshot retains:

- the frame expression;
- its `StackFrameInput`, when available;
- the selected instructions, when available;
- the current instruction pointer.

This data lets consumers perform optional analysis, such as comparing repeated
expressions and inputs to investigate possible recursion. Capturing references
is cheap and does not force lazy frame inputs to materialize.

Human-readable output is available through
`EvaluationError.RenderDisplayString`. Expression encoding, hashing, and Pine
value descriptions happen only when a caller requests rendering, not while the
VM constructs the error.

## Consequences for integrators

Integrators must choose quotas appropriate for each workload and pass
cancellation from the surrounding request or task. They should report quota
exhaustion as budget exhaustion rather than as proof of infinite recursion.
When deeper diagnosis is needed, they can analyze the structured stack and
inputs or render the error for logs.

Running untrusted workloads may still require process isolation and
operating-system CPU and memory limits.
