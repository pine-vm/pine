# Tail-call lowering and loop-carried locals implementation review

## Scope

This review records the implementation of the improvements proposed in
`2026-07-17-tail-call-lowering-and-loop-carried-locals.md`.

## Old and new implementation

### Tail-call identity and occurrence

The old compiler first enumerated tail-call expression values and stored them in
an expression-keyed dictionary. Compilation later queried that dictionary while
emitting any structurally equal `Eval` node. Expression equality was therefore
used for two unrelated purposes: identifying a function and classifying a call
occurrence as tail-position control flow.

The new compiler keeps one `TailLoopTarget` for the frame. It contains the
stable set of root expression forms, the optional environment class, and values
used by a dynamic guard. Each `Eval` occurrence is classified when it is
lowered. It can become a backedge only when the lowering context says that the
specific occurrence is in tail position and its encoded callee identifies the
root function. Structurally equal non-tail occurrences remain ordinary
invocations.

This removes `TailCallElimination`, `JumpToLoop`, and the separate
`EnumerateTailCalls` pass. Function identity is no longer represented by the
same key used for expression CSE.

### Optimization policy

The old feature flag did more than enable loop lowering. It also disabled the
first static-inlining pass and root environment specialization. As a result,
enabling tail recursion changed broad optimization policy and increased the
number of distinct entered frame expressions.

The new pipeline controls inlining, reduction, specialization, and tail-loop
lowering independently. Original and rewritten root forms are retained for
callee identity, so the normal optimization pipeline can run before
control-flow lowering. The language-service workload returns to its original
compiled-expression counts instead of retaining extra frame boundaries merely
to simplify tail-call recognition.

### Explicit control flow

The old `NodeCompilationResult` was only a linear instruction accumulator.
Branch targets, continuations, and stack state existed implicitly in offsets
and recursive emitter conventions.

The new final lowering stage builds `PineControlFlowGraph`. The graph contains
named basic blocks, virtual value IDs, block parameters, ordinary operations,
and explicit return, jump, conditional-jump, invoke, and tail-invoke
terminators. Validation checks:

- the entry and every edge target exist;
- edge argument arity matches the target block parameters;
- operations use virtual values only after definition;
- stack effects agree at every block entry;
- loop backedges carry an empty evaluation stack.

Only after validation are block IDs translated back to physical instruction
offsets. The existing stack-instruction VM remains the execution target, which
keeps this change compatible with existing precompiled and diagnostic tooling.

### Loop-carried inputs and locals

The old loop convention depended on replacing a whole environment value in a
special local. That representation made each iteration rebuild list-shaped
state and made safe simultaneous updates difficult.

The new frame interface exposes referenced environment paths as parameter
locals. A self-tail edge computes every next parameter before writing any
parameter local, then commits the values in reverse stack order. This is a
parallel-copy lowering: swaps and values derived from other current inputs
cannot observe partially updated state. Environment reconstruction is reserved
for boundaries that need a complete environment.

Virtual values in the CFG are separate from physical VM local indexes.
Expression-local reuse on a backedge is conservatively restricted to
environment-independent expressions. Loop-dependent CSE values are recomputed,
preventing a value from a previous iteration from being reused without an
invariance proof.

## Robustness and scalability

Occurrence-local tail classification closes the reported correctness hole and
also scales to shared, interned, reconstructed, or structurally equal expression
nodes. Adding another expression rewrite no longer requires preserving object
identity or disabling unrelated optimizations.

Explicit terminators make continuation requirements visible. A non-tail invoke
has a continuation block; a loop jump has none. This structure prevents future
features from silently inserting a backedge into an expression whose result is
still consumed.

Block parameters and virtual values provide typed roles even though runtime
Pine values remain dynamically shaped. Block IDs, virtual value IDs, expression
identity, and physical local indexes are distinct types rather than
interchangeable integers or expressions. New terminator variants must also be
handled explicitly by validation and lowering.

The CFG is an extension point for future customizations such as additional
terminators, alternative block scheduling, liveness-based local reuse, richer
value metadata, or another backend. Those changes can be made before physical
stack layout without duplicating tail-position rules throughout individual
expression emitters.

## Surprises

The repository already contained part of the proposed migration: tail-position
context propagation, split frame parameters, reverse-order loop-local writes,
stack-depth verification, and focused swap/derived-value tests. The remaining
expression-keyed discovery still obscured the distinction between stable
function identity and a call occurrence, so removing it was necessary rather
than merely adding another equality guard.

Re-enabling static inlining immediately restored the three language-service
compiled-expression counts to 193, 226, and 227. Other performance counters
changed substantially because the earlier snapshots measured the accidentally
disabled optimization pipeline. This required treating compiled-expression
count and runtime instruction/invocation counts as separate measurements rather
than assuming one implied the other.

The final aggregate frame-invocation totals for the same three scenarios are
1,278, 3,422, and 3,884, down from 4,398, 11,457, and 12,768. Parser-focused
performance snapshots were updated as well; these changes record the measured
effect of the restored optimization pipeline rather than changing correctness
expectations.

The initial CFG construction treated every block as reachable. Unreachable
instructions after an unconditional transfer can still be present in a valid
linear frame, and assigning those blocks an entry stack depth of zero caused a
false underflow. The implementation was adjusted to compute the minimum input
depth needed to model unreachable blocks while retaining strict propagation for
reachable edges.

## Backtracking and implementation learnings

An initial direction was to keep the tail-call dictionary and rely on the
already-added `IsTailPosition` check. That fixes the known reproduction, but it
leaves expression equality as hidden control-flow metadata and makes future
rewrites risky. The implementation instead removed discovery and classifies
the current occurrence against a separate root identity.

The first CFG draft represented `Parse_And_Eval_Binary` as an ordinary
operation. It was changed to an invoke terminator with an explicit continuation
so calls cannot hide a cross-frame control-flow edge inside a block.

The first virtual-value pass attached outgoing arguments before applying a
terminator's stack effect. Conditional edges then appeared to carry the
condition value and invoke continuations omitted their result. Applying the
terminator effect before forming edge arguments made the graph model the actual
edge state.

Finally, carrying all CSE locals across a loop looked attractive but was not
sound: an environment-dependent expression can denote a different value on the
next iteration. The implementation deliberately chose conservative
recomputation. A future liveness and invariance pass can recover reuse when it
can prove safety.
