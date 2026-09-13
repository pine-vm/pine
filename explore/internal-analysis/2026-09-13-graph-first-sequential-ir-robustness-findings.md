# Research findings: robustness risks in the current sequential-IR compiler

These findings come from static inspection of the current checkout. No reproduction tests were executed and no implementation was changed. Distinguish concrete code-level gaps from future-design limitations; do not assume every malformed-graph example is reachable through today's expression compiler.

The companion [refactoring plan](/home/runner/work/super-duper-disco/super-duper-disco/explore/internal-analysis/2026-09-13-graph-first-sequential-ir-refactoring-plan.md) assigns the work to separately testable increments.

## R1. Graph validation does not validate the graph's full dataflow

**Status:** confirmed validation gap; especially dangerous once graphs are edited directly.

**Evidence:** [PineControlFlowGraph.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineControlFlowGraph.cs), lines 267–319, checks operation inputs/results and edge target/argument counts. It does not check that edge arguments are in scope. Parameter duplicates are collapsed by `ToHashSet`; uniqueness is not checked across blocks. Terminators do not explicitly expose return operands, conditions, call arguments or returned-result definitions. Lines 1029–1070 invent virtual results for invocation stack effects without recording a corresponding result definition in the model.

**Consequence:** an edge can carry an undefined ID of the right arity and pass validation. Operation metadata can disagree with the embedded instruction. A later rewrite cannot safely reason about invocation results or return uses from this representation.

**Robust refactoring:** explicit semantic operands/results and invoke-return bindings, function-wide definition uniqueness, and validation of every use. Use structured diagnostics rather than exceptions as an optional-optimization decision mechanism.

**Regression coverage:** undefined edge argument; duplicate block parameter; duplicate result ID across blocks; wrong operation arity; return/condition using an undefined value; invoke result referenced before return or on an unrelated edge.

**Plan increments:** 2–3.

## R2. Lowering does not implement virtual-value transfers and assumes hidden layout constraints

**Status:** confirmed model/lowering mismatch; unsafe for general graph transformations.

**Evidence:** [PineControlFlowGraph.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineControlFlowGraph.cs), lines 642–731, emits each stored operation instruction without consulting `Inputs`, `Results`, or edge arguments. Conditional lowering encodes only `Branch`; switch lowering encodes case destinations but not the explicit `FallThrough` destination. Only implicit jumps and invokes get adjacency checks, using numeric IDs rather than actual array positions. Lines 465–522 impose fall-through ordering restrictions inside one optimization.

**Consequence:** exchanging two same-arity edge arguments changes the graph's stated dataflow but emits identical bytecode. Reordering otherwise valid blocks can make a conditional or switch execute the physically next block instead of its named default successor. A graph with a non-first `Entry` can pass the current entry-range check while emission still starts with the first array element.

These are not claims that the current reconstruction path normally produces such graphs; they show that its public model is not a reliable transformation boundary.

**Robust refactoring:** actual value-to-storage lowering, parallel edge copies, explicit return stubs and layout planning after semantic transformations. Schedule entry deliberately; use explicit branch/default edges and insert jumps where needed.

**Regression coverage:** swapped arguments with observable results; shared switch target with different arguments; permuted block order; sparse IDs; entry not first in storage; nonadjacent default/continuation; critical edges.

**Plan increments:** 4–5.

## R3. Loop validity is tied to numeric block order and an empty evaluation stack

**Status:** confirmed design limitation blocking the requested inlining architecture.

**Evidence:** [PineControlFlowGraph.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineControlFlowGraph.cs), lines 313–317, treats every target ID less than or equal to the source ID as a loop edge and rejects nonempty arguments. [PineIRCompiler.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineIRCompiler.cs), lines 65–82 and 121–124, has one root `TailLoopTarget`; lines 999–1014 and 1089–1096 calculate jumps to the frame start; lines 1110–1147 update fixed parameter locals.

**Consequence:** a simple renumbering can change graph validity. An acyclic edge to a smaller ID is treated like a loop, while loop-carried data is hidden in local writes. A separately compiled looping callee cannot simply be inserted into a caller without relocating its root-target and local assumptions.

**Robust refactoring:** arbitrary stable IDs, explicit loop-header parameters, derived SCC/dominance/natural-loop analyses, and alpha-renaming of all cloned definitions. Empty stack at transfers may remain a backend convention, never a graph-data restriction.

**Regression coverage:** renumbering invariance; value-carrying backedges; multiple latches/exits; independent loops; nested loops; looping callee cloned twice and called inside a loop.

**Plan increments:** 2–3, 8, 11–12.

## R4. Stack-resource checks conflate net stack change with minimum required inputs

**Status:** confirmed checks are incomplete; malformed instruction acceptance should be captured in focused tests.

**Evidence:**

- [StackFrameInstructions.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/StackFrameInstructions.cs), lines 93–145, checks only whether `inDepth - PopCount + PushCount` is negative. It does not first check `inDepth >= PopCount`.
- [StackInstruction.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/StackInstruction.cs), lines 1791–1800 and 1827–1830, describes operations that pop inputs and push a result. For example, a binary operation with one available value has nonnegative net output depth but insufficient operands.
- The same resource analysis writes `stackDepthIn[0]` even for empty input, ignores successors beyond the end, and does not guard negative successors before indexing.
- [PineControlFlowGraph.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineControlFlowGraph.cs), lines 882–935, computes unreachable-block minimum depth from net deltas, which has the same issue. Its reachable `ApplyStackEffect`, lines 975–988, does correctly check pop count; this is not a claim that both validators are identical.
- `Local_Set_Descending` reads existing stack values but has zero pop/push counts: [StackInstruction.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/StackInstruction.cs), lines 605–609 and 1369–1383. Stack-read requirements cannot be reconstructed from pop count alone.

**Consequence:** metadata validation can accept insufficient operands, report incidental indexing exceptions, or ignore an invalid exit. Duplicated analyses can disagree, making future backend changes harder to trust.

**Robust refactoring:** distinguish minimum read depth, consumed values, produced values and local reads/writes. Validate input shape and every target. Calculate compiler frame metadata from selected graph blocks, not a second instruction-to-control-flow recovery pass.

**Regression coverage:** empty frame; binary op with only one operand; non-consuming local store on an empty stack; out-of-range targets in both directions; missing return; unreachable block requiring operands; valid loops with exact stack bounds.

**Plan increments:** 4–5, 14.

## R5. Read-only interfaces conceal mutable compiler artifacts and linkage

**Status:** confirmed mutability boundaries, incompatible with the requested pure design.

**Evidence:**

- [StaticFunctionInterface.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/CodeAnalysis/StaticFunctionInterface.cs), lines 15–38 and 47–61, stores nested read-only interfaces and precomputes a hash. Sorting copies the outer sequence, not necessarily the inner path objects.
- [StackFrameInstructions.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/StackFrameInstructions.cs), lines 14–29, accepts an `IReadOnlyList` of instructions and computes cached bounds. A retained mutable backing list can change instructions without recomputing those bounds.
- [StackInstruction.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/StackInstruction.cs), lines 492–510 and 957–974, links targets through a mutable field inside `DirectInvocation`.
- [PineControlFlowGraph.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineControlFlowGraph.cs), lines 1029–1033 and 1113–1127, passes mutable lists and `ref` state between member functions.
- [PineVMParseCache.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/CodeAnalysis/PineVMParseCache.cs), lines 9–14 and 32–41, mutates an internal concurrent dictionary through parsing; [ExpressionCompilation.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/ExpressionCompilation.cs), lines 177–188, accepts a mutable reduction cache.

**Consequence:** read-only-looking values can change after validation/publication, cached metadata can become stale, and graph cloning can retain shared mutable linkage. These APIs also prevent a direct Elm translation.

**Robust refactoring:** owned deeply immutable collections, explicit memo-state returns, local-only builder mutation, and immutable function-ID resolution tables. Do not embed `StackInstruction` or mutable service handles in semantic models.

**Regression coverage:** mutate original caller-owned paths/instruction lists after construction and confirm published objects are unchanged; stable equality/hash/resource data; linking produces a new artifact without modifying the old one.

**Plan increments:** 2, 6, 13.

## R6. Guarded tail-loop compilation changes operand order and leaks tail position into an operand

**Status:** concrete source-level hazard; requires a targeted executable regression before changing behavior.

**Evidence:** normal evaluation compiles the environment before the encoded expression and clears tail position through `ContinueWithExpression`: [PineIRCompiler.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineIRCompiler.cs), lines 28–38 and 1020–1048. The [DirectInterpreter.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/DirectInterpreter.cs) reference likewise evaluates environment first, lines 120–132.

In contrast, `CompileGuardedJumpToLoop`, [PineIRCompiler.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineIRCompiler.cs), lines 1063–1077:

- Evaluates the encoded expression before the environment.
- Compiles the generic fallback's environment with `context.AddInstructionOffset(...)`, preserving the caller's true `IsTailPosition`.

**Consequence:** reordering can change which failure is observed or whether evaluation terminates. If the environment itself is an eligible tail `Eval`, the inherited tail context can compile it as a jump to the root, bypassing the outer invocation that should consume its result.

**Robust refactoring:** make operand sequencing explicit in graph construction. All operands use non-tail continuations; only the final invocation can transfer to a loop header. Any optimization that skips environment components needs a totality/equivalence proof.

**Regression coverage:** an unresolved outer encoded target and environment that itself invokes the root and terminates for a smaller input, with the outer target producing a distinguishable result; compare tail optimization on/off. Also use two differently failing operands to check order, and bounded VM tests for divergence-sensitive cases.

**Plan increments:** characterize in 1, resolve in 7–8.

## R7. Loop accounting and callbacks depend on physical jump direction

**Status:** confirmed runtime/layout coupling; a prerequisite for safe arbitrary layout and loop inlining.

**Evidence:** [PineVM.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineVM.cs), lines 609–616, increments iteration counters and emits a tail-loop event using the current physical frame expression/input. Lines 2159–2179 and 2256–2305 perform counting only for negative jump offsets.

**Consequence:** a zero-offset self-loop is not charged as a loop iteration by this test, although unconditional jumps do check cancellation. Reordering blocks or inserting edge-copy stubs can change counting without changing semantic cycles. Inlining also makes the physical frame identity differ from the inlinee's function identity. Do not claim cancellation is absent; the issue is accounting and provenance.

**Robust refactoring:** graph-derived immutable transfer/safepoint metadata, with explicit compatibility rules for current counters and callbacks. Prove every reachable cycle passes a charged safety checkpoint; distinguish synthetic layout transfers from semantic iteration edges. Avoid silently redefining existing callback payloads.

**Regression coverage:** zero-offset self-loop under a loop limit; ordinary root-loop callback compatibility; semantically identical layouts with stubs; nested/inlined loops; cancellation and quota exhaustion on all cyclic paths.

**Plan increments:** 8–9, prerequisite to 12.

## R8. Invocation boundaries are incomplete in the reconstructed graph

**Status:** confirmed representational gap for direct invocation instructions; current frontend reachability is not assumed.

**Evidence:** [PineControlFlowGraph.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineControlFlowGraph.cs), lines 171–179 and 802–809, recognizes `Eval_Binary` and `Eval_Const` as invocation boundaries. It does not recognize `Invoke_StackFrame_Const`. That opcode is explicitly a direct invocation with consumed parameters and a returned value in [StackInstruction.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/StackInstruction.cs), lines 927–951 and 1809–1825.

**Consequence:** directly supplied instructions can represent a call as an ordinary operation, despite the graph's explicit invoke-continuation abstraction. Transformations using terminators to identify calls would miss it.

**Robust refactoring:** represent all calls using the same semantic call/continuation model, with dynamic/known/linked distinctions in call-target data. Instruction selection chooses the concrete opcode afterward.

**Regression coverage:** dynamic call, literal encoded call, known direct call and tail call all expose explicit call operands/results and preserve caller live values; malformed target encodings remain runtime errors where required.

**Plan increments:** 2–3, 5, 7, 13.

## Prioritization

- Before using a semantic graph as an optimization boundary: R1–R3 and R8.
- Before trusting new backend metadata or publishing compiler artifacts: R4–R5.
- Before enabling tail-loop replacement on the new frontend: R6.
- Before allowing arbitrary loop layout and loop-containing inlining: R7.

Do not fix these by broad unrelated cleanup in the planning change. Each implementation increment should introduce its focused regression first and make the smallest compatible correction within its own responsibility.
