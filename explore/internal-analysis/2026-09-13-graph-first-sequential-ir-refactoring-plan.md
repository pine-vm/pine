# Plan: graph-first compilation from Pine expressions to sequential IR

## Scope and completion criteria

Replace the instruction-first intermediate-VM compiler with this one-way pipeline:

**Pine expression → prepared expression → semantic control-flow graph → optimized graph → selected blocks → sequential stack instructions → executable artifact.**

The graph is the authoritative representation of control and data flow. Sequential IR means the existing stack-machine instruction representation, not the new semantic graph.

Completion requires:

- No construction of a control-flow graph from sequential instructions, including in tests, diagnostics, compatibility adapters, or stack-usage analysis. Remove `PineControlFlowGraph.FromInstructions`, rather than merely renaming or hiding it.
- No `StackInstruction`, stack position, physical local index, jump offset, or required fall-through order in the semantic graph.
- Explicit operands, result definitions, block parameters, and edge arguments. Loops may carry values; several loops and nested loops must work.
- Graph-to-graph inlining of an already-looping callee into a non-tail call site, including inside another loop, demonstrated by executable tests.
- Immutable compiler models and explicit state-in/state-out transformations, portable to Elm.
- Preserved language results, error behavior, specialization safety, and runtime safety checks; separately measured optimization outcomes.

This is a plan, not an implementation or a claim that tests have passed. The companion findings document records current robustness issues and the increments that should address them:

[Robustness findings](/home/runner/work/super-duper-disco/super-duper-disco/explore/internal-analysis/2026-09-13-graph-first-sequential-ir-robustness-findings.md).

## Current implementation and reuse boundaries

Source references below describe the inspected checkout; proposed names in later sections are design names, not existing APIs.

| Existing component | Current responsibility and disposition |
| --- | --- |
| `ExpressionCompilation.CompileExpression` | Builds generic and environment-specialized variants. Retain the external responsibility, ordering of specialization selection, and configuration behavior. Separate preparation from graph construction and backend compilation. |
| `InstructionsFromExpressionTransitive` | Reduces/inlines expressions, compiles instructions, rewrites jumps to returns, appends a return, reconstructs the CFG, forwards Boolean branches, and lowers again. Replace the instruction-first portion entirely. |
| `PineIRCompiler` | Combines expression traversal, CSE/local allocation, builtin fusion, branch layout, invocation selection, and root-tail-loop emission. Split these responsibilities; do not reuse its instruction emitter as a graph builder. |
| `PineControlFlowGraph`, `PineBasicBlock`, operations and terminators | Reuse the basic-block vocabulary and ID concepts, but replace the stack-derived payloads and invariants. A separately named semantic model can coexist during migration; delete the old model at cutover. |
| `StaticFunctionInterface` | Reuse parameter-path inference and deterministic ordering. Make the representation deeply immutable before using it inside the new compiler. Distinguish the canonical environment interface from internal block parameter lists. |
| `StackInstruction`, `StackFrameInstructions` | Retain backend opcode semantics and the VM boundary, not their use as semantic IR. Harden published artifact immutability and linking; compute metadata from selected blocks without rediscovering control flow from instructions. |
| `PineVMParseCache`, reduction caches | Preserve memoization benefits through immutable compiler memo state. Existing mutable caches may remain outside the pure compiler during migration, but may not be passed into new pure mapping functions or hidden inside callbacks. |
| Intermediate `PineVM`, `DirectInterpreter` | Reuse as execution consumers and differential references. Existing mutable execution frames are not compiler models; rewriting the whole VM is outside this refactoring. Any new runtime metadata or transition descriptions must nevertheless be immutable. |

Evidence:

- [ExpressionCompilation.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/ExpressionCompilation.cs), lines 80–144, 177–296, 1169–1185.
- [PineIRCompiler.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineIRCompiler.cs), lines 21–175, 592–869, 902–1175.
- [PineControlFlowGraph.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineControlFlowGraph.cs), lines 12–105, 215–319, 322–463, 639–731.
- [StaticFunctionInterface.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/CodeAnalysis/StaticFunctionInterface.cs), lines 10–76.
- [StackInstruction.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/StackInstruction.cs), lines 482–562, 957–974; [StackFrameInstructions.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/StackFrameInstructions.cs), lines 14–35.

## Pure-programming constraints

These apply to all new or refactored compiler models and mapping functions, including private member functions, analyses, lowering, memoization, and linking.

1. Use immutable records/variants, immutable sequences, dictionaries and sets, and owned immutable nested paths. `IReadOnlyList`, `IReadOnlySet`, `ReadOnlyMemory`, or a record around a mutable object is not proof of immutability.
2. Top-level/member functions must neither mutate arguments nor accept mutable builders, `ref`/`out` state, mutable caches, or callbacks with hidden mutable compiler state. Do not reassign parameters.
3. Return updated immutable state explicitly: allocated IDs, accumulated blocks, memo entries, diagnostics and budget consumption. Repeated calls with equivalent inputs must produce equivalent outputs.
4. If imperative acceleration is needed, confine mutation to local functions nested inside a pure outer function. Local mutable builders must not escape, and must not alias caller-owned data. Document the equivalent immutable fold/worklist transition.
5. Use IDs and immutable lookup tables for graph and function references, not linked object cycles. Recursive call graphs must be representable as ordinary Elm dictionaries.
6. Define structural equality and deterministic ordering explicitly. Do not rely on C# collection reference equality, object identity, hash enumeration order, or mutable cached hashes.
7. Use data variants for outcomes and diagnostics; expected invalid graphs or unsupported optimizations return structured results. Exhaustive variant handling must follow the repository's explicit-case and throwing-default rule.
8. Preserve immutable source expressions/values where their ownership is safe; copy or harden any mutable collection boundaries actually admitted into compiler artifacts. Do not duplicate the language AST unnecessarily.

An unchanged legacy wrapper may translate existing configuration to immutable inputs and publish memo results outside the pure core. It is a migration boundary, not permission to call the old mutable-cache implementation from a supposedly pure function. Port the reused preparation and parsing functionality to pure overloads incrementally.

## Inventory of core data models

The graph uses **block-local SSA with block parameters**: every operation result has one definition, and every value entering a block comes through its parameters. This deliberately avoids implicit cross-block captures and mutable locals. Start with one semantic value category, `PineValue`; later representation refinements must be proven rather than assumed.

| Model | New/reused | Required contents and invariant |
| --- | --- | --- |
| `Expression`, `PineValue`, environment constraints | Reuse, harden relevant ownership boundaries | Language input, literals and specialization facts. Preserve generic fallback and canonical Pine encodings. |
| `CompilationRequest`, `PreparedFunction` | New | Root expression identity and alternative forms, prepared body, immutable options, specialization assumptions, inferred signature and provenance. Identity must not be confused with an occurrence of an inline expansion. |
| `FunctionId`, `PineBlockId`, `PineVirtualValueId`, `CallSiteId` | Reuse block/value concepts; add others | Opaque identifiers. Block identity is independent of sequence position. IDs have function-local namespaces; cloning allocates fresh IDs in the destination. |
| `FunctionSignature` | New, incorporates hardened `StaticFunctionInterface` | Canonical environment-to-parameter mapping, ordered parameter descriptions, ordered result descriptions. Initially one canonical Pine return value; allow a result vector without changing edge semantics. Do not assume a multi-return VM ABI already exists. |
| `FunctionGraph` | Replacement for current graph | Function ID, signature, entry block ID, immutable block dictionary, immutable provenance. No preferred physical layout in semantic validity. |
| `BasicBlock` | Replace payload of existing concept | Ordered parameter definitions, ordered semantic operations, exactly one terminator. All parameter and result IDs are unique within the function. |
| `SemanticOperation` | New | Explicit input references, explicit result definitions and semantic opcode. Initial families: literal, list construction, environment/path projection and builtin application. Calls are terminators, not hidden ordinary operations. No generic escape hatch containing sequential instruction fragments. |
| `OperationSemantics` | New | Operand/result arity, Pine behavior on malformed values, and conservative totality/speculation classification. Possible failure/divergence forbids speculative movement or elimination without proof. |
| `Edge` | New, replaces duplicated successor fields | Target block and ordered argument references. Each branch/switch case/default has its own edge, even if targets repeat. Transfers bind target parameters simultaneously. |
| `Terminator` | Replace current hierarchy | Return with explicit values; jump with an edge; equality branch with explicit tested value and literal plus two edges; switch with explicit selector, cases and default; invoke; tail invoke. No implicit fall-through, return-stack-top or instruction-carried operands. |
| `CallTarget`, `CallContract` | New | Dynamic encoded target, known expression target, or known function ID; explicit arguments and result signature; specialization proof/guard requirements and source identity. A literal encoded value is not automatically a valid callable expression. |
| `InvokeContinuation` | New | Continuation target and ordered bindings distinguishing caller values from indexed returned results. Returned results exist only on successful return, not in the invoking block or unrelated edges. Tail invokes have no local continuation. |
| `GraphBuildState`, `GraphBuildResult`, `CompilerMemo` | New | Immutable ID supply, completed blocks, open construction fragment, current lexical value bindings, compilation budget and parse/reduction/graph memo entries. Incomplete fragments are construction data, not valid graphs. |
| `GraphFacts`, `LoopAnalysis` | New | Predecessor **edges**, reachability, SCCs, dominators, def-use/liveness, natural-loop headers/latches/exits and nesting where applicable. Analyses are derived, immutable and invalidated by graph rewrites. |
| `InlineDecision`, `CloneMap`, `RewriteResult` | New | Eligibility/proof, bounded cost, block/value/call-site substitution, rewritten graph, diagnostics and invalidated analyses. Unsupported optimizations leave the valid input graph unchanged. |
| `SelectedFunction`, `SelectedBlock`, `SelectedTransfer` | New, backend-only | Graph-derived blocks with symbolic targets and selected machine operations. Preserve graph edges throughout selection; never split a flat instruction list to recover blocks. |
| `StorageAssignment`, `ParallelCopyPlan` | New | Value-to-local assignments, per-edge simultaneous copies and scratch storage. This describes runtime writes immutably; it is not mutable compiler state. |
| `LayoutPlan`, `FrameResourceUsage`, `TransferMetadata` | New | Deterministic block order, edge stubs, symbol-to-offset map, stack/local bounds, counted-loop and safety-check provenance. Layout cannot alter semantic successors. |
| `CompiledFunction`, `CompiledProgram` | New immutable publication boundary | Immutable instructions and metadata, function IDs and immutable resolution table. Preserve existing VM-facing behavior through an adapter; eliminate mutable instruction-linked targets in the final compiler artifact. |
| `GraphDiagnostic`, `CompilationDiagnostic` | New, using existing `Result` conventions | Phase, function/block/value/edge location and reason. Diagnostics are data and distinguish invalid input from a declined optional optimization. |

### Calls, edge data, and returns

- Ordinary edges contain only values defined in the source block. They bind target parameters by position and simultaneously, so exchanging two loop-carried values cannot clobber either input.
- An invoke continuation has a distinct binding form for a returned result slot. The validator checks the slot against the callee's declared result signature and rejects its use before return.
- All live caller values needed after a call are explicit continuation bindings. After inlining, each callee return supplies the appropriate result values and preserved caller values to the caller's continuation.
- A switch can send different arguments to the same target from different cases. Sharing branch code must not collapse those edge identities.
- Initially every dynamic/opaque call consumes a canonical environment and returns one canonical value. Known-call parameter projection and any future multi-result representation require explicit adapters and proofs; unknown targets and incompatible layouts keep the generic path.

### Multiple loops and inlining

Represent loops as graph cycles with parameterized headers, not a single `TailLoopTarget` or a jump to instruction zero. The function-entry adapter and a reusable loop header are distinct blocks.

Compute SCCs and dominance from edges, not numeric IDs. Natural loops can have several latches and exits; their nesting forest is derived information. General graph validity must not require reducibility. Unsupported irreducible-loop optimizations decline cleanly while conservative lowering still works.

Inlining must:

1. Establish a known callee, valid specialization assumptions, compatible signature and acceptable bounded growth. Dynamic calls stay calls unless a sound guard supplies a known branch.
2. Clone the entire callee graph with fresh block, value and call-site IDs, retaining internal cycles. Redirect only the call entry into the cloned entry adapter; internal loop edges continue to their cloned loop headers.
3. Map callee inputs from already-evaluated caller arguments. Carry caller live values through cloned blocks explicitly where needed; there are no hidden captures.
4. Replace every normal callee return with a jump to the caller's continuation, binding returned values and preserved caller values. Do not accidentally redirect a nested callee tail invoke to the outer function's return: at a non-tail inline site, its result must still reach the caller's continuation.
5. For a genuinely tail-position inline site, route returns to the caller's return contract. Preserve calls that cannot be inlined.
6. Revalidate and recompute affected graph facts. Bound recursive expansion using call-graph SCC/ancestry and size budgets; self/mutual recursion cannot trigger unbounded cloning.

Required demonstrations include two inlinings of the same looping callee, a looping callee in one arm of a conditional, consecutive independent loops, and a looping callee inside a caller loop. Preserve caller values across inner iterations and support multiple return exits.

## Inventory of mapping functions

Names describe responsibilities, not prescribed method signatures. Every mapping below has immutable inputs/outputs.

| Mapping | Input → output | Reuse/replacement and contract |
| --- | --- | --- |
| `PrepareFunction` | Compilation request + memo → prepared function + updated memo | Extract existing reduction, substitution and inlining policy from `InstructionsFromExpressionTransitive`. Keep preparation separate from physical lowering; migrate reused cache-dependent functions to pure forms. |
| `InferFunctionSignature` | Expression + constraint facts → signature | Reuse `StaticFunctionInterface.FromExpression` inference with owned immutable paths. |
| `CompileExpressionToGraph` | Prepared function + options + memo → validated function graph + updated memo | Replace `PineIRCompiler`'s recursive instruction production. Handle every existing expression variant explicitly, including labels. |
| `CompileNode`, `CompileConditional`, `CompileEval` | Expression + lexical bindings + continuation context + build state → build result | Create operations and explicit continuation blocks directly. Tail position is a continuation property; evaluating any call operand is non-tail. |
| `ClassifyCallTarget` | Target expression/value + constraints + memo → direct/guarded/opaque decision + memo | Reuse the reasoning in `ResolveTailCallToRoot`, not its root-offset encoding. Invalid constant encodings preserve runtime errors. |
| `RecognizeTailLoop` | Graph + call proof → graph with parameterized loop header | Replace `CompileLoopArguments`/root-offset jumps. Carry next parameters on the edge rather than overwriting semantic locals. Preserve specialization guards across every iteration. |
| `ValidateGraph` | Graph + signatures → validated graph or diagnostics | Replace `Validate`; verify definitions, every terminator operand, edge bindings, call return scope, signatures, targets and entry contract. Check unreachable blocks structurally too. |
| `AnalyzeGraph` | Validated graph → immutable graph facts | Replace stack-depth-derived topology and numeric-ID loop tests. Reuse pure predecessor enumeration concepts only. |
| `ForwardConstantBooleanBranches` | Graph + facts → rewrite result | Port existing optimization to explicit values/edges. No dependence on provider stack shape, IDs or required fall-through chains. Preserve noncanonical Boolean behavior. |
| `SimplifyGraph`, `EliminateDeadValues`, `ReuseCommonValues` | Graph + semantic facts → rewrite result | Stage separately. Use block scope/dominance and conservative failure/divergence rules; don't reuse stale loop-carried expression values. |
| `DecideInlining`, `CloneAndSubstituteGraph`, `InlineCall` | Caller/callee graphs + call site + facts + budget → decision/rewrite | Add reusable graph inlining; reuse expression heuristics only as policy inputs, not as the representation. |
| `SelectInstructions` | Validated graph + semantic facts → selected blocks | Move existing builtin/path/list/switch fusion patterns here where appropriate. Keep a complete generic builtin lowering so selection coverage is not dependent on every optimization being ported. |
| `AssignStorage`, `PlanEdgeCopies` | Selected blocks + liveness → assignments + copy plans | New. Start conservatively with distinct slots; optimize allocation later. Edge copies include invoke results and loop-carried values. |
| `ScheduleBlocks` | Selected blocks + copy plans → layout plan | New. Insert explicit jumps/edge stubs as needed; fall-through is an emission choice, never semantic validity. |
| `ComputeFrameResourceUsage` | Selected blocks + assignments + layout decisions → stack/local bounds | Replace production `ComputeMaxStackUsage` topology recovery. Simulate instruction effects within already-known blocks; include non-consuming reads and temporary copies. |
| `EmitSequentialInstructions` | Layout plan + resource metadata → immutable compiled function | The only phase assigning relative jump offsets. Replace current `LowerToStackInstructions`, which does not implement virtual-value data movement. |
| `ResolveProgramReferences` | Immutable compiled-function table → resolved immutable program | Replace setter-based linking with ID resolution; support recursive functions without object cycles. |
| `RenderGraph`, test evaluation helpers | Graph/artifact → deterministic text or evaluation result | Graph snapshots originate from graphs. Never add a bytecode-to-graph test converter. Reuse instruction rendering for backend snapshots. |

## Lowering and compatibility strategy

### Correctness-first backend

Initially assign every semantic value and block parameter a distinct physical local. Evaluate each operation by loading operands, executing selected instructions and storing results. Keep the evaluation stack empty at ordinary block boundaries. This is a **backend convention**, not a restriction on graph edges: edges may carry any number of values through locals.

Materialize edge copies only on the edge actually taken. Conditional/switch edges needing copies get dedicated edge stubs, including critical edges and repeated targets with different arguments. Parallel copies must handle swaps, longer cycles, duplicate sources and self-copies. Calls get a return stub that stores returned results before continuation copies; preserved caller locals remain available.

Only then improve stack scheduling, local coalescing and batched transfers. Keep exact opcode behavior from `StackInstruction.GetDetails` and the VM, but extend resource checking to distinguish required stack depth from pop/push counts. Assign offsets after all selected instruction expansion and stubs are fixed.

The current VM accepts a single return value. An internal result vector is extensibility in the graph, not permission to emit an unsupported ABI. Adapt to the canonical return until a separately tested ABI extension exists.

### Invocation machinery, caching, and runtime safety

Inlining and loop lowering remove generic parse/eval dispatch, frame management and invocation-cache work only when target identity and specialization validity are established. A known non-root target is a normal call, not a guarded root-loop candidate. Keep unknown, malformed and unproven calls on the generic path.

Preserve evaluation ordering: normal `Eval` evaluates environment before encoded target. Operand evaluation can fail or diverge, so graph rewrites must not speculate, drop or reorder it without a proof. In particular, extracting a next-environment parameter does not justify deleting evaluation of another failing component.

Separate value/error equivalence from instrumentation policy. Invocation/instruction counts and cache activity are expected to decrease for successful optimizations; errors, cancellation and bounded execution must not disappear. Before enabling inlining/layout changes:

- Define immutable transfer metadata for counted loop edges, function/call-site provenance and safety checkpoints. Existing root-tail-loop tests must retain their counting and callback behavior.
- Ensure every reachable cycle, including zero-length self jumps and irreducible cycles, crosses a budget/cancellation checkpoint. Derive coverage from graph cycles, not from negative physical offsets.
- Distinguish synthetic layout jumps from semantic iteration edges. Preserve existing manually supplied instruction behavior through the legacy VM boundary while new compiled artifacts use explicit metadata.
- State the callback behavior for inlined calls explicitly: preserve the existing API's physical-frame meaning unless a separately named logical event is added; do not fabricate cache insertions or callee-frame reports for frames that no longer exist.
- Include compilation options, specialization assumptions and relevant callee identity in immutable graph/artifact cache keys. Do not cache a graph compiled for one environment class as a generic graph.

Current dependencies: [PineVM.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineVM.cs), lines 448–482, 609–616, 636–656, 2159–2179, 2256–2305; [DirectInterpreter.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/DirectInterpreter.cs), lines 120–164.

## Incremental implementation sequence

Each increment is intended as a separately reviewable, independently testable unit. Dependencies, a bounded deliverable and a stop condition are specified so an agent can finish a coherent slice rather than attempt the migration in one session. Respect the coding agent's one-hour session limit: select a smaller sub-increment when the listed work cannot include its own validation within a session. Split by operation family or invariant before starting; do not leave an unvalidated half-switched compiler.

During migration, the existing instruction-first compiler may remain isolated as a temporary baseline. It is not a new compiler dependency and does not satisfy the final invariant. No increment may add a new instruction-to-graph bridge. The old production path and all old reconstruction tests are removed at cutover.

| Increment | Depends on | Bounded deliverable | Automated acceptance and stop condition |
| --- | --- | --- | --- |
| 1. Characterization | None | Capture current expression semantics, builtin selection, specialization and root-loop behavior; record known discrepancies from the findings document instead of blessing them as correct. | Existing focused suites plus explicit operand-order/non-tail-operand cases. Preserve failures as issue-specific regressions to resolve before migrating the relevant path. No production switch. |
| 2. Semantic models | 1 | Immutable IDs, signatures, blocks, operations, edges, call continuations and construction state; structural equality/rendering. | Hand-built literal, branch, call and loop graphs; input-aliasing and deterministic-rendering tests. No instruction-based fixtures. |
| 3. Validation | 2 | Scope/definition/target/arity/signature checks and structured diagnostics. | Negative tests for undefined edge values, duplicate parameters/results, bad invoke-result scope, missing entry/targets and malformed unreachable blocks; positive sparse/reordered IDs and value-carrying cycles. |
| 4. Straight-line backend | 3 | Storage assignment and lowering of literals, projections, generic builtins, lists and returns from hand-built graphs. | Execute graphs through the VM; compare values with direct interpretation. Verify operand order and stack/local bounds. |
| 5. Edge lowering and layout | 4 | Parallel-copy plans, branch/switch stubs, calls and continuations; graph-derived resource accounting and offsets. | Swaps/cycles, critical edges, shared targets with distinct arguments, nonadjacent successors, live values across calls, and block-order permutations execute correctly. |
| 6. Pure preparation boundary | 2 | Immutable request/memo and pure preparation/parsing overloads, preserving existing reduction policy. Split parsing and reduction memoization if needed. | Same prepared expression with cold/warm memo; unchanged inputs; deterministic specializations/options. Keep the old external wrapper operational. |
| 7. Expression graph frontend | 3–6 | Direct graph compilation of all expression variants with ordinary calls; initially disable new optional graph optimizations. | Differential terminating expression corpus, malformed builtin inputs, noncanonical conditions, both conditional arms, environment/encoded operand order and non-tail nested evals. No fallback that constructs a graph from emitted instructions. |
| 8. Loop analysis and tail-loop graphs | 5, 7 | Reachability/SCC/dominance analysis; parameterized self-tail loops and sound generic guards. | Direct/unresolved/known-non-root targets; changed specialization facts; permuted and duplicate arguments; multiple latches/exits; validate and execute hand-built independent/nested loops. |
| 9. Runtime safety metadata | 8 | Immutable counted-transfer/safepoint metadata and consumer support for generated frames. | Cancellation, quotas, callbacks and traces for root loops, self loops, nested loops and synthetic jumps; no cycle bypasses checks. Do not enable new inlining before this gate. |
| 10a. Graph simplification | 7–9 | Port Boolean branch forwarding, constant branches and unreachable cleanup without layout dependencies. | Existing positive/negative Boolean cases translated to direct graph fixtures; unknown predecessors and noncanonical values remain sound. |
| 10b. Builtin selection parity | 7 | Port list/path/slice/switch fusion in independently committed operation-family batches. | Existing compile fixtures and `SequentialIREfficiencyTests`; each batch has semantic equivalence and opcode-shape assertions. Never regenerate every snapshot blindly. |
| 10c. Value reuse and storage quality | 8, 10a | Dominance-safe CSE, liveness and local/copy coalescing. | Branch-local definitions do not escape; loop inputs invalidate old reuse assumptions; repeated calls/failing expressions are not speculated. Preserve conservative backend as a comparison mode. |
| 11. Graph cloning and acyclic inlining | 3, 7, 9 | Fresh-ID cloning, parameter substitution and all-return continuation wiring; bounded inline policy. | Two expansions of one callee, multiple exits, caller live values, tail/non-tail sites, opaque calls and specialization rejection. |
| 12. Loop-containing inlining | 8, 11 | Inline already-looping graphs, including nested caller/callee loops and remaining callee tail calls. | Execute consecutive/nested loop fixtures, multiple clone occurrences and caller continuations. Check result parity and reduced generic invocation/cache overhead. Bound recursive SCC expansion. |
| 13. Immutable publication/linking | 5, 9 | Deeply immutable instructions/frame metadata and ID-based program reference resolution. May proceed earlier, independently of optimization work. | Linked/unlinked known calls, recursive reference tables, stable equality/hashes and attempts to mutate retained input collections. No new compiler artifact contains mutable `DirectInvocation`. |
| 14. Production cutover and deletion | 6–13 | Route generic/specialized compilation through the graph pipeline; remove old graph reconstruction, offset-bearing frontend/context, duplicate graph recovery and migration mode. | Full core suite and affected integration suites; architecture test rejects instruction-to-graph APIs/dependencies; source inventory finds no `FromInstructions` callers or equivalent reconstruction. All relevant findings resolved or explicitly deferred outside the compiler boundary. |

Each implementation session should leave an immutable API contract, focused tests, and a short completion/next-dependency summary in its change description. Keep feature selection outside semantic graph data. Partial builtin optimization coverage is acceptable behind migration selection; partial language-semantic coverage is not acceptable at production cutover.

## Automated coverage and validation

### Reuse the existing test infrastructure

The test project is [Pine.Core.Tests.csproj](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj), lines 4–12 and 18–24: executable Microsoft.Testing.Platform with xUnit and AwesomeAssertions. Do not add a testing framework.

Relevant suites under [the intermediate VM test directory](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Interpreter/IntermediateVM):

- `PineControlFlowGraphTests`: replace instruction round trips with direct graph construction, retain the behavioral coverage of branch-forwarding cases.
- `CompileExpressionTests`: retain file-based instruction expectations and fused-opcode tests as backend contracts, separate from semantic equivalence.
- `DirectTailLoopCompilationTests`, `ExpressionCompilationInlineEvalTests`, `InlineSmallNonRecursiveCalleeRegressionTests`: retain tail-target resolution, specialization and inlining behavior.
- `InvokeStackFrameConstTests`, `StackFrameInputTests`, `StackFrameInstructionsTests`, `StackInstructionBatchTests`: ABI, input projection, resource bounds and batched-transfer compatibility.
- `EvaluationCancellationTests`, `EvaluationQuotaTests`, `LoopIterationCountLimitTests`, `TailLoopIterationCallbackTests`, `ExpressionCompiledCallbackTests`, `PineVMInstructionTraceTests`, `InvocationCacheTests`: safety and observability.
- [SequentialIREfficiencyTests.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/SequentialIREfficiencyTests.cs): integration-level shape/efficiency checks for parameter updates, wrapping tags and slices.

### New test dimensions

1. **Pure model/API tests:** deep immutability, stable structural equality, deterministic output, unchanged inputs, cold/warm memo parity, and ID-renaming equivalence.
2. **Graph validity:** every operand/edge/result rule; arbitrary IDs/order; unreachable structural validity; call success bindings; loop arguments, multiple exits, irreducible SCCs.
3. **Differential semantics:** compare direct interpretation, unoptimized graph lowering and optimized graph lowering on bounded terminating inputs. Normalize only diagnostic formatting differences that are explicitly allowed; compare error categories and evaluation order. Do not use the old compiler as the sole oracle.
4. **Deterministic generated cases:** bounded expressions and valid graph transformations using existing test infrastructure; print seed and graph on failure. Generate termination by construction for direct-interpreter comparisons; run infinite/cyclic adversarial cases only in the quota-aware VM.
5. **Metamorphic cases:** rename IDs, reorder block storage, split an edge, permute block parameters with matching edge arguments, and inline twice. Results must remain equal even when instruction layouts differ.
6. **Inlining/loop regressions:** inner loop restart after each outer iteration, zero iterations, caller state live across inner loops, swapped loop-carried values, distinct return exits, two clones of one function, nested opaque calls, and recursive inline-budget exhaustion.
7. **Backend correctness:** selected operand arities, non-consuming reads, empty entry, final termination, all targets in range, exact resource bounds, return stubs, parallel-copy cycles and no uninitialized local reads.
8. **Efficiency:** deterministic counters for dispatches/invocations, loop iterations, instructions and list construction; cache-on/off result parity. Add assertions that looping inlinees no longer pass through generic invocation machinery. Wall-clock observations are supplementary, not flaky unit-test gates.
9. **Architecture:** graph types cannot contain stack instructions or mutable linkage/cache services; frontend cannot call sequential emitters; production metadata analysis accepts graph-derived blocks rather than recovering successors from sequential bytecode.

### Running implementation checks

Follow [implementation-rules.md](/home/runner/work/super-duper-disco/super-duper-disco/implement/implementation-rules.md), lines 5–29. From the absolute test-project directory `/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests`, run tests with `dotnet run`, and focused existing methods with `dotnet run -- --filter-method="*MethodName*"`, not `dotnet test --filter`.

Retain complete test output; consult the per-run log before repeating a slow run. Run the existing build/checks and `dotnet format` for changed C# code. Every increment must pass its focused tests before expanding scope; cutover additionally requires the full core suite and affected integration coverage. Scan changes for secrets and run review/security validation before publishing implementation work.

No build or runtime test is required for this documentation-only change; the implementation gates above are future acceptance criteria.
