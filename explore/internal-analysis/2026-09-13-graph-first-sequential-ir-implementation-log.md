# Graph-first sequential IR: implementation log

Follow the [refactoring plan](/home/runner/work/super-duper-disco/super-duper-disco/explore/internal-analysis/2026-09-13-graph-first-sequential-ir-refactoring-plan.md). Entries record completed increments and design-relevant discoveries; the production pipeline remains instruction-first until the planned cutover.

## 2026-09-13 — Increment 1: characterization

- Added Example Alfa coverage: 13 inputs × four Elm-inlining/VM-tail configurations, comparing the intermediate VM with explicit expected results and `DirectInterpreter`.
- Added 22 operand-order, continuation, failure/divergence and parallel-parameter cases. Five initial failures confirmed R6. Guarded loops now evaluate invoking environments in full before the target and reuse the saved value in both branches; operand compilation clears tail position.
- **Discovery:** simply disabling guarded loops for invoking environments broke `Tail_loop_parameter_updates_use_batched_instructions`. Materializing the environment preserves both semantics and batched loop updates. Parameter projection alone never justifies dropping potentially failing/diverging evaluation.
- **Discovery:** disabling Elm syntax inlining does not disable VM preparation. Alfa counters are identical across Elm inlining settings; future graph-inlining tests must inspect actual call boundaries.

Alfa aggregate baseline (invocations / lists / loops / instructions): tail optimization off **19 / 19 / 0 / 370**; on **3 / 3 / 16 / 418**. Existing tail lowering trades 48 instructions for 16 fewer invocations/list constructions; the final allocation/frame-elimination goal remains open.

Validation from `/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests`:

`dotnet run -- --filter-query '/*/Pine.Core.Tests.Interpreter.IntermediateVM/*/*' '/*/*/SequentialIREfficiencyTests/*' '/*/*/TailLoopCompilationTests/*' '/*/*/GraphFirstCompilationCharacterizationTests/*' '/*/*/FunctionApplicationOverheadTests/*' --timeout 5m`

**147 passed, one existing skip** (`List_map_triple_with_function_parameter`), unchanged existing snapshots. Changed C# formatted; secret scan and independent review passed. Automated review backend unavailable; CodeQL skipped for database size, then unchanged-run cache. No full-suite run claimed.

## 2026-09-13 — Increment 2: semantic models

- Added `Semantic.FunctionGraph`, `GraphBuildState` and `GraphRendering`: immutable signatures, semantic operations, per-edge arguments, dynamic/known calls, explicit caller-value/returned-slot continuation bindings, persistent allocation/fragments, structural equality/hashing and deterministic rendering. Reuses block/value ID types only; no stack instructions or production compiler changes.
- Added 24 direct-model tests covering sparse/reordered IDs, shared targets with different arguments, tail/non-tail calls, nested/consecutive/irreducible cycles, parameter swaps, ownership, culture-independent rendering and persistent construction.
- **Discovery:** `PineValue` can retain caller-owned buffers, so literal payloads use immutable Blob/List variants; paths also own immutable sequences. Future frontend conversion must snapshot legacy data explicitly. Known callees are IDs rather than embedded mutable ASTs; preparation, provenance and specialization proofs remain later work.
- **Boundary:** complete block shapes are still unvalidated. Construction rejects protocol misuse, but dangling references remain representable for increment 3 diagnostics. No executable lowering or optimization benefit is claimed.

Validation: scoped `dotnet format`; **24/24** model tests passed. The same regression command recorded above now reports **171 passed, one existing skip**, with unchanged snapshots. Logs: `/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/artifacts/test-logs/semantic-model-{focused,regression}.log`.

Secret scan and independent review passed. Automated review remained unavailable; CodeQL skipped the oversized database.

## 2026-09-13 — Increment 3: graph validation

- Added pure validation with structured diagnostics for definitions, all operands and transfers, entry/call signatures and invoke return-slot scope. Checks unreachable blocks without imposing layout, reducibility or empty-edge-argument restrictions. Production compilation remains unchanged.

- **Boundary:** validation evidence retains the exact immutable graph and normalized signature table; it is not a record that could certify a different graph through `with`. Recursive calls resolve against the root signature, and any explicit root table entry must agree.
- Structural validation must not evaluate Pine primitives or parse dynamic encoded targets: even a literal target that will fail at runtime remains a valid operand. It establishes transfer contracts, not termination or evaluation success.

Validation: scoped `dotnet format`; **116/116** validator cases passed; the regression command above reports **287 passed, one existing skip**. Logs: `/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/artifacts/test-logs/semantic-validation-{focused,regression,format}.log`. Secret scan passed; independent review has no remaining findings after correcting an import placement before the passing runs. Automated review remains unavailable; CodeQL again skipped the oversized database, so no completed automated security analysis is claimed.

## 2026-09-13 — Increment 4: straight-line backend

- Added immutable selected instructions, deterministic distinct-local assignments and exact stack/local bounds for validated single-block graphs: literals, lists, list projections, all 17 builtins and one-value returns. Unsupported transfers, extra blocks (even unreachable) and other result arities are explicitly declined; production compilation is unchanged.

- **Discovery:** overriding `StackFrameInstructions.MaxStackUsage` in an object initializer still runs its instruction-derived analysis first. Graph-produced resource metadata needs a constructor path that bypasses that analysis, not merely a replacement property value.
- Semantic signature parameter order is meaningful; the legacy interface sorts paths. The compatibility adapter uses a canonical environment and explicit entry projections, preserving unsorted/duplicate paths. It materializes fresh legacy literal payloads without interning; mutable VM objects never enter the immutable artifact.
- **Discovery:** six generic builtin opcodes existed without VM handlers (`equal`, bitwise and/or/xor, and both shifts). Generic lowering needs these handlers, not assumptions that existing opcode declarations imply runtime support.
- List-only graph projection differs from generic `head`/`skip`, which also index blobs. Keep these semantics distinct when lowering and when the future frontend recognizes path expressions.

Validation: scoped `dotnet format`; **33/33** backend tests passed, including builtin differential inputs, ownership, parameter ordering, stack discipline and metadata bypass. The established regression gate reports **320 passed, one existing skip**. Logs: `/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/artifacts/test-logs/Pine.Core.Tests/2026-09-13T11-49-{07,22}_filtered.log`. Secret scan and independent review passed. Automated review remains unavailable; CodeQL skipped the oversized database, so no completed automated security analysis is claimed.

## 2026-09-13 — Increment 5a: non-call edges and layout

Added immutable selected blocks, per-edge copy plans, symbolic layout and final offset emission; shared operation selection with the straight-line backend. Supports return/jump/branch/switch across arbitrary block IDs, including unreachable blocks and parameterized cycles. Invoke/tail-invoke are explicitly declined until 5b; production compilation remains unchanged.

- Entry initialization must be separate from the semantic entry block: backedges to entry carry new parameters and must not rerun environment projections.
- Legacy conditional/switch jumps consume their selector; all jumps check cancellation, but only negative offsets count toward loop quotas. Keep explicit nonzero cycles through edge stubs until increment 9 supplies semantic safety metadata; current layout-dependent counts are not the final instrumentation contract.
- Parallel copies stage all sources before reverse-order stores: **3N instructions / N stack slots / no scratch locals** for N bindings. This preserves swaps, longer cycles, duplicate sources and self-copies; resources include unreachable fragments and transfer staging.
- **Discovery:** existing `bit_shift_left([-8, blob(01)])` throws `IndexOutOfRangeException` under direct interpretation; the VM wraps it as `InvalidIntermediateCodeException`. Regression coverage preserves reachable unused failures without evaluating unreachable operations. The preexisting primitive/error-classification discrepancy is not repaired by graph lowering.

Validation: scoped `dotnet format`; **41 graph-backend tests passed**, and the established regression gate reports **361 passed, one existing skip**. Subsequent immutable-test-helper and direct-error assertions passed targeted checks. Logs: `/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/artifacts/test-logs/increment5a-{gate,immutable-helpers,direct-error}.log`. Secret scan and independent review passed; automated review/security validation pending.

Next: increment 5b, calls and explicit successful-return continuations.

## Deferred improvement ideas

These are hypotheses for measurement after production cutover, not exemptions from the plan's required correctness, safety or performance gates. Retain the conservative backend as a comparison baseline.

| Question / candidate change | Measurement and acceptance gate |
| --- | --- |
| Should edge copies choose between stack staging and a scratch-local cycle algorithm based on transfer width and aliasing? | Compare instructions, peak stack, locals and compilation allocations on wide loop headers; retain swap/cycle/duplicate-source equivalence. Basic safe copying and planned coalescing remain pre-cutover requirements. |
| Would profile-weighted block scheduling and selective edge-stub sharing outperform deterministic layout? | Compare dispatch count, code size and compile cost; preserve distinct edge arguments and layout-independent safepoint coverage. Do not infer semantic loop counts from the chosen layout. |
| What case-count/literal-shape threshold favors indexed switches over ordered equality tests? | Compare dispatches, equality/hash cost and artifact size across skewed and uniform inputs; preserve exact literal matching, default behavior and one-time selector evaluation. Required switch-selection parity still belongs before cutover. |
| Can an owned immutable literal arena reduce repeated VM-adapter copies without restoring global interning aliases? | Measure publication allocations, retained memory and repeated compilation cost; require aliasing tests and immutable publication before sharing any backing storage. |
| When does a projected entry ABI outperform the canonical-environment prologue, especially after inlining? | Compare projection instructions, argument allocations and frame costs for generic versus specialized calls; preserve positional/duplicate paths, specialization guards and canonical fallback. Complete required invocation-overhead optimizations first. |
