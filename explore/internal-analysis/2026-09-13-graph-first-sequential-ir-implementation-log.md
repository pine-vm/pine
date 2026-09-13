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

Next: increment 3, graph validation of definitions, every operand/edge, call signatures and return-slot scope.
