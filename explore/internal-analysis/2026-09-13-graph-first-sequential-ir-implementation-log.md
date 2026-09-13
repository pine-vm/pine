# Graph-first sequential IR: implementation log

## Scope

Implementation follows the adjusted [refactoring plan](/home/runner/work/super-duper-disco/super-duper-disco/explore/internal-analysis/2026-09-13-graph-first-sequential-ir-refactoring-plan.md), including Example Alfa and the performance-regression requirement.

This log records completed slices, evidence, and design implications. It is not a declaration that the entire migration is complete.

## 2026-09-13 — Increment 1: characterization

### Work boundary

- Begin with the characterization increment, before introducing semantic graph models or switching the production pipeline.
- Preserve existing instruction/performance snapshots. Add explicit semantic tests rather than treating the old compiler as the only correctness oracle.
- Investigate the guarded-`Eval` operand-order/non-tail-context discrepancy R6. A demonstrated semantic failure must not become a passing expectation of incorrect behavior.
- Keep the new semantic models, graph validation, lowering, and eventual removal of `FromInstructions` for subsequent increments.

### Example Alfa

Added [GraphFirstCompilationCharacterizationTests.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/GraphFirstCompilationCharacterizationTests.cs).

The fixture preserves the plan's `skipIdentifier`, `isIdentifierStart`, `skipToIdentifierEnd`, and `isIdentifierChar` declarations. It checks 13 inputs across four configurations: Elm syntax inlining on/off and VM tail-recursion optimization on/off.

Coverage includes empty input, rejected first character, a digit that is legal only after the first character, each accepted first character, early termination, complete consumption, nonzero and out-of-range offsets, Unicode before the identifier, and a longer identifier.

Every case first checks `DirectInterpreter` against a concrete expected offset, then compares the intermediate VM result to that reference. Each VM is fresh, invocation caching is disabled, precompiled leaves are disabled by the existing profiling helper, and invocation/loop/stack limits are finite.

Measured aggregate baseline across the 13 inputs:

| Elm syntax inlining | VM tail optimization | Invocations | List constructions | Loop iterations | Instructions |
| --- | --- | ---: | ---: | ---: | ---: |
| Enabled | Disabled | 19 | 19 | 0 | 370 |
| Disabled | Disabled | 19 | 19 | 0 | 370 |
| Enabled | Enabled | 3 | 3 | 16 | 418 |
| Disabled | Enabled | 3 | 3 | 16 | 418 |

These are characterization snapshots, not the desired final graph-first performance. The initial counter assertions deliberately used placeholders to collect the baseline; all 52 value comparisons passed in those runs. Only the measured values are retained in the final assertions.

### Guarded `Eval`: R6 confirmed and narrowly corrected

Added [EvalOperandCompilationTests.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Interpreter/IntermediateVM/EvalOperandCompilationTests.cs), with 22 cases covering:

- Environment failure before encoded-operand failure or literal-target parsing.
- A root invocation used as either operand, which must return to the outer invocation.
- Failure versus divergence ordering, using the quota-aware VM for divergent paths rather than the unbounded direct interpreter.
- A failing environment component not referenced by the callee's projected parameter layout.
- Both outcomes of an ordinary safe root-identity guard, with tail optimization enabled and disabled.
- An invoking environment supplying nested, swapped loop parameters, with an assertion that loop optimization remains enabled.

Five cases failed before the correction, confirming the predicted failure-order, continuation, and skipped-evaluation problems. Rather than commit disabled tests or encode those incorrect results as expected behavior, this increment includes a small prerequisite correction in [PineIRCompiler.cs](/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Interpreter/IntermediateVM/PineIRCompiler.cs):

- A guarded candidate whose environment contains `Eval` evaluates that environment once, in full, into a local before evaluating the target. Such an environment may fail or diverge, so evaluating the encoded target first or projecting away environment components is unsafe.
- Both guarded branches reuse the evaluated environment. The loop branch projects parameters from the saved value before the existing batched parameter update, preserving simultaneous transfer semantics.
- The generic guard branch explicitly clears tail position while compiling the environment operand.

No graph representation or production-pipeline selection changed. The ordinary guarded path for an environment without `Eval` retains its existing allocation-free argument projection and counter behavior. `EvalCount` is not a general totality analysis; the future semantic graph still needs explicit sequencing and proof-based transformations.

The first correction conservatively used a normal invocation whenever the environment contained `Eval`. It passed the 34-case semantic/Alfa gate, but `SequentialIREfficiencyTests.Tail_loop_parameter_updates_use_batched_instructions` then failed: the expected `Local_Set_Descending (3, 4)` disappeared because the guarded loop had been removed. That broader run had 144 passes, one failure and one existing skip. The final correction preserves the loop by materializing and reusing the environment instead of disabling that optimization.

This advances the narrowly demonstrated part of R6 from its planned frontend/loop increment into the characterization prerequisite. It does not claim to resolve all other findings or every possible evaluation-elimination issue in the old compiler.

### Discoveries that influence the plan

1. **Separate frontend inlining from VM preparation.** Disabling Elm syntax inlining does not disable the VM's expression reduction/inlining. Example Alfa has identical counters in both Elm configurations with the existing profiling helper. Future graph-inlining acceptance must inspect the actual graph/call boundaries as well as execution counters; toggling the Elm option alone does not establish a non-inlined baseline.
2. **The existing tail-loop optimization trades instructions for fewer invocations/allocations.** Example Alfa currently replaces 16 invocations/list constructions with 16 loop iterations but executes 48 additional instructions. This is existing behavior, not a regression introduced here. Preserve per-configuration baselines and evaluate the new lowering across multiple counters rather than assuming fewer calls implies fewer instructions.
3. **Do not claim the motivating optimization is complete.** Even with tail optimization enabled the aggregate still contains three invocations and three list constructions. The new fixture does not assert the eventual zero-additional-frame/zero-canonical-argument-allocation goal, which requires the graph construction/inlining increments.
4. **Keep generated outputs out of the implementation change.** An intermediate status check showed an untracked `artifacts/` directory, but inspection confirmed `.gitignore` already covers `artifacts/test-logs/`. Explicitly redirected investigation logs outside that ignored subdirectory caused the status; they were moved under the ignored log directory. Do not infer that the per-run logs are unignored from the parent directory alone.
5. **An environment projection is not sufficient evidence that evaluation may be omitted.** One executable R6 regression has a failing environment component that the callee does not reference. Future graph-level argument scalarization must preserve evaluation of potentially failing/diverging components, not merely construct the demanded parameter values.

### Validation

Commands are run from `/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests`.

- `dotnet run -- --filter-method="*Example_alfa*"`: initial baseline collection completed; four theory cases reached only the intentional counter-placeholder failures, with all semantic assertions passing.
- `dotnet format Pine.Core.Tests.csproj --no-restore --include /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/GraphFirstCompilationCharacterizationTests.cs`: completed.
- Targeted `dotnet format` also completed for the operand tests and compiler correction.
- `dotnet run -- --filter-class '*EvalOperandCompilationTests' '*DirectTailLoopCompilationTests' --timeout 2m`: 30 passed, zero failed/skipped after the correction; the preceding run demonstrated five semantic failures.
- `dotnet run -- --filter-class '*EvalOperandCompilationTests' '*DirectTailLoopCompilationTests' '*GraphFirstCompilationCharacterizationTests' --timeout 2m`: 34 passed, zero failed/skipped with the first conservative correction, including all four Example Alfa snapshots.
- After refining the correction and adding the two nested-parameter cases, `dotnet run -- --filter-query '/*/Pine.Core.Tests.Interpreter.IntermediateVM/*/*' '/*/*/SequentialIREfficiencyTests/*' '/*/*/TailLoopCompilationTests/*' '/*/*/GraphFirstCompilationCharacterizationTests/*' '/*/*/FunctionApplicationOverheadTests/*' --timeout 5m`: **147 passed, zero failed, one pre-existing skip**. This includes the existing specialization, invocation-cache, callback, quota, instruction-shape and performance-counter tests in those suites. Existing expected snapshots were not changed.
- The existing skip is `FunctionApplicationOverheadTests.List_map_triple_with_function_parameter`: “TODO: Reimplement optimizations in Elm compiler or Pine compilation”. No skip was added by this change.
- Final broad-suite log: `/home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/artifacts/test-logs/Pine.Core.Tests/2026-09-13T10-52-24_filtered.log`.
- The final compiler and operand-test revisions were formatted with `dotnet format`. No full core or integration-suite run is claimed.
- Secret scanning passed. Final review/security results will be recorded before completing the increment.

### Next increment

After the characterization gate is green, implement increment 2: immutable semantic graph IDs, blocks, operations, explicit edges and call-return bindings, plus structural equality/rendering and direct graph fixtures. Do not build these models by wrapping emitted instructions. Preserve this fixture and the existing performance suites through subsequent cutover work.
