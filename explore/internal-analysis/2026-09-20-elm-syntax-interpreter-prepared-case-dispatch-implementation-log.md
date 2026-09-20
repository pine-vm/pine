# 2026-09-20 Elm Syntax Interpreter Prepared Case Dispatch Implementation Log

## Summary

- Implemented an interpreter-owned prepared Elm syntax model.
- Switched interpreter preparation/runtime use from abstract expressions to prepared expressions.
- Added prepared case-dispatch segments for constant-value cases, discard-only cases, and general pattern cases.
- Added prepared-model and semantic regression tests, preserved prepared JSON roundtrip support, and reran focused plus broader interpreter validation.
- Followed up on review feedback by adding backward-compatible legacy prepared JSON loading and a legacy abstract-declaration constructor overload without changing current prepared JSON serialization.
- Followed up with cached `PineValueInProcess` prepared literals so unit/string/char/integer/float literals reuse one in-process value, including integer and top-level list metadata, across runtime evaluation and prepared JSON reloads.
- Followed up again by extending prepared constant-case detection from primitive/nullary shapes to exact binding-free composite patterns (tuples, non-empty lists, nested constructor patterns, and exact uncons chains that resolve to list constants) while preserving contiguous dispatch segmentation and `_` specialization.

## Progress

- [x] Added prepared declarations/functions/let/case/record-setter/expression model under `ElmSyntaxInterpreter`
- [x] Collapsed abstract literal expressions into `PreparedExpression.ValueLiteral`
- [x] Prepared case dispatch into ordered segments that preserve source priority
- [x] Optimized constant and discard-only case branches to avoid unnecessary binding/environment allocation
- [x] Updated closures/runtime/resolution paths to use prepared syntax
- [x] Added prepared-model inspection tests
- [x] Added semantic regression tests for constant, discard, binding fallback, nesting, and overlap/order behavior
- [x] Preserved prepared JSON roundtrip coverage
- [x] Added legacy prepared JSON decode fallback plus legacy abstract-declaration constructor coverage
- [x] Switched prepared literal nodes to cached `PineValueInProcess` instances with JSON reconstruction via underlying `PineValue`
- [x] Extended constant-case preparation to exact binding-free composite patterns and added mixed-segmentation regression coverage
- [x] Formatted changed C# files and reran validation

## Design Notes

- The new prepared model keeps reusing `ElmSyntaxAbstract.Pattern`, but owns its own declaration/function/let/record-setter/case/expression types.
- Literal expressions now normalize to `PreparedExpression.ValueLiteral(PineValue)`, covering unit, string, char, integer, and float.
- Case preparation emits an ordered `PreparedCaseDispatch` made of:
  - `ConstantValuePatterns` for contiguous constant-pattern branches
  - `DiscardCase` for `_`
  - `PatternCase` for all other patterns
- Dispatch stays ordered by source position, so overlapping patterns keep Elm branch priority semantics.
- Only `AllPattern` gets the discard fast path. Bare variable patterns still bind and therefore remain general pattern cases.
- `ElmSyntaxInterpreterPreparedJson.FromJsonString` now first attempts the current prepared shape and only falls back to legacy abstract declarations when that legacy decode succeeds, preserving current-format failures otherwise.
- `ElmSyntaxInterpreter.Prepared` now offers a source-compatible constructor overload from `IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration>`, but the `Declarations` property intentionally stays on the new prepared type, so full property-type/binary compatibility is not achievable.
- `PreparedExpression.ValueLiteral` now stores `PineValueInProcess` created once during preparation via `CreateFullyRepresented`, and the prepared JSON boundary serializes only the underlying `PineValue` while rebuilding derived runtime metadata (`_integer`, top-level `_list`, etc.) on load.
- `TryPrepareConstantPatternValue` now recursively emits one exact `PineValue` for binding-free tuple/list/named/uncons patterns when every nested sub-pattern is itself exact, but it still rejects vars, nested `_`, aliases, and record patterns so any case arm that binds or destructures through shape-only matching stays on the general pattern path.

## Validation

Commands run:

1. `dotnet run --project /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj -- --filter-class="*PreparedSyntaxTests" --filter-class="*JsonRoundtripTests" --no-progress --no-ansi`
   - Result: Passed, 5 tests
   - Log: `artifacts/test-logs/Pine.Core.Tests/2026-09-20T11-14-22_filtered.log`

2. `dotnet run --project /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj -- --filter-class="*CaseBlockTests" --filter-class="*ValuesEqualInProcessTests" --filter-class="*DictBuiltinTests" --filter-class="*PineValueInProcessTests" --no-progress --no-ansi`
   - Result: Passed, 12 tests
   - Log: `artifacts/test-logs/Pine.Core.Tests/2026-09-20T11-14-37_filtered.log`

3. `dotnet run --project /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj -- --filter-class="*ElmSyntaxInterpreter*" --no-progress --no-ansi`
   - Result: Passed, 1024 tests
   - Log: `artifacts/test-logs/Pine.Core.Tests/2026-09-20T11-14-52_filtered.log`

4. `dotnet format /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Pine.Core.csproj --include /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.PreparedSyntax.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.Modules.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.Values.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.cs`
   - Result: Completed successfully

5. `dotnet format /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj --include /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxInterpreter/PreparedSyntaxTests.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxAbstract/JsonRoundtripTests.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxInterpreter/DictBuiltinTests.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxInterpreter/UnqualifiedReferenceModuleResolutionTests.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxInterpreter/ValuesEqualInProcessTests.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Internal/PineValueInProcessTests.cs`
   - Result: Completed successfully

6. `dotnet format /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Pine.Core.csproj --include /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.Modules.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreterPreparedJson.cs`
   - Result: Completed successfully

7. `dotnet format /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj --include /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxInterpreter/PreparedSyntaxTests.cs`
   - Result: Completed successfully

8. `dotnet run --project /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj -- --filter-class="*PreparedSyntaxTests" --filter-class="*JsonRoundtripTests" --no-progress --no-ansi`
   - Result: Passed, 7 tests
   - Log: `artifacts/test-logs/Pine.Core.Tests/2026-09-20T11-29-09_filtered.log`

9. `dotnet format /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Pine.Core.csproj --include /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.PreparedSyntax.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreterPreparedJson.cs`
   - Result: Completed successfully

10. `dotnet format /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj --include /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxInterpreter/PreparedSyntaxTests.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxAbstract/JsonRoundtripTests.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxInterpreter/DictBuiltinTests.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxInterpreter/ValuesEqualInProcessTests.cs /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Internal/PineValueInProcessTests.cs`
    - Result: Completed successfully

11. `dotnet run --project /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj -- --filter-class="*PreparedSyntaxTests" --filter-class="*JsonRoundtripTests" --no-progress --no-ansi`
    - Result: Passed, 8 tests
    - Log: `artifacts/test-logs/Pine.Core.Tests/2026-09-20T11-46-30_filtered.log`

12. `dotnet format /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Pine.Core.csproj --include /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxInterpreter.PreparedSyntax.cs`
    - Result: Completed successfully

13. `dotnet format /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj --include /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Elm/ElmSyntax/ElmSyntaxInterpreter/PreparedSyntaxTests.cs`
    - Result: Completed successfully

14. `dotnet run --project /home/runner/work/super-duper-disco/super-duper-disco/implement/Pine.Core.Tests/Pine.Core.Tests.csproj -- --filter-class="*PreparedSyntaxTests" --filter-class="*CaseBlockTests" --no-progress --no-ansi`
    - Result: Passed, 10 tests
    - Log: `artifacts/test-logs/Pine.Core.Tests/2026-09-20T11-58-55_filtered.log`

15. `dotnet run -- --filter-method="*Prepared_case_dispatch_preserves_contiguous_constant_segments_around_var_bearing_patterns*" --no-progress --no-ansi`
    - Result: Passed, 1 test
    - Log: `artifacts/test-logs/Pine.Core.Tests/2026-09-20T12-05-10_filtered.log`

## Challenges and Backtracking

- Initial prepared semantic tests used unqualified `Just` / `Nothing` in direct interpreter entry expressions, which bypassed the module exposure context used inside source files. I corrected those checks to use `Test.Just` / `Test.Nothing`.
- A broader regression test still expected resolver output bodies to be `ElmSyntaxAbstract.Expression`. After the migration, the resolver correctly returns prepared expressions, so the test was updated to assert the prepared expression shape instead.
- The first follow-up focused test invocation used repeated `--filter-method` arguments, which only exercised one matching method under this test runner. I switched to focused class filters for `PreparedSyntaxTests` and `JsonRoundtripTests` to validate the whole changed surface.
- Review identified that the first mixed-dispatch test used disjoint constructors around the variable-bearing arm and therefore did not prove source-order behavior for overlapping branches. I strengthened it with an earlier variable-bearing `Any text` arm before `Any "constant"` and equivalent `[ 1, 2 ]` / `1 :: 2 :: []` constants.
- No other significant backtracking was needed.
