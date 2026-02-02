# Debugging Summary: Let Expression Formatting Issue

This document summarizes the discoveries, challenges, and improvement suggestions found during the debugging and fixing of the `Formats_let_block_with_uneven_indentation` test case.

## Problem Statement

The test `Formats_let_block_with_uneven_indentation` was failing. The test expected the formatter to produce output matching the classic avh4-elm-format behavior for Elm code with unusual indentation in let blocks.

### Input Code (simplified)
```elm
decl =
    Just <| (let
        _ =
            Test.Coverage.track 1225564788
     in
     False
    )
        || (let
                _ =
                    Test.Coverage.track 440883689
            in
            True
           )
```

### Expected Behavior
1. `Just <|` should have its right operand on a new line because it's a multiline expression
2. Nested let expressions inside parentheses should have proper indentation

## Key Discoveries

### Discovery 1: Parser Treating Input as Incomplete Declaration

**Initial Symptom**: Formatter was not changing the output at all.

**Investigation**: Adding debug code to the formatter revealed that:
- `declarations.Count = 0` 
- `incompleteDeclarations.Count = 1`

**Root Cause**: The parser was treating the entire declaration as an "incomplete declaration" because of the unusual let block indentation. When `let` appears inside parentheses on the same line (e.g., `(let`), the `let` keyword can be at a high column position (e.g., column 13), while its declarations are at lower column positions (e.g., column 5).

The original parser condition required let declarations to be MORE indented than the `let` keyword:
```csharp
while (
    NextTokenMatches(
        peek =>
        firstIdentifierToken.Range.Start.Column < peek.Start.Column &&
        peek.Lexeme is not "in"))
```

When `let` is at column 13 and the first declaration is at column 5, this condition fails immediately.

**Fix**: Modified the parser to use the minimum of `indentMin` and the `let` keyword column:
```csharp
var letDeclIndentCheck = System.Math.Min(indentMin, firstIdentifierToken.Range.Start.Column);
```

### Discovery 2: Formatter `CreateIndentedRef` Using Stale IndentSpaces

**Symptom**: After parser fix, the first let block formatted correctly, but the second let block had incorrect indentation.

**Investigation**: Debug output showed:
```
First let: context=(col=10, indent=8), indentedRef=(col=13, indent=12) ✓
Second let: context=(col=17, indent=12), indentedRef=(col=17, indent=16) ✗
```

The second let should have `indentedRef` at column 21, not 17.

**Root Cause**: The `CreateIndentedRef()` method used `SetIndentColumn()` which calculates position based on `IndentSpaces`:
```csharp
public FormattingContext SetIndentColumn() =>
    new(CurrentRow, 1 + IndentSpaces, IndentSpaces, Comments);
```

When entering nested contexts (like parentheses), `IndentSpaces` wasn't updated to reflect the new position, so `CreateIndentedRef()` calculated the wrong indent level.

**Fix**: Added a new method `CreateIndentedRefFromCurrentColumn()` that calculates indentation from the current column position rather than from `IndentSpaces`:
```csharp
public FormattingContext CreateIndentedRefFromCurrentColumn()
{
    var nextIndentColumn = GetNextMultipleOfFourColumn();
    return new FormattingContext(
        currentRow: CurrentRow,
        currentColumn: nextIndentColumn,
        indentSpaces: nextIndentColumn - 1,
        comments: Comments);
}
```

### Discovery 3: Multiline Detection for `<|` Operator

**Issue**: The `<|` operator wasn't putting its right operand on a new line when the right operand was multiline but started on the same line.

**Fix**: Added check for multiline right operand:
```csharp
var treatAsMultiline = rightOnNewLine ||
    (opApp.Operator.Value is "<|" && rightIsMultiline);
```

## Challenges Encountered

### Challenge 1: Debug Exceptions Not Being Thrown

During debugging, exceptions added to the formatter code were not being thrown, which initially seemed like a build caching issue. Investigation revealed that the exceptions were in methods that weren't being called because the parser was producing `IncompleteDeclaration` instead of regular declarations.

**Lesson**: When debugging formatter issues, first verify that the parser produces the expected AST structure.

### Challenge 2: Regression in Other Tests

The initial fix to `CreateIndentedRef()` (changing it to always use current column) caused regressions in module exposing list formatting. The exposing list expected indent based on `IndentSpaces` (column 5), but the new implementation calculated indent from the higher current column position (column 17).

**Solution**: Keep both behaviors:
- `CreateIndentedRef()` - original behavior for top-level/module formatting
- `CreateIndentedRefFromCurrentColumn()` - new behavior for nested expression contexts

### Challenge 3: Complex Indentation Rules in Elm

Elm has complex indentation rules that differ based on context:
- Top-level declarations use consistent 4-space indentation
- Expressions inside parentheses can have different base columns
- Let expressions can have their `in` keyword at various indentation levels

## Suggestions for Improvement

### 1. FormattingContext State Management

**Problem**: The `IndentSpaces` field in `FormattingContext` can become stale when entering nested contexts.

**Suggestion**: Consider updating `IndentSpaces` automatically when entering certain syntactic contexts (parentheses, records, etc.), or make the current column the primary source of truth for indentation calculations.

### 2. Parser Error Recovery

**Problem**: The parser silently converts unparseable declarations to `IncompleteDeclaration`, which the formatter then passes through unchanged.

**Suggestion**: 
- Add logging or telemetry for incomplete declarations
- Consider adding a "strict mode" that fails on incomplete declarations for testing
- Document which Elm patterns can cause incomplete declarations

### 3. Test Infrastructure

**Suggestion**: Add helper tests that:
- Verify the parser produces complete declarations for valid Elm code
- Print the AST structure alongside formatted output in failure messages
- Test specifically for nested let expressions in various contexts

### 4. Indentation Calculation Clarity

**Problem**: The relationship between `CurrentColumn`, `IndentSpaces`, and the various `*Ref` contexts is complex and not immediately obvious.

**Suggestion**: 
- Add more documentation explaining when to use each method
- Consider renaming methods to be more explicit (e.g., `CreateIndentedRefRelativeToStoredIndent` vs `CreateIndentedRefRelativeToCurrentPosition`)
- Add assertions or warnings when `IndentSpaces` and `CurrentColumn` diverge significantly

## Files Modified

1. **ElmSyntaxParser.cs** (Parser fix)
   - Modified let expression parsing to handle unusual indentation
   
2. **Avh4Format.cs** (Formatter fixes)
   - Added `CreateIndentedRefFromCurrentColumn()` method
   - Used new method for let expression formatting
   - Added multiline detection for `<|` operator with multiline right operand

## Test Results

- All 1115 tests pass
- 3 tests skipped (pre-existing skips)
- Target test `Formats_let_block_with_uneven_indentation` now passes
