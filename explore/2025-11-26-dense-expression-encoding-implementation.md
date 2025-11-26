# 2025-11-26 - Dense Expression Encoding Implementation

Since we gave up the symmetry between file tree encoding and expression encoding, we might as well experiment with further optimizing expression encoding for efficiency.

## Overview

This document outlines a plan for implementing a more dense encoding (2025 format) for Pine expressions. The new encoding would replace the current encoding (2024 format) while maintaining backwards compatibility through automatic fallback parsing.

## Current State Analysis

### Current Encoding Format (2024)

The current encoding uses a choice type representation with string tags:

```
ListValue [ tagNameValue, ListValue [ arguments ] ]
```

Where:
- `tagNameValue` is a UTF-32 encoded string (e.g., "Literal", "List", "ParseAndEval", "KernelApplication", "Conditional", "Environment", "StringTag")
- Arguments are wrapped in a ListValue

**Inefficiencies in 2024 encoding:**

1. **Nested list wrapper**: Extra ListValue wrapping for arguments adds overhead and increases nesting depth
2. **Redundant wrapping**: For expressions with arguments, the arguments are wrapped in an additional ListValue unnecessarily

### Expression Variants to Encode

Based on the Expression type in both Elm and C#:

1. **Literal** - Contains a single PineValue
2. **List** - Contains a list of Expression items  
3. **ParseAndEval** - Contains two Expression fields (encoded, environment)
4. **KernelApplication** (2024) / **BuiltinApp** (2025) - Contains a function name (string) and an input Expression
5. **Conditional** - Contains three Expression fields (condition, falseBranch, trueBranch)
6. **Environment** - No fields
7. **StringTag** (2024) / **Label** (2025) - Contains a label value (any PineValue in 2025, string in 2024) and a labeled Expression

### Current Implementation Locations

**Elm:**
- File: `/implement/pine/Elm/elm-compiler/src/Pine.elm`
- Encoding function: `encodeExpressionAsValue` (line 1399)
- Decoding function: `parseExpressionFromValue` (line 1472)
- Helper functions: `encodeUnionToPineValue`, `encodeListExpressionAsValueReversed`

**C#:**
- Primary file: `/implement/Pine.Core/CommonEncodings/ExpressionEncoding.cs`
- Relay file: `/implement/Pine.Core/PopularEncodings/ExpressionEncoding.cs`
- Encoding method: `EncodeExpressionAsValue`
- Decoding method: `ParseExpressionFromValue` (two overloads)
- Helper methods: `EncodeChoiceTypeVariantAsPineValue`, various Parse* methods

### Current API Consumers

**Direct consumers identified:**
- `Pine.Core/CodeAnalysis/PineVMParseCache.cs` - Caches parsed expressions
- `Pine.Core/CodeAnalysis/CodeAnalysis.cs`
- `Pine.Core/CodeAnalysis/FunctionRecord.cs`
- `Pine.Core/CodeAnalysis/NamesFromCompiledEnv.cs`
- `pine/Pine/PineVM/ProfilingPineVM.cs` - VM profiling
- `pine/Pine/PineVM/Precompiled.cs` - Precompiled expressions
- `pine/Pine/CompilePineToDotNet/CompileToCSharp.cs`
- `pine/ElmInteractive/InteractiveSessionPine.cs`
- `Pine.Core/DotNet/StaticProgramCSharp.cs`
- `Pine.Core/PineValue.cs`

**Test consumers:**
- `/implement/Pine.IntegrationTests/PineVMEncodeExpressionTests.cs`
- `/implement/Pine.Core.Tests/CodeAnalysis/ReducePineExpressionTests.cs`

## Proposed Dense Encoding (2025 Format)

### Tag Values (2025)

The 2025 format introduces renamed tags for better alignment with popular language:

```
"Literal" = UTF-32 encoded string (unchanged from 2024)
"List" = UTF-32 encoded string (unchanged from 2024)
"ParseAndEval" = UTF-32 encoded string (unchanged from 2024)
"BuiltinApp" = UTF-32 encoded string (renamed from "KernelApplication" in 2024)
"Conditional" = UTF-32 encoded string (unchanged from 2024)
"Environment" = UTF-32 encoded string (unchanged from 2024)
"Label" = UTF-32 encoded string (renamed from "StringTag" in 2024)
```

Tags are represented as blob values containing the UTF-32 encoded string.

### Encoding Structure (2025)

**General format:**
```
ListValue [ tagValue, ...arguments ]
```

Where `tagValue` is the same UTF-32 encoded string blob used in 2024 format.

**Key difference from 2024**: Arguments are placed directly in the ListValue, not wrapped in an additional nested ListValue.

**Per-variant encoding:**

1. **Literal**: `ListValue [ stringTag("Literal"), literalValue ]`
2. **List**: `ListValue [ stringTag("List"), ...encodedItems ]` - items are directly appended (not wrapped in nested ListValue)
   - **Empty list**: `ListValue [ stringTag("List") ]` (no items after tag)
3. **ParseAndEval**: `ListValue [ stringTag("ParseAndEval"), encodedExpr, envExpr ]`
4. **BuiltinApp**: `ListValue [ stringTag("BuiltinApp"), functionNameValue, inputExpr ]`
5. **Conditional**: `ListValue [ stringTag("Conditional"), conditionExpr, falseBranchExpr, trueBranchExpr ]`
6. **Environment**: `ListValue [ stringTag("Environment") ]` (no arguments)
7. **Label**: `ListValue [ stringTag("Label"), labelValue, labeledExpr ]`

**Design Decisions:**
- **Empty list handling**: Empty lists are encoded as `ListValue [ stringTag("List") ]` with no items after the tag
- **Tag renaming**: 2025 format renames "KernelApplication" to "BuiltinApp" and "StringTag" to "Label"
- **Label value type**: In 2025 format, the Label expression accepts any PineValue as the label, not just strings

## Appendices

### A. Encoding Format Comparison

**Environment Expression:**
```
2024 format: ListValue [
  BlobValue [0,0,0,69, 0,0,0,110, 0,0,0,118, ...],  // "Environment" in UTF-32
  ListValue []
]

2025 format: ListValue [
  BlobValue [0,0,0,69, 0,0,0,110, 0,0,0,118, ...]   // "Environment" in UTF-32
]
```
Note: Same tag value, but eliminates the nested empty ListValue.

**Literal Expression:**
```
2024 format: ListValue [
  BlobValue [0,0,0,76, 0,0,0,105, 0,0,0,116, ...],  // "Literal" in UTF-32
  ListValue [ <literal-value> ]
]

2025 format: ListValue [
  BlobValue [0,0,0,76, 0,0,0,105, 0,0,0,116, ...],  // "Literal" in UTF-32
  <literal-value>
]
```
Note: Same tag value, but argument placed directly (not in nested ListValue).

**List Expression (with 3 items):**
```
2024 format: ListValue [
  BlobValue [0,0,0,76, 0,0,0,105, 0,0,0,115, 0,0,0,116],  // "List" in UTF-32
  ListValue [ ListValue [ <item1-value>, <item2-value>, <item3-value> ] ]
]

2025 format: ListValue [
  BlobValue [0,0,0,76, 0,0,0,105, 0,0,0,115, 0,0,0,116],  // "List" in UTF-32
  <item1-value>, <item2-value>, <item3-value>
]
```
Note: Same tag value. Items are flattened directly after tag, not double-nested.

**Empty List Expression:**
```
2024 format: ListValue [
  BlobValue [0,0,0,76, 0,0,0,105, 0,0,0,115, 0,0,0,116],  // "List" in UTF-32
  ListValue [ ListValue [] ]
]

2025 format: ListValue [
  BlobValue [0,0,0,76, 0,0,0,105, 0,0,0,115, 0,0,0,116]   // "List" in UTF-32
]
```
Note: Same tag value. Empty list has only the tag, no items after it.

**KernelApplication Expression (2024) / BuiltinApp Expression (2025):**
```
2024 format: ListValue [
  BlobValue [0,0,0,75, 0,0,0,101, 0,0,0,114, ...],  // "KernelApplication" in UTF-32
  ListValue [ <function-name-value>, <input-expr-value> ]
]

2025 format: ListValue [
  BlobValue [0,0,0,66, 0,0,0,117, 0,0,0,105, ...],  // "BuiltinApp" in UTF-32
  <function-name-value>, <input-expr-value>
]
```

Note: The main improvement is reducing nesting depth by eliminating the argument wrapper ListValue nodes, which improves both size (fewer nodes) and performance (less indirection). The tag renaming provides additional savings.

### C. References

- Current Expression type definition: `/implement/Pine.Core/Expression.cs`
- Current PineValue type: `/implement/Pine.Core/PineValue.cs`
- Elm Pine module: `/implement/pine/Elm/elm-compiler/src/Pine.elm`
- Existing encoding tests: `/implement/Pine.IntegrationTests/PineVMEncodeExpressionTests.cs`
