# Analysis: Flattening Function Parameters into the Root Environment

## Summary

This document analyzes a proposed change to the environment composition for full function applications: instead of placing the parameters of the source function as a nested list at index `[1]` of the overall environment, place them at the root level, one item after the item which holds all env functions (and possibly closure data).

---

## Current Environment Layout

As described in the [elm-compiler-implementation-guide.md](elm-compiler-implementation-guide.md) section "Full Function Applications", the current environment for a full function application is a **2-item list**:

```
[ <envFunctions>, <arguments> ]
```

- **Index `[0]`**: A list of encoded function bodies needed for further applications (env functions). This includes all transitively referenced functions not inlined.
- **Index `[1]`**: A list of arguments from the source Elm code.

### How Parameters Are Accessed Today

In the compiler implementation (`BuiltinHelpers.cs`, line 16–19), the `BuildPathToParameter` method generates the access path `[1, parameterIndex]`:

```csharp
public static Expression BuildPathToParameter(int parameterIndex) =>
    ExpressionBuilder.BuildExpressionForPathInExpression(
        [1, parameterIndex],
        Expression.EnvironmentInstance);
```

Environment functions are accessed at `[0, functionIndex]` (see `ExpressionCompiler.cs`, lines 521–523):

```csharp
var functionRef =
    ExpressionBuilder.BuildExpressionForPathInExpression(
        [0, functionIndex],
        Expression.EnvironmentInstance);
```

### Concrete Example: Current Layout

Consider the following Elm function that has both dependencies (env functions) and multiple parameters:

```elm
module Example exposing (applyTwice)

double : Int -> Int
double x =
    Pine_builtin.int_mul [ x, 2 ]

applyTwice : Int -> Int -> Int
applyTwice a b =
    Pine_builtin.int_add [ double a, double b ]
```

Here, `applyTwice` has:
- **2 parameters**: `a` and `b`
- **1 env function dependency**: `double` (needed for the call inside the body)

#### Current Environment for a Call to `applyTwice`

When calling `applyTwice 3 5`, the environment composed by the caller is:

```
[                              -- overall env (2-item list)
  [ <encoded_double> ],        -- env[0]: list of env functions
  [ 3, 5 ]                     -- env[1]: list of arguments [a, b]
]
```

#### Access Paths in the Current Layout

| What                 | Access Path | Expression                         |
|----------------------|-------------|------------------------------------|
| Env function `double`| `[0, 0]`   | `head(head(env))`                  |
| Parameter `a`        | `[1, 0]`   | `head(head(skip(1, env)))`         |
| Parameter `b`        | `[1, 1]`   | `head(skip(1, head(skip(1, env))))` |

The path `[1, 0]` means: skip 1 item in the root list (past the env functions), take the head (the arguments list), then take the head of that (first argument). The path `[1, 1]` means: same first two steps, then skip 1 and take the head.

---

## Proposed New Layout

The proposed change **flattens the parameters into the root list**. Instead of nesting all arguments in a sub-list at index `[1]`, each parameter occupies its own slot at the root level, starting right after the env functions item:

```
[ <envFunctions>, <arg0>, <arg1>, ... , <argN> ]
```

### Concrete Example: Proposed Layout

Using the same `applyTwice 3 5` call:

```
[                              -- overall env (variable-length list)
  [ <encoded_double> ],        -- env[0]: list of env functions
  3,                            -- env[1]: parameter a
  5                             -- env[2]: parameter b
]
```

#### Access Paths in the Proposed Layout

| What                 | Access Path | Expression                         |
|----------------------|-------------|------------------------------------|
| Env function `double`| `[0, 0]`   | `head(head(env))`                  |
| Parameter `a`        | `[1]`      | `head(skip(1, env))`               |
| Parameter `b`        | `[2]`      | `head(skip(2, env))`               |

Each parameter is accessed with a **single-level path** `[N]` instead of the current **two-level path** `[1, N]`.

### Side-by-Side Comparison for the `factorial` Example

From the implementation guide, the recursive `factorial` function:

```elm
factorial : Int -> Int
factorial n =
    if Pine_builtin.int_is_sorted_asc [ n, 1 ] then
        1
    else
        Pine_builtin.int_mul
            [ factorial (Pine_builtin.int_add [ n, -1 ])
            , n
            ]
```

**Current layout** (recursive call environment):
```
[ [ current_env[0][0] ]                             -- env functions (factorial itself)
, [ Pine_builtin.int_add [ current_env[1][0], -1 ] ] -- arguments list (one item: n-1)
]
```

**Proposed layout** (recursive call environment):
```
[ [ current_env[0][0] ]                            -- env functions (factorial itself)
, Pine_builtin.int_add [ current_env[1], -1 ]       -- parameter n-1 (directly at root)
]
```

Note: in the current layout, `n` is accessed as `current_env[1][0]`; in the proposed layout, `n` is accessed as `current_env[1]`.

### Multi-Parameter Example with Dependencies

For the `applyTwice` example, the body accesses:

**Current form** — body expression references:
```
double  → env[0][0]
a       → env[1][0]
b       → env[1][1]
```

**Proposed form** — body expression references:
```
double  → env[0][0]
a       → env[1]
b       → env[2]
```

---

## Pros and Cons

### Pros of the Proposed Flat Layout

1. **Fewer runtime operations for parameter access**: Each parameter access saves one `head` operation. The current path `[1, N]` requires two `head` operations (one to extract the arguments sub-list, one to index into it), while the proposed path `[N+1]` requires only one `head` after a `skip`. For functions with many parameter accesses this compounds.

2. **Simpler parameter access expressions**: The generated Pine expressions for parameter access are shorter. `BuildPathToParameter(i)` becomes `BuildExpressionForPathInExpression([1 + i], env)` — a single-level path instead of two-level.

3. **More uniform with the "without env functions" case**: The existing `EmitFunctionValueWithoutEnvFunctions` path already uses a flat list of arguments (`[arg0, arg1, ...]`). The proposed layout makes the "with env functions" case structurally similar — just prepending the env functions item — reducing conceptual divergence.

4. **Simpler construction at call sites**: The caller no longer needs to wrap arguments in a sub-list. Instead of `List([envFuncs, List([arg0, arg1, ...])])`, it becomes `List([envFuncs, arg0, arg1, ...])`, saving one list construction.

### Cons of the Proposed Flat Layout

1. **Variable-length root list**: The current layout always has exactly 2 items at the root, making it easy for tooling (profilers, snapshot tests, `FunctionRecord` parsing) to identify the structure. The proposed layout has `1 + paramCount` items, which varies per function and requires knowing the parameter count to parse.

2. **Tooling and inspection impact**: The `FunctionRecord.cs` parser (`ParseFunctionRecordTagged`, line 254) currently identifies the "WithEnvFunctions" format by checking `envList.Items.Count is 2`. With the proposed change, this check would need to accept any count ≥ 1, making format detection more ambiguous. Distinguishing between "WithEnvFunctions" and "WithoutEnvFunctions" formats becomes harder.

3. **Function value wrapper complexity**: The `FunctionValueBuilder` builds curried wrappers that accumulate arguments. The innermost level currently creates `[envFuncs, concat(captured, [lastArg])]`. In the proposed form, the innermost level would create `concat([envFuncs], captured, [lastArg])` — the env functions item must be prepended to a flat list rather than placed as a peer of a single arguments sub-list. This slightly complicates the wrapper construction.

4. **Breaking change scope**: Multiple components depend on the current `[1, N]` parameter indexing:
   - `BuiltinHelpers.BuildPathToParameter`
   - `ExpressionCompiler.CompileApplication` (call environment construction)
   - `FunctionValueBuilder` (innermost expression, env structure encoding)
   - `FunctionRecord.cs` (parsing/recognition of function formats)
   - Snapshot tests that validate emitted Pine expressions
   - Any profiling or inspection tooling that parses the 2-item structure

5. **Env functions access unchanged**: The env functions are still accessed at `[0, index]`, so the primary benefit is only for parameter access. If a function body accesses env functions more often than parameters (e.g., calling many helper functions), the benefit is smaller.

---

## Implementation Strategy

### Step 1: Update `BuildPathToParameter`

Change the parameter access path from `[1, parameterIndex]` to `[1 + parameterIndex]`:

```csharp
// In BuiltinHelpers.cs
public static Expression BuildPathToParameter(int parameterIndex) =>
    ExpressionBuilder.BuildExpressionForPathInExpression(
        [1 + parameterIndex],
        Expression.EnvironmentInstance);
```

### Step 2: Update Call Environment Construction in `ExpressionCompiler`

In `ExpressionCompiler.cs` (around line 564–569), change the call environment from:

```csharp
var callEnvironment =
    Expression.ListInstance(
        [
        callEnvFunctions,
        Expression.ListInstance(fullApplicationArgs)
        ]);
```

to:

```csharp
var callEnvironment =
    Expression.ListInstance(
        [callEnvFunctions, .. fullApplicationArgs]);
```

This flattens `fullApplicationArgs` directly into the root list after `callEnvFunctions`.

### Step 3: Update `FunctionValueBuilder` Innermost Expression

In `FunctionValueBuilder.cs`, the `BuildInnermostExpression` method (line 597–624) currently builds:

```csharp
var invocationEnv = Expression.ListInstance([envFuncsExpr, fullArgsExpr]);
```

Change this to prepend `envFuncsExpr` to the flat list of arguments:

```csharp
var invocationEnv =
    BuiltinAppConcatBinary(
        Expression.ListInstance([envFuncsExpr]),
        fullArgsExpr);
```

Or equivalently, restructure the innermost expression to produce `[envFuncs, arg0, arg1, ..., argN-1]`.

### Step 4: Update `FunctionRecord` Parsing

In `FunctionRecord.cs` (around line 254), update the format detection logic. Instead of requiring exactly 2 items, detect the "WithEnvFunctions" format by checking whether the first item can be parsed as an env functions list and the remaining items are parameters. The parameter count would be `envList.Items.Count - 1`.

### Step 5: Update `EmitFunctionExpression` and Related Methods

The `EmitFunctionExpression` method and the encoding/decoding helpers in `FunctionValueBuilder.cs` (lines 115–170, 340–435) that build the static encoding of environment structures need to produce the flat form.

### Step 6: Update Snapshot Tests and Inspection Tooling

Any snapshot tests that validate the shape of emitted Pine expressions will need updating to expect the new flat structure. Profiling and inspection tooling that relies on the 2-item root structure must be updated to handle the variable-length root.

### Step 7: Validate

Run the full test suite to verify correctness. From the `implement/Pine.Core.Tests` directory:
```
dotnet run
```

Verify that all existing Elm programs compile and run correctly with the new layout, paying special attention to:
- Recursive functions (env function forwarding)
- Multi-parameter functions
- Partial applications (the curried wrapper chain must produce the correct flat structure on the final application)
- Over-applications (remaining arguments applied generically after the full application)
- Function values escaping their definition context

---

## Conclusion

The proposed flat layout simplifies parameter access and reduces runtime overhead per parameter access by one `head` operation. The main tradeoffs are increased complexity in tooling that parses environment structures (since the root list length becomes variable) and a broader scope of changes across the compiler pipeline. The implementation is straightforward but touches several components, making it a moderate-effort change that benefits from thorough testing.
