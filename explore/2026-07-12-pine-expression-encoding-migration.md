# 2026-07-12 Pine Expression Encoding Migration

Goal is to switch the encoding of Pine expression to a more compact format, mostly as already introduced in [2025-11-26-dense-expression-encoding-implementation.md](./2025-11-26-dense-expression-encoding-implementation.md)

## Backward Compatibility for Incremental Migration

We do not want to wait for a step to migrate all at once. Therefore, we switch to the new encoding while keeping temporary backward compatibility around for now, for values loaded from various storage locations at runtime.

This means the public API for parsing already available today and referenced throughout the project will attempt to parse both the new and old formats.

### Literal Confusion

This approach of incremental migration introduces a challenge: when the encoded expression is a `Literal` at the root, both parsers will succeed, yielding different literal values depending on which parser is tried first. To avoid this confusion, we switch the label to `Litral` in the 2026 format.

## 2026 Encoding Specification

The UTF32-encoded labels are the following:

+ `List`
+ `Litral`
+ `Builtin`
+ `Condition`
+ `Environment`
+ `Eval`
+ `Label`

### Label Value

In contrast to 2024, in 2026 format, the `Label` expression accepts any PineValue as the label, not just strings.

## Migration Process

This migration process covers only C# implementations. The Elm-based implementations remain as is for now. This works because encoded expressions flow only from Elm-based implementations to C#-based implementations, not the other way.

For the delegates and caches reachable or injectable via the public API, we forward the variant specific to the default encoding. After switching to the 2026 format, the public API forwards the caches and delegates only for the 2026 format. The joint entry point then supplies `static` scoped dictionaries when invoking the parser for the 2024 format. This means there will be some memory overhead for applications that use this path.

1. Add and implement both `ExpressionEncoding2026.cs` and `ExpressionEncoding2026Tests.cs` completely.
2. Move old encoding and parsing methods to `ExpressionEncoding2024.cs` and add `ExpressionEncoding.cs` with same encoding and parsing entry points for consumers: `ExpressionEncoding.EncodeExpressionAsValue` invokes `ExpressionEncoding2024.EncodeExpressionAsValue` and `ExpressionEncoding.ParseExpressionFromValue` invokes `ExpressionEncoding2024.ParseExpressionFromValue` so that all consumers can remain the same.
3. Change `ExpressionEncoding.EncodeExpressionAsValue` to invoke `ExpressionEncoding2026.EncodeExpressionAsValue`
4. Change `FunctionValueBuilder.cs` to emit the 2026 encoding.
5. Update performance counters in snapshots.
