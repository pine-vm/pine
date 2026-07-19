using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// The standard encoding of Pine expressions as Pine values.
/// Emits the compact 2026 format and accepts both the 2026 and legacy 2024 formats.
/// </summary>
public static class ExpressionEncoding
{
    public static PineValue.ListValue EncodeExpressionAsValue(Expression expression) =>
        EncodeExpressionAsValue(expression, cache: null);

    public static PineValue.ListValue EncodeExpressionAsValue(
        Expression expression,
        PineExpressionEncodingCache? cache) =>
        cache is null
        ?
        ExpressionEncoding2026.EncodeExpressionAsValue(expression)
        :
        cache.EncodeExpressionAsValue(expression);

    internal static PineValue.ListValue EncodeExpressionAsValueViaPostOrder(
        Expression rootExpression,
        ConcurrentDictionary<Expression, PineValue.ListValue> store) =>
        ExpressionEncoding2026.EncodeExpressionAsValueViaPostOrder(rootExpression, store);

    public static PineValue.ListValue EncodeExpressionAsValueWithoutTopLevelCacheLookup(
        Expression expression,
        Func<Expression, PineValue.ListValue> encodeSubexpression) =>
        ExpressionEncoding2026.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
            expression,
            encodeSubexpression);

    public static Result<string, Expression> ParseExpressionFromValue(PineValue value)
    {
        var parsed2026 =
            ExpressionEncoding2026.ParseExpressionFromValueWithoutResultAllocation(value);

        if (parsed2026.IsOk)
            return parsed2026.ToPublicResult();

        return ExpressionEncoding2024.ParseExpressionFromValue(value);
    }

    internal static ExpressionEncoding2026.ParseExpressionResult ParseExpressionFromValueViaPostOrder(
        PineValue rootValue,
        IDictionary<PineValue, ExpressionEncoding2026.ParseExpressionResult> store)
    {
        var parsed2026 =
            ExpressionEncoding2026.ParseExpressionFromValueViaPostOrder(
                rootValue,
                store);

        if (parsed2026.IsOk)
            return parsed2026;

        var parsed2024 = ExpressionEncoding2024.ParseExpressionFromValue(rootValue);

        var parsed2024Struct =
            ExpressionEncoding2026.ParseExpressionResult.FromPublicResult(parsed2024);

        store[rootValue] = parsed2024Struct;

        return parsed2024Struct;
    }

    public static Result<string, Expression> ParseExpressionFromValue(
        PineValue value,
        Func<PineValue, Result<string, Expression>> generalParser)
    {
        var parsed2026 =
            ExpressionEncoding2026.ParseExpressionFromValue(
                value,
                generalParser);

        if (parsed2026.IsOkOrNull() is not null)
            return parsed2026;

        return ExpressionEncoding2024.ParseExpressionFromValue(value);
    }
}
