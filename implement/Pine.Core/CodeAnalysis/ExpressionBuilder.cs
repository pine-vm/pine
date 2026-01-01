using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Shared utility methods for building Pine expressions.
/// Used by <see cref="FunctionValueBuilder"/> and <see cref="FunctionRecord"/> for consistent expression construction.
/// </summary>
public static class ExpressionBuilder
{
    /// <summary>
    /// Returns a sub-expression of <paramref name="expression"/> corresponding to <paramref name="path"/>, if resolvable.
    /// This overload accepts a read-only list for ergonomics when the caller already has a list instance.
    /// </summary>
    /// <param name="expression">Root expression to traverse. List nodes are navigated by index.</param>
    /// <param name="path">Sequence of indices selecting nested list items. An empty path returns <paramref name="expression"/>.</param>
    /// <returns>
    /// The concrete <see cref="Expression"/> reached by the path, or <c>null</c> if any index is out of range.
    /// </returns>
    public static Expression? ExpressionForPathInExpression(
        Expression expression,
        IReadOnlyList<int> path) =>
        ExpressionForPathInExpression(expression, [.. path]);

    /// <summary>
    /// Returns a sub-expression of <paramref name="expression"/> corresponding to <paramref name="path"/>, if resolvable.
    /// The traversal first descends through contiguous <see cref="Expression.List"/> nodes while possible.
    /// Any remaining indices (when a non-list node is reached early) are reified into a chain of <c>skip/head</c> kernel applications
    /// via <see cref="BuildExpressionForPathInExpression(ReadOnlySpan{int}, Expression)"/> so that the resulting expression still denotes
    /// the requested position relative to the last concrete node visited.
    /// </summary>
    /// <param name="expression">Root expression to traverse.</param>
    /// <param name="path">Sequence of indices selecting nested list items. Empty path returns <paramref name="expression"/>.</param>
    /// <returns>
    /// The concrete or synthesized <see cref="Expression"/> matching the path, or <c>null</c> if an index is out of range while descending list nodes.
    /// </returns>
    /// <example>
    /// If <paramref name="expression"/> is a list <c>[A,B,C]</c> and <paramref name="path"/> is <c>[1]</c>, returns <c>B</c>.
    /// If <paramref name="expression"/> is <c>A</c> (non-list) and <paramref name="path"/> is <c>[2,0]</c>, returns an expression equivalent to <c>head (skip 2 A)</c> then another <c>head</c> for index 0.
    /// </example>
    public static Expression? ExpressionForPathInExpression(
        Expression expression,
        ReadOnlySpan<int> path)
    {
        var current = expression;

        var itemIndexAfterReduction = 0;

        for (; itemIndexAfterReduction < path.Length; itemIndexAfterReduction++)
        {
            if (current is Expression.List listExpr)
            {
                if (path[itemIndexAfterReduction] < 0 || path[itemIndexAfterReduction] >= listExpr.Items.Count)
                    return null;

                current = listExpr.Items[path[itemIndexAfterReduction]];
            }
            else
            {
                break;
            }
        }

        var pathRemaining = path[itemIndexAfterReduction..];

        return
            BuildExpressionForPathInExpression(pathRemaining, current);
    }

    /// <summary>
    /// Builds (or completes) an expression that selects a nested element identified by <paramref name="path"/> starting from <paramref name="expression"/>.
    /// For each index in <paramref name="path"/>, constructs the equivalent Pine kernel expression using <c>skip</c> (when index &gt; 0) followed by <c>head</c>.
    /// </summary>
    /// <param name="path">Sequence of indices to navigate. Empty path returns <paramref name="expression"/> unchanged.</param>
    /// <param name="expression">The starting expression. This becomes the base for the generated access chain.</param>
    /// <returns>An <see cref="Expression"/> that, when evaluated, yields the value at the requested path.</returns>
    /// <remarks>
    /// This method does not validate that <paramref name="expression"/> structurally supports the path at runtime; it only encodes the navigation.
    /// It is typically used after partially consuming a path through concrete list nesting where remaining indices must be represented symbolically.
    /// </remarks>
    public static Expression BuildExpressionForPathInExpression(
        ReadOnlySpan<int> path,
        Expression expression)
    {
        var current = expression;

        for (var i = 0; i < path.Length; i++)
        {
            var nextOffset = path[i];

            var skipExpr =
                nextOffset is 0
                ?
                current
                :
                Expression.KernelApplicationInstance(
                    function: "skip",
                    input:
                    Expression.ListInstance(
                        items: [new Expression.Literal(IntegerEncoding.EncodeSignedInteger(nextOffset)), current]));

            var headExpr =
                Expression.KernelApplicationInstance(
                    function: "head",
                    input: skipExpr);

            current = headExpr;
        }

        return current;
    }

    /// <summary>
    /// Creates a literal List expression containing the given values.
    /// </summary>
    /// <param name="values">The values to include in the list expression.</param>
    /// <returns>A List expression containing literal expressions for each value.</returns>
    public static Expression CreateLiteralList(IReadOnlyList<PineValue> values)
    {
        if (values.Count is 0)
            return Expression.EmptyList;

        var items = new Expression[values.Count];

        for (var i = 0; i < values.Count; i++)
        {
            items[i] = Expression.LiteralInstance(values[i]);
        }

        return Expression.ListInstance(items);
    }
}
