using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Helpers to analyze Pine <see cref="Expression"/> trees with respect to their relationship to the environment.
/// </summary>
public class CodeAnalysis
{
    /// <summary>
    /// Returns the value at the specified <paramref name="path"/> within the given <paramref name="environment"/> value.
    /// The path is interpreted as a sequence of indices, where each index selects an element from a list or skips bytes in a blob.
    /// Returns <c>null</c> if the path cannot be resolved in the given environment.
    /// </summary>
    /// <param name="environment">The root <see cref="PineValue"/> to traverse.</param>
    /// <param name="path">A span of indices representing the path to traverse within the environment.</param>
    /// <returns>
    /// The <see cref="PineValue"/> at the specified path, or <c>null</c> if the path is invalid.
    /// </returns>
    public static PineValue? ValueFromPathInValue(
        PineValue environment,
        ReadOnlySpan<int> path)
    {
        if (path.Length is 0)
            return environment;

        if (environment is PineValue.BlobValue blobValue)
        {
            if (1 < path.Length)
                return null;

            return KernelFunctionSpecialized.skip(path[0], blobValue);
        }

        if (environment is not PineValue.ListValue listValue)
            return null;

        if (path[0] < 0)
            return null;

        if (path[0] >= listValue.Items.Length)
            return null;

        return ValueFromPathInValue(listValue.Items.Span[path[0]], path[1..]);
    }

    /// <summary>
    /// Returns a mapping for the given expression if it corresponds to a path from the environment root,
    /// otherwise returns <c>null</c>.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <returns>
    /// <see cref="ExprMappedToParentEnv.PathInParentEnv"/> with the path indices when the expression is composed of
    /// environment access via skip/head, a <see cref="ExprMappedToParentEnv.LiteralInParentEnv"/> for literals,
    /// or <c>null</c> when it cannot be mapped.
    /// </returns>
    public static ExprMappedToParentEnv? TryParseExpressionAsIndexPathFromEnv(Expression expression)
    {
        return
            TryParseExpressionAsIndexPath(
                pathExpression: expression,
                rootExpression: Expression.EnvironmentInstance);
    }

    /// <summary>
    /// Tries to interpret <paramref name="pathExpression"/> as a path relative to <paramref name="rootExpression"/>,
    /// following the Pine built-ins <c>skip</c> and <c>head</c>.
    /// </summary>
    /// <param name="pathExpression">The expression to interpret as a path.</param>
    /// <param name="rootExpression">The root expression considered as the origin of the path.</param>
    /// <returns>
    /// <see cref="ExprMappedToParentEnv.PathInParentEnv"/> for a recognized path, <see cref="ExprMappedToParentEnv.LiteralInParentEnv"/> for literals,
    /// or <c>null</c> if the expression does not match a supported pattern.
    /// </returns>
    public static ExprMappedToParentEnv? TryParseExpressionAsIndexPath(
        Expression pathExpression,
        Expression rootExpression)
    {
        if (pathExpression == rootExpression)
            return new ExprMappedToParentEnv.PathInParentEnv([]);

        if (pathExpression is Expression.Literal literal)
            return new ExprMappedToParentEnv.LiteralInParentEnv(literal.Value);

        if (pathExpression is Expression.StringTag stringTagExpr)
            return TryParseExpressionAsIndexPath(stringTagExpr.Tagged, rootExpression);

        if (pathExpression is not Expression.KernelApplication kernelApplication)
            return null;

        if (kernelApplication.Function is not nameof(KernelFunction.head))
            return null;

        if (kernelApplication.Input is Expression.KernelApplication inputKernelApplication &&
            inputKernelApplication.Function is nameof(KernelFunction.skip))
        {
            if (inputKernelApplication.Input is not Expression.List skipInputList)
                return null;

            if (skipInputList.items.Count is not 2)
                return null;

            if (skipInputList.items[0] is not Expression.Literal skipCountLiteral)
                return null;

            if (TryParseExpressionAsIndexPath(skipInputList.items[1], rootExpression) is not ExprMappedToParentEnv.PathInParentEnv pathPrefix)
                return null;

            return
                KernelFunction.SignedIntegerFromValueRelaxed(skipCountLiteral.Value) is { } skipValue
                ?
                new ExprMappedToParentEnv.PathInParentEnv([.. pathPrefix.Path, (int)skipValue])
                :
                null;
        }

        {
            if (TryParseExpressionAsIndexPath(kernelApplication.Input, rootExpression) is not ExprMappedToParentEnv.PathInParentEnv pathPrefix)
                return null;

            return new ExprMappedToParentEnv.PathInParentEnv([.. pathPrefix.Path, 0]);
        }
    }


    public static Result<string, PineValueClass> MinimalValueClassForStaticProgram(
        Expression expression,
        PineValueClass availableEnvironment)
    {
        var allParseAndEvalExpressions =
            Expression.EnumerateSelfAndDescendants(expression)
            .OfType<Expression.ParseAndEval>()
            .ToImmutableArray();

        var parsedItems = new Dictionary<IReadOnlyList<int>, PineValue>();

        foreach (var parseAndEval in allParseAndEvalExpressions)
        {
            if (parseAndEval.Environment.ReferencesEnvironment)
            {
                return "Not implemented: parseAndEval.Environment.ReferencesEnvironment";
            }

            if (!parseAndEval.Encoded.ReferencesEnvironment)
            {
                continue;
            }

            if (TryParseExpressionAsIndexPathFromEnv(parseAndEval.Encoded) is not ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
            {
                return "Could not interpret parseAndEval.Encoded as path from environment";
            }

            if (availableEnvironment.TryGetValue(pathInParentEnv.Path) is not { } valueAtPath)
            {
                return "Environment does not contain value at path required by parseAndEval";
            }

            parsedItems[pathInParentEnv.Path] = valueAtPath;
        }

        return
            PineValueClass.Create([.. parsedItems]);
    }
}
