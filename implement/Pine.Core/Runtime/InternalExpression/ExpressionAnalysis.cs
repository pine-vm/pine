using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;

using BuiltinId = Pine.Core.Runtime.InternalExpression.BuiltinFunction;
using Kernel = Pine.Core.PineLanguage.BuiltinFunction;

namespace Pine.Core.Runtime.InternalExpression;

internal static class ExpressionAnalysis
{
    public static IReadOnlyList<int>? TryParseExprAsPathInEnv(
        Expression pathExpression)
    {
        if (!pathExpression.ReferencesEnvironment)
            return null;

        return
            TryParseExprAsPathInExpr(
                pathExpression,
                rootExpression: Expression.EnvironmentInstance);
    }

    /// <summary>
    /// Tries to interpret <paramref name="pathExpression"/> as a path relative to <paramref name="rootExpression"/>,
    /// as a combination of Pine kernel applications <c>head</c> and <c>skip</c>.
    /// </summary>
    /// <param name="pathExpression">The expression to interpret as a path.</param>
    /// <param name="rootExpression">The root expression considered as the origin of the path.</param>
    public static IReadOnlyList<int>? TryParseExprAsPathInExpr(
        Expression pathExpression,
        Expression rootExpression)
    {
        if (pathExpression.ConditionCount > 0)
        {
            return null;
        }

        if (pathExpression.EvalCount > 0)
        {
            return null;
        }

        if (pathExpression == rootExpression)
            return [];

        var pathSegments = new List<int>();

        var currentExpression = pathExpression;

        while (true)
        {
            if (currentExpression == rootExpression)
            {
                pathSegments.Reverse();

                return pathSegments;
            }

            if (currentExpression is Expression.HeadSkip head)
            {
                pathSegments.Add(Math.Max(0, head.Count));
                currentExpression = head.Input;
                continue;
            }

            if (currentExpression is not Expression.Builtin kernelApplication)
                return null;

            if (kernelApplication.Function is not BuiltinId.Head)
                return null;

            if (kernelApplication.Input is Expression.BuiltinBinary { Function: BuiltinId.Skip } binary)
            {
                if (binary.Left is not Expression.Literal literal ||
                    Kernel.SignedIntegerFromValueRelaxed(literal.Value) is not { } count || count > int.MaxValue)
                {
                    return null;
                }

                pathSegments.Add(count < 0 ? 0 : (int)count);
                currentExpression = binary.Right;
            }
            else if (kernelApplication.Input is Expression.Builtin inputKernelApplication &&
                inputKernelApplication.Function is BuiltinId.Skip)
            {
                if (inputKernelApplication.Input is not Expression.List skipInputList)
                    return null;

                if (skipInputList.Items.Length is not 2)
                    return null;

                if (skipInputList.Items[0] is not Expression.Literal skipCountLiteral)
                    return null;

                if (Kernel.SignedIntegerFromValueRelaxed(skipCountLiteral.Value) is not { } skipValue ||
                    skipValue > int.MaxValue)
                {
                    return null;
                }

                pathSegments.Add(skipValue < 0 ? 0 : (int)skipValue);

                currentExpression = skipInputList.Items[1];
            }
            else
            {
                pathSegments.Add(0);

                currentExpression = kernelApplication.Input;
            }
        }
    }

    /// <summary>
    /// Returns the literal <see cref="PineValue"/> carried by <paramref name="expression"/>,
    /// unfolding <see cref="Expression.Label"/> layers when present.
    /// </summary>
    /// <param name="expression">Expression to inspect for an embedded literal value.</param>
    /// <returns>The literal value when the expression represents one; otherwise <c>null</c>.</returns>
    public static PineValue? TryParseAsLiteral(Expression expression)
    {
        if (expression is Expression.Literal literal)
        {
            return literal.Value;
        }

        if (expression is Expression.Label stringTagExpr)
        {
            return TryParseAsLiteral(stringTagExpr.Tagged);
        }

        return null;
    }

    /// <summary>
    /// Builds the generic incremental form of a function application as emitted by the
    /// frontend compilers (for example the Elm compiler in
    /// <c>Pine.Core.Elm.ElmCompilerInDotnet.ExpressionCompiler</c>).
    /// <para>
    /// The result is a chain of nested <see cref="Expression.Eval"/> expressions,
    /// one per supplied argument, applying arguments to <paramref name="functionExpr"/>
    /// from left to right. With zero arguments the input <paramref name="functionExpr"/>
    /// is returned unchanged.
    /// </para>
    /// <para>
    /// This is the inverse of
    /// <see cref="ParseGenericFunctionApplication(Expression)"/>.
    /// </para>
    /// </summary>
    /// <param name="functionExpr">The expression representing the function value.</param>
    /// <param name="arguments">The arguments to apply, in application order.</param>
    /// <returns>
    /// The chain of nested <see cref="Expression.Eval"/> wrappers, or
    /// <paramref name="functionExpr"/> itself when <paramref name="arguments"/> is empty.
    /// </returns>
    public static Expression BuildGenericFunctionApplication(
        Expression functionExpr,
        IReadOnlyList<Expression> arguments)
    {
        var result = functionExpr;

        for (var i = 0; i < arguments.Count; ++i)
        {
            result =
                new Expression.Eval(
                    encoded: result,
                    environment: arguments[i]);
        }

        return result;
    }

    /// <summary>
    /// Inverse of <see cref="BuildGenericFunctionApplication(Expression, IReadOnlyList{Expression})"/>.
    /// <para>
    /// Recognizes a chain of nested <see cref="Expression.Eval"/> expressions
    /// of the form
    /// <c>ParseAndEval(... ParseAndEval(ParseAndEval(F, a0), a1) ..., a_{n-1})</c>
    /// and returns the function expression <c>F</c> together with the list of
    /// arguments <c>[a0, a1, ..., a_{n-1}]</c> in application order.
    /// </para>
    /// <para>
    /// Returns <c>null</c> when <paramref name="expression"/> is not a
    /// <see cref="Expression.Eval"/> (i.e. when no arguments would be
    /// recovered). For any <see cref="Expression.Eval"/> the result has
    /// at least one argument.
    /// </para>
    /// </summary>
    /// <param name="expression">The expression to interpret as a generic application chain.</param>
    /// <returns>
    /// A pair of the recovered function expression and the recovered argument list,
    /// or <c>null</c> when the input is not a <see cref="Expression.Eval"/>.
    /// </returns>
    public static (Expression functionExpr, IReadOnlyList<Expression> arguments)?
        ParseGenericFunctionApplication(Expression expression)
    {
        if (expression is not Expression.Eval)
        {
            return null;
        }

        var argumentsReversed = new List<Expression>();

        var current = expression;

        while (current is Expression.Eval parseAndEval)
        {
            argumentsReversed.Add(parseAndEval.Environment);

            current = parseAndEval.Encoded;
        }

        argumentsReversed.Reverse();

        return (current, argumentsReversed);
    }

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
                Expression.BuiltinInst(
                    function: BuiltinId.Skip,
                    input:
                    Expression.ListInst(
                        items: [Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(nextOffset)), current]));

            var headExpr =
                Expression.BuiltinInst(
                    function: BuiltinId.Head,
                    input: skipExpr);

            current = headExpr;
        }

        return current;
    }

}
