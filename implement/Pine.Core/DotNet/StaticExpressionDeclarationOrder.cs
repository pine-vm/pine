using System;
using System.Collections.Generic;
using Pine.Core.CodeAnalysis;

namespace Pine.Core.DotNet;

/// <summary>
/// Provides a strict total order for <see cref="StaticExpression{DeclQualifiedName}"/> instances.
/// </summary>
public sealed class StaticExpressionDeclarationOrder : IComparer<StaticExpression<DeclQualifiedName>>
{
    /// <summary>
    /// Shared instance of the comparer.
    /// </summary>
    public static StaticExpressionDeclarationOrder Instance { get; } = new();

    private StaticExpressionDeclarationOrder()
    {
    }

    /// <inheritdoc />
    public int Compare(StaticExpression<DeclQualifiedName>? x, StaticExpression<DeclQualifiedName>? y)
    {
        if (ReferenceEquals(x, y))
        {
            return 0;
        }

        if (x is null)
        {
            return -1;
        }

        if (y is null)
        {
            return 1;
        }

        var subexpressionComparison = x.SubexpressionCount.CompareTo(y.SubexpressionCount);

        if (subexpressionComparison != 0)
        {
            return subexpressionComparison;
        }

        if (x.Equals(y))
        {
            return 0;
        }

        var kindComparison = GetKind(x).CompareTo(GetKind(y));

        if (kindComparison != 0)
        {
            return kindComparison;
        }

        return CompareWithinKind(x, y);
    }

    private static ExpressionKind GetKind(StaticExpression<DeclQualifiedName> expression) => expression switch
    {
        StaticExpression<DeclQualifiedName>.Environment =>
        ExpressionKind.Environment,

        StaticExpression<DeclQualifiedName>.Literal =>
        ExpressionKind.Literal,

        StaticExpression<DeclQualifiedName>.List =>
        ExpressionKind.List,

        StaticExpression<DeclQualifiedName>.KernelApplication =>
        ExpressionKind.KernelApplication,

        StaticExpression<DeclQualifiedName>.FunctionApplication =>
        ExpressionKind.FunctionApplication,

        StaticExpression<DeclQualifiedName>.Conditional =>
        ExpressionKind.Conditional,

        StaticExpression<DeclQualifiedName>.CrashingParseAndEval =>
        ExpressionKind.CrashingParseAndEval,

        _ =>
        throw new NotSupportedException($"Unknown static expression type: {expression.GetType()}")
    };

    private int CompareWithinKind(StaticExpression<DeclQualifiedName> x, StaticExpression<DeclQualifiedName> y) => (x, y) switch
    {
        (StaticExpression<DeclQualifiedName>.Environment, StaticExpression<DeclQualifiedName>.Environment) =>
        0,

        (StaticExpression<DeclQualifiedName>.Literal xLiteral, StaticExpression<DeclQualifiedName>.Literal yLiteral) =>
        CSharpDeclarationOrder.ValueDeclarationOrder.Instance.Compare(xLiteral.Value, yLiteral.Value),

        (StaticExpression<DeclQualifiedName>.List xList, StaticExpression<DeclQualifiedName>.List yList) =>
        CompareLists(xList, yList),

        (StaticExpression<DeclQualifiedName>.KernelApplication xKernel, StaticExpression<DeclQualifiedName>.KernelApplication yKernel) =>
        CompareKernelApplications(xKernel, yKernel),

        (StaticExpression<DeclQualifiedName>.FunctionApplication xFunction, StaticExpression<DeclQualifiedName>.FunctionApplication yFunction) =>
        CompareFunctionApplications(xFunction, yFunction),

        (StaticExpression<DeclQualifiedName>.Conditional xConditional, StaticExpression<DeclQualifiedName>.Conditional yConditional) =>
        CompareConditionals(xConditional, yConditional),

        (StaticExpression<DeclQualifiedName>.CrashingParseAndEval xParseAndEval, StaticExpression<DeclQualifiedName>.CrashingParseAndEval yParseAndEval) =>
        CompareCrashingParseAndEval(xParseAndEval, yParseAndEval),

        _ =>
        throw new NotSupportedException(
            $"Comparison for expression type {x.GetType()} is not implemented.")
    };

    private int CompareLists(StaticExpression<DeclQualifiedName>.List xList, StaticExpression<DeclQualifiedName>.List yList)
    {
        if (xList.Items.Count != yList.Items.Count)
        {
            return xList.Items.Count.CompareTo(yList.Items.Count);
        }

        for (var i = 0; i < xList.Items.Count; i++)
        {
            var itemComparison = Compare(xList.Items[i], yList.Items[i]);

            if (itemComparison != 0)
            {
                return itemComparison;
            }
        }

        return 0;
    }

    private int CompareKernelApplications(
        StaticExpression<DeclQualifiedName>.KernelApplication xKernel,
        StaticExpression<DeclQualifiedName>.KernelApplication yKernel)
    {
        var functionComparison = string.CompareOrdinal(xKernel.Function, yKernel.Function);

        if (functionComparison != 0)
        {
            return functionComparison;
        }

        return Compare(xKernel.Input, yKernel.Input);
    }

    private int CompareFunctionApplications(
        StaticExpression<DeclQualifiedName>.FunctionApplication xFunction,
        StaticExpression<DeclQualifiedName>.FunctionApplication yFunction)
    {
        var nameComparison = CompareQualifiedName(xFunction.FunctionName, yFunction.FunctionName);

        if (nameComparison != 0)
        {
            return nameComparison;
        }

        return Compare(xFunction.Arguments, yFunction.Arguments);
    }

    private int CompareConditionals(
        StaticExpression<DeclQualifiedName>.Conditional xConditional,
        StaticExpression<DeclQualifiedName>.Conditional yConditional)
    {
        var conditionComparison = Compare(xConditional.Condition, yConditional.Condition);

        if (conditionComparison != 0)
        {
            return conditionComparison;
        }

        var trueBranchComparison = Compare(xConditional.TrueBranch, yConditional.TrueBranch);

        if (trueBranchComparison != 0)
        {
            return trueBranchComparison;
        }

        return Compare(xConditional.FalseBranch, yConditional.FalseBranch);
    }

    private int CompareCrashingParseAndEval(
        StaticExpression<DeclQualifiedName>.CrashingParseAndEval xParseAndEval,
        StaticExpression<DeclQualifiedName>.CrashingParseAndEval yParseAndEval)
    {
        var encodedComparison = Compare(xParseAndEval.Encoded, yParseAndEval.Encoded);

        if (encodedComparison != 0)
        {
            return encodedComparison;
        }

        return Compare(xParseAndEval.EnvironmentExpr, yParseAndEval.EnvironmentExpr);
    }

    private static int CompareQualifiedName(DeclQualifiedName x, DeclQualifiedName y)
    {
        var minLength = Math.Min(x.Namespaces.Count, y.Namespaces.Count);

        for (var i = 0; i < minLength; i++)
        {
            var namespaceComparison = string.CompareOrdinal(x.Namespaces[i], y.Namespaces[i]);

            if (namespaceComparison != 0)
            {
                return namespaceComparison;
            }
        }

        var namespaceLengthComparison = x.Namespaces.Count.CompareTo(y.Namespaces.Count);

        if (namespaceLengthComparison != 0)
        {
            return namespaceLengthComparison;
        }

        return string.CompareOrdinal(x.DeclName, y.DeclName);
    }

    private enum ExpressionKind
    {
        Environment = 0,
        Literal = 1,
        List = 2,
        KernelApplication = 3,
        FunctionApplication = 4,
        Conditional = 5,
        CrashingParseAndEval = 6
    }
}
