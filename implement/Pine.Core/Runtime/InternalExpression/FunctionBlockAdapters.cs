using Pine.Core.CodeAnalysis;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Layout = Pine.Core.Runtime.InternalExpression.FunctionBlockLowering.EnvironmentLayout;

namespace Pine.Core.Runtime.InternalExpression;

public static class FunctionBlockAdapters
{
    /// <summary>
    /// Import a frontend's already assigned body table while it is still syntax, before
    /// encoding any bodies as Pine values. A table inspected as data is not a closed call group.
    /// </summary>
    public static Expression.LetFunctions? TryImportBodies(
        IReadOnlyList<PineLanguage.Expression> bodies,
        Layout layout,
        FunctionBlockOptimization.Policy? policy = null)
    {
        policy ??= new();
        long remainingSyntax = policy.MaximumAnalysisSteps;

        foreach (var body in bodies)
        {
            remainingSyntax -= body.SubexpressionCount + 1;

            if (remainingSyntax < 0 || body.MaxDepth > policy.MaximumExpressionDepth)
                return null;
        }

        var budget = new FunctionBlockOptimization.Budget(policy.MaximumAnalysisSteps, policy.MaximumExpressionDepth);

        var imported =
            ImportBodies(bodies.Select(body => ExpressionConversion.FromCanonical(body)), bodies.Count, layout, budget);

        return imported.IsDefault ? null : new(imported, Expression.Environment.Instance);
    }

    /// <summary>Optimize a frontend table without changing the return contracts of its exported functions.</summary>
    public static ImmutableArray<PineLanguage.Expression> OptimizeFrontendBodies(
        IReadOnlyList<PineLanguage.Expression> bodies, Layout layout, FunctionBlockOptimization.Policy? policy = null)
    {
        if (TryImportBodies(bodies, layout, policy) is not { } block)
            return [.. bodies];

        var optimized =
            FunctionBlockOptimization.Optimize(block, policy, block.Functions.Select(function => function.Id));

        return
            ReferenceEquals(optimized.Block, block)
            ?
            [.. bodies]
            :
            FunctionBlockLowering.ExportBodies(optimized.Block, layout);
    }

    internal static Expression OptimizeKnownCalls(
        Expression root, PineVMParseCache parseCache, PineValueClass? constraint, FunctionBlockOptimization.Policy? policy = null)
    {
        if (TryLift(root, parseCache, policy, constraint) is not { } block)
            return root;

        var optimized = FunctionBlockOptimization.Optimize(block, policy);

        return
            ReferenceEquals(optimized.Block, block)
            ?
            root
            :
            FunctionBlockLowering.Lower(optimized.Block, Layout.Packed);
    }

    /// <summary>Lift a known call and its closed table from retained Pine code.</summary>
    public static Expression.LetFunctions? TryLift(
        Expression root,
        PineVMParseCache parseCache,
        FunctionBlockOptimization.Policy? policy = null,
        PineValueClass? constraint = null)
    {
        policy ??= new();
        root.RequireLoweredFunctionBindings();

        if (root is not { EvalCount: > 0 } || root.SubexpressionCount > policy.MaximumAnalysisSteps)
            return null;

        var budget = new FunctionBlockOptimization.Budget(policy.MaximumAnalysisSteps, policy.MaximumExpressionDepth);
        ImmutableArray<Expression.FunctionDefinition> functions = default;
        var entry = Search(root, 0);
        return functions.IsDefault || entry is null ? null : new(functions, entry);

        Expression? Search(Expression node, int depth)
        {
            if (!budget.Spend(depth))
                return null;

            if (!functions.IsDefault || node.EvalCount is 0)
                return node;

            if (node is Expression.Eval { Environment: Expression.List { Items.Length: > 0 } frame } call &&
                KnownValue(call.Encoded) is { } target &&
                KnownValue(frame.Items[0]) is PineValue.ListValue table &&
                table.NodesCount <= policy.MaximumAnalysisSteps &&
                table.MaxDepth <= policy.MaximumExpressionDepth &&
                table.Items.Length > 0 && budget.Spend(depth, table.Items.Length))
            {
                var values = table.Items.Span;
                var entrySlot = values.IndexOf(target);

                if (entrySlot >= 0)
                {
                    var retainedBodies = new List<Expression>();

                    foreach (var encoded in values)
                    {
                        if (parseCache.ParseInternalExpression(encoded).IsOkOrNull() is not { } body)
                            break;

                        retainedBodies.Add(body);
                    }

                    if (retainedBodies.Count == values.Length)
                    {
                        var packed =
                            frame.Items.Length == 2
                            ?
                            ImportBodies(retainedBodies, retainedBodies.Count, Layout.Packed, budget)
                            :
                            default;

                        var imported =
                            !packed.IsDefault
                            ?
                            packed
                            :
                            ImportBodies(retainedBodies, retainedBodies.Count, Layout.FlatArguments, budget);

                        if (!imported.IsDefault)
                        {
                            functions = imported;

                            var environment =
                                packed.IsDefault
                                ?
                                new Expression.List(frame.Items[1..])
                                :
                                frame.Items[1];

                            return new Expression.Call(entrySlot, environment);
                        }
                    }
                }
            }

            return Map(node, child => Search(child, depth + 1));
        }

        PineValue? KnownValue(Expression expression) =>
            expression is Expression.Literal literal
            ?
            literal.Value
            :
            constraint is not null && ExpressionAnalysis.TryParseExprAsPathInEnv(expression) is { } path
            ?
            constraint.TryGetValue(path)
            :
            null;
    }

    private static ImmutableArray<Expression.FunctionDefinition> ImportBodies(
        IEnumerable<Expression> bodies, int count, Layout layout, FunctionBlockOptimization.Budget budget)
    {
        if (!budget.Spend(0, count))
            return default;

        var output = ImmutableArray.CreateBuilder<Expression.FunctionDefinition>(count);

        foreach (var body in bodies)
        {
            if (Import(body, 0) is not { } imported)
                return default;

            output.Add(new(output.Count, imported));
        }

        return output.MoveToImmutable();

        Expression? Import(Expression node, int depth)
        {
            if (!budget.Spend(depth))
                return null;

            if (!node.ReferencesEnvironment)
                return node;

            if (node is Expression.Eval { Environment: Expression.List { Items.Length: > 0 } frame } call &&
                FunctionBlockLowering.Projection(call.Encoded) is { Index: >= 0 } target &&
                target.Index < count && IsTable(target.Input) && IsTable(frame.Items[0]))
            {
                switch (layout)
                {
                    case Layout.Packed when frame.Items.Length == 2:
                        return
                            Import(frame.Items[1], depth + 1) is { } input
                            ?
                            new Expression.Call(target.Index, input)
                            :
                            null;

                    case Layout.FlatArguments:
                        var args = ImmutableArray.CreateBuilder<Expression>(frame.Items.Length - 1);

                        foreach (var arg in frame.Items.AsSpan()[1..])
                        {
                            if (Import(arg, depth + 1) is not { } imported)
                                return null;

                            args.Add(imported);
                        }

                        return new Expression.Call(target.Index, new Expression.List(args.MoveToImmutable()));

                    default:
                        return null;
                }
            }

            if (FunctionBlockLowering.Projection(node) is { Input: Expression.Environment } projection)
            {
                return layout switch
                {
                    Layout.Packed when projection.Index == 1 => Expression.Environment.Instance,

                    Layout.FlatArguments when projection.Index > 0 =>
                    FunctionBlockLowering.At(projection.Index - 1, Expression.Environment.Instance),

                    _ =>
                    null
                };
            }

            // A physical environment/table that is observed as data or sent to unknown Eval
            // cannot be replaced with a different set of encoded bodies.
            return node is Expression.Environment ? null : Map(node, child => Import(child, depth + 1));
        }
    }

    private static bool IsTable(Expression expression) =>
        FunctionBlockLowering.Projection(expression) is { Index: 0, Input: Expression.Environment };

    private static Expression? Map(Expression node, Func<Expression, Expression?> map)
    {
        var failed = false;
        var rebuilt = ExpressionFusion.Rebuild(node, MapChild);
        return failed ? null : rebuilt;

        Expression MapChild(Expression child)
        {
            if (failed)
                return child;

            if (map(child) is { } mapped)
                return mapped;

            failed = true;
            return child;
        }
    }
}
