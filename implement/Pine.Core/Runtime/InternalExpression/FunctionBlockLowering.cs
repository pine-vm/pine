using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Canonical = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Runtime.InternalExpression;

/// <summary>Lowers scoped function bindings to Eval and encoded code tables.</summary>
public static class FunctionBlockLowering
{
    /// <summary>The representation of the callee's logical environment in the physical Eval frame.</summary>
    public enum EnvironmentLayout
    {
        /// <summary>[code table, logical environment], for arbitrary Pine values.</summary>
        Packed,

        /// <summary>[code table, arg0, ...], for frontends whose logical environment is an argument list.</summary>
        FlatArguments
    }

    /// <summary>Lowers compiler-only function bindings to ordinary retained syntax using the selected call layout.</summary>
    public static Expression Lower(Expression expression, EnvironmentLayout layout)
    {
        ArgumentNullException.ThrowIfNull(expression);
        return Lower(expression, layout, Expression.Environment.Instance, null, null, insideFunction: false);
    }

    /// <summary>Exports bodies in table order using the selected layout, without the entry expression.</summary>
    public static ImmutableArray<Canonical> ExportBodies(Expression.LetFunctions block, EnvironmentLayout layout) =>
        [.. LowerBodies(block, layout, Slots(block)).Select(ExpressionConversion.ToCanonical)];

    private static Dictionary<int, int> Slots(Expression.LetFunctions block) =>
        block.Functions.Select((function, index) => (function.Id, index)).ToDictionary(
            pair => pair.Id,
            pair => pair.index);

    private static ImmutableArray<Expression> LowerBodies(
        Expression.LetFunctions block, EnvironmentLayout layout, Dictionary<int, int> slots) =>
        [
        .. block.Functions.Select(
            function =>
            Lower(function.Body, layout, LogicalEnvironment(layout), Table, slots, insideFunction: true))
        ];

    private static Expression Table { get; } = At(0, Expression.Environment.Instance);

    private static Expression LogicalEnvironment(EnvironmentLayout layout) =>
        layout is EnvironmentLayout.Packed
        ?
        At(1, Expression.Environment.Instance)
        :
        new Expression.BuiltinBinary(BuiltinFunction.Skip, Number(1), Expression.Environment.Instance);

    internal static Expression At(int index, Expression input) =>
        index == 0 ? new Expression.Builtin(BuiltinFunction.Head, input) : new Expression.HeadSkip(index, input);

    private static Expression Number(int value) =>
        new Expression.Literal(Encodings.IntegerEncoding.EncodeSignedInteger(value));

    private static Expression Lower(
        Expression node,
        EnvironmentLayout layout,
        Expression environment,
        Expression? table,
        Dictionary<int, int>? slots,
        bool insideFunction)
    {
        if (!node.ContainsFunctionBindings &&
            (ReferenceEquals(environment, Expression.Environment.Instance) || !node.ReferencesEnvironment))
        {
            return node;
        }

        Expression Child(Expression value) => Lower(value, layout, environment, table, slots, insideFunction);

        switch (node)
        {
            case Expression.LetFunctions block:
                var innerSlots = Slots(block);

                var encoded =
                    LowerBodies(block, layout, innerSlots).Select(ExpressionValueEncoding.EncodeExpressionAsValue);

                var innerTable = new Expression.Literal(PineValue.List([.. encoded]));
                // The block changes the table used by symbolic calls, not the logical environment.
                return Lower(block.Entry, layout, environment, innerTable, innerSlots, insideFunction);

            case Expression.Call call:
                if (slots is null || table is null || !slots.TryGetValue(call.FunctionId, out var slot))
                    throw new ArgumentException($"Call to function {call.FunctionId} outside its block.");

                var target =
                    table is Expression.Literal { Value: PineValue.ListValue bodies }
                    ?
                    new Expression.Literal(bodies.Items.Span[slot])
                    :
                    At(slot, table);

                var input = Child(call.Environment);

                Expression frame =
                    layout switch
                    {
                        EnvironmentLayout.Packed => new Expression.List([table, input]),

                        EnvironmentLayout.FlatArguments when input is Expression.List args =>
                        new Expression.List([table, .. args.Items]),

                        EnvironmentLayout.FlatArguments =>
                        new Expression.BuiltinBinary(BuiltinFunction.Concat, new Expression.List([table]), input),

                        _ =>
                        throw new ArgumentOutOfRangeException(nameof(layout))
                    };

                return new Expression.Eval(target, frame);

            case Expression.Environment:
                return environment;

            case Expression.Literal:
            case Expression.List:
            case Expression.Eval:
            case Expression.Builtin:
            case Expression.BuiltinBinary:
            case Expression.Conditional:
            case Expression.Switch:
            case Expression.Label:
            case Expression.Length_Equal_Binary:
            case Expression.HeadSkip:
                if (insideFunction && layout is EnvironmentLayout.FlatArguments &&
                    Projection(node) is { Input: Expression.Environment, Index: >= 0 and < int.MaxValue } item)
                {
                    return At(item.Index + 1, Expression.Environment.Instance);
                }

                return ExpressionFusion.Rebuild(node, Child);

            default:
                throw new NotImplementedException(
                    nameof(Lower) + " does not handle expression variant: " + node.GetType().Name);
        }
    }

    internal static (int Index, Expression Input)? Projection(Expression expression) =>
        expression switch
        {
            Expression.HeadSkip head => (head.Count, head.Input),
            Expression.Builtin { Function: BuiltinFunction.Head } head => (0, head.Input),

            Expression.Literal or Expression.List or Expression.Environment or Expression.Eval or Expression.Builtin or
            Expression.Conditional or Expression.Label or Expression.BuiltinBinary or Expression.Length_Equal_Binary or
            Expression.Switch or Expression.Call or Expression.LetFunctions =>
            null,

            _ =>
            throw new NotImplementedException(
                nameof(Projection) + " does not handle expression variant: " + expression.GetType().Name)
        };
}
