using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

using Canonical = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Runtime.InternalExpression;

/// <summary>Explicit boundary conversions between public syntax and retained runtime syntax.</summary>
public static class ExpressionConversion
{
    private static readonly ConditionalWeakTable<Canonical, Expression> s_internal = [];

    private static readonly ConditionalWeakTable<Expression, Canonical> s_canonical = [];

    internal static Expression FromCanonicalCached(Canonical expression) =>
        s_internal.GetValue(
            expression,
            static canonical =>
            {
                var retained = FromCanonicalCore(canonical, fusePopularShapes: true, cacheIdentities: true);
                RememberCanonical(retained, canonical);
                return retained;
            });

    internal static Canonical ToCanonicalCached(Expression expression) =>
        s_canonical.GetValue(expression, static retained => ToCanonicalCore(retained, cacheIdentities: true));

    internal static bool HasCachedCanonical(Expression expression) =>
        s_canonical.TryGetValue(expression, out _);

    private static Expression RememberInternal(Canonical canonical, Expression retained) =>
        s_internal.GetValue(canonical, _ => retained);

    private static Canonical RememberCanonical(Expression retained, Canonical canonical) =>
        s_canonical.GetValue(retained, _ => canonical);

    /// <summary>
    /// Converts canonical syntax, preserving shared children and exact literal/label payloads.
    /// Popular shapes are fused unless disabled. Unknown builtin strings are rejected.
    /// </summary>
    public static Expression FromCanonical(Canonical expression, bool fusePopularShapes = true) =>
        FromCanonicalCore(expression, fusePopularShapes, cacheIdentities: false);

    private static Expression FromCanonicalCore(Canonical expression, bool fusePopularShapes, bool cacheIdentities)
    {
        ArgumentNullException.ThrowIfNull(expression);
        var converted = new Dictionary<Canonical, Expression>(ReferenceEqualityComparer.Instance);
        var pending = new Stack<(Canonical Node, bool Expanded)>();
        pending.Push((expression, false));

        while (pending.TryPop(out var next))
        {
            var node = next.Node;

            if (converted.ContainsKey(node))
                continue;

            if (cacheIdentities && s_internal.TryGetValue(node, out var cached))
            {
                converted.Add(node, cached);
                continue;
            }

            if (!next.Expanded)
            {
                pending.Push((node, true));
                PushCanonicalChildren(node, pending, fusePopularShapes);
                continue;
            }

            Expression result;

            switch (node)
            {
                case Canonical.Litral literal:
                    result = new Expression.Literal(literal.Value);
                    break;

                case Canonical.List list:
                    var items = new Expression[list.Items.Count];

                    for (var index = 0; index < items.Length; index++)
                        items[index] = converted[list.Items[index]];

                    result = new Expression.List(ImmutableCollectionsMarshal.AsImmutableArray(items));
                    break;

                case Canonical.Eval eval:
                    result = new Expression.Eval(converted[eval.Encoded], converted[eval.Environment]);
                    break;

                case Canonical.Builtin builtin:
                    var function = BuiltinFunctions.FromName(builtin.Function);

                    if (fusePopularShapes &&
                        builtin.Input is Canonical.List { Items: [var left, var right] })
                    {
                        result =
                            function is BuiltinFunction.Equal &&
                            TryLengthEqual(left, right, out var sequence, out var other, out var lengthOnLeft)
                            ?
                            new Expression.Length_Equal_Binary(converted[sequence], converted[other], lengthOnLeft)
                            :
                            new Expression.BuiltinBinary(function, converted[left], converted[right]);
                    }
                    else if (fusePopularShapes && function is BuiltinFunction.Head &&
                        TryHeadSkip(builtin.Input, out var count, out var input))
                    {
                        result = new Expression.HeadSkip(count, converted[input]);
                    }
                    else
                        result = new Expression.Builtin(function, converted[builtin.Input]);

                    break;

                case Canonical.Conditional conditional:
                    result =
                        fusePopularShapes
                        ?
                        Expression.FuseConditional(
                            converted[conditional.Condition],
                            converted[conditional.FalseBranch],
                            converted[conditional.TrueBranch])
                        :
                        new Expression.Conditional(
                            converted[conditional.Condition],
                            converted[conditional.FalseBranch],
                            converted[conditional.TrueBranch]);

                    break;

                case Canonical.Environment:
                    result = Expression.Environment.Instance;
                    break;

                case Canonical.Label label:
                    result = new Expression.Label(label.LabelValue, converted[label.Tagged]);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(FromCanonical) + " does not handle expression variant: " + node.GetType().Name);
            }

            if (cacheIdentities)
            {
                if (!ReferenceEquals(node, expression))
                    result = RememberInternal(node, result);

                RememberCanonical(result, node);
            }

            converted.Add(node, result);
        }

        return converted[expression];
    }

    private static void PushCanonicalChildren(
        Canonical node, Stack<(Canonical Node, bool Expanded)> pending, bool fuse)
    {
        switch (node)
        {
            case Canonical.Litral:
            case Canonical.Environment:
                break;

            case Canonical.List list:
                for (var index = list.Items.Count - 1; index >= 0; index--)
                    pending.Push((list.Items[index], false));

                break;

            case Canonical.Eval eval:
                pending.Push((eval.Environment, false));
                pending.Push((eval.Encoded, false));
                break;

            case Canonical.Builtin builtin:
                _ = BuiltinFunctions.FromName(builtin.Function);

                if (fuse &&
                    builtin.Input is Canonical.List { Items: [var left, var right] })
                {
                    if (builtin.Function is "equal" &&
                        TryLengthEqual(left, right, out var sequence, out var other, out _))
                    {
                        pending.Push((other, false));
                        pending.Push((sequence, false));
                    }
                    else
                    {
                        pending.Push((right, false));
                        pending.Push((left, false));
                    }
                }
                else if (fuse && builtin.Function is "head" && TryHeadSkip(builtin.Input, out _, out var input))
                    pending.Push((input, false));

                else
                    pending.Push((builtin.Input, false));

                break;

            case Canonical.Conditional conditional:
                pending.Push((conditional.TrueBranch, false));
                pending.Push((conditional.FalseBranch, false));
                pending.Push((conditional.Condition, false));
                break;

            case Canonical.Label label:
                pending.Push((label.Tagged, false));
                break;

            default:
                throw new NotImplementedException(
                    nameof(PushCanonicalChildren) + " does not handle expression variant: " + node.GetType().Name);
        }
    }

    private static bool TryLengthEqual(
        Canonical left, Canonical right, out Canonical sequence, out Canonical other, out bool lengthOnLeft)
    {
        if (left is Canonical.Builtin { Function: "length" } leftLength)
        {
            sequence = leftLength.Input;
            other = right;
            lengthOnLeft = true;
            return true;
        }

        if (right is Canonical.Builtin { Function: "length" } rightLength)
        {
            sequence = rightLength.Input;
            other = left;
            lengthOnLeft = false;
            return true;
        }

        sequence = left;
        other = right;
        lengthOnLeft = default;
        return false;
    }

    private static bool TryHeadSkip(Canonical expression, out int count, out Canonical input)
    {
        if (expression is Canonical.Builtin
            {
                Function: "skip",
                Input: Canonical.List { Items: [Canonical.Litral literal, var sequence] }
            } &&
            literal.Value is PineValue.BlobValue blob &&
            ExpressionBinary.TryCanonicalInt32(blob.Bytes.Span, out count))
        {
            input = sequence;
            return true;
        }

        count = default;
        input = expression;
        return false;
    }

    /// <summary>Expands fused nodes back to their exact canonical syntax, retaining shared children.</summary>
    public static Canonical ToCanonical(Expression expression) =>
        ToCanonicalCore(expression, cacheIdentities: false);

    private static Canonical ToCanonicalCore(Expression expression, bool cacheIdentities)
    {
        ArgumentNullException.ThrowIfNull(expression);
        var converted = new Dictionary<Expression, Canonical>(ReferenceEqualityComparer.Instance);
        var pending = new Stack<(Expression Node, bool Expanded)>();
        pending.Push((expression, false));

        while (pending.TryPop(out var next))
        {
            var node = next.Node;

            if (converted.ContainsKey(node))
                continue;

            if (cacheIdentities && s_canonical.TryGetValue(node, out var cached))
            {
                converted.Add(node, cached);
                continue;
            }

            if (!next.Expanded)
            {
                pending.Push((node, true));
                PushRetainedChildren(node, pending);
                continue;
            }

            Canonical.List ConvertList(Expression.List list)
            {
                var items = new Canonical[list.Items.Length];

                for (var index = 0; index < items.Length; index++)
                    items[index] = converted[list.Items[index]];

                return Canonical.ListInst(items);
            }

            Canonical result =
                node switch
                {
                    Expression.Literal literal => Canonical.LitralInst(literal.Value),
                    Expression.List list => ConvertList(list),
                    Expression.Eval eval => new Canonical.Eval(converted[eval.Encoded], converted[eval.Environment]),

                    Expression.Builtin builtin =>
                    Canonical.BuiltinInst(BuiltinFunctions.ToName(builtin.Function), converted[builtin.Input]),

                    Expression.Conditional conditional =>
                    Canonical.ConditionalInst(
                        converted[conditional.Condition],
                        converted[conditional.FalseBranch],
                        converted[conditional.TrueBranch]),

                    Expression.Switch dispatch => ExpandSwitch(dispatch, child => converted[child]),

                    Expression.Environment => Canonical.EnvironmentInstance,
                    Expression.Label label => new Canonical.Label(label.Value, converted[label.Tagged]),

                    Expression.BuiltinBinary equal =>
                    Canonical.BuiltinInst(
                        BuiltinFunctions.ToName(equal.Function),
                        Canonical.ListInst([converted[equal.Left], converted[equal.Right]])),

                    Expression.Length_Equal_Binary length =>
                    ExpandLengthEqual(length, converted[length.Sequence], converted[length.Other]),

                    Expression.HeadSkip head =>
                    Canonical.BuiltinInst(
                        "head",
                        Canonical.BuiltinInst(
                            "skip",
                            Canonical.ListInst(
                                [
                                Canonical.LitralInst(IntegerEncoding.EncodeSignedInteger(head.Count)),
                                converted[head.Input]
                                ]))),

                    _ =>
                    throw new NotImplementedException(
                        nameof(ToCanonical) + " does not handle expression variant: " + node.GetType().Name)
                };

            if (cacheIdentities && !ReferenceEquals(node, expression))
                result = RememberCanonical(node, result);

            converted.Add(node, result);
        }

        return converted[expression];
    }

    private static Canonical ExpandLengthEqual(
        Expression.Length_Equal_Binary expression,
        Canonical sequence,
        Canonical other)
    {
        var length = Canonical.BuiltinInst("length", sequence);

        return
            Canonical.BuiltinInst(
                "equal",
                Canonical.ListInst(expression.LengthOnLeft ? [length, other] : [other, length]));
    }

    private static Canonical ExpandSwitch(Expression.Switch dispatch, Func<Expression, Canonical> convert)
    {
        var selector = convert(dispatch.Selector);
        var tail = convert(dispatch.Default);

        foreach (var (key, literalOnLeft, branch) in Enumerable.Reverse(dispatch.Arms))
        {
            var literal = Canonical.LitralInst(key);

            var condition =
                Canonical.BuiltinInst(
                    "equal",
                    Canonical.ListInst(literalOnLeft ? [literal, selector] : [selector, literal]));

            tail = Canonical.ConditionalInst(condition, tail, convert(branch));
        }

        return tail;
    }

    internal static void PushRetainedChildren(Expression node, Stack<(Expression Node, bool Expanded)> pending)
    {
        switch (node)
        {
            case Expression.Literal:
            case Expression.Environment:
                break;

            case Expression.List list:
                for (var index = list.Items.Length - 1; index >= 0; index--)
                    pending.Push((list.Items[index], false));

                break;

            case Expression.Eval eval:
                pending.Push((eval.Environment, false));
                pending.Push((eval.Encoded, false));
                break;

            case Expression.Builtin builtin:
                pending.Push((builtin.Input, false));
                break;

            case Expression.Conditional conditional:
                pending.Push((conditional.TrueBranch, false));
                pending.Push((conditional.FalseBranch, false));
                pending.Push((conditional.Condition, false));
                break;

            case Expression.Switch dispatch:
                pending.Push((dispatch.Default, false));

                foreach (var arm in dispatch.Arms)
                    pending.Push((arm.Branch, false));

                pending.Push((dispatch.Selector, false));
                break;

            case Expression.Label label:
                pending.Push((label.Tagged, false));
                break;

            case Expression.BuiltinBinary equal:
                pending.Push((equal.Right, false));
                pending.Push((equal.Left, false));
                break;

            case Expression.Length_Equal_Binary length:
                pending.Push((length.Other, false));
                pending.Push((length.Sequence, false));
                break;

            case Expression.HeadSkip head:
                pending.Push((head.Input, false));
                break;

            default:
                throw new NotImplementedException(
                    nameof(PushRetainedChildren) + " does not handle expression variant: " + node.GetType().Name);
        }
    }
}
