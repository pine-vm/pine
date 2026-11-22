using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Pine.Core.Runtime.InternalExpression;

internal static class ExpressionFusion
{
    internal static Expression Fuse(Expression expression)
    {
        var mapped = new Dictionary<Expression, Expression>(ReferenceEqualityComparer.Instance);
        var pending = new Stack<(Expression Node, bool Expanded)>();
        pending.Push((expression, false));

        while (pending.TryPop(out var work))
        {
            if (mapped.ContainsKey(work.Node))
                continue;

            if (!work.Expanded)
            {
                pending.Push((work.Node, true));

                if (TryBinary(work.Node) is { } equal)
                {
                    if (equal.Function is BuiltinFunction.Equal &&
                        equal.Left is Expression.Builtin { Function: BuiltinFunction.Length } leftLength)
                    {
                        pending.Push((equal.Right, false));
                        pending.Push((leftLength.Input, false));
                    }
                    else if (equal.Function is BuiltinFunction.Equal &&
                        equal.Right is Expression.Builtin { Function: BuiltinFunction.Length } rightLength)
                    {
                        pending.Push((rightLength.Input, false));
                        pending.Push((equal.Left, false));
                    }
                    else
                    {
                        pending.Push((equal.Right, false));
                        pending.Push((equal.Left, false));
                    }
                }
                else if (TryHeadSkip(work.Node) is { } head)
                    pending.Push((head.Input, false));

                else
                    ExpressionConversion.PushRetainedChildren(work.Node, pending);

                continue;
            }

            Expression Child(Expression child) => mapped[child];

            if (TryBinary(work.Node) is { } pair)
            {
                if (pair.Function is BuiltinFunction.Equal &&
                    pair.Left is Expression.Builtin { Function: BuiltinFunction.Length } leftLength)
                {
                    mapped[work.Node] = new Expression.Length_Equal_Binary(Child(leftLength.Input), Child(pair.Right));
                }
                else if (pair.Function is BuiltinFunction.Equal &&
                    pair.Right is Expression.Builtin { Function: BuiltinFunction.Length } rightLength)
                {
                    mapped[work.Node] =
                        new Expression.Length_Equal_Binary(Child(rightLength.Input), Child(pair.Left), false);
                }
                else
                {
                    mapped[work.Node] =
                        new Expression.BuiltinBinary(pair.Function, Child(pair.Left), Child(pair.Right));
                }

                continue;
            }

            if (TryHeadSkip(work.Node) is { } projection)
            {
                mapped[work.Node] = new Expression.HeadSkip(projection.Count, Child(projection.Input));
                continue;
            }

            mapped[work.Node] =
                work.Node is Expression.Conditional conditional
                ?
                Expression.FuseConditional(
                    Child(conditional.Condition),
                    Child(conditional.FalseBranch),
                    Child(conditional.TrueBranch))
                :
                Rebuild(work.Node, Child);
        }

        return mapped[expression];
    }

    private static (BuiltinFunction Function, Expression Left, Expression Right)? TryBinary(Expression expression)
    {
        if (expression is Expression.Builtin
            { Input: Expression.List { Items: [var left, var right] } } builtin)
        {
            return (builtin.Function, left, right);
        }

        return null;
    }

    private static (int Count, Expression Input)? TryHeadSkip(Expression expression)
    {
        if (expression is Expression.Builtin
            {
                Function: BuiltinFunction.Head,
                Input: Expression.Builtin
                {
                    Function: BuiltinFunction.Skip,
                    Input: Expression.List { Items: [Expression.Literal { Value: PineValue.BlobValue count }, var input] }
                }
            } && ExpressionBinary.TryCanonicalInt32(count.Bytes.Span, out var integer))
        {
            return (integer, input);
        }

        return null;
    }

    internal static Expression Rebuild(Expression node, Func<Expression, Expression> child)
    {
        switch (node)
        {
            case Expression.Literal:
            case Expression.Environment:
                return node;

            case Expression.List list:
                Expression[]? items = null;

                for (var index = 0; index < list.Items.Length; index++)
                {
                    var mapped = child(list.Items[index]);

                    if (items is null && !ReferenceEquals(mapped, list.Items[index]))
                        items = [.. list.Items];

                    if (items is not null)
                        items[index] = mapped;
                }

                return items is null ? list : new Expression.List(ImmutableCollectionsMarshal.AsImmutableArray(items));

            case Expression.Builtin builtin:
                var input = child(builtin.Input);
                return ReferenceEquals(input, builtin.Input) ? builtin : Expression.BuiltinInst(builtin.Function, input);

            case Expression.Eval eval:
                var encoded = child(eval.Encoded);
                var environment = child(eval.Environment);

                return
                    ReferenceEquals(encoded, eval.Encoded) && ReferenceEquals(environment, eval.Environment)
                    ?
                    eval
                    :
                    new Expression.Eval(encoded, environment);

            case Expression.Conditional conditional:
                var condition = child(conditional.Condition);
                var whenFalse = child(conditional.FalseBranch);
                var whenTrue = child(conditional.TrueBranch);

                return
                    ReferenceEquals(condition, conditional.Condition) &&
                    ReferenceEquals(whenFalse, conditional.FalseBranch) &&
                    ReferenceEquals(whenTrue, conditional.TrueBranch)
                    ?
                    conditional
                    :
                    new Expression.Conditional(condition, whenFalse, whenTrue);

            case Expression.Label label:
                var tagged = child(label.Tagged);
                return ReferenceEquals(tagged, label.Tagged) ? label : new Expression.Label(label.Value, tagged);

            case Expression.BuiltinBinary equal:
                var left = child(equal.Left);
                var right = child(equal.Right);

                return
                    ReferenceEquals(left, equal.Left) && ReferenceEquals(right, equal.Right)
                    ?
                    equal
                    :
                    Expression.BuiltinBinaryInst(equal.Function, left, right);

            case Expression.Length_Equal_Binary length:
                var sequence = child(length.Sequence);
                var other = child(length.Other);

                return
                    ReferenceEquals(sequence, length.Sequence) && ReferenceEquals(other, length.Other)
                    ?
                    length
                    :
                    new Expression.Length_Equal_Binary(sequence, other, length.LengthOnLeft);

            case Expression.HeadSkip head:
                var source = child(head.Input);
                return ReferenceEquals(source, head.Input) ? head : new Expression.HeadSkip(head.Count, source);

            case Expression.Switch dispatch:
                return dispatch.MapChildren(child);

            case Expression.Call call:
                var argument = child(call.Environment);
                return ReferenceEquals(argument, call.Environment) ? call : new Expression.Call(call.FunctionId, argument);

            case Expression.LetFunctions block:
                return block.MapChildren(child);

            default:
                throw new NotImplementedException(
                    nameof(Rebuild) + " does not handle expression variant: " + node.GetType().Name);
        }
    }
}
