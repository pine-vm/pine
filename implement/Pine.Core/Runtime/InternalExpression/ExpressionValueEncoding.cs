using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Runtime.InternalExpression;

/// <summary>Direct conversion between retained syntax and the canonical Pine expression value representation.</summary>
public static partial class ExpressionValueEncoding
{
    private static readonly PineValue s_literalTag = StringEncoding.ValueFromString("Litral");

    private static readonly PineValue s_listTag = StringEncoding.ValueFromString("List");

    private static readonly PineValue s_evalTag = StringEncoding.ValueFromString("Eval");

    private static readonly PineValue s_builtinTag = StringEncoding.ValueFromString("Builtin");

    private static readonly PineValue s_conditionalTag = StringEncoding.ValueFromString("Conditional");

    private static readonly PineValue s_environmentTag = StringEncoding.ValueFromString("Environment");

    private static readonly PineValue s_labelTag = StringEncoding.ValueFromString("Label");

    private static readonly PineValue.ListValue s_environment = PineValue.List([s_environmentTag]);

    private static readonly PineValue[] s_builtinNames =
        [
        .. Enum.GetValues<BuiltinFunction>().Select(
            function => StringEncoding.ValueFromString(BuiltinFunctions.ToName(function)))
        ];

    private static PineValue.ListValue Literal(PineValue value) => PineValue.List([s_literalTag, value]);

    private static PineValue.ListValue List(params PineValue[] items) => PineValue.List([s_listTag, .. items]);

    private static PineValue.ListValue Builtin(BuiltinFunction function, PineValue input) =>
        PineValue.List([s_builtinTag, s_builtinNames[(int)function], input]);

    internal static bool TryParseBuiltinName(PineValue value, out BuiltinFunction function)
    {
        var index = Array.IndexOf(s_builtinNames, value);
        function = (BuiltinFunction)index;
        return index >= 0;
    }

    /// <summary>Emits the standard compact encoding, expanding fused wrappers only as Pine values.</summary>
    public static PineValue.ListValue EncodeExpressionAsValue(Expression expression)
    {
        ArgumentNullException.ThrowIfNull(expression);
        var encoded = new Dictionary<Expression, PineValue.ListValue>(ReferenceEqualityComparer.Instance);
        var pending = new Stack<(Expression Node, bool Expanded)>();
        pending.Push((expression, false));

        while (pending.TryPop(out var next))
        {
            var node = next.Node;

            if (encoded.ContainsKey(node))
                continue;

            if (!next.Expanded)
            {
                pending.Push((node, true));
                ExpressionConversion.PushRetainedChildren(node, pending);
                continue;
            }

            PineValue.ListValue EncodeList(Expression.List list)
            {
                var items = new PineValue[list.Items.Length + 1];
                items[0] = s_listTag;

                for (var index = 0; index < list.Items.Length; index++)
                    items[index + 1] = encoded[list.Items[index]];

                return PineValue.List(items);
            }

            PineValue.ListValue EncodeLengthEqual(Expression.Length_Equal_Binary length)
            {
                var measured = Builtin(BuiltinFunction.Length, encoded[length.Sequence]);
                var other = encoded[length.Other];

                return
                    Builtin(
                        BuiltinFunction.Equal,
                        length.LengthOnLeft ? List(measured, other) : List(other, measured));
            }

            PineValue.ListValue EncodeSwitch(Expression.Switch dispatch)
            {
                var selector = encoded[dispatch.Selector];
                var tail = encoded[dispatch.Default];

                foreach (var (key, literalOnLeft, branch) in Enumerable.Reverse(dispatch.Arms))
                {
                    var literal = Literal(key);

                    var test =
                        Builtin(
                            BuiltinFunction.Equal,
                            literalOnLeft ? List(literal, selector) : List(selector, literal));

                    tail = PineValue.List([s_conditionalTag, test, tail, encoded[branch]]);
                }

                return tail;
            }

            encoded.Add(
                node,
                node switch
                {
                    Expression.Literal literal => Literal(literal.Value),
                    Expression.List list => EncodeList(list),

                    Expression.Eval eval =>
                    PineValue.List([s_evalTag, encoded[eval.Encoded], encoded[eval.Environment]]),

                    Expression.Builtin builtin => Builtin(builtin.Function, encoded[builtin.Input]),

                    Expression.Conditional conditional =>
                    PineValue.List(
                        [
                        s_conditionalTag,
                        encoded[conditional.Condition],
                        encoded[conditional.FalseBranch],
                        encoded[conditional.TrueBranch]
                        ]),

                    Expression.Environment => s_environment,
                    Expression.Switch dispatch => EncodeSwitch(dispatch),
                    Expression.Label label => PineValue.List([s_labelTag, label.Value, encoded[label.Tagged]]),

                    Expression.BuiltinBinary equal =>
                    Builtin(equal.Function, List(encoded[equal.Left], encoded[equal.Right])),

                    Expression.Length_Equal_Binary length => EncodeLengthEqual(length),

                    Expression.HeadSkip head =>
                    Builtin(
                        BuiltinFunction.Head,
                        Builtin(
                            BuiltinFunction.Skip,
                            List(Literal(IntegerEncoding.EncodeSignedInteger(head.Count)), encoded[head.Input]))),

                    _ =>
                    throw new NotImplementedException(
                        nameof(EncodeExpressionAsValue) + " does not handle expression variant: " + node.GetType().Name)
                });
        }

        return encoded[expression];
    }
}
