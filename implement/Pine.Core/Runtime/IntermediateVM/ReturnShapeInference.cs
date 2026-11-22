using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.InternalExpression;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Expression = Pine.Core.Runtime.InternalExpression.Expression;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>Bounded, monotone return-shape inference over known Pine callees, including recursive groups.</summary>
internal static class ReturnShapeInference
{
    private abstract record Shape
    {
        public sealed record Bottom : Shape;

        public sealed record Dynamic : Shape;

        public sealed record Constant(PineValue Value) : Shape;

        public sealed record Items(ImmutableArray<Shape> Values) : Shape;

        public sealed record Union(ImmutableArray<Shape> Cases) : Shape;
    }

    public static ReturnValueLayout Infer(
        Expression root,
        PineVMParseCache parseCache,
        Func<Expression, PineValue, Expression, Expression?> prepareCallee,
        ReturnValueTaggingPolicy? taggingPolicy = null)
    {
        var results = new Dictionary<Expression, Shape> { [root] = new Shape.Bottom() };
        var complete = false;

        for (var iteration = 0; iteration < 16; iteration++)
        {
            var changed = false;

            foreach (var expression in results.Keys.ToArray())
            {
                var budget = 4096;

                Shape Analyze(Expression item)
                {
                    if (--budget < 0)
                        return new Shape.Dynamic();

                    switch (item)
                    {
                        case Expression.Literal literal:
                            return new Shape.Constant(literal.Value);

                        case Expression.List list:
                            var items = list.Items.Select(Analyze).ToImmutableArray();
                            return items.Any(value => value is Shape.Bottom) ? new Shape.Bottom() : new Shape.Items(items);

                        case Expression.Conditional conditional:
                            return Join(Analyze(conditional.FalseBranch), Analyze(conditional.TrueBranch));

                        case Expression.Switch dispatch:
                            var joined = Analyze(dispatch.Default);

                            foreach (var arm in Enumerable.Reverse(dispatch.Arms))
                                joined = Join(joined, Analyze(arm.Branch));

                            return joined;

                        case Expression.Label label:
                            return Analyze(label.Tagged);

                        case Expression.Eval eval:
                            if (eval.Encoded.EvalCount is 0 && !eval.Encoded.ReferencesEnvironment &&
                                ExpressionReduction.TryEvalIndependent(eval.Encoded, parseCache).Value is { } encoded &&
                                ExpressionTemplatePlan.TryParseTemplate(encoded, parseCache) is null &&
                                parseCache.ParseInternalExpression(encoded).IsOkOrNull() is { } target)
                            {
                                if (prepareCallee(target, encoded, eval.Environment) is not { } prepared)
                                    return new Shape.Dynamic();

                                if (!results.TryGetValue(prepared, out var value))
                                {
                                    if (results.Count >= 128)
                                        return new Shape.Dynamic();

                                    value = new Shape.Bottom();
                                    results.Add(prepared, value);
                                    changed = true;
                                }

                                return value;
                            }

                            return new Shape.Dynamic();

                        case Expression.Environment:
                        case Expression.Builtin:
                        case Expression.BuiltinBinary:
                        case Expression.Length_Equal_Binary:
                        case Expression.HeadSkip:
                            return new Shape.Dynamic();

                        case Expression.Call:
                        case Expression.LetFunctions:
                            throw Expression.UnloweredFunctionBindings();

                        default:
                            throw new NotImplementedException("Infer does not handle expression: " + item.GetType().Name);
                    }
                }

                var inferred = Join(results[expression], Analyze(expression));

                if (!Same(results[expression], inferred))
                {
                    results[expression] = inferred;
                    changed = true;
                }
            }

            if (!changed)
            {
                complete = true;
                break;
            }
        }

        if (!complete || results[root] is Shape.Bottom or Shape.Dynamic)
            return ReturnValueLayout.Canonical;

        var slots = ImmutableArray.CreateBuilder<ReturnValueLayout.Slot>();

        ReturnValueLayout.Node Build(
            Shape shape,
            ImmutableArray<int> path,
            ImmutableArray<ReturnValueLayout.Guard> guards,
            int depth)
        {
            if (depth > 16)
                return Component([]);

            ReturnValueLayout.Node Component(ImmutableArray<PineValue> values)
            {
                var index = slots.Count;
                slots.Add(new(path, values, guards));
                return new ReturnValueLayout.Node.Component(index, values);
            }

            switch (shape)
            {
                case Shape.Bottom:
                case Shape.Dynamic:
                    return Component([]);

                case Shape.Constant constant:
                    return new ReturnValueLayout.Node.Fixed(constant.Value);

                case Shape.Items items:
                    return
                        new ReturnValueLayout.Node.List(
                            [
                            .. items.Values.Select((item, index) => Build(item, path.Add(index), guards, depth + 1))
                            ]);

                case Shape.Union union:
                    if (union.Cases.All(item => item is Shape.Constant))
                    {
                        return
                            Component(
                                [
                                .. union.Cases.Cast<Shape.Constant>().Select(item => item.Value).OrderBy(
                                    Hash,
                                    StringComparer.Ordinal)
                                ]);
                    }

                    var lists = union.Cases.Select(Expand).OfType<Shape.Items>().ToArray();

                    if (lists.Length != union.Cases.Length)
                        return Component([]);

                    for (var discriminator = 0; discriminator < lists.Min(list => list.Values.Length); discriminator++)
                    {
                        if (!lists.All(list => list.Values[discriminator] is Shape.Constant))
                            continue;

                        var tags = lists.Select(list => ((Shape.Constant)list.Values[discriminator]).Value).ToArray();

                        if (tags.Distinct().Count() != tags.Length)
                            continue;

                        var order =
                            Enumerable.Range(0, tags.Length)
                            .OrderBy(index => Hash(tags[index]), StringComparer.Ordinal).ToArray();

                        var index = slots.Count;
                        var tagValues = order.Select(item => tags[item]).ToImmutableArray();
                        slots.Add(new(path.Add(discriminator), tagValues, guards));

                        var cases =
                            order.Select(
                                item =>
                                Build(
                                    lists[item],
                                    path,
                                    guards.Add(new(path.Add(discriminator), tags[item])),
                                    depth + 1))
                            .ToImmutableArray();

                        return new ReturnValueLayout.Node.Variant(index, tagValues, cases);
                    }

                    return Component([]);

                default:
                    throw new NotImplementedException("Build does not handle shape: " + shape.GetType().Name);
            }
        }

        var recipe = Build(results[root], [], [], 0);
        return slots.Count > 256 ? ReturnValueLayout.Canonical : new(recipe, slots.ToImmutable(), taggingPolicy);
    }

    private static string Hash(PineValue value) => Convert.ToHexStringLower(PineValueHashTree.ComputeHash(value).Span);

    private static Shape Expand(Shape shape) =>
        shape is Shape.Constant { Value: PineValue.ListValue list }
        ?
        new Shape.Items([.. list.Items.ToArray().Select(value => (Shape)new Shape.Constant(value))])
        :
        shape;

    private static Shape Join(Shape left, Shape right, int depth = 0)
    {
        if (Same(left, right) || right is Shape.Bottom)
            return left;

        if (left is Shape.Bottom)
            return right;

        if (depth > 16 || left is Shape.Dynamic || right is Shape.Dynamic)
            return new Shape.Dynamic();

        if (left is Shape.Constant && right is Shape.Constant)
            return new Shape.Union([left, right]);

        var expandedLeft = Expand(left);
        var expandedRight = Expand(right);

        if (expandedLeft is Shape.Items a && expandedRight is Shape.Items b && a.Values.Length == b.Values.Length)
            return new Shape.Items([.. a.Values.Zip(b.Values, (x, y) => Join(x, y, depth + 1))]);

        var choices = new List<Shape>();

        foreach (var item in Cases(left).Concat(Cases(right)))
        {
            if (!choices.Any(choice => Same(choice, item)))
                choices.Add(item);
        }

        return choices.Count > 32 ? new Shape.Dynamic() : new Shape.Union([.. choices]);
    }

    private static IEnumerable<Shape> Cases(Shape shape) => shape is Shape.Union union ? union.Cases : [shape];

    private static bool Same(Shape left, Shape right) =>
        left switch
        {
            Shape.Bottom => right is Shape.Bottom,
            Shape.Dynamic => right is Shape.Dynamic,
            Shape.Constant a => right is Shape.Constant b && a.Value.Equals(b.Value),

            Shape.Items a =>
            right is Shape.Items b && a.Values.Length == b.Values.Length &&
            a.Values.Zip(b.Values, Same).All(item => item),

            Shape.Union a =>
            right is Shape.Union b && a.Cases.Length == b.Cases.Length &&
            a.Cases.All(item => b.Cases.Any(other => Same(item, other))),

            _ =>
            throw new NotImplementedException("Same does not handle shape: " + left.GetType().Name)
        };
}
