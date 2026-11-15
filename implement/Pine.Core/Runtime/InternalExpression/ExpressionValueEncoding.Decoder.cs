using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Pine.Core.Runtime.InternalExpression;

public static partial class ExpressionValueEncoding
{
    private enum Kind
    {
        Literal, List, Eval, Builtin, Conditional, Environment, Label, BuiltinBinary, Length_Equal_Binary, HeadSkip
    }

    private readonly record struct Shape(
        Kind Kind, ReadOnlyMemory<PineValue> Items = default, PineValue? Payload = null,
        PineValue? First = null, PineValue? Second = null, PineValue? Third = null,
        BuiltinFunction Function = default, int Count = default, bool LengthOnLeft = true)
    {
        internal int ChildCount =>
            Kind is Kind.List
            ?
            Items.Length
            :
            Third is not null ? 3 : Second is not null ? 2 : First is not null ? 1 : 0;

        internal PineValue Child(int index) =>
            Kind is Kind.List
            ?
            Items.Span[index]
            :
            index switch
            {
                0 => First!,
                1 => Second!,
                2 => Third!,

                _ =>
                throw new ArgumentOutOfRangeException(nameof(index))
            };
    }

    private readonly record struct Work(PineValue Node, Shape Shape = default, bool Expanded = false);

    /// <summary>
    /// Parses 2026 canonical values directly, retaining payloads and shared children.
    /// Fusion skips discarded builtin/list/literal wrappers. Errors are returned explicitly.
    /// </summary>
    public static Result<string, Expression> ParseExpressionFromValue(PineValue value, bool fusePopularShapes = true)
    {
        var parsed = new Dictionary<PineValue, ValueResult<string, Expression>>(ReferenceEqualityComparer.Instance);
        var root = ParseExpressionFromValueCached(value, fusePopularShapes, parsed);
        return root.IsOk ? Result<string, Expression>.ok(root.Value) : Result<string, Expression>.err(root.Error);
    }

    internal static ValueResult<string, Expression> ParseExpressionFromValueCached(
        PineValue value, bool fusePopularShapes, IDictionary<PineValue, ValueResult<string, Expression>> parsed)
    {
        ArgumentNullException.ThrowIfNull(value);

        if (parsed.TryGetValue(value, out var cached))
            return cached;

        var pending = new Stack<Work>();
        pending.Push(new(value));

        while (pending.TryPop(out var work))
        {
            if (parsed.ContainsKey(work.Node))
                continue;

            ValueResult<string, Expression> result;

            if (!work.Expanded)
            {
                var shape = ReadShape(work.Node, fusePopularShapes);

                if (shape.IsOk)
                {
                    pending.Push(work with { Shape = shape.Value, Expanded = true });

                    for (var index = shape.Value.ChildCount - 1; index >= 0; index--)
                        pending.Push(new(shape.Value.Child(index)));

                    continue;
                }

                result = shape.Error;
            }
            else
                result = Construct(work.Shape, parsed);

            parsed[work.Node] = result;
        }

        return parsed[value];
    }

    private static ValueResult<string, Expression> Construct(
        Shape shape, IDictionary<PineValue, ValueResult<string, Expression>> parsed)
    {
        for (var index = 0; index < shape.ChildCount; index++)
        {
            var child = parsed[shape.Child(index)];

            if (child.IsErr)
                return "Failed to parse " + shape.Kind + " child " + index + ": " + child.Error;
        }

        Expression Child(int index) => parsed[shape.Child(index)].Value;

        Expression.List BuildList()
        {
            var items = new Expression[shape.Items.Length];

            for (var index = 0; index < items.Length; index++)
                items[index] = Child(index);

            return new(ImmutableCollectionsMarshal.AsImmutableArray(items));
        }

        return shape.Kind switch
        {
            Kind.Literal => new Expression.Literal(shape.Payload!),
            Kind.List => BuildList(),
            Kind.Eval => new Expression.Eval(Child(0), Child(1)),
            Kind.Builtin => new Expression.Builtin(shape.Function, Child(0)),
            Kind.Conditional => new Expression.Conditional(Child(0), Child(1), Child(2)),
            Kind.Environment => Expression.Environment.Instance,
            Kind.Label => new Expression.Label(shape.Payload!, Child(0)),
            Kind.BuiltinBinary => new Expression.BuiltinBinary(shape.Function, Child(0), Child(1)),
            Kind.Length_Equal_Binary => new Expression.Length_Equal_Binary(Child(0), Child(1), shape.LengthOnLeft),
            Kind.HeadSkip => new Expression.HeadSkip(shape.Count, Child(0)),

            _ =>
            throw new NotImplementedException(nameof(Construct) + " does not handle shape: " + shape.Kind)
        };
    }

    private static ValueResult<string, Shape> ReadShape(PineValue value, bool fuse)
    {
        if (value is not PineValue.ListValue list || list.Items.Length is 0)
            return "Expected a nonempty expression list.";

        var tag = list.Items.Span[0];
        var fields = list.Items[1..];

        if (tag.Equals(s_literalTag))
        {
            return
                fields.Length is 1
                ?
                new Shape(Kind.Literal, Payload: fields.Span[0])
                :
                "Invalid literal arity.";
        }

        if (tag.Equals(s_environmentTag))
            return fields.Length is 0 ? new Shape(Kind.Environment) : "Invalid environment arity.";

        if (tag.Equals(s_listTag))
            return new Shape(Kind.List, Items: fields);

        if (tag.Equals(s_evalTag))
        {
            return
                fields.Length is 2
                ?
                new Shape(Kind.Eval, First: fields.Span[0], Second: fields.Span[1])
                :
                "Invalid Eval arity.";
        }

        if (tag.Equals(s_conditionalTag) || tag.Equals(s_conditionTag))
        {
            return
                fields.Length is 3
                ?
                new Shape(Kind.Conditional, First: fields.Span[0], Second: fields.Span[1], Third: fields.Span[2])
                :
                "Invalid conditional arity.";
        }

        if (tag.Equals(s_labelTag))
        {
            if (fields.Length is not 2)
                return "Invalid label arity.";

            return new Shape(Kind.Label, Payload: fields.Span[0], First: fields.Span[1]);
        }

        if (tag.Equals(s_builtinTag))
        {
            if (fields.Length != 2)
                return "Invalid builtin arity.";

            var function = Array.IndexOf(s_builtinNames, fields.Span[0]);

            if (function < 0)
            {
                var name = StringEncoding.StringFromValueWithoutResultAllocation(fields.Span[0]);

                if (!name.IsOk)
                    return "Invalid builtin name: " + name.Error;

                if (!BuiltinFunctions.TryFromName(name.Value!, out var identifier))
                    return "Unknown Pine builtin function: " + name.Value;

                function = (int)identifier;
            }

            var input = fields.Span[1];
            var builtin = new Shape(Kind.Builtin, First: input, Function: (BuiltinFunction)function);
            return fuse ? Fuse(builtin) : builtin;
        }

        var unknown = StringEncoding.StringFromValueWithoutResultAllocation(tag);
        return unknown.IsOk ? "Unknown expression tag: " + unknown.Value : "Invalid expression tag: " + unknown.Error;
    }

    private static Shape Fuse(Shape builtin)
    {
        if (TryPair(builtin.First!, out var left, out var right))
        {
            if (builtin.Function is BuiltinFunction.Equal && TryBuiltin(left, BuiltinFunction.Length, out var sequence))
                return new(Kind.Length_Equal_Binary, First: sequence, Second: right);

            if (builtin.Function is BuiltinFunction.Equal && TryBuiltin(right, BuiltinFunction.Length, out sequence))
                return new(Kind.Length_Equal_Binary, First: sequence, Second: left, LengthOnLeft: false);

            return new(Kind.BuiltinBinary, First: left, Second: right, Function: builtin.Function);
        }

        if (builtin.Function is BuiltinFunction.Head &&
            TryBuiltin(builtin.First!, BuiltinFunction.Skip, out var skip) &&
            TryPair(skip, out var count, out var input))
        {
            var literal = ReadShape(count, fuse: false);

            if (literal.IsOk && literal.Value.Kind is Kind.Literal &&
                literal.Value.Payload is PineValue.BlobValue blob &&
                ExpressionBinary.TryCanonicalInt32(blob.Bytes.Span, out var integer))
            {
                return new(Kind.HeadSkip, First: input, Count: integer);
            }
        }

        return builtin;
    }

    private static bool TryBuiltin(PineValue value, BuiltinFunction function, out PineValue input)
    {
        var shape = ReadShape(value, fuse: false);

        if (shape.IsOk && shape.Value.Kind is Kind.Builtin && shape.Value.Function == function)
        {
            input = shape.Value.First!;
            return true;
        }

        input = value;
        return false;
    }

    private static bool TryPair(PineValue value, out PineValue left, out PineValue right)
    {
        var shape = ReadShape(value, fuse: false);

        if (shape.IsOk && shape.Value.Kind is Kind.List && shape.Value.Items.Length is 2)
        {
            left = shape.Value.Items.Span[0];
            right = shape.Value.Items.Span[1];
            return true;
        }

        left = right = value;
        return false;
    }
}
