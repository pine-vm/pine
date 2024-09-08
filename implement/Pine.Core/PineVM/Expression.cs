using Pine.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json.Serialization;

namespace Pine.PineVM;

public delegate Result<string, PineValue> EvalExprDelegate(Expression expression, PineValue environment);

public delegate EvalExprDelegate? OverrideEvalExprDelegate(EvalExprDelegate evalExprDelegate);

public delegate Result<string, Expression> ParseExprDelegate(PineValue value);

public delegate ParseExprDelegate OverrideParseExprDelegate(ParseExprDelegate parseExprDelegate);


[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Expression
{
    public static readonly Expression EnvironmentInstance = new Environment();

    public static readonly Literal LiteralEmptyListInstance =
        LiteralInstance(PineValue.EmptyList);

    public static readonly Literal LiteralEmptyBlobInstance =
        LiteralInstance(PineValue.EmptyBlob);

    public static Expression EnsureReuseInstanceGeneral(Expression expression)
    {
        if (ReusedInstances.Instance?.Expressions?.TryGetValue(expression, out var reused) ?? false)
        {
            return reused;
        }

        return expression;
    }

    public static Literal LiteralInstance(PineValue pineValue)
    {
        if (ReusedInstances.Instance.LiteralExpressions?.TryGetValue(pineValue, out var literal) ?? false)
            return literal;

        return new Literal(pineValue);
    }

    public static List ListInstance(IReadOnlyList<Expression> items)
    {
        var listKey = new List.ListStruct(items);

        if (ReusedInstances.Instance.ListExpressions is { } ReusedListExpressions)
        {
            if (ReusedListExpressions.TryGetValue(listKey, out var list))
                return list;
        }

        return new List(listKey);
    }

    public static Conditional ConditionalInstance(
        Expression condition,
        Expression falseBranch,
        Expression trueBranch)
    {
        var conditionalStruct =
            new Conditional.ConditionalStruct(condition, falseBranch, trueBranch);

        if (ReusedInstances.Instance.ConditionalExpressions is { } ReusedConditionalExpressions)
        {
            if (ReusedConditionalExpressions.TryGetValue(conditionalStruct, out var conditional))
                return conditional;
        }

        return new Conditional(conditionalStruct);
    }

    public record Literal(
        PineValue Value)
        : Expression;

    public record List
        : Expression
    {
        private readonly int slimHashCode;

        public IReadOnlyList<Expression> items { get; }

        internal List(IReadOnlyList<Expression> items)
            :
            this(new ListStruct(items))
        {
        }

        internal List(ListStruct listKey)
        {
            items = listKey.Items;

            slimHashCode = listKey.slimHashCode;
        }

        public virtual bool Equals(List? other)
        {
            if (other is not { } notNull)
                return false;

            if (ReferenceEquals(this, notNull))
                return true;

            if (!(slimHashCode == notNull.slimHashCode))
                return false;

            if (items.Count != notNull.items.Count)
                return false;

            for (int i = 0; i < items.Count; ++i)
            {
                if (!items[i].Equals(notNull.items[i]))
                    return false;
            }

            return true;
        }

        public override int GetHashCode() =>
            slimHashCode;

        internal readonly record struct ListStruct
        {
            public IReadOnlyList<Expression> Items { get; }

            internal readonly int slimHashCode;

            public ListStruct(IReadOnlyList<Expression> items)
            {
                Items = items;

                slimHashCode = ComputeHashCode(items);
            }

            public override int GetHashCode()
            {
                return slimHashCode;
            }

            public static int ComputeHashCode(IReadOnlyList<Expression> items)
            {
                var hashCode = new HashCode();

                for (int i = 0; i < items.Count; ++i)
                {
                    hashCode.Add(items[i].GetHashCode());
                }

                return hashCode.ToHashCode();
            }
        }
    }

    public record ParseAndEval(
        Expression encoded,
        Expression environment)
        : Expression;

    public record KernelApplication(
        string function,
        Expression input)
        : Expression
    {
        public virtual bool Equals(KernelApplication? other)
        {
            if (other is not { } notNull)
                return false;

            return
                notNull.function == function &&
                (ReferenceEquals(notNull.input, input) || notNull.input.Equals(input));
        }

        public override int GetHashCode()
        {
            var hash = new HashCode();

            hash.Add(function);
            hash.Add(input);

            return hash.ToHashCode();
        }
    }

    public record Conditional
        : Expression
    {
        private readonly int slimHashCode;

        public Expression condition { get; }

        public Expression falseBranch { get; }

        public Expression trueBranch { get; }

        internal Conditional(
            Expression condition,
            Expression falseBranch,
            Expression trueBranch)
            :
            this(new ConditionalStruct(condition, falseBranch, trueBranch))
        {
        }

        internal Conditional(
            ConditionalStruct conditionalStruct)
        {
            condition = conditionalStruct.Condition;
            falseBranch = conditionalStruct.FalseBranch;
            trueBranch = conditionalStruct.TrueBranch;

            slimHashCode = conditionalStruct.slimHashCode;
        }

        public virtual bool Equals(Conditional? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is not { } notNull)
                return false;

            return
                slimHashCode == notNull.slimHashCode &&
                condition.Equals(notNull.condition) &&
                falseBranch.Equals(notNull.falseBranch) &&
                trueBranch.Equals(notNull.trueBranch);
        }

        public override int GetHashCode() =>
            slimHashCode;

        public record struct ConditionalStruct
        {
            public Expression Condition { get; }

            public Expression FalseBranch { get; }

            public Expression TrueBranch { get; }

            public readonly int slimHashCode;

            public ConditionalStruct(
                Expression condition,
                Expression falseBranch,
                Expression trueBranch)
            {
                Condition = condition;
                FalseBranch = falseBranch;
                TrueBranch = trueBranch;

                slimHashCode = ComputeHashCode(condition, falseBranch, trueBranch);
            }

            public override int GetHashCode() =>
                slimHashCode;

            public bool Equals(ConditionalStruct other)
            {
                return
                    other.slimHashCode == slimHashCode &&
                    other.Condition.Equals(Condition) &&
                    other.FalseBranch.Equals(FalseBranch) &&
                    other.TrueBranch.Equals(TrueBranch);
            }

            public static int ComputeHashCode(
                Expression condition,
                Expression falseBranch,
                Expression trueBranch)
            {
                return HashCode.Combine(condition, falseBranch, trueBranch);
            }
        }
    }

    public record Environment : Expression;

    public record StringTag(
        string tag,
        Expression tagged)
        : Expression;

    public record StackReferenceExpression(int offset)
        : Expression;

    /// <summary>
    /// Fusion of the two applications of the kernel functions 'skip' and 'head',
    /// as an implementation detail of the interpreter.
    /// </summary>
    public record KernelApplications_Skip_Head_Path(
        ReadOnlyMemory<int> SkipCounts,
        Expression Argument)
        : Expression
    {
        public virtual bool Equals(KernelApplications_Skip_Head_Path? other)
        {
            if (other is null)
                return false;

            return
                other.SkipCounts.Span.SequenceEqual(SkipCounts.Span) &&
                Argument.Equals(Argument);
        }

        public override int GetHashCode()
        {
            var hashCode = Argument.GetHashCode();

            for (int i = 0; i < SkipCounts.Length; ++i)
            {
                hashCode = HashCode.Combine(SkipCounts.Span[i], hashCode);
            }

            return hashCode;
        }
    }

    /// <summary>
    /// Fusion of the special case of an application of the kernel function 'equal',
    /// with a list expression of length two as the argument.
    /// </summary>
    public record KernelApplication_Equal_Two(
        Expression left,
        Expression right)
        : Expression;

    /// <summary>
    /// Returns true if the expression is independent of the environment, that is none of the expressions in the tree contain an <see cref="Environment"/>.
    /// </summary>
    public static bool IsIndependent(Expression expression) =>
        expression switch
        {
            Environment =>
            false,

            Literal =>
            true,

            List list =>
            list.items.All(IsIndependent),

            ParseAndEval decodeAndEvaluate =>
            IsIndependent(decodeAndEvaluate.encoded) && IsIndependent(decodeAndEvaluate.environment),

            KernelApplication kernelApplication =>
            IsIndependent(kernelApplication.input),

            Conditional conditional =>
            IsIndependent(conditional.condition) && IsIndependent(conditional.trueBranch) && IsIndependent(conditional.falseBranch),

            StringTag stringTag =>
            IsIndependent(stringTag.tagged),

            StackReferenceExpression =>
            false,

            KernelApplications_Skip_Head_Path fused =>
            IsIndependent(fused.Argument),

            KernelApplication_Equal_Two fused =>
            IsIndependent(fused.left) && IsIndependent(fused.right),

            _ =>
            throw new NotImplementedException(),
        };

    public static IEnumerable<Expression> EnumerateSelfAndDescendants(
        Expression expression) =>
        EnumerateSelfAndDescendants(expression, skipDescendants: null);

    public static IEnumerable<Expression> EnumerateSelfAndDescendants(
        Expression rootExpression,
        Func<Expression, bool>? skipDescendants)
    {
        var stack = new Stack<Expression>([rootExpression]);

        while (stack.TryPop(out var expression))
        {
            yield return expression;

            if (skipDescendants?.Invoke(expression) ?? false)
                continue;

            switch (expression)
            {
                case Environment:
                case Literal:
                    break;

                case List list:
                    for (var i = 0; i < list.items.Count; i++)
                    {
                        stack.Push(list.items[i]);
                    }
                    break;

                case ParseAndEval parseAndEvaluate:

                    stack.Push(parseAndEvaluate.encoded);
                    stack.Push(parseAndEvaluate.environment);

                    break;

                case KernelApplication kernelApplication:

                    stack.Push(kernelApplication.input);

                    break;

                case Conditional conditional:

                    stack.Push(conditional.condition);
                    stack.Push(conditional.falseBranch);
                    stack.Push(conditional.trueBranch);

                    break;

                case StringTag stringTag:

                    stack.Push(stringTag.tagged);

                    break;

                case StackReferenceExpression:
                    break;

                case KernelApplication_Equal_Two equalTwo:

                    stack.Push(equalTwo.left);
                    stack.Push(equalTwo.right);

                    break;

                case KernelApplications_Skip_Head_Path skipHead:

                    stack.Push(skipHead.Argument);

                    break;

                default:
                    throw new NotImplementedException("Unknown expression type: " + expression.GetType().FullName);
            }
        }
    }
}
