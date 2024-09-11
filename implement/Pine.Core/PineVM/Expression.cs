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
    public abstract int SubexpressionCount { get; }

    /// <summary>
    /// True if the expression itself or any its subexpressions is of type <see cref="Environment"/>.
    /// </summary>
    public abstract bool ReferencesEnvironment { get; }

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
        : Expression
    {
        public override int SubexpressionCount { get; } = 0;

        public override bool ReferencesEnvironment { get; } = false;
    }

    public record List
        : Expression
    {
        private readonly int slimHashCode;

        public override int SubexpressionCount { get; }

        public override bool ReferencesEnvironment { get; } = false;

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

            SubexpressionCount = items.Count;

            for (int i = 0; i < items.Count; ++i)
            {
                SubexpressionCount += items[i].SubexpressionCount;

                if (items[i].ReferencesEnvironment)
                    ReferencesEnvironment = true;
            }
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

    public record ParseAndEval
        : Expression
    {
        public Expression encoded { get; }

        public Expression environment { get; }

        public override int SubexpressionCount { get; }

        public override bool ReferencesEnvironment { get; }

        public ParseAndEval(
            Expression encoded,
            Expression environment)
        {
            this.encoded = encoded;
            this.environment = environment;

            SubexpressionCount =
                encoded.SubexpressionCount + environment.SubexpressionCount + 2;

            ReferencesEnvironment =
                encoded.ReferencesEnvironment || environment.ReferencesEnvironment;
        }
    }

    public record KernelApplication
        : Expression
    {
        public string function { get; }

        public Expression input { get; }

        public override int SubexpressionCount { get; }

        public override bool ReferencesEnvironment { get; }

        public KernelApplication(
            string function,
            Expression input)
        {
            this.function = function;
            this.input = input;

            SubexpressionCount = input.SubexpressionCount + 1;
            ReferencesEnvironment = input.ReferencesEnvironment;
        }

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

        public override int SubexpressionCount { get; }

        public override bool ReferencesEnvironment { get; }

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

            SubexpressionCount =
                condition.SubexpressionCount +
                falseBranch.SubexpressionCount +
                trueBranch.SubexpressionCount +
                3;

            ReferencesEnvironment =
                condition.ReferencesEnvironment ||
                falseBranch.ReferencesEnvironment ||
                trueBranch.ReferencesEnvironment;
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

    public record Environment : Expression
    {
        public override int SubexpressionCount { get; } = 0;

        public override bool ReferencesEnvironment { get; } = true;
    }

    public record StringTag
        : Expression
    {
        public string tag { get; }

        public Expression tagged { get; }

        public override int SubexpressionCount { get; }

        public override bool ReferencesEnvironment { get; }

        public readonly int slimHashCode;

        public StringTag(
            string tag,
            Expression tagged)
        {
            this.tag = tag;
            this.tagged = tagged;

            SubexpressionCount = tagged.SubexpressionCount + 1;
            ReferencesEnvironment = tagged.ReferencesEnvironment;

            slimHashCode = HashCode.Combine(tag, tagged);
        }

        public override int GetHashCode() =>
            slimHashCode;

        public virtual bool Equals(StringTag? other)
        {
            if (ReferenceEquals(other, this))
                return true;

            if (other is null)
                return false;

            return
                other.slimHashCode == slimHashCode &&
                other.tag == tag &&
                other.tagged == tagged;
        }
    }

    public record StackReferenceExpression(int offset)
        : Expression
    {
        public override int SubexpressionCount { get; } = 0;

        public override bool ReferencesEnvironment { get; } = false;
    }

    /// <summary>
    /// Fusion of the two applications of the kernel functions 'skip' and 'head',
    /// as an implementation detail of the interpreter.
    /// </summary>
    public record KernelApplications_Skip_Head_Path(
        ReadOnlyMemory<int> SkipCounts,
        Expression Argument)
    : Expression
    {
        public override int SubexpressionCount { get; } =
            Argument.SubexpressionCount + 1;

        public override bool ReferencesEnvironment { get; } =
            Argument.ReferencesEnvironment;

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
        : Expression
    {
        public override int SubexpressionCount { get; } =
            left.SubexpressionCount + right.SubexpressionCount + 2;

        public override bool ReferencesEnvironment { get; } =
            left.ReferencesEnvironment || right.ReferencesEnvironment;
    }

    public static IReadOnlySet<Expression> CollectAllComponentsFromRoots(
        IEnumerable<Expression> roots)
    {
        var components = new HashSet<Expression>();

        var stack = new Stack<Expression>(roots);

        while (stack.TryPop(out var expression))
        {
            if (components.Contains(expression))
                continue;

            components.Add(expression);

            IReadOnlyList<Expression> childItems =
                expression switch
                {
                    List listExpression =>
                    listExpression.items,

                    Conditional conditionalExpression =>
                    [
                        conditionalExpression.condition,
                        conditionalExpression.falseBranch,
                        conditionalExpression.trueBranch
                    ],

                    KernelApplication kernelApplicationExpression =>
                    [kernelApplicationExpression.input],

                    ParseAndEval parseAndEval =>
                    [parseAndEval.environment, parseAndEval.encoded],

                    StringTag stringTagExpression =>
                    [stringTagExpression.tagged],

                    Literal =>
                    [],

                    Environment =>
                    [],

                    StackReferenceExpression =>
                    [],

                    _ =>
                    throw new NotImplementedException(
                        "Unexpected expression type: " + expression.GetType())
                };

            foreach (var item in childItems)
            {
                stack.Push(item);
            }
        }

        return components;
    }

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
                    throw new NotImplementedException(
                        "Unknown expression type: " + expression.GetType().FullName);
            }
        }
    }
}
