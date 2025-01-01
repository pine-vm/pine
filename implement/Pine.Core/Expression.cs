using Pine.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json.Serialization;

namespace Pine.Core;

public delegate Result<string, PineValue> EvalExprDelegate(Expression expression, PineValue environment);

public delegate Result<string, Expression> ParseExprDelegate(PineValue value);


/// <summary>
/// An expression in the Pine language.
/// 
/// For a listing of expression types in the Pine language, see the <see href="https://github.com/pine-vm/pine/blob/cc29e742801ef0d5121682dd8cadeaa4af447757/implement/pine/Elm/elm-compiler/src/Pine.elm#L46-L63"/>>
/// </summary>
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Expression
{
    /// <summary>
    /// Number of subexpressions contained in this expression.
    /// </summary>
    public abstract int SubexpressionCount { get; }

    /// <summary>
    /// True if the expression itself or any its subexpressions is of type <see cref="Environment"/>.
    /// </summary>
    public abstract bool ReferencesEnvironment { get; }

    /// <summary>
    /// Instance of the <see cref="Environment"/> expression type.
    /// </summary>
    public static readonly Expression EnvironmentInstance = new Environment();

    /// <summary>
    /// For a given expression, checks if an equivalent instance is available in the cache of reused instances.
    /// Returns the reused instance if available, otherwise returns the input expression.
    /// </summary>
    public static Expression EnsureReuseInstanceGeneral(Expression expression)
    {
        if (ReusedInstances.Instance?.Expressions?.TryGetValue(expression, out var reused) ?? false)
        {
            return reused;
        }

        return expression;
    }

    /// <summary>
    /// Returns an instance of the <see cref="Literal"/> expression type for the given <see cref="PineValue"/>.
    /// 
    /// Checks the cache of reused instances to avoid allocating a new instance if an equivalent instance is already available.
    /// </summary>
    public static Literal LiteralInstance(PineValue pineValue)
    {
        if (ReusedInstances.Instance.LiteralExpressions?.TryGetValue(pineValue, out var literal) ?? false)
            return literal;

        return new Literal(pineValue);
    }

    /// <summary>
    /// Returns an instance of the <see cref="List"/> expression type for the given list of subexpressions.
    /// 
    /// Checks the cache of reused instances to avoid allocating a new instance if an equivalent instance is already available.
    /// </summary>
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

    /// <summary>
    /// Returns an instance of the <see cref="Conditional"/> expression type for the given condition and branches.
    /// 
    /// Checks the cache of reused instances to avoid allocating a new instance if an equivalent instance is already available.
    /// </summary>
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

    /// <summary>
    /// A literal expression only contains a concrete value.
    /// </summary>
    public record Literal(
        PineValue Value)
        : Expression
    {
        /// <summary>
        /// Always returns zero, as a <see cref="Literal"/> expression does not contain any subexpressions.
        /// </summary>
        public override int SubexpressionCount { get; } = 0;

        /// <summary>
        /// Always returns false, as a <see cref="Literal"/> expression does not contain any subexpressions.
        /// </summary>
        public override bool ReferencesEnvironment { get; } = false;
    }

    /// <summary>
    /// A list expression contains a list of subexpressions.
    /// </summary>
    public record List
        : Expression
    {
        private readonly int slimHashCode;

        /// <ihneritdoc/>
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; } = false;

        /// <summary>
        /// The list of subexpressions.
        /// </summary>
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

        /// <inheritdoc/>
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

        /// <inheritdoc/>
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

    /// <summary>
    /// A Parse-and-Eval expression allows for the instantiation of a new environment and the evaluation of an encoded expression.
    /// 
    /// Similar to 'eval' features in other languages, this expression enables meta programming by treating data as a program.
    /// 
    /// Program execution crashes if the value obtained via the <see cref="Encoded"/> expression part
    /// is not a valid encoding of a Pine expression.
    /// </summary>
    public record ParseAndEval
        : Expression
    {
        /// <summary>
        /// Subexpression that is evaluated to obtain the value to be parsed as a Pine expression.
        /// </summary>
        public Expression Encoded { get; }

        /// <summary>
        /// Subexpression that is evaluated to obtain the environment value for the evaluation of the encoded expression.
        /// </summary>
        new public Expression Environment { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        /// <summary>
        /// Creates a new instance of a Parse-and-Eval expression.
        /// </summary>
        public ParseAndEval(
            Expression encoded,
            Expression environment)
        {
            Encoded = encoded;
            Environment = environment;

            SubexpressionCount =
                encoded.SubexpressionCount + environment.SubexpressionCount + 2;

            ReferencesEnvironment =
                encoded.ReferencesEnvironment || environment.ReferencesEnvironment;
        }
    }

    /// <summary>
    /// Application of a kernel function to an input expression.
    /// 
    /// Kernel functions are the built-in functions of the Pine language.
    /// 
    /// Kernel functions never crash the program, but may return default values in case of nonsensical input.
    /// Therefore it remains the responsibility of the caller to add branches for error messages as needed.
    /// </summary>
    public record KernelApplication
        : Expression
    {
        /// <summary>
        /// The name of the kernel function to be applied.
        /// </summary>
        public string Function { get; }

        /// <summary>
        /// Input for the kernel function.
        /// </summary>
        public Expression Input { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        /// <summary>
        /// Creates a new instance of a kernel application.
        /// </summary>
        public KernelApplication(
            string function,
            Expression input)
        {
            Function = function;
            Input = input;

            SubexpressionCount = input.SubexpressionCount + 1;
            ReferencesEnvironment = input.ReferencesEnvironment;
        }

        /// <inheritdoc/>
        public virtual bool Equals(KernelApplication? other)
        {
            if (other is not { } notNull)
                return false;

            return
                notNull.Function == Function &&
                (ReferenceEquals(notNull.Input, Input) || notNull.Input.Equals(Input));
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hash = new HashCode();

            hash.Add(Function);
            hash.Add(Input);

            return hash.ToHashCode();
        }
    }

    /// <summary>
    /// A conditional expression contains a condition and two branches.
    /// </summary>
    public record Conditional
        : Expression
    {
        private readonly int slimHashCode;

        /// <summary>
        /// The expression evaluated to determine which branch to take.
        /// </summary>
        public Expression Condition { get; }

        /// <summary>
        /// The expression evaluated if the condition does not evaluate to true.
        /// </summary>
        public Expression FalseBranch { get; }

        /// <summary>
        /// The expression evaluated if the condition evaluates to true.
        /// </summary>
        public Expression TrueBranch { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
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
            Condition = conditionalStruct.Condition;
            FalseBranch = conditionalStruct.FalseBranch;
            TrueBranch = conditionalStruct.TrueBranch;

            slimHashCode = conditionalStruct.slimHashCode;

            SubexpressionCount =
                Condition.SubexpressionCount +
                FalseBranch.SubexpressionCount +
                TrueBranch.SubexpressionCount +
                3;

            ReferencesEnvironment =
                Condition.ReferencesEnvironment ||
                FalseBranch.ReferencesEnvironment ||
                TrueBranch.ReferencesEnvironment;
        }

        /// <inheritdoc/>
        public virtual bool Equals(Conditional? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is not { } notNull)
                return false;

            return
                slimHashCode == notNull.slimHashCode &&
                Condition.Equals(notNull.Condition) &&
                FalseBranch.Equals(notNull.FalseBranch) &&
                TrueBranch.Equals(notNull.TrueBranch);
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            slimHashCode;

        internal readonly record struct ConditionalStruct
        {
            public Expression Condition { get; }

            public Expression FalseBranch { get; }

            public Expression TrueBranch { get; }

            internal readonly int slimHashCode;

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

            /// <inheritdoc/>
            public override readonly int GetHashCode() =>
                slimHashCode;

            /// <inheritdoc/>
            public readonly bool Equals(ConditionalStruct other)
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

    /// <summary>
    /// The environment expression is the only way to parameterize an expression with an environment.
    /// 
    /// Since the environment is a <see cref="PineValue"/>,
    /// it can be used to pass any data to an expression and package any number of values in lists.
    /// </summary>
    public record Environment : Expression
    {
        /// <inheritdoc/>
        public override int SubexpressionCount { get; } = 0;

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; } = true;
    }

    /// <summary>
    /// A string tag expression attaches a string tag to an expression.
    /// These tags are not meant to be observable by the program, but to help with inspection, tracing and profiling.
    /// </summary>
    public record StringTag
        : Expression
    {
        /// <summary>
        /// The tag string.
        /// This tag is not meant to be observable by the program, but to help with inspection, tracing and profiling.
        /// </summary>
        public string Tag { get; }

        /// <summary>
        /// Fully determines the value returned by this expression.
        /// </summary>
        public Expression Tagged { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        internal readonly int slimHashCode;

        /// <summary>
        /// Creates a new instance of a string tag expression.
        /// </summary>
        public StringTag(
            string tag,
            Expression tagged)
        {
            Tag = tag;
            Tagged = tagged;

            SubexpressionCount = tagged.SubexpressionCount + 1;
            ReferencesEnvironment = tagged.ReferencesEnvironment;

            slimHashCode = HashCode.Combine(tag, tagged);
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            slimHashCode;

        /// <inheritdoc/>
        public virtual bool Equals(StringTag? other)
        {
            if (ReferenceEquals(other, this))
                return true;

            if (other is null)
                return false;

            return
                other.slimHashCode == slimHashCode &&
                other.Tag == Tag &&
                other.Tagged == Tagged;
        }
    }

    /// <summary>
    /// A reference to a value on the stack by its offset relative to the current stack pointer.
    /// </summary>
    public record StackReferenceExpression(int offset)
        : Expression
    {
        /// <inheritdoc/>
        public override int SubexpressionCount { get; } = 0;

        /// <inheritdoc/>
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
        /// <inheritdoc/>
        public override int SubexpressionCount { get; } =
            Argument.SubexpressionCount + 1;

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; } =
            Argument.ReferencesEnvironment;

        /// <inheritdoc/>
        public virtual bool Equals(KernelApplications_Skip_Head_Path? other)
        {
            if (other is null)
                return false;

            return
                other.SkipCounts.Span.SequenceEqual(SkipCounts.Span) &&
                Argument.Equals(Argument);
        }

        /// <inheritdoc/>
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
    /// Fusion of the two applications of the kernel functions 'skip' and 'take',
    /// as an implementation detail of the interpreter.
    /// </summary>
    public record KernelApplications_Skip_Take(
        Expression SkipCount,
        Expression TakeCount,
        Expression Argument)
    : Expression
    {
        /// <inheritdoc/>
        public override int SubexpressionCount { get; } =
            Argument.SubexpressionCount +
            TakeCount.SubexpressionCount +
            SkipCount.SubexpressionCount + 3;

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; } =
            Argument.ReferencesEnvironment ||
            TakeCount.ReferencesEnvironment ||
            SkipCount.ReferencesEnvironment;

        /// <inheritdoc/>
        public virtual bool Equals(KernelApplications_Skip_Take? other)
        {
            if (other is null)
                return false;

            return
                other.SkipCount.Equals(SkipCount) &&
                other.TakeCount.Equals(TakeCount) &&
                other.Argument.Equals(Argument);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            return HashCode.Combine(SkipCount.GetHashCode(), TakeCount.GetHashCode(), Argument.GetHashCode());
        }
    }

    /// <summary>
    /// Fusion of the special case of an application of the kernel function 'equal',
    /// with a list expression of length two as the argument.
    /// </summary>
    public record KernelApplication_Equal_Two(
        Expression Left,
        Expression Right)
        : Expression
    {
        /// <inheritdoc/>
        public override int SubexpressionCount { get; } =
            Left.SubexpressionCount + Right.SubexpressionCount + 2;

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; } =
            Left.ReferencesEnvironment || Right.ReferencesEnvironment;
    }

    /// <summary>
    /// Collects all unique expressions contained in the given roots and their descendants.
    /// </summary>
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
                        conditionalExpression.Condition,
                        conditionalExpression.FalseBranch,
                        conditionalExpression.TrueBranch
                    ],

                    KernelApplication kernelApplicationExpression =>
                    [kernelApplicationExpression.Input],

                    ParseAndEval parseAndEval =>
                    [parseAndEval.Environment, parseAndEval.Encoded],

                    StringTag stringTagExpression =>
                    [stringTagExpression.Tagged],

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

    /// <summary>
    /// Enumerates the given expression and all its descendants.
    /// </summary>
    public static IEnumerable<Expression> EnumerateSelfAndDescendants(
        Expression expression) =>
        EnumerateSelfAndDescendants(expression, skipDescendants: null);

    /// <summary>
    /// Enumerates the given expression and all its descendants.
    /// </summary>
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

                    stack.Push(parseAndEvaluate.Encoded);
                    stack.Push(parseAndEvaluate.Environment);

                    break;

                case KernelApplication kernelApplication:

                    stack.Push(kernelApplication.Input);

                    break;

                case Conditional conditional:

                    stack.Push(conditional.Condition);
                    stack.Push(conditional.FalseBranch);
                    stack.Push(conditional.TrueBranch);

                    break;

                case StringTag stringTag:

                    stack.Push(stringTag.Tagged);

                    break;

                case StackReferenceExpression:
                    break;

                case KernelApplication_Equal_Two equalTwo:

                    stack.Push(equalTwo.Left);
                    stack.Push(equalTwo.Right);

                    break;

                case KernelApplications_Skip_Head_Path skipHead:

                    stack.Push(skipHead.Argument);

                    break;

                case KernelApplications_Skip_Take skipTake:

                    stack.Push(skipTake.Argument);
                    stack.Push(skipTake.SkipCount);
                    stack.Push(skipTake.TakeCount);

                    break;

                default:
                    throw new NotImplementedException(
                        "Unknown expression type: " + expression.GetType().FullName);
            }
        }
    }
}
