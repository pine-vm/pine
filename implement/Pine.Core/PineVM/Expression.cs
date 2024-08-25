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

    public record Literal(
        PineValue Value)
        : Expression;

    public record List
        : Expression
    {
        private readonly int slimHashCode;

        public IReadOnlyList<Expression> items { get; }

        public List(IReadOnlyList<Expression> items)
        {
            this.items = items;

            var hashCode = new HashCode();

            foreach (var item in items)
            {
                hashCode.Add(item.GetHashCode());
            }

            slimHashCode = hashCode.ToHashCode();
        }

        public virtual bool Equals(List? other)
        {
            if (other is not { } notNull)
                return false;

            return
                ReferenceEquals(this, notNull) ||
                slimHashCode == notNull.slimHashCode &&
                items.Count == notNull.items.Count &&
                items.SequenceEqual(notNull.items);
        }

        public override int GetHashCode() =>
            slimHashCode;
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

        public Conditional(
            Expression condition,
            Expression falseBranch,
            Expression trueBranch)
        {
            this.condition = condition;
            this.falseBranch = falseBranch;
            this.trueBranch = trueBranch;

            slimHashCode = HashCode.Combine(condition, falseBranch, trueBranch);
        }

        public virtual bool Equals(Conditional? other)
        {
            if (other is not { } notNull)
                return false;

            return
                ReferenceEquals(this, notNull) ||
                slimHashCode == notNull.slimHashCode &&
                condition.Equals(notNull.condition) &&
                falseBranch.Equals(notNull.falseBranch) &&
                trueBranch.Equals(notNull.trueBranch);
        }

        public override int GetHashCode() =>
            slimHashCode;
    }

    public record Environment : Expression;

    public record StringTag(
        string tag,
        Expression tagged)
        : Expression;

    public record DelegatingExpression(Func<EvalExprDelegate, PineValue, Result<string, PineValue>> Delegate)
        : Expression;

    public record StackReferenceExpression(int offset)
        : Expression;

    /// <summary>
    /// Fusion of the two applications of the kernel functions 'skip' and 'list_head',
    /// as an implementation detail of the interpreter.
    /// </summary>
    public record KernelApplications_Skip_ListHead_Path(
        ReadOnlyMemory<int> SkipCounts,
        Expression Argument)
        : Expression
    {
        public virtual bool Equals(KernelApplications_Skip_ListHead_Path? other)
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

            DelegatingExpression =>
            false,

            StackReferenceExpression =>
            false,

            KernelApplications_Skip_ListHead_Path fused =>
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
                case DelegatingExpression:
                    break;

                case List list:
                    foreach (var item in list.items)
                    {
                        stack.Push(item);
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

                case KernelApplications_Skip_ListHead_Path skipListHead:

                    stack.Push(skipListHead.Argument);

                    break;

                default:
                    throw new NotImplementedException("Unknown expression type: " + expression.GetType().FullName);
            }
        }
    }
}
