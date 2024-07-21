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
    public static readonly Expression Environment = new EnvironmentExpression();

    public record LiteralExpression(
        PineValue Value)
        : Expression;

    public record ListExpression
        : Expression
    {
        private readonly int slimHashCode;

        public IReadOnlyList<Expression> List { get; }

        public ListExpression(IReadOnlyList<Expression> List)
        {
            this.List = List;

            var hashCode = new HashCode();

            foreach (var item in List)
            {
                hashCode.Add(item.GetHashCode());
            }

            slimHashCode = hashCode.ToHashCode();
        }

        public virtual bool Equals(ListExpression? other)
        {
            if (other is not { } notNull)
                return false;

            return
                ReferenceEquals(this, notNull) ||
                slimHashCode == notNull.slimHashCode &&
                List.Count == notNull.List.Count &&
                List.SequenceEqual(notNull.List);
        }

        public override int GetHashCode() =>
            slimHashCode;
    }

    public record ParseAndEvalExpression(
        Expression expression,
        Expression environment)
        : Expression;

    public record KernelApplicationExpression(
        string functionName,
        Expression argument,

        [property: JsonIgnore]
            Func<PineValue, PineValue> function)
        : Expression
    {
        public virtual bool Equals(KernelApplicationExpression? other)
        {
            if (other is not { } notNull)
                return false;

            return
                notNull.functionName == functionName &&
                (ReferenceEquals(notNull.argument, argument) || notNull.argument.Equals(argument));
        }

        public override int GetHashCode()
        {
            var hash = new HashCode();

            hash.Add(functionName);
            hash.Add(argument);

            return hash.ToHashCode();
        }
    }

    public record ConditionalExpression
        : Expression
    {
        private readonly int slimHashCode;

        public Expression condition { get; }

        public Expression trueBranch { get; }

        public Expression falseBranch { get; }

        public ConditionalExpression(Expression condition, Expression trueBranch, Expression falseBranch)
        {
            this.condition = condition;
            this.trueBranch = trueBranch;
            this.falseBranch = falseBranch;

            slimHashCode = HashCode.Combine(condition, trueBranch, falseBranch);
        }

        public virtual bool Equals(ConditionalExpression? other)
        {
            if (other is not { } notNull)
                return false;

            return
                ReferenceEquals(this, notNull) ||
                slimHashCode == notNull.slimHashCode &&
                condition.Equals(notNull.condition) &&
                trueBranch.Equals(notNull.trueBranch) &&
                falseBranch.Equals(notNull.falseBranch);
        }

        public override int GetHashCode() =>
            slimHashCode;
    }

    public record EnvironmentExpression : Expression;

    public record StringTagExpression(
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
    public record KernelApplications_Skip_ListHead_Path_Expression(
        ReadOnlyMemory<int> SkipCounts,
        Expression Argument)
        : Expression
    {
        public virtual bool Equals(KernelApplications_Skip_ListHead_Path_Expression? other)
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
    /// Returns true if the expression is independent of the environment, that is none of the expressions in the tree contain an <see cref="EnvironmentExpression"/>.
    /// </summary>
    public static bool IsIndependent(Expression expression) =>
        expression switch
        {
            EnvironmentExpression =>
            false,

            LiteralExpression =>
            true,

            ListExpression list =>
            list.List.All(IsIndependent),

            ParseAndEvalExpression decodeAndEvaluate =>
            IsIndependent(decodeAndEvaluate.expression) && IsIndependent(decodeAndEvaluate.environment),

            KernelApplicationExpression kernelApplication =>
            IsIndependent(kernelApplication.argument),

            ConditionalExpression conditional =>
            IsIndependent(conditional.condition) && IsIndependent(conditional.trueBranch) && IsIndependent(conditional.falseBranch),

            StringTagExpression stringTag =>
            IsIndependent(stringTag.tagged),

            DelegatingExpression =>
            false,

            StackReferenceExpression =>
            false,

            KernelApplications_Skip_ListHead_Path_Expression fused =>
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
                case EnvironmentExpression:
                case LiteralExpression:
                case DelegatingExpression:
                    break;

                case ListExpression list:
                    foreach (var item in list.List)
                    {
                        stack.Push(item);
                    }
                    break;

                case ParseAndEvalExpression parseAndEvaluate:

                    stack.Push(parseAndEvaluate.expression);
                    stack.Push(parseAndEvaluate.environment);

                    break;

                case KernelApplicationExpression kernelApplication:

                    stack.Push(kernelApplication.argument);

                    break;

                case ConditionalExpression conditional:

                    stack.Push(conditional.condition);
                    stack.Push(conditional.falseBranch);
                    stack.Push(conditional.trueBranch);

                    break;

                case StringTagExpression stringTag:

                    stack.Push(stringTag.tagged);

                    break;

                case StackReferenceExpression:
                    break;

                case KernelApplication_Equal_Two equalTwo:

                    stack.Push(equalTwo.left);
                    stack.Push(equalTwo.right);

                    break;

                case KernelApplications_Skip_ListHead_Path_Expression skipListHead:

                    stack.Push(skipListHead.Argument);

                    break;

                default:
                    throw new NotImplementedException("Unknown expression type: " + expression.GetType().FullName);
            }
        }
    }
}
