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

    public record ConditionalExpression(
        Expression condition,
        Expression ifTrue,
        Expression ifFalse)
        : Expression;

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
    public record KernelApplications_Skip_ListHead_Expression(
        int skipCount,
        Expression argument)
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
            IsIndependent(conditional.condition) && IsIndependent(conditional.ifTrue) && IsIndependent(conditional.ifFalse),

            StringTagExpression stringTag =>
            IsIndependent(stringTag.tagged),

            DelegatingExpression =>
            false,

            StackReferenceExpression =>
            false,

            KernelApplications_Skip_ListHead_Expression fused =>
            IsIndependent(fused.argument),

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
                    stack.Push(conditional.ifFalse);
                    stack.Push(conditional.ifTrue);

                    break;

                case StringTagExpression stringTag:

                    stack.Push(stringTag.tagged);

                    break;

                default:
                    throw new NotImplementedException("Unknown expression type: " + expression.GetType().FullName);
            }
        }
    }
}
