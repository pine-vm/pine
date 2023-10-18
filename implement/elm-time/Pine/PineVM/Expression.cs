using Pine.Json;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.Json.Serialization;

namespace Pine.PineVM;


[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Expression
{
    public record LiteralExpression(
        PineValue Value)
        : Expression;

    public record ListExpression(
        ImmutableArray<Expression> List)
        : Expression
    {
        public virtual bool Equals(ListExpression? other)
        {
            if (other is not { } notNull)
                return false;

            return
                ReferenceEquals(this, notNull) ||
                List.Length == notNull.List.Length &&
                List.SequenceEqual(notNull.List);
        }

        public override int GetHashCode()
        {
            var hashCode = new HashCode();

            foreach (var item in List)
            {
                hashCode.Add(item.GetHashCode());
            }

            return hashCode.ToHashCode();
        }
    }

    public record DecodeAndEvaluateExpression(
        Expression expression,
        Expression environment)
        : Expression;

    public record KernelApplicationExpression(
        string functionName,
        Expression argument,

        [property: JsonIgnore]
            Func<PineValue, Result<string, PineValue>> function)
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

    public record DelegatingExpression(Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>> Delegate)
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

            DecodeAndEvaluateExpression decodeAndEvaluate =>
            IsIndependent(decodeAndEvaluate.expression) && IsIndependent(decodeAndEvaluate.environment),

            KernelApplicationExpression kernelApplication =>
            IsIndependent(kernelApplication.argument),

            ConditionalExpression conditional =>
            IsIndependent(conditional.condition) && IsIndependent(conditional.ifTrue) && IsIndependent(conditional.ifFalse),

            StringTagExpression stringTag =>
            IsIndependent(stringTag.tagged),

            DelegatingExpression =>
            false,

            _ =>
            throw new NotImplementedException(),
        };

    public static IEnumerable<Expression> EnumerateSelfAndDescendants(Expression expression)
    {
        yield return expression;

        switch (expression)
        {
            case EnvironmentExpression:
            case LiteralExpression:
            case DelegatingExpression:
                break;

            case ListExpression list:
                foreach (var item in list.List)
                {
                    foreach (var descendant in EnumerateSelfAndDescendants(item))
                    {
                        yield return descendant;
                    }
                }
                break;

            case DecodeAndEvaluateExpression decodeAndEvaluate:
                foreach (var descendant in EnumerateSelfAndDescendants(decodeAndEvaluate.expression))
                {
                    yield return descendant;
                }
                foreach (var descendant in EnumerateSelfAndDescendants(decodeAndEvaluate.environment))
                {
                    yield return descendant;
                }
                break;

            case KernelApplicationExpression kernelApplication:
                foreach (var descendant in EnumerateSelfAndDescendants(kernelApplication.argument))
                {
                    yield return descendant;
                }
                break;

            case ConditionalExpression conditional:
                foreach (var descendant in EnumerateSelfAndDescendants(conditional.condition))
                {
                    yield return descendant;
                }
                foreach (var descendant in EnumerateSelfAndDescendants(conditional.ifTrue))
                {
                    yield return descendant;
                }
                foreach (var descendant in EnumerateSelfAndDescendants(conditional.ifFalse))
                {
                    yield return descendant;
                }
                break;

            case StringTagExpression stringTag:
                foreach (var descendant in EnumerateSelfAndDescendants(stringTag.tagged))
                {
                    yield return descendant;
                }
                break;

            default:
                throw new NotImplementedException();
        }
    }
}
