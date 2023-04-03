using Pine.Json;
using System;
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
            if (other is not ListExpression notNull)
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
            if (other is not KernelApplicationExpression notNull)
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
}
