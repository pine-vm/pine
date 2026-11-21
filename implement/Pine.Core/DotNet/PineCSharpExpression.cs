using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.DotNet;

internal enum PineCSharpExpressionType
{
    PineValue,
    NativeValue,
    Boolean,
    Int64,
    BigInteger,
    NullableBigInteger,
    BlobMemory,
    ListMemory
}

/// <summary>A representation of one semantic value, with its conversion/operation cost.</summary>
internal sealed record PineCSharpExpression(string Code, PineCSharpExpressionType Type, int Cost = 0);

/// <summary>Facts belong to a lexical/dominance scope, never to an unrelated incoming CFG path.</summary>
internal sealed class PineCSharpExpressionScope(PineCSharpExpressionScope? parent = null)
{
    private readonly Dictionary<string, PineCSharpExpression> _integers = [];

    private readonly Dictionary<string, PineCSharpExpression> _values = [];

    private readonly Dictionary<string, (string Positive, string Negative, HashSet<string> Inputs)> _predicates = [];

    public PineCSharpExpression? Integer(string value) =>
        _integers.GetValueOrDefault(value) ?? parent?.Integer(value);

    public PineCSharpExpression? Value(string value) =>
        _values.GetValueOrDefault(value) ?? parent?.Value(value);

    public void SetInteger(string value, PineCSharpExpression expression) => _integers[value] = expression;

    public void SetValue(string value, PineCSharpExpression expression) => _values[value] = expression;

    public void SetPredicate(string value, string positive, string negative, IEnumerable<string> inputs) =>
        _predicates[value] = (positive, negative, inputs.ToHashSet());

    public string? Predicate(string value, bool negated) =>
        _predicates.TryGetValue(value, out var predicate)
        ?
        negated ? predicate.Negative : predicate.Positive
        :
        parent?.Predicate(value, negated);

    public void Forget(string value)
    {
        _integers.Remove(value);
        _values.Remove(value);

        foreach (var key in _predicates.Keys.ToArray())
        {
            if (key == value || _predicates[key].Inputs.Contains(value))
                _predicates.Remove(key);
        }
    }

    public static string Equality(PineCSharpExpression left, PineCSharpExpression right, bool negated) =>
        left.Code + (negated ? " != " : " == ") + right.Code;

    public static string Cheapest(IEnumerable<(string Code, int Cost)> candidates) =>
        candidates.MinBy(candidate => candidate.Cost).Code;
}
