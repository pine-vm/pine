using Pine.Core.Addressing;
using Pine.Core.PineLanguage;
using System;
using System.Threading;

using Retained = Pine.Core.Runtime.InternalExpression.Expression;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Retains runtime syntax without converting or hashing it. Canonical views are computed once,
/// thread-safely, only when requested by a consumer.
/// </summary>
public sealed class ExpressionIdentity
{
    private readonly Lazy<Expression> _canonicalExpression;

    private readonly Lazy<PineValue> _encodedValue;

    private readonly Lazy<string> _hashBase16;

    private PineValue? _knownEncodedValue;

    /// <summary>The immutable runtime expression; accessing it performs no conversion.</summary>
    public Retained InternalExpression { get; }

    /// <summary>Materializes the canonical expression model on demand.</summary>
    public Expression CanonicalExpression => _canonicalExpression.Value;

    /// <summary>Returns an already available compact encoding, or encodes the runtime expression on demand.</summary>
    public PineValue EncodedValue => _encodedValue.Value;

    /// <summary>Computes the canonical encoded-value SHA-256, not an internal-representation hash, on demand.</summary>
    public string HashBase16 => _hashBase16.Value;

    /// <summary>Retains an expression without computing any canonical views.</summary>
    public ExpressionIdentity(Retained expression)
        : this(expression, null)
    {
    }

    internal ExpressionIdentity(Retained expression, PineValue? encodedValue)
    {
        ArgumentNullException.ThrowIfNull(expression);
        InternalExpression = expression;
        _knownEncodedValue = encodedValue;
        _canonicalExpression = new(() => Runtime.InternalExpression.ExpressionConversion.ToCanonicalCached(expression));

        _encodedValue =
            new(
                () => Volatile.Read(ref _knownEncodedValue) ??
                    Runtime.InternalExpression.ExpressionValueEncoding.EncodeExpressionAsValue(expression));

        _hashBase16 =
            new(
                () => Convert.ToHexStringLower(PineValueHashFlat.ComputeHashForValue(EncodedValue).hashBytes.Span));
    }

    /// <summary>Adapts canonical input syntax; no value encoding or cryptographic hashing is performed.</summary>
    public static ExpressionIdentity FromCanonical(Expression expression) =>
        new(Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(expression));

    internal void RegisterEncodedValue(PineValue encodedValue)
    {
        if (!_encodedValue.IsValueCreated)
            Interlocked.CompareExchange(ref _knownEncodedValue, encodedValue, null);
    }

    internal bool IsCanonicalExpressionCreated => _canonicalExpression.IsValueCreated;

    internal bool IsEncodedValueCreated => _encodedValue.IsValueCreated;

    internal bool IsHashCreated => _hashBase16.IsValueCreated;
}
