using Pine.Core.Addressing;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;
using System.Text;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Immutable expression allow-list for both invocation-cache admission and lookup.
/// Equality compares complete expression values as a set, independent of enumeration order.
/// Applying a new policy does not clear existing cache entries.
/// </summary>
public sealed class InvocationCachePolicy : IEquatable<InvocationCachePolicy>
{
    /// <summary>A policy that neither admits nor looks up invocation results.</summary>
    public static InvocationCachePolicy Empty { get; } = new([]);

    /// <summary>The complete encoded expressions enabled by this policy.</summary>
    public ImmutableHashSet<PineValue> Expressions { get; }

    private readonly int _hashCode;

    private string? _id;

    /// <summary>Constructs a policy, eliminating duplicates and taking an immutable copy.</summary>
    public InvocationCachePolicy(IEnumerable<PineValue> expressions)
    {
        ArgumentNullException.ThrowIfNull(expressions);
        Expressions = [.. expressions];

        if (Expressions.Contains(null!))
            throw new ArgumentException("Policy expressions cannot be null.", nameof(expressions));

        var hash = 0;

        foreach (var expression in Expressions)
            hash ^= expression.GetHashCode();

        _hashCode = HashCode.Combine(Expressions.Count, hash);
    }

    /// <summary>
    /// Stable content identifier for logs and cross-process comparison. Unlike GetHashCode,
    /// this is deterministic across processes; equality still compares complete values.
    /// </summary>
    public string Id =>
        _id ??=
            Convert.ToHexStringLower(
                SHA256.HashData(
                    Encoding.UTF8.GetBytes(
                        string.Join("\n", Expressions.Select(ExpressionId).Order(StringComparer.Ordinal)))));

    /// <summary>Deterministic full value hash identifying an encoded expression in diagnostics.</summary>
    public static string ExpressionId(PineValue expression) =>
        Convert.ToHexStringLower(PineValueHashFlat.ComputeHashForValue(expression).hashBytes.Span);

    /// <summary>Wraps existing storage without changing or discarding its entries.</summary>
    public IInvocationCacheAccess Apply(IInvocationCacheAccess cache)
    {
        ArgumentNullException.ThrowIfNull(cache);
        return new PolicyCache(cache, this);
    }

    /// <inheritdoc/>
    public bool Equals(InvocationCachePolicy? other) =>
        ReferenceEquals(this, other) ||
        other is not null && _hashCode == other._hashCode && Expressions.SetEquals(other.Expressions);

    /// <inheritdoc/>
    public override bool Equals(object? obj) => obj is InvocationCachePolicy other && Equals(other);

    /// <inheritdoc/>
    public override int GetHashCode() => _hashCode;

    /// <summary>Compares policies by expression-set contents.</summary>
    public static bool operator ==(InvocationCachePolicy? left, InvocationCachePolicy? right) =>
        Equals(left, right);

    /// <summary>Compares policies by expression-set contents.</summary>
    public static bool operator !=(InvocationCachePolicy? left, InvocationCachePolicy? right) =>
        !Equals(left, right);

    private sealed class PolicyCache(IInvocationCacheAccess cache, InvocationCachePolicy policy)
        : IInvocationCacheAccess
    {
        public bool ShouldAdmitExpression(PineValue expressionValue) =>
            policy.Expressions.Contains(expressionValue) && cache.ShouldAdmitExpression(expressionValue);

        public bool MayContainExpression(PineValue expressionValue) =>
            policy.Expressions.Contains(expressionValue) && cache.MayContainExpression(expressionValue);

        public bool TryGet(EvalCacheEntryKey key, out PineValue value)
        {
            if (policy.Expressions.Contains(key.ExprValue))
                return cache.TryGet(key, out value);

            value = null!;
            return false;
        }

        public bool TryAdd(EvalCacheEntryKey key, PineValue value) =>
            policy.Expressions.Contains(key.ExprValue) && cache.TryAdd(key, value);

        public void Merge(IReadOnlyDictionary<EvalCacheEntryKey, PineValue> entries) =>
            cache.Merge(
                entries.Where(entry => policy.Expressions.Contains(entry.Key.ExprValue))
                .ToDictionary(entry => entry.Key, entry => entry.Value));
    }
}
