using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Equality comparer for integer paths represented as <see cref="IReadOnlyList{T}"/> of <see cref="int"/>.
/// Two paths are equal when they have the same length and equal elements in the same order.
/// </summary>
public class IntPathEqualityComparer : IEqualityComparer<IReadOnlyList<int>?>
{
    /// <summary>
    /// A reusable singleton instance of <see cref="IntPathEqualityComparer"/>.
    /// </summary>
    public static readonly IntPathEqualityComparer Instance = new();

    /// <inheritdoc />
    public bool Equals(IReadOnlyList<int>? x, IReadOnlyList<int>? y) =>
        ReferenceEquals(x, y) ||
        (x is not null && y is not null && x.SequenceEqual(y));

    /// <summary>
    /// Computes a hash code for the given integer path by combining the hash codes of its elements.
    /// </summary>
    /// <param name="obj">The path.</param>
    /// <returns>A hash code for the path.</returns>
    public int GetHashCode(IReadOnlyList<int> obj) =>
        obj.Aggregate(0, (acc, next) => acc ^ next.GetHashCode());
}
