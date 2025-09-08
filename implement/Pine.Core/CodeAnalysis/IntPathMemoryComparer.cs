using System;
using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Compares two paths represented as <see cref="ReadOnlyMemory{T}"/> of <see cref="int"/>.
/// Comparison is lexicographic by elements, with length as a tiebreaker when sequences differ in length.
/// </summary>
public class IntPathMemoryComparer : IComparer<ReadOnlyMemory<int>>
{
    /// <summary>
    /// A reusable singleton instance of <see cref="IntPathMemoryComparer"/>.
    /// </summary>
    public static readonly IntPathMemoryComparer Instance = new();

    /// <summary>
    /// Compares two integer paths.
    /// Returns a negative number if <paramref name="x"/> sorts before <paramref name="y"/>,
    /// zero if they are equal, or a positive number if <paramref name="x"/> sorts after <paramref name="y"/>.
    /// </summary>
    /// <param name="x">First path.</param>
    /// <param name="y">Second path.</param>
    /// <returns>The relative ordering of the two paths.</returns>
    public int Compare(ReadOnlyMemory<int> x, ReadOnlyMemory<int> y)
    {
        if (x.Equals(y))
            return 0;

        if (x.Length != y.Length)
            return x.Length.CompareTo(y.Length);

        for (var i = 0; i < x.Length; i++)
        {
            var comparison = x.Span[i].CompareTo(y.Span[i]);

            if (comparison != 0)
                return comparison;
        }

        return 0;
    }
}
