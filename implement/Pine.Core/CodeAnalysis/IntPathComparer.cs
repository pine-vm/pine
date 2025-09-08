using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Compares two integer paths represented as <see cref="IReadOnlyList{T}"/> of <see cref="int"/>.
/// Comparison is lexicographic by elements, with length as a tiebreaker when sequences differ in length.
/// </summary>
public class IntPathComparer : IComparer<IReadOnlyList<int>>
{
    /// <summary>
    /// A reusable singleton instance of <see cref="IntPathComparer"/>.
    /// </summary>
    public static readonly IntPathComparer Instance = new();

    /// <summary>
    /// Compares two integer paths.
    /// Returns a negative number if <paramref name="x"/> sorts before <paramref name="y"/>,
    /// zero if they are equal, or a positive number if <paramref name="x"/> sorts after <paramref name="y"/>.
    /// </summary>
    /// <param name="x">First path.</param>
    /// <param name="y">Second path.</param>
    /// <returns>The relative ordering of the two paths.</returns>
    public int Compare(IReadOnlyList<int>? x, IReadOnlyList<int>? y)
    {
        if (ReferenceEquals(x, y))
            return 0;

        if (x is null)
            return -1;

        if (y is null)
            return 1;

        if (x.Count != y.Count)
            return x.Count.CompareTo(y.Count);

        for (var i = 0; i < x.Count; i++)
        {
            var comparison = x[i].CompareTo(y[i]);

            if (comparison != 0)
                return comparison;
        }

        return 0;
    }
}
