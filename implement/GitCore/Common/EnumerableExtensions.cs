using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace GitCore.Common;

/// <summary>
/// Extension methods for <see cref="IEnumerable{T}"/>.
/// </summary>
public static class EnumerableExtensions
{
    /// <summary>
    /// Creates an <see cref="IEqualityComparer{T}"/> for sequences of <see cref="IComparable"/> elements.
    /// </summary>
    /// <typeparam name="T">The type of the enumerable, which must implement <see cref="IEnumerable{IComparable}"/>.</typeparam>
    /// <returns>An <see cref="IEqualityComparer{T}"/> that compares sequences element by element.</returns>
    public static IEqualityComparer<T> EqualityComparer<T>() where T : IEnumerable<IComparable> =>
        new IEnumerableEqualityComparer<T>();

    /// <summary>
    /// Creates an <see cref="IComparer{T}"/> for sequences of <see cref="IComparable"/> elements.
    /// </summary>
    /// <typeparam name="T">The type of the enumerable, which must implement <see cref="IEnumerable{IComparable}"/>.</typeparam>
    /// <returns>An <see cref="IComparer{T}"/> that compares sequences element by element.</returns>
    public static IComparer<T> Comparer<T>() where T : IEnumerable<IComparable> => new IEnumerableComparer<T>();

    private class IEnumerableEqualityComparer<T> : IEqualityComparer<T> where T : IEnumerable<IComparable>
    {
        public bool Equals(T? x, T? y)
        {
            if (ReferenceEquals(x, y))
                return true;

            if (x is null || y is null)
            {
                return false;
            }

            return x.SequenceEqual(y);
        }

        public int GetHashCode(T obj) => 0;
    }

    private class IEnumerableComparer<T> : IComparer<T> where T : IEnumerable<IComparable>
    {
        public static int Compare([AllowNull] IEnumerable<IComparable> x, [AllowNull] IEnumerable<IComparable> y)
        {
            if (ReferenceEquals(x, y))
                return 0;

            if (x is null)
                return -1;

            if (y is null)
                return 1;

            if (x.Equals(y))
                return 0;

            var xEnumerator = x.GetEnumerator();
            var yEnumerator = y.GetEnumerator();

            while (true)
            {
                var xHasCurrent = xEnumerator.MoveNext();
                var yHasCurrent = yEnumerator.MoveNext();

                if (!xHasCurrent && !yHasCurrent)
                    return 0;

                if (!xHasCurrent)
                    return -1;

                if (!yHasCurrent)
                    return 1;

                var currentComparison = xEnumerator.Current.CompareTo(yEnumerator.Current);

                if (currentComparison is not 0)
                    return currentComparison;
            }
        }

        public int Compare(T? x, T? y)
        {
            return IEnumerableComparer<T>.Compare(x, y);
        }
    }
}
