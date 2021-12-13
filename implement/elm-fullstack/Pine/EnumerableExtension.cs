using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace Pine;

static public class EnumerableExtension
{
    static public IEnumerable<T>? WhereHasValue<T>(this IEnumerable<T?>? orig) where T : struct =>
        orig?.Where(i => i.HasValue).Select(i => i!.Value);

    static public IEnumerable<T>? WhereNotNull<T>(this IEnumerable<T?>? orig) where T : class =>
        orig?.Where(i => i != null).Cast<T>();

    static public IEnumerable<T> EmptyIfNull<T>(this IEnumerable<T>? orig) =>
        orig ?? Array.Empty<T>();

    static public IEqualityComparer<T> EqualityComparer<T>() where T : IEnumerable<IComparable> => new IEnumerableEqualityComparer<T>();

    static public IComparer<T> Comparer<T>() where T : IEnumerable<IComparable> => new IEnumerableComparer<T>();

    // From https://github.com/morelinq/MoreLINQ/blob/07bd0861658b381ce97c8b44d3b9f2cd3c9bf769/MoreLinq/TakeUntil.cs
    static public IEnumerable<TSource> TakeUntil<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate)
    {
        if (source == null) throw new ArgumentNullException(nameof(source));
        if (predicate == null) throw new ArgumentNullException(nameof(predicate));

        return _(); IEnumerable<TSource> _()
        {
            foreach (var item in source)
            {
                yield return item;
                if (predicate(item))
                    yield break;
            }
        }
    }

    class IEnumerableEqualityComparer<T> : IEqualityComparer<T> where T : IEnumerable<IComparable>
    {
        public bool Equals(T? x, T? y)
        {
            return ReferenceEquals(x, y) || (x != null && y != null && x.SequenceEqual(y));
        }

        public int GetHashCode(T obj) => 0;
    }

    class IEnumerableComparer<T> : IComparer<T> where T : IEnumerable<IComparable>
    {
        public static int Compare([AllowNull] IEnumerable<IComparable> x, [AllowNull] IEnumerable<IComparable> y)
        {
            if (x == y)
                return 0;

            if (x == null)
                return -1;

            if (y == null)
                return 1;

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

                if (currentComparison != 0)
                    return currentComparison;
            }
        }

        public int Compare(T? x, T? y)
        {
            return IEnumerableComparer<T>.Compare(x, (IEnumerable<IComparable>?)y);
        }
    }
}
