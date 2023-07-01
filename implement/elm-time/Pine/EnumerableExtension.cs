using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text.RegularExpressions;

namespace Pine;

public static class EnumerableExtension
{
    public static IEnumerable<T> WhereHasValue<T>(this IEnumerable<T?> source) where T : struct
    {
        foreach (var item in source)
        {
            if (item.HasValue)
                yield return item.Value;
        }
    }

    public static IEnumerable<T> WhereNotNull<T>(this IEnumerable<T?> source) where T : class
    {
        foreach (var item in source)
            if (item is not null)
                yield return item;
    }

    public static IEnumerable<OutT> SelectWhereNotNull<InT, OutT>(this IEnumerable<InT> source, Func<InT, OutT?> selector) where OutT : class
    {
        foreach (var item in source)
            if (selector(item) is { } notNull)
                yield return notNull;
    }

    public static IEnumerable<T> EmptyIfNull<T>(this IEnumerable<T>? orig) =>
        orig ?? Array.Empty<T>();

    public static IEqualityComparer<T> EqualityComparer<T>() where T : IEnumerable<IComparable> => new IEnumerableEqualityComparer<T>();

    public static IComparer<T> Comparer<T>() where T : IEnumerable<IComparable> => new IEnumerableComparer<T>();

    // From https://github.com/morelinq/MoreLINQ/blob/07bd0861658b381ce97c8b44d3b9f2cd3c9bf769/MoreLinq/TakeUntil.cs
    public static IEnumerable<TSource> TakeUntil<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate)
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

    private class IEnumerableEqualityComparer<T> : IEqualityComparer<T> where T : IEnumerable<IComparable>
    {
        public bool Equals(T? x, T? y)
        {
            return ReferenceEquals(x, y) || (x != null && y != null && x.SequenceEqual(y));
        }

        public int GetHashCode(T obj) => 0;
    }

    private class IEnumerableComparer<T> : IComparer<T> where T : IEnumerable<IComparable>
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
            return IEnumerableComparer<T>.Compare(x, y);
        }
    }

    public static IEnumerable<string> OrderByNatural(this IEnumerable<string> items, StringComparer? stringComparer = null) =>
        OrderByNatural(items, s => s, stringComparer);

    public static IEnumerable<T> OrderByNatural<T>(this IEnumerable<T> items, Func<T, string> selector, StringComparer? stringComparer = null)
    {
        var regex = new Regex(@"\d+", RegexOptions.Compiled);

        int maxDigits =
            items
            .SelectMany(i => regex.Matches(selector(i)).Cast<Match>().Select(digitChunk => (int?)digitChunk.Value.Length))
            .Max() ?? 0;

        return
            items
            .OrderBy(i => regex.Replace(selector(i), match => match.Value.PadLeft(maxDigits, '0')), stringComparer ?? StringComparer.CurrentCulture);
    }

    public static IEnumerable<OutT> SelectWhere<InT, OutT>(this IEnumerable<InT> source, Func<InT, Maybe<OutT>> selector)
    {
        foreach (var item in source)
        {
            if (selector(item) is Maybe<OutT>.Just just)
                yield return just.Value;
        }
    }

    public static IEnumerable<T> Intersperse<T>(this IEnumerable<T> source, T separator)
    {
        using var enumerator = source.GetEnumerator();

        if (!enumerator.MoveNext())
            yield break;

        yield return enumerator.Current;

        while (enumerator.MoveNext())
        {
            yield return separator;
            yield return enumerator.Current;
        }
    }

    public static IEnumerable<T> DequeueEnumerable<T>(this ConcurrentQueue<T> queue)
    {
        if (!queue.TryDequeue(out var item))
            yield break;

        yield return item;
    }
}
