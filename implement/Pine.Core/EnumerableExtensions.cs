using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text.RegularExpressions;

namespace Pine.Core;

/// <summary>
/// Extension methods for <see cref="IEnumerable{T}"/>.
/// </summary>
public static partial class EnumerableExtensions
{
    /// <summary>
    /// Filters a sequence of nullable value types, returning only the elements that have a value.
    /// </summary>
    /// <typeparam name="T">The underlying value type.</typeparam>
    /// <param name="source">The sequence of nullable value types.</param>
    /// <returns>An <see cref="IEnumerable{T}"/> containing only the non-null values.</returns>
    public static IEnumerable<T> WhereHasValue<T>(this IEnumerable<T?> source) where T : struct
    {
        foreach (var item in source)
        {
            if (item.HasValue)
                yield return item.Value;
        }
    }

    /// <summary>
    /// Filters a sequence of nullable reference types, returning only the elements that are not null.
    /// </summary>
    /// <typeparam name="T">The reference type.</typeparam>
    /// <param name="source">The sequence of nullable reference types.</param>
    /// <returns>An <see cref="IEnumerable{T}"/> containing only the non-null values.</returns>
    public static IEnumerable<T> WhereNotNull<T>(this IEnumerable<T?> source) where T : class
    {
        foreach (var item in source)
            if (item is not null)
                yield return item;
    }

    /// <summary>
    /// Projects each element of a sequence to a new form and filters out null results.
    /// </summary>
    /// <typeparam name="InT">The type of the elements of <paramref name="source"/>.</typeparam>
    /// <typeparam name="OutT">The type of the value returned by <paramref name="selector"/>.</typeparam>
    /// <param name="source">An <see cref="IEnumerable{T}"/> to invoke a transform function on.</param>
    /// <param name="selector">A transform function to apply to each element, which may return null.</param>
    /// <returns>An <see cref="IEnumerable{OutT}"/> whose elements are the non-null results of invoking the transform function on each element of <paramref name="source"/>.</returns>
    public static IEnumerable<OutT> SelectWhereNotNull<InT, OutT>(this IEnumerable<InT> source, Func<InT, OutT?> selector) where OutT : class
    {
        foreach (var item in source)
            if (selector(item) is { } notNull)
                yield return notNull;
    }

    /// <summary>
    /// Returns an empty enumerable if the original enumerable is null; otherwise, returns the original enumerable.
    /// </summary>
    /// <typeparam name="T">The type of the elements in the enumerable.</typeparam>
    /// <param name="orig">The original enumerable.</param>
    /// <returns>An empty enumerable if <paramref name="orig"/> is null; otherwise, <paramref name="orig"/>.</returns>
    public static IEnumerable<T> EmptyIfNull<T>(this IEnumerable<T>? orig) =>
        orig ?? [];

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

    /// <summary>
    /// Returns elements from a sequence as long as a specified condition is true, and then returns the element that made the condition false.
    /// </summary>
    /// <typeparam name="TSource">The type of the elements of <paramref name="source"/>.</typeparam>
    /// <param name="source">An <see cref="IEnumerable{TSource}"/> to return elements from.</param>
    /// <param name="predicate">A function to test each element for a condition.</param>
    /// <returns>An <see cref="IEnumerable{TSource}"/> that contains elements from the input sequence that satisfy the condition, plus the one that makes it false.</returns>
    /// <exception cref="ArgumentNullException"><paramref name="source"/> or <paramref name="predicate"/> is null.</exception>
    // From https://github.com/morelinq/MoreLINQ/blob/07bd0861658b381ce97c8b44d3b9f2cd3c9bf769/MoreLinq/TakeUntil.cs
    public static IEnumerable<TSource> TakeUntil<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate)
    {
        ArgumentNullException.ThrowIfNull(source);
        ArgumentNullException.ThrowIfNull(predicate);

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

    /// <summary>
    /// Sorts the elements of a sequence of strings in natural order.
    /// Natural sort order means that numeric parts of strings are sorted based on their numeric value, not their lexicographical value.
    /// For example, "item2" comes before "item10".
    /// </summary>
    /// <param name="items">An <see cref="IEnumerable{String}"/> to sort.</param>
    /// <param name="stringComparer">An optional <see cref="StringComparer"/> to use for comparing non-numeric parts of the strings. If null, <see cref="StringComparer.CurrentCulture"/> is used.</param>
    /// <returns>An <see cref="IEnumerable{String}"/> whose elements are sorted according to natural sort order.</returns>
    public static IEnumerable<string> OrderByNatural(this IEnumerable<string> items, StringComparer? stringComparer = null) =>
        items.OrderByNatural(s => s, stringComparer);

    /// <summary>
    /// Sorts the elements of a sequence in natural order according to a key.
    /// Natural sort order means that numeric parts of the key strings are sorted based on their numeric value, not their lexicographical value.
    /// For example, a key "item2" would come before "item10".
    /// </summary>
    /// <typeparam name="T">The type of the elements of <paramref name="items"/>.</typeparam>
    /// <param name="items">An <see cref="IEnumerable{T}"/> to sort.</param>
    /// <param name="selector">A function to extract a string key from an element.</param>
    /// <param name="stringComparer">An optional <see cref="StringComparer"/> to use for comparing non-numeric parts of the keys. If null, <see cref="StringComparer.CurrentCulture"/> is used.</param>
    /// <returns>An <see cref="IEnumerable{T}"/> whose elements are sorted according to the natural sort order of their keys.</returns>
    public static IEnumerable<T> OrderByNatural<T>(this IEnumerable<T> items, Func<T, string> selector, StringComparer? stringComparer = null)
    {
        var regex = oneOrMoreDigitsRegex();

        var maxDigits =
            items
            .SelectMany(i => regex.Matches(selector(i)).Cast<Match>().Select(digitChunk => (int?)digitChunk.Value.Length))
            .Max() ?? 0;

        return
            items
            .OrderBy(i => regex.Replace(selector(i), match => match.Value.PadLeft(maxDigits, '0')), stringComparer ?? StringComparer.CurrentCulture);
    }

    /// <summary>
    /// Projects each element of a sequence to a <see cref="Maybe{OutT}"/> and returns a sequence of the <c>Just</c> values.
    /// </summary>
    /// <typeparam name="InT">The type of the elements of <paramref name="source"/>.</typeparam>
    /// <typeparam name="OutT">The type of the value in the <c>Just</c> case of the <see cref="Maybe{OutT}"/> returned by <paramref name="selector"/>.</typeparam>
    /// <param name="source">An <see cref="IEnumerable{InT}"/> to invoke a transform function on.</param>
    /// <param name="selector">A transform function to apply to each element, returning a <see cref="Maybe{OutT}"/>.</param>
    /// <returns>An <see cref="IEnumerable{OutT}"/> whose elements are the <c>Just</c> values from the results of invoking the transform function on each element of <paramref name="source"/>.</returns>
    public static IEnumerable<OutT> SelectWhere<InT, OutT>(this IEnumerable<InT> source, Func<InT, Maybe<OutT>> selector)
    {
        foreach (var item in source)
        {
            if (selector(item) is Maybe<OutT>.Just just)
                yield return just.Value;
        }
    }

    /// <summary>
    /// Filters a sequence of <see cref="Maybe{JustT}"/>, returning only the elements that are <c>Just</c>.
    /// </summary>
    /// <typeparam name="JustT">The type of the value in the <c>Just</c> case.</typeparam>
    /// <param name="source">The sequence of <see cref="Maybe{JustT}"/>.</param>
    /// <returns>An <see cref="IEnumerable{JustT}"/> containing only the values from the <c>Just</c> elements.</returns>
    public static IEnumerable<JustT> WhereNotNothing<JustT>(this IEnumerable<Maybe<JustT>> source)
    {
        foreach (var item in source)
        {
            if (item is Maybe<JustT>.Just just)
                yield return just.Value;
        }
    }

    /// <summary>
    /// Intersperse a separator element between each element of a sequence.
    /// </summary>
    /// <typeparam name="T">The type of the elements of <paramref name="source"/> and the type of the <paramref name="separator"/>.</typeparam>
    /// <param name="source">An <see cref="IEnumerable{T}"/> to intersperse.</param>
    /// <param name="separator">The element to intersperse between elements of the <paramref name="source"/> sequence.</param>
    /// <returns>An <see cref="IEnumerable{T}"/> containing the original elements with the separator interspersed.</returns>
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

    /// <summary>
    /// Dequeues all items from a <see cref="ConcurrentQueue{T}"/> and returns them as an <see cref="IEnumerable{T}"/>.
    /// This method will continue to dequeue items until the queue is empty.
    /// </summary>
    /// <typeparam name="T">The type of elements in the queue.</typeparam>
    /// <param name="queue">The <see cref="ConcurrentQueue{T}"/> to dequeue items from.</param>
    /// <returns>An <see cref="IEnumerable{T}"/> containing all items dequeued from the queue.</returns>
    public static IEnumerable<T> DequeueAllEnumerable<T>(this ConcurrentQueue<T> queue)
    {
        while (true)
        {
            if (!queue.TryDequeue(out var item))
                yield break;

            yield return item;
        }
    }

    [GeneratedRegex(@"\d+", RegexOptions.Compiled)]
    private static partial Regex oneOrMoreDigitsRegex();
}
