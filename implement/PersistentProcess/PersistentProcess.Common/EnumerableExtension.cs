using System;
using System.Collections.Generic;
using System.Linq;

namespace Kalmit
{
    static public class EnumerableExtension
    {
        static public IEnumerable<T> WhereHasValue<T>(this IEnumerable<T?> orig) where T : struct =>
            orig?.Where(i => i.HasValue).Select(i => i.Value);

        static public IEnumerable<T> WhereNotNull<T>(this IEnumerable<T> orig) where T : class =>
            orig?.Where(i => i != null);

        static public IEnumerable<T> EmptyIfNull<T>(this IEnumerable<T> orig) =>
            orig ?? Array.Empty<T>();

        static public IEqualityComparer<IEnumerable<T>> EqualityComparer<T>() => new IEnumerableComparer<T>();

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

        class IEnumerableComparer<T> : IEqualityComparer<IEnumerable<T>>
        {
            public bool Equals(IEnumerable<T> x, IEnumerable<T> y)
            {
                return Object.ReferenceEquals(x, y) || (x != null && y != null && x.SequenceEqual(y));
            }

            public int GetHashCode(IEnumerable<T> obj) => 0;
        }
    }
}