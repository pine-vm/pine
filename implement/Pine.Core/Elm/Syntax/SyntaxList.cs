using Pine.Core.Elm.Syntax.Concrete;
using System;
using System.Collections.Generic;
using System.Text;

namespace Pine.Core.Elm.Syntax;

internal static class SyntaxList
{
    internal static TResult[] Map<TSource, TResult>(
        IReadOnlyList<TSource> items,
        Func<TSource, TResult> convert)
    {
        if (items.Count is 0)
            return [];

        var result = new TResult[items.Count];

        for (var i = 0; i < result.Length; i++)
            result[i] = convert(items[i]);

        return result;
    }

    internal static TResult[] Map<TSource, TResult>(
        SeparatedSyntaxList<TSource> items,
        Func<TSource, TResult> convert)
    {
        if (items.Count is 0)
            return [];

        var result = new TResult[items.Count];

        for (var i = 0; i < result.Length; i++)
            result[i] = convert(items[i]);

        return result;
    }

    internal static T[] OrderByOrdinal<T>(
        IReadOnlyList<T> items,
        Func<T, string> getKey)
    {
        if (items.Count is 0)
            return [];

        var entries = new (T Item, string Key, int Index)[items.Count];

        for (var i = 0; i < entries.Length; i++)
            entries[i] = (items[i], getKey(items[i]), i);

        Array.Sort(
            entries,
            static (left, right) =>
            {
                var comparison = StringComparer.Ordinal.Compare(left.Key, right.Key);

                // Preserve input order for equal keys, as LINQ's OrderBy does.
                return comparison is 0 ? left.Index.CompareTo(right.Index) : comparison;
            });

        var result = new T[entries.Length];

        for (var i = 0; i < result.Length; i++)
            result[i] = entries[i].Item;

        return result;
    }

    internal static string Join(string separator, IReadOnlyList<string> items)
    {
        if (items is string[] array)
            return string.Join(separator, array);

        if (items.Count is 0)
            return string.Empty;

        if (items.Count is 1)
            return items[0] ?? string.Empty;

        var builder = new StringBuilder();

        for (var i = 0; i < items.Count; i++)
        {
            if (i > 0)
                builder.Append(separator);

            builder.Append(items[i]);
        }

        return builder.ToString();
    }
}
