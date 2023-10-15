using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public class CSharpDeclarationOrder
{
    public static IEnumerable<PineValue> OrderValuesForDeclaration(IEnumerable<PineValue> pineValues)
    {
        var blobs =
            pineValues.OfType<PineValue.BlobValue>()
            .Order(new BlobValueDeclarationOrder());

        var originalLists = pineValues.OfType<PineValue.ListValue>().Distinct();

        var orderedLists = OrderListValuesBySize(originalLists);

        return blobs.Cast<PineValue>().Concat(orderedLists);
    }

    public class BlobValueDeclarationOrder : IComparer<PineValue.BlobValue>
    {
        public int Compare(PineValue.BlobValue? x, PineValue.BlobValue? y)
        {
            if (x == y)
                return 0;

            if (x is null)
                return -1;

            if (y is null)
                return 1;

            if (x.Bytes.Length < y.Bytes.Length)
                return -1;

            if (y.Bytes.Length < x.Bytes.Length)
                return 1;

            for (var i = 0; i < x.Bytes.Length; i++)
            {
                if (x.Bytes.Span[i] < y.Bytes.Span[i])
                    return -1;

                if (y.Bytes.Span[i] < x.Bytes.Span[i])
                    return 1;
            }

            return 0;
        }
    }

    public static IEnumerable<PineValue.ListValue> OrderListValuesBySize(IEnumerable<PineValue.ListValue> listValues) =>
        listValues
        .Select(value => (value, size: AggregateSizeIncludingDescendants(value)))
        .OrderBy(tuple => tuple.size)
        .Select(tuple => tuple.value);

    public static IEnumerable<PineValue.ListValue> OrderListValuesByContainment(IEnumerable<PineValue.ListValue> listValues)
    {
        var listValuesSortedBySize =
            listValues
            .OrderBy(AggregateSizeIncludingDescendants)
            .ToImmutableList();

        var descendantLists =
            EnumerateDescendantListsBreadthFirst(listValuesSortedBySize)
            .ToImmutableList();

        var orderedLists =
            listValuesSortedBySize
            .Concat(descendantLists.Reverse())
            .Distinct()
            .Intersect(listValuesSortedBySize)
            .ToImmutableList();

        return orderedLists;
    }

    public static IEnumerable<PineVM.Expression> OrderExpressionsByContainment(IEnumerable<PineVM.Expression> expressions)
    {
        var descendantLists =
            expressions
            .SelectMany(PineVM.Expression.EnumerateSelfAndDescendants)
            .ToImmutableList();

        var ordered =
            descendantLists.Reverse()
            .Distinct()
            .Intersect(expressions);

        return ordered;
    }

    private static int AggregateSizeIncludingDescendants(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blobValue =>
            blobValue.Bytes.Length,

            PineValue.ListValue listValue =>
            listValue.Elements.Count + listValue.Elements.Sum(AggregateSizeIncludingDescendants),

            _ => throw new Exception("Unknown value type: " + value.GetType().FullName)
        };

    private static IEnumerable<PineValue.ListValue> EnumerateDescendantListsBreadthFirst(IEnumerable<PineValue.ListValue> roots)
    {
        var queue = new Queue<PineValue.ListValue>(roots);

        while (queue.Any())
        {
            foreach (var item in queue.Dequeue().Elements.OfType<PineValue.ListValue>())
            {
                yield return item;

                if (!queue.Contains(item))
                    queue.Enqueue(item);
            }
        }
    }
}
