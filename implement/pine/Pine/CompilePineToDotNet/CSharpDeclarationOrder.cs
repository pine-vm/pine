using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Pine.Core;

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

    public class ValueSyntaxKindDeclarationOrder : IComparer<CompileToCSharp.ValueSyntaxKind>
    {
        public int Compare(CompileToCSharp.ValueSyntaxKind? x, CompileToCSharp.ValueSyntaxKind? y)
        {
            if (x == y)
                return 0;

            if (x is null)
                return -1;

            if (y is null)
                return 1;

            if (x is CompileToCSharp.ValueSyntaxKind.AsSignedInteger xSignedInt)
            {
                if (y is CompileToCSharp.ValueSyntaxKind.AsSignedInteger ySignedInt)
                    return xSignedInt.Value.CompareTo(ySignedInt.Value);

                return -1;
            }

            if (y is CompileToCSharp.ValueSyntaxKind.AsSignedInteger)
                return 1;

            if (x is CompileToCSharp.ValueSyntaxKind.AsListOfSignedIntegers xListOfSignedIntegers)
            {
                if (y is CompileToCSharp.ValueSyntaxKind.AsListOfSignedIntegers yListOfSignedIntegers)
                {
                    if (xListOfSignedIntegers.Values.Count < yListOfSignedIntegers.Values.Count)
                        return -1;

                    if (yListOfSignedIntegers.Values.Count < xListOfSignedIntegers.Values.Count)
                        return 1;

                    for (var i = 0; i < xListOfSignedIntegers.Values.Count; i++)
                    {
                        if (xListOfSignedIntegers.Values[i] < yListOfSignedIntegers.Values[i])
                            return -1;

                        if (yListOfSignedIntegers.Values[i] < xListOfSignedIntegers.Values[i])
                            return 1;
                    }

                    return 0;
                }

                return -1;
            }

            if (y is CompileToCSharp.ValueSyntaxKind.AsListOfSignedIntegers)
                return 1;

            if (x is CompileToCSharp.ValueSyntaxKind.AsString xString)
            {
                if (y is CompileToCSharp.ValueSyntaxKind.AsString yString)
                    return xString.Value.CompareTo(yString.Value);

                return -1;
            }

            if (y is CompileToCSharp.ValueSyntaxKind.AsString)
                return 1;

            return 0;
        }
    }

    public class ValueDeclarationOrder : IComparer<PineValue>
    {
        public int Compare(PineValue? x, PineValue? y)
        {
            if (x == y)
                return 0;

            if (x is null)
                return -1;

            if (y is null)
                return 1;

            if (x is PineValue.BlobValue xBlob)
            {
                if (y is PineValue.BlobValue yBlob)
                    return new BlobValueDeclarationOrder().Compare(xBlob, yBlob);

                return -1;
            }

            if (y is PineValue.BlobValue)
                return 1;

            if (x is PineValue.ListValue xList)
            {
                if (y is PineValue.ListValue yList)
                {
                    var xSize = AggregateSizeIncludingDescendants(xList);
                    var ySize = AggregateSizeIncludingDescendants(yList);

                    if (xSize < ySize)
                        return -1;

                    if (xSize > ySize)
                        return 1;

                    if (xList.Elements.Length < yList.Elements.Length)
                        return -1;

                    if (yList.Elements.Length < xList.Elements.Length)
                        return 1;

                    for (var i = 0; i < xList.Elements.Length; i++)
                    {
                        var itemComparison = Compare(xList.Elements.Span[i], yList.Elements.Span[i]);

                        if (itemComparison != 0)
                            return itemComparison;
                    }

                    return 0;
                }

                return -1;
            }

            return 0;
        }
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

    public static IEnumerable<Expression> OrderExpressionsByContainment(IEnumerable<Expression> expressions)
    {
        var descendantLists =
            expressions
            .SelectMany(Expression.EnumerateSelfAndDescendants)
            .ToImmutableList();

        var ordered =
            descendantLists.Reverse()
            .Distinct()
            .Intersect(expressions);

        return ordered;
    }

    private static long AggregateSizeIncludingDescendants(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blobValue =>
            blobValue.Bytes.Length,

            PineValue.ListValue listValue =>
            listValue.NodesCount + listValue.BlobsBytesCount + 1,

            _ =>
            throw new NotImplementedException(
                "Unknown value type: " + value.GetType().FullName)
        };

    private static IEnumerable<PineValue.ListValue> EnumerateDescendantListsBreadthFirst(IEnumerable<PineValue.ListValue> roots)
    {
        var queue = new Queue<PineValue.ListValue>(roots);

        while (queue.Any())
        {
            var current = queue.Dequeue();

            if (current is PineValue.ListValue listValue)
            {
                for (var i = 0; i < listValue.Elements.Length; i++)
                {
                    var item = listValue.Elements.Span[i];

                    if (item is PineValue.ListValue listItem)
                    {
                        yield return listItem;

                        if (!queue.Contains(listItem))
                            queue.Enqueue(listItem);
                    }
                }
            }
        }
    }
}
