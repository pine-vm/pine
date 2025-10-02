using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.DotNet;

/// <summary>
/// Helpers that provide deterministic ordering for <see cref="PineValue"/> instances and Pine expressions.
/// </summary>
/// <remarks>
/// These orderings are used during code generation to emit stable, human-friendly declarations.
/// See also <see cref="ValueSyntaxKindDeclarationOrder"/> which orders based on <see cref="ValueSyntaxKind"/> classification.
/// </remarks>
public class CSharpDeclarationOrder
{
    /// <summary>
    /// Orders a mixed set of <see cref="PineValue"/>s for declaration emission.
    /// </summary>
    /// <remarks>
    /// The ordering strategy is:
    /// <list type="number">
    /// <item><description>All blobs, ordered by <see cref="BlobValueDeclarationOrder"/>.</description></item>
    /// <item><description>Then all lists, ordered by <see cref="OrderListValuesBySize(IEnumerable{PineValue.ListValue})"/>.</description></item>
    /// </list>
    /// </remarks>
    /// <param name="pineValues">Values to order.</param>
    /// <returns>An ordered sequence suitable for deterministic declaration.</returns>
    public static IEnumerable<PineValue> OrderValuesForDeclaration(IEnumerable<PineValue> pineValues)
    {
        var blobs =
            pineValues.OfType<PineValue.BlobValue>()
            .Order(new BlobValueDeclarationOrder());

        var originalLists = pineValues.OfType<PineValue.ListValue>().Distinct();

        var orderedLists = OrderListValuesBySize(originalLists);

        return blobs.Cast<PineValue>().Concat(orderedLists);
    }

    /// <summary>
    /// Comparer that orders instances of <see cref="ValueSyntaxKind"/>.
    /// </summary>
    /// <remarks>
    /// Priority from highest to lowest:
    /// <list type="number">
    /// <item><description><see cref="ValueSyntaxKind.AsSignedInteger"/> (by numeric value).</description></item>
    /// <item><description><see cref="ValueSyntaxKind.AsListOfSignedIntegers"/> (by length, then lexicographically by items).</description></item>
    /// <item><description><see cref="ValueSyntaxKind.AsString"/> (lexicographically).</description></item>
    /// <item><description><see cref="ValueSyntaxKind.Other"/>.</description></item>
    /// </list>
    /// </remarks>
    public class ValueSyntaxKindDeclarationOrder : IComparer<ValueSyntaxKind>
    {
        /// <summary>
        /// Shared singleton instance of this comparer.
        /// </summary>
        public static readonly ValueSyntaxKindDeclarationOrder Instance = new();

        /// <summary>
        /// Compares two <see cref="ValueSyntaxKind"/> instances following the priority described in the type remarks.
        /// </summary>
        /// <param name="x">Left operand.</param>
        /// <param name="y">Right operand.</param>
        /// <returns>
        /// A signed integer that indicates the relative values of <paramref name="x"/> and <paramref name="y"/>.
        /// Less than zero means <paramref name="x"/> precedes <paramref name="y"/>; greater than zero means it follows; zero means equal.
        /// </returns>
        public int Compare(ValueSyntaxKind? x, ValueSyntaxKind? y)
        {
            if (x == y)
                return 0;

            if (x is null)
                return -1;

            if (y is null)
                return 1;

            if (x is ValueSyntaxKind.AsSignedInteger xSignedInt)
            {
                if (y is ValueSyntaxKind.AsSignedInteger ySignedInt)
                    return xSignedInt.Value.CompareTo(ySignedInt.Value);

                return -1;
            }

            if (y is ValueSyntaxKind.AsSignedInteger)
                return 1;

            if (x is ValueSyntaxKind.AsListOfSignedIntegers xListOfSignedIntegers)
            {
                if (y is ValueSyntaxKind.AsListOfSignedIntegers yListOfSignedIntegers)
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

            if (y is ValueSyntaxKind.AsListOfSignedIntegers)
                return 1;

            if (x is ValueSyntaxKind.AsString xString)
            {
                if (y is ValueSyntaxKind.AsString yString)
                    return xString.Value.CompareTo(yString.Value);

                return -1;
            }

            if (y is ValueSyntaxKind.AsString)
                return 1;

            return 0;
        }
    }

    /// <summary>
    /// Comparer that orders <see cref="PineValue"/>s of the same primitive kind for deterministic output.
    /// </summary>
    /// <remarks>
    /// Ordering is defined as:
    /// <list type="bullet">
    /// <item><description>Blobs: by <see cref="BlobValueDeclarationOrder"/>.</description></item>
    /// <item><description>Lists: by aggregate size (nodes + contained bytes), then by item count, then lexicographically by items.</description></item>
    /// </list>
    /// </remarks>
    public class ValueDeclarationOrder : IComparer<PineValue>
    {
        /// <summary>
        /// Shared singleton instance of this comparer.
        /// </summary>
        public static readonly ValueDeclarationOrder Instance = new();

        /// <summary>
        /// Compares two <see cref="PineValue"/> instances of compatible kinds.
        /// </summary>
        /// <param name="x">Left operand.</param>
        /// <param name="y">Right operand.</param>
        /// <returns>
        /// A signed integer that indicates the relative values of <paramref name="x"/> and <paramref name="y"/>.
        /// Less than zero means <paramref name="x"/> precedes <paramref name="y"/>; greater than zero means it follows; zero means equal.
        /// </returns>
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

                    if (xList.Items.Length < yList.Items.Length)
                        return -1;

                    if (yList.Items.Length < xList.Items.Length)
                        return 1;

                    for (var i = 0; i < xList.Items.Length; i++)
                    {
                        var itemComparison = Compare(xList.Items.Span[i], yList.Items.Span[i]);

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

    /// <summary>
    /// Comparer for <see cref="PineValue.BlobValue"/> that orders by payload length and then by byte content.
    /// </summary>
    public class BlobValueDeclarationOrder : IComparer<PineValue.BlobValue>
    {
        /// <summary>
        /// Shared singleton instance of this comparer.
        /// </summary>
        public static readonly BlobValueDeclarationOrder Instance = new();

        /// <summary>
        /// Compares two blob values by length, then lexicographically by bytes.
        /// </summary>
        /// <param name="x">Left operand.</param>
        /// <param name="y">Right operand.</param>
        /// <returns>
        /// A signed integer that indicates the relative values of <paramref name="x"/> and <paramref name="y"/>.
        /// Less than zero means <paramref name="x"/> precedes <paramref name="y"/>; greater than zero means it follows; zero means equal.
        /// </returns>
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

    /// <summary>
    /// Orders list values by their aggregate size (nodes + contained bytes).
    /// </summary>
    /// <param name="listValues">The list values to order.</param>
    /// <returns>Lists ordered from smaller to larger structural footprint.</returns>
    public static IEnumerable<PineValue.ListValue> OrderListValuesBySize(IEnumerable<PineValue.ListValue> listValues) =>
        listValues
        .Select(value => (value, size: AggregateSizeIncludingDescendants(value)))
        .OrderBy(tuple => tuple.size)
        .Select(tuple => tuple.value);

    /// <summary>
    /// Orders list values considering containment relationships to obtain a stable order.
    /// </summary>
    /// <remarks>
    /// Starts from lists ordered by size, then appends descendants (breadth-first, reversed) and de-duplicates,
    /// preserving only those present in the input. This prefers smaller/ancestor lists over larger/descendant lists.
    /// </remarks>
    /// <param name="listValues">The list values to order.</param>
    /// <returns>An ordered sequence of list values accounting for containment.</returns>
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

    /// <summary>
    /// Orders expressions so that ancestors appear before their descendants in a deterministic way.
    /// </summary>
    /// <param name="expressions">The expressions to order.</param>
    /// <returns>An ordered sequence of expressions respecting containment.</returns>
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

        while (queue.Count is not 0)
        {
            var current = queue.Dequeue();

            if (current is PineValue.ListValue listValue)
            {
                for (var i = 0; i < listValue.Items.Length; i++)
                {
                    var item = listValue.Items.Span[i];

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
