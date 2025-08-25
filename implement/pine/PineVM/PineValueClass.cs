using Pine.Core;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;

/// <summary>
/// A set of constraints relative to the generic value model.
/// Compilers use these to model inferred types and create optimized program code to improve runtime efficiency.
/// </summary>
public record PineValueClass
{
    readonly static CompilePineToDotNet.CompilerMutableCache s_compilerCache = new();

    public IReadOnlyDictionary<IReadOnlyList<int>, PineValue> ParsedItems { get; }

    readonly public string HashBase16;

    readonly private FastRepresentation _fastRepresentation;

    /// <summary>
    /// Representation optimized for fast check at runtime.
    /// Independent of the identity of the constraint, this internal representation could
    /// use different ordering to check the most distinctive items first.
    /// </summary>
    private record FastRepresentation(
        ReadOnlyMemory<(ReadOnlyMemory<int>, PineValue)> Constraints)
    {
        public bool SatisfiedByValue(PineValue concreteValue)
        {
            for (var i = 0; i < Constraints.Length; ++i)
            {
                var (path, expectedValue) = Constraints.Span[i];

                if (CodeAnalysis.ValueFromPathInValue(concreteValue, path.Span) is not { } pathValue)
                    return false;

                if (!pathValue.Equals(expectedValue))
                    return false;
            }

            return true;
        }
    }

    private PineValueClass(
        IReadOnlyDictionary<IReadOnlyList<int>, PineValue> parsedItems,
        string hashBase16)
    {
        ParsedItems = parsedItems;

        HashBase16 = hashBase16;

        var constraintsList = new (ReadOnlyMemory<int>, PineValue)[parsedItems.Count];

        for (var i = 0; i < parsedItems.Count; ++i)
        {
            var kv = parsedItems.ElementAt(i);

            constraintsList[i] = (new ReadOnlyMemory<int>([.. kv.Key]), kv.Value);
        }

        var constraintsMemory = new ReadOnlyMemory<(ReadOnlyMemory<int>, PineValue)>(constraintsList);

        _fastRepresentation = new FastRepresentation(constraintsMemory);
    }

    public PineValue? TryGetValue(IReadOnlyList<int> path)
    {
        if (ParsedItems.TryGetValue(path, out var value))
            return value;

        if (path.Count is 0)
            return null;

        if (TryGetValue([.. path.SkipLast(1)]) is not { } parentValue)
            return null;

        return CodeAnalysis.ValueFromPathInValue(parentValue, [.. path.TakeLast(1)]);
    }

    public override int GetHashCode()
    {
        return HashBase16.GetHashCode();
    }

    public static bool Equal(PineValueClass? id0, PineValueClass? id1)
    {
        if (ReferenceEquals(id0, id1))
            return true;

        if (id0 is null || id1 is null)
            return false;

        return id0.HashBase16 == id1.HashBase16;
    }

    public static PineValueClass CreateEquals(PineValue pineValue) =>
        Create([new KeyValuePair<IReadOnlyList<int>, PineValue>([], pineValue)]);

    public static PineValueClass Create(
        ExpressionEnvClass.ConstrainedEnv observedPart,
        PineValue concreteValue,
        bool skipUnavailableItems)
    {
        var parsedItems =
            observedPart.ParsedEnvItems
            .SelectMany(path =>
            {
                var itemValue = CodeAnalysis.ValueFromPathInValue(concreteValue, [.. path]);

                if (itemValue is null)
                {
                    if (skipUnavailableItems)
                        return Enumerable.Empty<KeyValuePair<IReadOnlyList<int>, PineValue>>();

                    throw new Exception("Item value null for path " + string.Join(", ", path));
                }

                return [new KeyValuePair<IReadOnlyList<int>, PineValue>(path, itemValue)];
            })
            .OrderBy(kv => kv.Key, IntPathComparer.Instance)
            .ToImmutableArray();

        return Create(parsedItems);
    }

    public static PineValueClass Create(
        IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>> parsedItems)
    {
        PineValue[] parsedItemsPineValues =
            [.. parsedItems
        .OrderBy(kv => kv.Key, IntPathComparer.Instance)
        .Select(item =>
        (PineValue)
        PineValue.List(
            [PineValue.List([.. item.Key.Select(pathItem => IntegerEncoding.EncodeSignedInteger(pathItem))]),
            item.Value]))];

        var hashBase16 =
            Convert.ToHexStringLower(s_compilerCache.ComputeHash(PineValue.List(parsedItemsPineValues)).Span);

        return new PineValueClass(
            parsedItems.ToImmutableSortedDictionary(keyComparer: IntPathComparer.Instance),
            hashBase16: hashBase16);
    }

    public virtual bool Equals(PineValueClass? other) =>
        Equal(this, other);

    public bool SatisfiedByValue(PineValue concreteValue)
    {
        return _fastRepresentation.SatisfiedByValue(concreteValue);
    }

    public bool SatisfiedByConstraint(PineValueClass otherClass)
    {
        foreach (var item in ParsedItems)
        {
            if (item.Value is not { } expectedValue)
                return false;

            if (otherClass.TryGetValue(item.Key) is not { } pathValue)
                return false;

            if (!pathValue.Equals(expectedValue))
                return false;
        }

        return true;
    }

    public static PineValueClass CreateIntersection(
        PineValue valueA,
        PineValue valueB,
        int depthLimit) =>
        Create(Intersection(valueA, valueB, depthLimit));


    public static IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>> Intersection(
        PineValue valueA,
        PineValue valueB,
        int depthLimit)
    {
        var intersectionTree = IntersectionTree(valueA, valueB, depthLimit);

        var mutatedCollection = new List<KeyValuePair<IReadOnlyList<int>, PineValue>>();

        void CollectLeafesRecursively(ImmutableQueue<int> stack, IntersectionNode node)
        {
            if (node is IntersectionNode.IntersectionLeaf leaf)
            {
                mutatedCollection.Add(new KeyValuePair<IReadOnlyList<int>, PineValue>([.. stack], leaf.Value));
            }

            if (node is IntersectionNode.IntersectionBranch branch)
            {
                foreach (var (offset, childNode) in branch.Children)
                {
                    CollectLeafesRecursively(stack.Enqueue(offset), childNode);
                }
            }
        }

        CollectLeafesRecursively([], intersectionTree);

        return mutatedCollection;
    }

    private abstract record IntersectionNode
    {
        public sealed record IntersectionLeaf(PineValue Value)
            : IntersectionNode;

        public sealed record IntersectionBranch(IReadOnlyList<(int offset, IntersectionNode childNode)> Children)
            : IntersectionNode;
    }

    private static IntersectionNode IntersectionTree(
        PineValue valueA,
        PineValue valueB,
        int depthLimit)
    {
        if (valueA == valueB)
        {
            return new IntersectionNode.IntersectionLeaf(valueA);
        }

        if (depthLimit < 1)
        {
            return new IntersectionNode.IntersectionBranch([]);
        }

        if (valueA is not PineValue.ListValue listA || valueB is not PineValue.ListValue listB)
        {
            return new IntersectionNode.IntersectionBranch([]);
        }

        var commonLength =
            listA.Items.Length < listB.Items.Length ?
            listA.Items.Length :
            listB.Items.Length;

        var children = new List<(int, IntersectionNode)>();

        for (var i = 0; i < commonLength; ++i)
        {
            var childNode = IntersectionTree(listA.Items.Span[i], listB.Items.Span[i], depthLimit - 1);

            if (childNode is IntersectionNode.IntersectionBranch branch && branch.Children.Count is 0)
                continue;

            children.Add((i, childNode));
        }

        return new IntersectionNode.IntersectionBranch(children);
    }

    public static PineValueClass CreateIntersection(PineValueClass constraint, PineValue value)
    {
        if (constraint.SatisfiedByValue(value))
            return constraint;

        var intersectionItems = new Dictionary<IReadOnlyList<int>, PineValue>();

        foreach (var item in constraint.ParsedItems)
        {
            if (CodeAnalysis.ValueFromPathInValue(value, [.. item.Key]) is not { } pathValue)
                continue;

            if (pathValue.Equals(item.Value))
            {
                intersectionItems[item.Key] = item.Value;
                continue;
            }

            if (item.Value is not PineValue.ListValue constraintItemList || pathValue is not PineValue.ListValue valueItemList)
            {
                continue;
            }

            for (var i = 0; i < constraintItemList.Items.Length && i < valueItemList.Items.Length; ++i)
            {
                var constraintChildItem = constraintItemList.Items.Span[i];
                var foundChildItem = valueItemList.Items.Span[i];

                var childConstraint = CreateEquals(constraintChildItem);

                var childConstraintIntersection = CreateIntersection(childConstraint, foundChildItem);

                foreach (var childConstraintItem in childConstraintIntersection.ParsedItems)
                {
                    intersectionItems[[.. item.Key, i, .. childConstraintItem.Key]] = childConstraintItem.Value;
                }
            }
        }

        return Create([.. intersectionItems]);
    }

    public PineValueClass PartUnderPath(IReadOnlyList<int> path)
    {
        return Create(
            [..
        ParsedItems
        .SelectMany(item =>
        {
            if (!item.Key.Take(path.Count).SequenceEqual(path))
            {
                return (IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>>)[];
            }

            return [new KeyValuePair<IReadOnlyList<int>, PineValue>([.. item.Key.Skip(path.Count)], item.Value)];
        })]);
    }
}


public class PineValueClassSpecificityComparer : IComparer<PineValueClass>
{
    public readonly static PineValueClassSpecificityComparer Instance = new();

    public int Compare(PineValueClass? x, PineValueClass? y)
    {
        if (x is null && y is null)
            return 0;

        if (x is null)
            return -1;

        if (y is null)
            return 1;

        if (x.SatisfiedByConstraint(y))
        {
            if (!y.SatisfiedByConstraint(x))
            {
                return -1;
            }
        }

        if (y.SatisfiedByConstraint(x))
        {
            if (!x.SatisfiedByConstraint(y))
            {
                return 1;
            }
        }

        return x.ParsedItems.Count - y.ParsedItems.Count;
    }
}

