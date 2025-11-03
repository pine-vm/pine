using Pine.Core.Internal;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// A set of constraints relative to the generic value model.
/// Compilers use these to model inferred types and create optimized program code to improve runtime efficiency.
/// </summary>
public record PineValueClass
{
    /// <summary>
    /// Cache used to compute stable, fast hashes for the canonical representation of this class.
    /// </summary>
    readonly static Addressing.ConcurrentPineValueHashCache s_valueHashCache = new();

    /// <summary>
    /// The set of constraints captured by this class.
    /// Each entry maps an integer path into a value to the expected <see cref="PineValue"/> found at that path.
    /// </summary>
    public IReadOnlyDictionary<IReadOnlyList<int>, PineValue> ParsedItems { get; }

    /// <summary>
    /// Hash of the canonical representation of this class, encoded in lowercase hexadecimal.
    /// </summary>
    public readonly string HashBase16;

    /// <summary>
    /// Internal representation optimized for fast membership checks.
    /// </summary>
    private readonly FastRepresentation _fastRepresentation;

    /// <summary>
    /// Gets an empty <see cref="PineValueClass"/> with no constraints.
    /// </summary>
    public static readonly PineValueClass Empty = Create([]);

    /// <summary>
    /// Representation optimized for fast check at runtime.
    /// Independent of the identity of the constraint, this internal representation could
    /// use different ordering to check the most distinctive items first.
    /// </summary>
    private record FastRepresentation(
        ReadOnlyMemory<(ReadOnlyMemory<int>, PineValue)> Constraints)
    {
        /// <summary>
        /// Tests whether the given concrete value is a member of the class.
        /// </summary>
        /// <param name="concreteValue">The value to test.</param>
        /// <returns>True if all constraints are satisfied; otherwise false.</returns>
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

        /// <summary>
        /// Tests whether the given <see cref="PineValueInProcess"/> is a member of the class.
        /// </summary>
        /// <param name="concreteValue">The value to test.</param>
        /// <returns>True if all constraints are satisfied; otherwise false.</returns>
        public bool SatisfiedByValue(PineValueInProcess concreteValue)
        {
            for (var i = 0; i < Constraints.Length; ++i)
            {
                var (path, expectedValue) = Constraints.Span[i];

                if (PineValueInProcess.ValueFromPathOrNull(concreteValue, path.Span) is not { } pathValue)
                    return false;

                if (!pathValue.Equals(expectedValue))
                    return false;
            }

            return true;
        }
    }

    /// <summary>
    /// Initializes a new instance of <see cref="PineValueClass"/> from the given parsed items and hash.
    /// </summary>
    /// <param name="parsedItems">The constraints mapping paths to expected values.</param>
    /// <param name="hashBase16">Canonical hash of the class, in base16.</param>
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

    /// <summary>
    /// Returns the concrete value specified for the given path,
    /// if this class specifies a value for that path at all.
    /// </summary>
    /// <param name="path">The integer path to look up.</param>
    /// <returns>The expected <see cref="PineValue"/> for the path, or null if not specified.</returns>
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

    /// <inheritdoc />
    public override int GetHashCode()
    {
        return HashBase16.GetHashCode();
    }

    /// <inheritdoc />
    public override string ToString()
    {
        if (ParsedItems.Count is 0)
            return "ValueClass<empty>";

        var itemsString = string.Join(", ", ParsedItems.Select(kv => $"[{string.Join(", ", kv.Key)}] = {kv.Value}"));

        return
            $"ValueClass<Hash={HashBase16[..8]}, Items={{ {itemsString} }}>";
    }

    /// <summary>
    /// Tests two <see cref="PineValueClass"/> instances for equality by identity of their canonical hash.
    /// </summary>
    /// <param name="id0">First instance.</param>
    /// <param name="id1">Second instance.</param>
    /// <returns>True if both are the same reference or have equal <see cref="HashBase16"/>; otherwise false.</returns>
    public static bool Equal(PineValueClass? id0, PineValueClass? id1)
    {
        if (ReferenceEquals(id0, id1))
            return true;

        if (id0 is null || id1 is null)
            return false;

        return id0.HashBase16 == id1.HashBase16;
    }

    /// <summary>
    /// Creates a class that only contains the given concrete value at the root path.
    /// </summary>
    public static PineValueClass CreateEquals(PineValue pineValue) =>
        Create([new KeyValuePair<IReadOnlyList<int>, PineValue>([], pineValue)]);

    /// <summary>
    /// Creates a <see cref="PineValueClass"/> from the observed environment items and a concrete environment value.
    /// </summary>
    /// <param name="observedPart">Observed paths in the environment for the current expression.</param>
    /// <param name="concreteValue">Concrete environment value to read the items from.</param>
    /// <param name="skipUnavailableItems">If true, ignores paths not present in the concrete value; otherwise throws.</param>
    /// <returns>A new <see cref="PineValueClass"/> representing constraints for the observed items.</returns>
    public static PineValueClass Create(
        ExpressionEnvClass.ConstrainedEnv observedPart,
        PineValue concreteValue,
        bool skipUnavailableItems)
    {
        return
            Create(
                observedPart,
                CreateEquals(concreteValue),
                skipUnavailableItems);
    }

    /// <summary>
    /// Creates a <see cref="PineValueClass"/> from the observed environment items and a concrete environment value.
    /// </summary>
    public static PineValueClass Create(
        ExpressionEnvClass.ConstrainedEnv observedPart,
        PineValueClass valueClass,
        bool skipUnavailableItems)
    {
        var parsedItems =
            observedPart.ParsedEnvItems
            .SelectMany(path =>
            {
                var itemValue = valueClass.TryGetValue(path);

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

    /// <summary>
    /// Creates a <see cref="PineValueClass"/> from an explicit list of path-to-value constraints.
    /// </summary>
    /// <param name="parsedItems">Constraints mapping paths to expected values.</param>
    /// <returns>A new <see cref="PineValueClass"/> with a canonical hash computed from the constraints.</returns>
    public static PineValueClass Create(
        IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>> parsedItems)
    {
        if (parsedItems.Count is 0 && Empty is not null)
            return Empty;

        PineValue[] parsedItemsPineValues =
            [.. parsedItems
            .OrderBy(kv => kv.Key, IntPathComparer.Instance)
            .Select(item =>
            (PineValue)
            PineValue.List(
                [PineValue.List([.. item.Key.Select(pathItem => IntegerEncoding.EncodeSignedInteger(pathItem))]),
                item.Value]))];

        var hashBase16 =
            Convert.ToHexStringLower(s_valueHashCache.GetHash(PineValue.List(parsedItemsPineValues)).Span);

        return new PineValueClass(
            parsedItems.ToImmutableSortedDictionary(keyComparer: IntPathComparer.Instance),
            hashBase16: hashBase16);
    }

    /// <summary>
    /// Determines whether the specified object is equal to the current object, using <see cref="Equal(PineValueClass?, PineValueClass?)"/>.
    /// </summary>
    /// <param name="other">The object to compare with the current object.</param>
    /// <returns>True if the instances are equal; otherwise false.</returns>
    public virtual bool Equals(PineValueClass? other) =>
        Equal(this, other);

    /// <summary>
    /// Tests whether the provided concrete value satisfies all constraints in this class.
    /// </summary>
    /// <param name="concreteValue">The value to test.</param>
    /// <returns>True if the value satisfies all constraints; otherwise false.</returns>
    public bool SatisfiedByValue(PineValue concreteValue)
    {
        return _fastRepresentation.SatisfiedByValue(concreteValue);
    }

    /// <summary>
    /// Tests whether the provided concrete value satisfies all constraints in this class.
    /// </summary>
    /// <param name="concreteValue">The value to test.</param>
    /// <returns>True if the value satisfies all constraints; otherwise false.</returns>
    public bool SatisfiedByValue(PineValueInProcess concreteValue)
    {
        return _fastRepresentation.SatisfiedByValue(concreteValue);
    }

    /// <summary>
    /// Tests whether all constraints in this class are implied by (are satisfied by) another class.
    /// </summary>
    /// <param name="otherClass">The other class to test implication against.</param>
    /// <returns>
    /// True if for every path in this class, <paramref name="otherClass"/> specifies the same value; otherwise false.
    /// </returns>
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

    /// <summary>
    /// Builds a class that captures the structural intersection of two values up to a depth limit.
    /// </summary>
    /// <param name="valueA">First value.</param>
    /// <param name="valueB">Second value.</param>
    /// <param name="depthLimit">Maximum list nesting depth to inspect for intersection.</param>
    /// <returns>A new <see cref="PineValueClass"/> that matches the common structure and values.</returns>
    public static PineValueClass CreateIntersection(
        PineValue valueA,
        PineValue valueB,
        int depthLimit) =>
        Create(Intersection(valueA, valueB, depthLimit));


    /// <summary>
    /// Computes a collection of path-to-value pairs representing the intersection between two values.
    /// </summary>
    /// <param name="valueA">First value.</param>
    /// <param name="valueB">Second value.</param>
    /// <param name="depthLimit">Maximum list nesting depth to inspect.</param>
    /// <returns>A list of constraints representing the intersection.</returns>
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

    /// <summary>
    /// Internal node type for building an intersection tree between two values.
    /// </summary>
    private abstract record IntersectionNode
    {
        /// <summary>
        /// Leaf node representing a concrete value shared by both inputs at a given path.
        /// </summary>
        /// <param name="Value">The common value.</param>
        public sealed record IntersectionLeaf(PineValue Value)
            : IntersectionNode;

        /// <summary>
        /// Branch node carrying children indexed by list offsets.
        /// </summary>
        /// <param name="Children">Child nodes keyed by element offset.</param>
        public sealed record IntersectionBranch(IReadOnlyList<(int offset, IntersectionNode childNode)> Children)
            : IntersectionNode;
    }

    /// <summary>
    /// Builds the intersection tree of two values up to a depth limit.
    /// </summary>
    /// <param name="valueA">First value.</param>
    /// <param name="valueB">Second value.</param>
    /// <param name="depthLimit">Maximum list depth to traverse.</param>
    /// <returns>The root of the intersection tree.</returns>
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

    /// <summary>
    /// Intersects an existing constraint with a concrete value, returning a refined constraint.
    /// </summary>
    /// <param name="constraint">The starting constraint.</param>
    /// <param name="value">The concrete value to intersect with.</param>
    /// <returns>A new <see cref="PineValueClass"/> capturing only items that match in both.</returns>
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

    /// <summary>
    /// Projects this constraint down to the given subpath, removing the path prefix from each item retained.
    /// </summary>
    /// <param name="path">The path that must be a prefix of items to retain.</param>
    /// <returns>A new <see cref="PineValueClass"/> containing only items under the given path.</returns>
    public PineValueClass PartUnderPath(IReadOnlyList<int> path)
    {
        var filteredUnderPath =
            ParsedItems
            .SelectMany(item =>
            {
                if (!item.Key.Take(path.Count).SequenceEqual(path))
                {
                    return (IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>>)[];
                }

                return [new KeyValuePair<IReadOnlyList<int>, PineValue>([.. item.Key.Skip(path.Count)], item.Value)];
            }).ToList();

        if (TryGetValue(path) is { } valueAtPath)
        {
            filteredUnderPath.Add(new KeyValuePair<IReadOnlyList<int>, PineValue>([], valueAtPath));
        }

        return Create(filteredUnderPath);
    }

    /// <summary>
    /// Attempts to infer a value-class for an expression under a given environment constraint.
    /// </summary>
    /// <param name="envClass">Constraint describing known values available at the root environment.</param>
    /// <param name="expression">
    /// Expression to analyze. Recognized cases:
    /// - <see cref="Expression.Environment"/>: returns <paramref name="envClass"/>.
    /// - <see cref="Expression.Literal"/>: returns an equality constraint to the literal value.
    /// - Path in parent env (via <see cref="CodeAnalysis.TryParseExprAsPathInEnv(Expression)"/>):
    ///   if the path resolves in <paramref name="envClass"/>, returns an equality constraint to that value;
    ///   otherwise returns the projection of <paramref name="envClass"/> under that path via <see cref="PartUnderPath"/>.
    /// - <see cref="Expression.List"/>: maps each item recursively and combines constraints under their list indices.
    /// Other expression kinds are not handled and yield null.
    /// </param>
    /// <returns>
    /// A <see cref="PineValueClass"/> describing the values the expression can take under
    /// <paramref name="envClass"/>, or null if no useful constraint can be inferred.
    /// </returns>
    public static PineValueClass? MapValueClass(
        PineValueClass envClass,
        Expression expression)
    {
        if (expression is Expression.Environment)
        {
            return envClass;
        }

        if (expression is Expression.Literal literal)
        {
            return CreateEquals(literal.Value);
        }

        if (CodeAnalysis.TryParseExprAsPathInEnv(expression) is { } pathInParentEnv)
        {
            if (envClass.TryGetValue(pathInParentEnv) is { } valueAtPath)
            {
                return CreateEquals(valueAtPath);
            }

            return envClass.PartUnderPath(pathInParentEnv);
        }

        if (expression is Expression.List listExpr)
        {
            var itemClasses = new PineValueClass?[listExpr.Items.Count];

            for (var itemIndex = 0; itemIndex < listExpr.Items.Count; ++itemIndex)
            {
                var item = listExpr.Items[itemIndex];

                var itemClass = MapValueClass(envClass, item);

                itemClasses[itemIndex] = itemClass;
            }

            var itemsFlattened = new List<KeyValuePair<IReadOnlyList<int>, PineValue>>();

            for (var itemIndex = 0; itemIndex < itemClasses.Length; ++itemIndex)
            {
                if (itemClasses[itemIndex] is not { } itemClass)
                {
                    continue;
                }

                foreach (var classItem in itemClass.ParsedItems)
                {
                    itemsFlattened.Add(
                        new KeyValuePair<IReadOnlyList<int>, PineValue>(
                            [itemIndex, .. classItem.Key],
                            classItem.Value));
                }
            }

            return Create(itemsFlattened);
        }

        return null;
    }
}

