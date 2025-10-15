using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.DotNet.Builtins;

/// <summary>
/// Tracks a sequence of <see cref="KernelFunction.concat"/> applications to enable evaluation at a later time.
/// This immutable variant creates a new builder instance on each append operation, avoiding mutation.
/// Its suitable for scenarios where we cannot prove sufficient constraints to employ opportunistic mutation.
/// </summary>
public abstract record ImmutableConcatBuilder
{
    /// <summary>
    /// Maximum number of items in a leaf node that can be consolidated during append/prepend operations.
    /// When appending or prepending to a builder, if the end or start node is a leaf with fewer items
    /// than this threshold, the items will be merged into a single leaf to reduce tree depth.
    /// This is similar to the ConsString optimization in V8.
    /// </summary>
    private const int MaxLeafSizeForConsolidation = 32;

    /// <summary>
    /// Gets the total number of items contained across all leaves of this builder.
    /// </summary>
    public abstract int AggregateItemsCount { get; }

    /// <summary>
    /// Evaluates this builder into a single <see cref="PineValue"/> by applying the recorded concatenations.
    /// </summary>
    public abstract PineValue Evaluate();

    /// <summary>
    /// Evaluates this builder by reversing the order of items, then applying concatenations.
    /// Equivalent to Evaluate() followed by reverse(), but more efficient.
    /// </summary>
    public abstract PineValue EvaluateReverse();

    /// <summary>
    /// Predict whether the result from <see cref="Evaluate"/> would be a <see cref="PineValue.ListValue"/> value without fully evaluating.
    /// </summary>
    public abstract bool IsList();

    /// <summary>
    /// Predict whether the result from <see cref="Evaluate"/> would be a <see cref="PineValue.BlobValue"/> value without fully evaluating.
    /// </summary>
    public abstract bool IsBlob();

    /// <summary>
    /// Returns a new builder that appends the specified sequence of items to the end.
    /// </summary>
    public ImmutableConcatBuilder AppendItems(IEnumerable<PineValue> values)
    {
        var newItems = values as IReadOnlyList<PineValue> ?? [.. values];

        // If there are no new items, return this builder unchanged
        if (newItems.Count is 0)
        {
            return this;
        }

        // Try to consolidate with the rightmost leaf if it's small enough
        if (this is Node node && node.Items.Count > 0)
        {
            var lastChild = node.Items[node.Items.Count - 1];

            if (lastChild is Leaf lastLeaf &&
                lastLeaf.Values.Count + newItems.Count <= MaxLeafSizeForConsolidation)
            {
                // Consolidate: merge the new items into the last leaf
                var consolidatedValues = new PineValue[lastLeaf.Values.Count + newItems.Count];

                for (var i = 0; i < lastLeaf.Values.Count; i++)
                {
                    consolidatedValues[i] = lastLeaf.Values[i];
                }

                for (var i = 0; i < newItems.Count; i++)
                {
                    consolidatedValues[lastLeaf.Values.Count + i] = newItems[i];
                }

                var consolidatedLeaf = new Leaf(consolidatedValues);

                // Create a new node with the consolidated leaf replacing the last child
                var newNodeItems = new ImmutableConcatBuilder[node.Items.Count];

                for (var i = 0; i < node.Items.Count - 1; i++)
                {
                    newNodeItems[i] = node.Items[i];
                }

                newNodeItems[node.Items.Count - 1] = consolidatedLeaf;

                return new Node(newNodeItems);
            }
        }
        else if (this is Leaf thisLeaf &&
                 thisLeaf.Values.Count + newItems.Count <= MaxLeafSizeForConsolidation)
        {
            // Consolidate: merge the new items into the current leaf
            var consolidatedValues = new PineValue[thisLeaf.Values.Count + newItems.Count];

            for (var i = 0; i < thisLeaf.Values.Count; i++)
            {
                consolidatedValues[i] = thisLeaf.Values[i];
            }

            for (var i = 0; i < newItems.Count; i++)
            {
                consolidatedValues[thisLeaf.Values.Count + i] = newItems[i];
            }

            return new Leaf(consolidatedValues);
        }

        // No consolidation possible, create a new node
        return new Node([this, new Leaf(newItems)]);
    }

    /// <summary>
    /// Returns a new builder that appends a single item to the end.
    /// </summary>
    public ImmutableConcatBuilder AppendItem(PineValue value)
    {
        return AppendItems([value]);
    }

    /// <summary>
    /// Returns a new builder that prepends the specified items at the beginning.
    /// </summary>
    public ImmutableConcatBuilder PrependItems(IEnumerable<PineValue> values)
    {
        var newItems = values as IReadOnlyList<PineValue> ?? [.. values];

        // If there are no new items, return this builder unchanged
        if (newItems.Count is 0)
        {
            return this;
        }

        // Try to consolidate with the leftmost leaf if it's small enough
        if (this is Node node && node.Items.Count > 0)
        {
            var firstChild = node.Items[0];

            if (firstChild is Leaf firstLeaf &&
                firstLeaf.Values.Count + newItems.Count <= MaxLeafSizeForConsolidation)
            {
                // Consolidate: merge the new items into the first leaf
                var consolidatedValues = new PineValue[newItems.Count + firstLeaf.Values.Count];

                for (var i = 0; i < newItems.Count; i++)
                {
                    consolidatedValues[i] = newItems[i];
                }

                for (var i = 0; i < firstLeaf.Values.Count; i++)
                {
                    consolidatedValues[newItems.Count + i] = firstLeaf.Values[i];
                }

                var consolidatedLeaf = new Leaf(consolidatedValues);

                // Create a new node with the consolidated leaf replacing the first child
                var newNodeItems = new ImmutableConcatBuilder[node.Items.Count];
                newNodeItems[0] = consolidatedLeaf;

                for (var i = 1; i < node.Items.Count; i++)
                {
                    newNodeItems[i] = node.Items[i];
                }

                return new Node(newNodeItems);
            }
        }
        else if (this is Leaf thisLeaf &&
                 thisLeaf.Values.Count + newItems.Count <= MaxLeafSizeForConsolidation)
        {
            // Consolidate: merge the new items into the current leaf
            var consolidatedValues = new PineValue[newItems.Count + thisLeaf.Values.Count];

            for (var i = 0; i < newItems.Count; i++)
            {
                consolidatedValues[i] = newItems[i];
            }

            for (var i = 0; i < thisLeaf.Values.Count; i++)
            {
                consolidatedValues[newItems.Count + i] = thisLeaf.Values[i];
            }

            return new Leaf(consolidatedValues);
        }

        // No consolidation possible, create a new node
        return new Node([new Leaf(newItems), this]);
    }

    /// <summary>
    /// Returns a new builder that prepends a single item at the beginning.
    /// </summary>
    public ImmutableConcatBuilder PrependItem(PineValue value)
    {
        return PrependItems([value]);
    }

    /// <summary>
    /// Creates a new builder from the provided items.
    /// </summary>
    public static ImmutableConcatBuilder Create(
        IEnumerable<PineValue> values)
    {
        return new Leaf([.. values]);
    }

    /// <summary>
    /// Leaf node holding a concrete list of values.
    /// </summary>
    public sealed record Leaf(IReadOnlyList<PineValue> Values) : ImmutableConcatBuilder
    {
        /// <inheritdoc/>
        public override int AggregateItemsCount => Values.Count;

        /// <inheritdoc/>
        public override PineValue Evaluate()
        {
            return Internal.KernelFunctionSpecialized.concat(Values.ToArray());
        }

        /// <inheritdoc/>
        public override PineValue EvaluateReverse()
        {
            var reversed = Values.ToArray();
            Array.Reverse(reversed);
            return Internal.KernelFunctionSpecialized.concat(reversed);
        }

        /// <inheritdoc/>
        public override bool IsList()
        {
            // concat() returns a list if the first element is a list, or if empty
            if (Values.Count is 0)
            {
                return true; // Empty concat returns EmptyList
            }

            return Values[0] is PineValue.ListValue;
        }

        /// <inheritdoc/>
        public override bool IsBlob()
        {
            // concat() returns a blob if the first element is a blob
            if (Values.Count is 0)
            {
                return false; // Empty concat returns EmptyList, not a blob
            }

            return Values[0] is PineValue.BlobValue;
        }
    }

    /// <summary>
    /// Internal node combining multiple builders. Used to model cheap append/prepend without mutation.
    /// </summary>
    public sealed record Node
        : ImmutableConcatBuilder
    {
        /// <inheritdoc/>
        public override int AggregateItemsCount { get; }

        /// <summary>
        /// The child builders forming this node.
        /// </summary>
        public IReadOnlyList<ImmutableConcatBuilder> Items { get; }

        /// <summary>
        /// Initializes a new <see cref="Node"/> with the provided <paramref name="items"/>.
        /// </summary>
        public Node(IReadOnlyList<ImmutableConcatBuilder> items)
        {
            Items = items;

            var count = 0;

            for (var i = 0; i < items.Count; i++)
            {
                count += items[i].AggregateItemsCount;
            }

            AggregateItemsCount = count;
        }

        /// <inheritdoc/>
        public override PineValue Evaluate()
        {
            var flattenedItems = new PineValue[AggregateItemsCount];

            var currentIndex = 0;

            // Use an explicit stack to avoid deep recursion when the tree degenerates into a chain.
            var stack = new Stack<(ImmutableConcatBuilder Builder, int NextChildIndex)>();

            stack.Push((this, 0));

            while (stack.Count > 0)
            {
                var (builder, nextChildIndex) = stack.Pop();

                switch (builder)
                {
                    case Leaf leaf:
                        {
                            foreach (var value in leaf.Values)
                            {
                                flattenedItems[currentIndex++] = value;
                            }

                            break;
                        }

                    case Node node:
                        {
                            if (nextChildIndex < node.Items.Count)
                            {
                                stack.Push((node, nextChildIndex + 1));
                                stack.Push((node.Items[nextChildIndex], 0));
                            }

                            break;
                        }

                    default:
                        throw new System.NotImplementedException(
                            "Unrecognized ImmutableConcatBuilder variant: " + builder.GetType().FullName);
                }
            }

            return Internal.KernelFunctionSpecialized.concat(flattenedItems);
        }

        /// <inheritdoc/>
        public override PineValue EvaluateReverse()
        {
            var flattenedItems = new PineValue[AggregateItemsCount];

            var currentIndex = 0;

            // Use an explicit stack to avoid deep recursion when the tree degenerates into a chain.
            var stack = new Stack<(ImmutableConcatBuilder Builder, int NextChildIndex)>();

            stack.Push((this, Items.Count - 1));

            while (stack.Count > 0)
            {
                var (builder, nextChildIndex) = stack.Pop();

                switch (builder)
                {
                    case Leaf leaf:
                        {
                            for (var i = leaf.Values.Count - 1; i >= 0; i--)
                            {
                                flattenedItems[currentIndex++] = leaf.Values[i];
                            }

                            break;
                        }

                    case Node node:
                        {
                            if (nextChildIndex >= 0)
                            {
                                stack.Push((node, nextChildIndex - 1));

                                var childBuilder = node.Items[nextChildIndex];
                                var childStartIndex = childBuilder switch
                                {
                                    Node childNode => childNode.Items.Count - 1,
                                    _ => 0
                                };
                                stack.Push((childBuilder, childStartIndex));
                            }

                            break;
                        }

                    default:
                        throw new System.NotImplementedException(
                            "Unrecognized ImmutableConcatBuilder variant: " + builder.GetType().FullName);
                }
            }

            return Internal.KernelFunctionSpecialized.concat(flattenedItems);
        }

        /// <inheritdoc/>
        public override bool IsList()
        {
            // The result type of concat depends on the first value in the flattened sequence
            // If no items, concat returns EmptyList
            if (Items.Count is 0)
            {
                return true;
            }

            // Find the first builder's result type
            return Items[0].IsList();
        }

        /// <inheritdoc/>
        public override bool IsBlob()
        {
            // The result type of concat depends on the first value in the flattened sequence
            // If no items, concat returns EmptyList (not a blob)
            if (Items.Count is 0)
            {
                return false;
            }

            // Find the first builder's result type
            return Items[0].IsBlob();
        }
    }

    /// <inheritdoc/>
    public virtual bool Equals(ImmutableConcatBuilder? other)
    {
        throw new System.InvalidOperationException(
            "Equals should not be called directly, instead explicitly evaluate first.");
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        throw new System.InvalidOperationException(
            "GetHashCode should not be called directly, instead explicitly evaluate first.");
    }
}
