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
    public PineValue Evaluate()
    {
        var flattenedItems = Flatten(this);

        return Internal.KernelFunctionSpecialized.concat(flattenedItems.Span);
    }


    /// <summary>
    /// Evaluates this builder by reversing the order of items, then applying concatenations.
    /// Equivalent to Evaluate() followed by reverse(), but more efficient.
    /// </summary>
    public PineValue EvaluateReverse()
    {
        var flattenedItems = Flatten(this);

        return Internal.KernelFunctionFused.ConcatAndReverse(flattenedItems.Span);
    }

    /// <summary>
    /// Predict whether the result from <see cref="Evaluate"/> would be a <see cref="PineValue.ListValue"/> value without fully evaluating.
    /// </summary>
    public bool IsList()
    {
        if (ContainsNonEmptyListInternal())
        {
            return true;
        }

        return !ContainsBlobInternal();
    }

    /// <summary>
    /// Predict whether the result from <see cref="Evaluate"/> would be a <see cref="PineValue.BlobValue"/> value without fully evaluating.
    /// </summary>
    public bool IsBlob()
    {
        if (ContainsNonEmptyListInternal())
        {
            return false;
        }

        return ContainsBlobInternal();
    }

    /// <summary>
    /// Determines whether any leaf in this builder contains a blob value.
    /// </summary>
    protected abstract bool ContainsBlobInternal();

    /// <summary>
    /// Determines whether the current instance contains at least one non-empty list.
    /// </summary>
    /// <returns>true if a non-empty list is present; otherwise, false.</returns>
    protected abstract bool ContainsNonEmptyListInternal();

    /// <summary>
    /// Returns a new builder that appends the specified sequence of items to the end.
    /// </summary>
    public ImmutableConcatBuilder AppendItems(IEnumerable<PineValue> values)
    {
        var newItems =
            values is PineValue[] arrayValues
                ? arrayValues
                : [.. values];

        // If there are no new items, return this builder unchanged
        if (newItems.Length is 0)
        {
            return this;
        }

        // Try to consolidate with the rightmost leaf if it's small enough
        if (this is Node node && node.Items.Count > 0)
        {
            var lastChild = node.Items[node.Items.Count - 1];

            if (lastChild is Leaf lastLeaf &&
                lastLeaf.Values.Length + newItems.Length <= MaxLeafSizeForConsolidation)
            {
                // Consolidate: merge the new items into the last leaf
                var consolidatedValues = new PineValue[lastLeaf.Values.Length + newItems.Length];

                for (var i = 0; i < lastLeaf.Values.Length; i++)
                {
                    consolidatedValues[i] = lastLeaf.Values.Span[i];
                }

                for (var i = 0; i < newItems.Length; i++)
                {
                    consolidatedValues[lastLeaf.Values.Length + i] = newItems[i];
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
                 thisLeaf.Values.Length + newItems.Length <= MaxLeafSizeForConsolidation)
        {
            // Consolidate: merge the new items into the current leaf
            var consolidatedValues = new PineValue[thisLeaf.Values.Length + newItems.Length];

            for (var i = 0; i < thisLeaf.Values.Length; i++)
            {
                consolidatedValues[i] = thisLeaf.Values.Span[i];
            }

            for (var i = 0; i < newItems.Length; i++)
            {
                consolidatedValues[thisLeaf.Values.Length + i] = newItems[i];
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
        var newItems = (values as PineValue[]) ?? [.. values];

        // If there are no new items, return this builder unchanged
        if (newItems.Length is 0)
        {
            return this;
        }

        // Try to consolidate with the leftmost leaf if it's small enough
        if (this is Node node && node.Items.Count > 0)
        {
            var firstChild = node.Items[0];

            if (firstChild is Leaf firstLeaf &&
                firstLeaf.Values.Length + newItems.Length <= MaxLeafSizeForConsolidation)
            {
                // Consolidate: merge the new items into the first leaf
                var consolidatedValues = new PineValue[newItems.Length + firstLeaf.Values.Length];

                for (var i = 0; i < newItems.Length; i++)
                {
                    consolidatedValues[i] = newItems[i];
                }

                for (var i = 0; i < firstLeaf.Values.Length; i++)
                {
                    consolidatedValues[newItems.Length + i] = firstLeaf.Values.Span[i];
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
                 thisLeaf.Values.Length + newItems.Length <= MaxLeafSizeForConsolidation)
        {
            // Consolidate: merge the new items into the current leaf
            var consolidatedValues = new PineValue[newItems.Length + thisLeaf.Values.Length];

            for (var i = 0; i < newItems.Length; i++)
            {
                consolidatedValues[i] = newItems[i];
            }

            for (var i = 0; i < thisLeaf.Values.Length; i++)
            {
                consolidatedValues[newItems.Length + i] = thisLeaf.Values.Span[i];
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
        return new Leaf(values.ToArray());
    }

    /// <summary>
    /// Leaf node holding a concrete list of values.
    /// </summary>
    public sealed record Leaf(ReadOnlyMemory<PineValue> Values) : ImmutableConcatBuilder
    {
        /// <inheritdoc/>
        public override int AggregateItemsCount => Values.Length;

        /// <inheritdoc/>
        protected override bool ContainsBlobInternal()
        {
            for (var i = 0; i < Values.Length; i++)
            {
                if (Values.Span[i] is PineValue.BlobValue)
                {
                    return true;
                }
            }

            return false;
        }

        /// <inheritdoc/>
        protected override bool ContainsNonEmptyListInternal()
        {
            for (var i = 0; i < Values.Length; i++)
            {
                if (Values.Span[i] is PineValue.ListValue listValue && listValue.Items.Length > 0)
                {
                    return true;
                }
            }

            return false;
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
        protected override bool ContainsBlobInternal()
        {
            for (var i = 0; i < Items.Count; i++)
            {
                if (Items[i].ContainsBlobInternal())
                {
                    return true;
                }
            }

            return false;
        }

        /// <inheritdoc/>
        protected override bool ContainsNonEmptyListInternal()
        {
            for (var i = 0; i < Items.Count; i++)
            {
                if (Items[i].ContainsNonEmptyListInternal())
                {
                    return true;
                }
            }

            return false;
        }
    }

    /// <summary>
    /// Flattens the immutable concat tree rooted at <paramref name="builder"/> into a contiguous buffer
    /// of <see cref="PineValue"/> items in left-to-right order.
    /// </summary>
    /// <param name="builder">The root <see cref="ImmutableConcatBuilder"/> to flatten.</param>
    /// <returns>
    /// A <see cref="ReadOnlyMemory{T}"/> segment containing all items from the tree in evaluation order.
    /// The returned memory is backed by a newly allocated array whose length equals <see cref="AggregateItemsCount"/>.
    /// </returns>
    protected static ReadOnlyMemory<PineValue> Flatten(ImmutableConcatBuilder builder)
    {
        var result = new PineValue[builder.AggregateItemsCount];

        var currentIndex = 0;

        // Use an explicit stack to avoid deep recursion when the tree degenerates into a chain.
        var stack = new Stack<(ImmutableConcatBuilder Builder, int NextChildIndex)>();

        stack.Push((builder, 0));

        while (stack.Count > 0)
        {
            var (currentBuilder, nextChildIndex) = stack.Pop();

            switch (currentBuilder)
            {
                case Leaf leaf:
                    {
                        var span = leaf.Values.Span;

                        for (var i = 0; i < span.Length; i++)
                        {
                            result[currentIndex++] = span[i];
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
                    throw new NotImplementedException(
                        "Unrecognized ImmutableConcatBuilder variant: " + currentBuilder.GetType().FullName);
            }
        }

        return result;
    }

    /// <inheritdoc/>
    public virtual bool Equals(ImmutableConcatBuilder? other)
    {
        throw new InvalidOperationException(
            "Equals should not be called directly, instead explicitly evaluate first.");
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        throw new InvalidOperationException(
            "GetHashCode should not be called directly, instead explicitly evaluate first.");
    }
}
