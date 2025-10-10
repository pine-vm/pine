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
    /// Gets the total number of items contained across all leaves of this builder.
    /// </summary>
    public abstract int AggregateItemsCount { get; }

    /// <summary>
    /// Evaluates this builder into a single <see cref="PineValue"/> by applying the recorded concatenations.
    /// </summary>
    public abstract PineValue Evaluate();

    /// <summary>
    /// Returns a new builder that appends the specified sequence of items to the end.
    /// </summary>
    public ImmutableConcatBuilder AppendItems(IEnumerable<PineValue> values)
    {
        return new Node([this, new Leaf([.. values])]);
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
        return new Node([new Leaf([.. values]), this]);
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
