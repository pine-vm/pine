using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Extension methods for <see cref="PineValueClass"/>.
/// </summary>
public static class PineValueClassExtensions
{
    private static readonly IReadOnlyList<int> s_emptyPath = [];

    /// <summary>
    /// Create a minimal <see cref="PineValue"/> that satisfies the given <see cref="PineValueClass"/>.
    /// <para>
    /// The created <see cref="PineValue"/> will have the least amount of data necessary to satisfy the
    /// constraints of the <see cref="PineValueClass"/>, using empty list values in unspecified paths for
    /// which existence is implied by siblings.
    /// </para>
    /// </summary>
    public static PineValue CreateMinimalValue(this PineValueClass pvClass)
    {
        // If there's a constraint at the root path, return it directly
        if (pvClass.ParsedItems.TryGetValue(s_emptyPath, out var rootValue))
        {
            return rootValue;
        }

        // If there are no constraints, return the smallest possible value (empty blob)
        if (pvClass.ParsedItems.Count is 0)
        {
            return PineValue.EmptyBlob;
        }

        // Build a tree structure from the constraints
        var tree = BuildTreeFromConstraints(pvClass.ParsedItems);

        return ConstructValueFromTree(tree);
    }

    /// <summary>
    /// Builds a tree structure representing the minimal value needed to satisfy constraints.
    /// </summary>
    private static TreeNode BuildTreeFromConstraints(IReadOnlyDictionary<IReadOnlyList<int>, PineValue> constraints)
    {
        var root = new TreeNode();

        foreach (var constraint in constraints)
        {
            var path = constraint.Key;
            var value = constraint.Value;

            if (path.Count is 0)
            {
                root.Value = value;
                continue;
            }

            var currentNode = root;

            // Navigate through the path, creating nodes as needed
            for (var i = 0; i < path.Count; i++)
            {
                var index = path[i];

                // Ensure children dictionary exists
                currentNode.Children ??= [];

                // Get or create child node at this index
                if (!currentNode.Children.TryGetValue(index, out var childNode))
                {
                    childNode = new TreeNode();
                    currentNode.Children[index] = childNode;
                }

                // If this is the last element in the path, set the value
                if (i == path.Count - 1)
                {
                    childNode.Value = value;
                }

                currentNode = childNode;
            }
        }

        return root;
    }

    /// <summary>
    /// Constructs a PineValue from the tree structure.
    /// </summary>
    private static PineValue ConstructValueFromTree(TreeNode node)
    {
        // If this node has a concrete value, return it
        if (node.Value is not null)
        {
            return node.Value;
        }

        // If this node has children, construct a list
        if (node.Children is not null && node.Children.Count > 0)
        {
            // Find the maximum index to determine list size
            var maxIndex = node.Children.Keys.Max();
            var listItems = new PineValue[maxIndex + 1];

            // Fill in constrained items
            for (var i = 0; i <= maxIndex; i++)
            {
                if (node.Children.TryGetValue(i, out var childNode))
                {
                    listItems[i] = ConstructValueFromTree(childNode);
                }
                else
                {
                    // Use empty list for unspecified items in a list context
                    listItems[i] = PineValue.EmptyList;
                }
            }

            return PineValue.List(listItems);
        }

        // Default: empty list for nodes with no specific value or children
        return PineValue.EmptyList;
    }

    /// <summary>
    /// Internal tree node structure for building the minimal value.
    /// </summary>
    private class TreeNode
    {
        public PineValue? Value { get; set; }
        public Dictionary<int, TreeNode>? Children { get; set; }
    }
}
