using System;
using System.Collections.Generic;

namespace Pine.Core.PopularEncodings;

/// <summary>
/// Encoding file trees as Pine values and parsing Pine values as file trees.
/// </summary>
public static class FileTreeEncoding
{
    /// <summary>
    /// Parse a Pine value as a file tree.
    /// <para>
    /// The error case contains a list of path items that can be used to construct an
    /// error message pointing to the problematic part of the tree.
    /// </para>
    /// </summary>
    public static Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath> Parse(
        PineValue composition)
    {
        return
            composition switch
            {
                PineValue.BlobValue compositionAsBlob =>
                    Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath>.ok(
                        TreeNodeWithStringPath.Blob(compositionAsBlob.Bytes)),

                PineValue.ListValue compositionAsList =>
                    Parse(compositionAsList),

                _ =>
                throw new NotImplementedException(
                    "Unexpected composition type: " + composition.GetType().FullName)
            };
    }

    private static Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath> Parse(
        PineValue.ListValue compositionAsList)
    {
        var parsedItems = new (string name, TreeNodeWithStringPath component)[compositionAsList.Items.Length];

        for (var itemIndex = 0; itemIndex < compositionAsList.Items.Length; itemIndex++)
        {
            var item = compositionAsList.Items.Span[itemIndex];

            if (item is not PineValue.ListValue itemAsList || itemAsList.Items.Length is not 2)
            {
                return Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath>.err([]);
            }

            if (StringEncoding.StringFromValue(itemAsList.Items.Span[0]).IsOkOrNull() is not { } itemName)
            {
                return Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath>.err([]);
            }

            var itemComponent = itemAsList.Items.Span[1];

            if (Parse(itemComponent).IsOkOrNull() is not { } itemComponentOk)
            {
                return Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath>.err([]);
            }

            parsedItems[itemIndex] = (itemName, itemComponentOk);
        }

        return TreeNodeWithStringPath.SortedTree(parsedItems);
    }

    /// <summary>
    /// Encode a file tree as a Pine value.
    /// </summary>
    public static PineValue Encode(TreeNodeWithStringPath node)
    {
        if (node is TreeNodeWithStringPath.BlobNode blob)
        {
            return PineValue.Blob(blob.Bytes);
        }

        if (node is TreeNodeWithStringPath.TreeNode tree)
        {
            var encodedItems = new PineValue[tree.Elements.Count];

            for (var itemIndex = 0; itemIndex < tree.Elements.Count; itemIndex++)
            {
                var (name, component) = tree.Elements[itemIndex];

                encodedItems[itemIndex] =
                    PineValue.List(
                        [
                            StringEncoding.ValueFromString(name),
                            Encode(component)
                        ]);
            }

            return PineValue.List(encodedItems);
        }

        throw new NotImplementedException(
            "Unexpected node type: " + node.GetType());
    }
}
