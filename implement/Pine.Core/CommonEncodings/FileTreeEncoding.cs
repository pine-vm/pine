using Pine.Core.Files;
using System;
using System.Collections.Generic;

namespace Pine.Core.CommonEncodings;

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
    public static Result<IReadOnlyList<(int index, string name)>, FileTree> Parse(
        PineValue composition)
    {
        return
            composition switch
            {
                PineValue.BlobValue compositionAsBlob =>
                    Result<IReadOnlyList<(int index, string name)>, FileTree>.ok(
                        FileTree.File(compositionAsBlob.Bytes)),

                PineValue.ListValue compositionAsList =>
                    Parse(compositionAsList),

                _ =>
                throw new NotImplementedException(
                    "Unexpected composition type: " + composition.GetType().FullName)
            };
    }

    private static Result<IReadOnlyList<(int index, string name)>, FileTree> Parse(
        PineValue.ListValue compositionAsList)
    {
        var parsedItems = new (string name, FileTree component)[compositionAsList.Items.Length];

        for (var itemIndex = 0; itemIndex < compositionAsList.Items.Length; itemIndex++)
        {
            var item = compositionAsList.Items.Span[itemIndex];

            if (item is not PineValue.ListValue itemAsList || itemAsList.Items.Length is not 2)
            {
                return Result<IReadOnlyList<(int index, string name)>, FileTree>.err([]);
            }

            if (StringEncoding.StringFromValue(itemAsList.Items.Span[0]).IsOkOrNull() is not { } itemName)
            {
                return Result<IReadOnlyList<(int index, string name)>, FileTree>.err([]);
            }

            var itemComponent = itemAsList.Items.Span[1];

            if (Parse(itemComponent).IsOkOrNull() is not { } itemComponentOk)
            {
                return Result<IReadOnlyList<(int index, string name)>, FileTree>.err([]);
            }

            parsedItems[itemIndex] = (itemName, itemComponentOk);
        }

        return FileTree.SortedDirectory(parsedItems);
    }

    /// <summary>
    /// Encode a file tree as a Pine value.
    /// </summary>
    public static PineValue Encode(FileTree node)
    {
        if (node is FileTree.FileNode file)
        {
            return PineValue.Blob(file.Bytes);
        }

        if (node is FileTree.DirectoryNode directory)
        {
            var encodedItems = new PineValue[directory.Items.Count];

            for (var itemIndex = 0; itemIndex < directory.Items.Count; itemIndex++)
            {
                var (name, component) = directory.Items[itemIndex];

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
