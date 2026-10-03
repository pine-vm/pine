using Pine.Core.Files;
using System;
using System.Collections.Generic;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// The flat 2026 encoding of file trees as Pine values.
/// Files are blobs and directories are lists of alternating names and children:
/// <c>[name0, child0, name1, child1, ...]</c>.
/// Names are UTF-32 big-endian blobs, as emitted by <see cref="StringEncoding.ValueFromString"/>.
/// Child directories use the same flat encoding recursively.
/// </summary>
public static class FileTreeEncoding2026
{
    /// <summary>
    /// Parses a Pine value as a file tree in the flat 2026 format.
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
                    "Parse does not handle composition type: " + composition.GetType().FullName)
            };
    }

    private static Result<IReadOnlyList<(int index, string name)>, FileTree> Parse(
        PineValue.ListValue compositionAsList)
    {
        if (compositionAsList.Items.Length % 2 is not 0)
        {
            return Result<IReadOnlyList<(int index, string name)>, FileTree>.err([]);
        }

        var parsedItems = new (string name, FileTree component)[compositionAsList.Items.Length / 2];

        for (var itemIndex = 0; itemIndex < parsedItems.Length; itemIndex++)
        {
            var nameValue = compositionAsList.Items.Span[itemIndex * 2];

            if (nameValue is not PineValue.BlobValue nameBlob ||
                StringEncoding.StringFromBlobValue(nameBlob.Bytes).IsOkOrNull() is not { } itemName)
            {
                return Result<IReadOnlyList<(int index, string name)>, FileTree>.err([]);
            }

            var itemComponent = compositionAsList.Items.Span[itemIndex * 2 + 1];

            if (Parse(itemComponent).IsOkOrNull() is not { } itemComponentOk)
            {
                return Result<IReadOnlyList<(int index, string name)>, FileTree>.err([]);
            }

            parsedItems[itemIndex] = (itemName, itemComponentOk);
        }

        return FileTree.SortedDirectory(parsedItems);
    }

    /// <summary>
    /// Encodes a file tree in the flat 2026 format, preserving the order of directory entries.
    /// </summary>
    public static PineValue Encode(FileTree node)
    {
        switch (node)
        {
            case FileTree.FileNode file:
                return PineValue.Blob(file.Bytes);

            case FileTree.DirectoryNode directory:
                {
                    var encodedItems = new PineValue[directory.Items.Count * 2];

                    for (var itemIndex = 0; itemIndex < directory.Items.Count; itemIndex++)
                    {
                        var (name, component) = directory.Items[itemIndex];

                        encodedItems[itemIndex * 2] = StringEncoding.ValueFromString(name);
                        encodedItems[itemIndex * 2 + 1] = Encode(component);
                    }

                    return PineValue.List(encodedItems);
                }

            default:
                throw new NotImplementedException(
                    "Encode does not handle file tree variant: " + node.GetType().FullName);
        }
    }
}
