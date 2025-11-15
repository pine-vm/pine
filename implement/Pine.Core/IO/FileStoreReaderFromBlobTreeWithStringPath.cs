using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.IO;


/// <summary>
/// <see cref="IFileStoreReader"/> backed by an immutable <see cref="FileTree"/>.
/// </summary>
/// <param name="root">Root tree node representing the file hierarchy.</param>
public class FileStoreReaderFromBlobTreeWithStringPath(
    FileTree root)
    : IFileStoreReader
{
    /// <inheritdoc />
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path)
    {
        var node = root.GetNodeAtPath(path);

        if (node is FileTree.FileNode blobNode)
        {
            return blobNode.Bytes;
        }

        return null;
    }

    /// <inheritdoc />
    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath)
    {
        var directoryNode = root.GetNodeAtPath(directoryPath);

        if (directoryNode is null)
        {
            return [];
        }

        if (directoryNode is FileTree.DirectoryNode directory)
        {
            return
                directory.EnumerateFilesTransitive()
                .Select(blob => blob.path);
        }

        return [[]];
    }
}
