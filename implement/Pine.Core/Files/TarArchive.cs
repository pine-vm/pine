using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Pine.Core.Files;

/// <summary>
/// Helpers for working with TAR archives.
/// </summary>
public static class TarArchive
{
    /// <summary>
    /// Creates a tree representation of the contents of a TAR archive, using string-based paths for each entry.
    /// </summary>
    /// <remarks>The method reads the entire TAR archive from memory and constructs a tree where each file and
    /// directory is accessible by its string path. The input buffer is not modified. This method is suitable for
    /// scenarios where the archive is already loaded into memory.</remarks>
    /// <param name="tarArchive">A read-only memory buffer containing the bytes of the TAR archive to be parsed. Must contain a valid TAR archive
    /// format.</param>
    public static BlobTreeWithStringPath TreeWithStringPathFromTarArchive(ReadOnlyMemory<byte> tarArchive)
    {
        using var archiveReader =
            SharpCompress.Archives.Tar.TarArchive.Open(new MemoryStream(tarArchive.ToArray()));

        return TreeWithStringPathFromTarArchiveEntries(archiveReader.Entries);
    }

    /// <summary>
    /// Creates a tree structure of blobs from the file entries in a TAR archive, using string paths as keys.
    /// </summary>
    /// <remarks>The resulting tree preserves the original file paths from the TAR archive as string keys.
    /// Directory entries in the archive are excluded from the tree.</remarks>
    /// <param name="entries">A collection of TAR archive entries to include in the tree. Only file entries are processed; directory entries
    /// are ignored.</param>
    public static BlobTreeWithStringPath TreeWithStringPathFromTarArchiveEntries(
        IEnumerable<SharpCompress.Archives.Tar.TarArchiveEntry> entries)
    {
        var treeEntries =
            entries
            .Where(tarEntry => !tarEntry.IsDirectory)
            .Select(tarEntry =>
            {
                using var memoryStream = new MemoryStream();
                using var tarEntryStream = tarEntry.OpenEntryStream();

                tarEntryStream.CopyTo(memoryStream);

                var componentBytes = memoryStream.ToArray();

                return (name: tarEntry.Key, component: BlobTreeWithStringPath.Blob(componentBytes));
            }).ToImmutableList();

        return BlobTreeWithStringPath.SortedTree(treeEntries);
    }
}
