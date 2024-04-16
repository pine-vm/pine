using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Pine;

public static class TarArchive
{
    public static TreeNodeWithStringPath TreeWithStringPathFromTarArchive(ReadOnlyMemory<byte> tarArchive)
    {
        using var archiveReader = SharpCompress.Archives.Tar.TarArchive.Open(new MemoryStream(tarArchive.ToArray()));

        return TreeWithStringPathFromTarArchiveEntries(archiveReader.Entries);
    }

    public static TreeNodeWithStringPath TreeWithStringPathFromTarArchiveEntries(
        IEnumerable<SharpCompress.Archives.Tar.TarArchiveEntry> entries)
    {
        var treeEntries =
            entries.Where(tarEntry => !tarEntry.IsDirectory)
            .Select(tarEntry =>
            {
                using var memoryStream = new MemoryStream();
                using var tarEntryStream = tarEntry.OpenEntryStream();

                tarEntryStream.CopyTo(memoryStream);

                var componentBytes = memoryStream.ToArray();

                return (name: tarEntry.Key, component: TreeNodeWithStringPath.Blob(componentBytes));
            }).ToImmutableList();

        return TreeNodeWithStringPath.SortedTree(treeEntries);
    }
}
