using System.IO;

namespace Pine;

public static class LoadFromLocalFilesystem
{
    public static TreeNodeWithStringPath? LoadSortedTreeFromPath(string path)
    {
        if (File.Exists(path))
            return TreeNodeWithStringPath.Blob(blobContent: File.ReadAllBytes(path));

        if (!Directory.Exists(path))
            return null;

        var blobs = Filesystem.GetAllFilesFromDirectory(path);

        return PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(blobs);
    }
}
