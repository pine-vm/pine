using System.Collections.Immutable;
using System.IO;

namespace Pine
{
    static public class LoadFromLocalFilesystem
    {
        static public Composition.TreeWithStringPath LoadSortedTreeFromPath(string path)
        {
            if (File.Exists(path))
                return Composition.TreeWithStringPath.Blob(blobContent: File.ReadAllBytes(path));

            if (!Directory.Exists(path))
                return null;

            var blobs = Filesystem.GetAllFilesFromDirectory(path);

            return Composition.SortedTreeFromSetOfBlobsWithStringPath(blobs);
        }
    }
}
