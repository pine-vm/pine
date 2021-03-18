using System.Collections.Immutable;
using System.IO;

namespace Pine
{
    static public class LoadFromLocalFilesystem
    {
        static public Composition.TreeWithStringPath LoadSortedTreeFromPath(string path)
        {
            if (File.Exists(path))
                return new Composition.TreeWithStringPath { BlobContent = File.ReadAllBytes(path).ToImmutableList() };

            if (!Directory.Exists(path))
                return null;

            var blobs = Filesystem.GetAllFilesFromDirectory(path);

            return Composition.SortedTreeFromSetOfBlobsWithStringPath(blobs);
        }
    }
}
