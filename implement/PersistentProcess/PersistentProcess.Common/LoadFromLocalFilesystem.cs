using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Kalmit
{
    static public class LoadFromLocalFilesystem
    {
        static public Composition.TreeWithStringPath LoadSortedTreeFromPath(string path)
        {
            if (File.Exists(path))
                return new Composition.TreeWithStringPath { BlobContent = File.ReadAllBytes(path).ToImmutableList() };

            if (!Directory.Exists(path))
                return null;

            var blobs =
                Filesystem.GetAllFilesFromDirectory(path)
                .Select(file => (path: (System.Collections.Immutable.IImmutableList<string>)file.path.Split('/', '\\').ToImmutableList(), content: file.content))
                .ToImmutableList();

            return
                Composition.SortedTreeFromSetOfBlobsWithStringPath(blobs);
        }
    }
}
