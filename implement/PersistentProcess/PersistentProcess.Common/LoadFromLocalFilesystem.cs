using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;

namespace Kalmit
{
    static public class LoadFromLocalFilesystem
    {
        static public Composition.TreeComponent LoadTreeFromPath(string path)
        {
            if (File.Exists(path))
                return new Composition.TreeComponent { BlobContent = File.ReadAllBytes(path).ToImmutableList() };

            if (!Directory.Exists(path))
                return null;

            var treeEntries =
                Directory.EnumerateFileSystemEntries(path)
                .Select(fileSystemEntry =>
                {
                    var name = (IImmutableList<byte>)Encoding.UTF8.GetBytes(Path.GetRelativePath(path, fileSystemEntry)).ToImmutableList();

                    return (name, LoadTreeFromPath(fileSystemEntry));
                })
                .ToImmutableList();

            return new Composition.TreeComponent
            {
                TreeContent = treeEntries,
            };
        }
    }
}
