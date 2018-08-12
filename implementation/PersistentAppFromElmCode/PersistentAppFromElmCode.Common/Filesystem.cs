using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace PersistentAppFromElmCode
{
    public class Filesystem
    {
        static public IReadOnlyCollection<(string name, byte[] content)> GetAllFilesFromDirectory(string directoryPath) =>
                Directory.GetFiles(directoryPath, "*", SearchOption.AllDirectories)
                .Select(filePath => (GetRelativePath(directoryPath, filePath), File.ReadAllBytes(filePath)))
                .ToList();

        static public string GetRelativePath(
            string relativeTo, string path, StringComparison comparisonType = StringComparison.InvariantCultureIgnoreCase)
        {
            if (path.StartsWith(relativeTo, comparisonType))
                return path.Substring(relativeTo.Length + 1);

            return path;
        }

        static public string MakeRandomDirectoryInTempDirectory()
        {
            var directory = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
            Directory.CreateDirectory(directory);
            return directory;
        }
    }
}