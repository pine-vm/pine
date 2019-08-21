using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;

namespace Kalmit
{
    public class Filesystem
    {
        //  https://stackoverflow.com/questions/39224518/path-to-localappdata-in-asp-net-core-application#comment83608153_39225227
        static public string CacheDirectory =>
            Path.Combine(
                Environment.GetEnvironmentVariable(
                    RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "LOCALAPPDATA" : "HOME"),
                "kalmit", ".cache");

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

        static public string CreateRandomDirectoryInTempDirectory()
        {
            var directory = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
            Directory.CreateDirectory(directory);
            return directory;
        }
    }
}