using System;
using System.Collections.Generic;
using System.Collections.Immutable;
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

        static public IReadOnlyCollection<(string path, IImmutableList<byte> content)> GetAllFilesFromDirectory(string directoryPath) =>
            GetFilesFromDirectory(
                directoryPath: directoryPath,
                filterByRelativeName: _ => true);

        static public IReadOnlyCollection<(string path, IImmutableList<byte> content)> GetFilesFromDirectory(
            string directoryPath,
            Func<string, bool> filterByRelativeName) =>
            Directory.GetFiles(directoryPath, "*", SearchOption.AllDirectories)
            .Select(filePath => (absolutePath: filePath, relativePath: GetRelativePath(directoryPath, filePath)))
            .Where(filePath => filterByRelativeName(filePath.relativePath))
            .Select(filePath => (filePath.relativePath, (IImmutableList<byte>)File.ReadAllBytes(filePath.absolutePath).ToImmutableList()))
            .ToList();

        static public string GetRelativePath(
            string relativeTo, string path, StringComparison comparisonType = StringComparison.InvariantCultureIgnoreCase)
        {
            if (!path.StartsWith(relativeTo, comparisonType) || !(0 < relativeTo?.Length))
                return path;

            return path.Substring(relativeTo.Length).TrimStart(Path.DirectorySeparatorChar);
        }

        static public string CreateRandomDirectoryInTempDirectory()
        {
            var directory = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
            Directory.CreateDirectory(directory);
            return directory;
        }
    }
}