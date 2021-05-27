using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;

namespace Pine
{
    public class Filesystem
    {
        //  https://stackoverflow.com/questions/39224518/path-to-localappdata-in-asp-net-core-application#comment83608153_39225227
        static public string CacheDirectory =>
            Path.Combine(
                Environment.GetEnvironmentVariable(
                    RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "LOCALAPPDATA" : "HOME"),
                "pine", ".cache");

        static public IReadOnlyCollection<(IImmutableList<string> path, IReadOnlyList<byte> content)> GetAllFilesFromDirectory(string directoryPath) =>
            GetFilesFromDirectory(
                directoryPath: directoryPath,
                filterByRelativeName: _ => true);

        static public IReadOnlyCollection<(IImmutableList<string> path, IReadOnlyList<byte> content)> GetFilesFromDirectory(
            string directoryPath,
            Func<IImmutableList<string>, bool> filterByRelativeName) =>
            Directory.GetFiles(directoryPath, "*", SearchOption.AllDirectories)
            .Select(filePath =>
                (absolutePath: filePath,
                relativePath: (IImmutableList<string>)GetRelativePath(directoryPath, filePath).Split(Path.DirectorySeparatorChar).ToImmutableList()))
            .Where(filePath => filterByRelativeName(filePath.relativePath))
            .Select(filePath => (filePath.relativePath, (IReadOnlyList<byte>)File.ReadAllBytes(filePath.absolutePath)))
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

        static public string MakePlatformSpecificPath(IImmutableList<string> path) =>
            string.Join(Path.DirectorySeparatorChar.ToString(), path);
    }
}