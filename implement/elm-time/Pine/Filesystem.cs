using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;

namespace Pine;

public static class Filesystem
{
    //  https://stackoverflow.com/questions/39224518/path-to-localappdata-in-asp-net-core-application#comment83608153_39225227
    public static string CacheDirectory =>
        Path.Combine(
            Environment.GetEnvironmentVariable(RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "LOCALAPPDATA" : "HOME")!,
            "pine", ".cache");

    public static IReadOnlyCollection<(IReadOnlyList<string> path, ReadOnlyMemory<byte> content)> GetAllFilesFromDirectory(string directoryPath) =>
        GetFilesFromDirectory(
            directoryPath: directoryPath,
            filterByRelativeName: _ => true);

    public static IReadOnlyCollection<(IReadOnlyList<string> path, ReadOnlyMemory<byte> content)> GetFilesFromDirectory(
        string directoryPath,
        Func<IReadOnlyList<string>, bool> filterByRelativeName) =>
        Directory.GetFiles(directoryPath, "*", SearchOption.AllDirectories)
        .Select(filePath =>
            (absolutePath: filePath,
            relativePath: (IReadOnlyList<string>)GetRelativePath(directoryPath, filePath).Split(Path.DirectorySeparatorChar)))
        .Where(filePath => filterByRelativeName(filePath.relativePath))
        .Select(filePath => (filePath.relativePath, (ReadOnlyMemory<byte>)File.ReadAllBytes(filePath.absolutePath)))
        .ToList();

    public static string GetRelativePath(
        string relativeTo, string path, StringComparison comparisonType = StringComparison.InvariantCultureIgnoreCase)
    {
        if (!path.StartsWith(relativeTo, comparisonType) || !(0 < relativeTo?.Length))
            return path;

        return path[relativeTo.Length..].TrimStart(Path.DirectorySeparatorChar);
    }

    public static string CreateRandomDirectoryInTempDirectory()
    {
        var directory = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
        Directory.CreateDirectory(directory);
        return directory;
    }

    public static string MakePlatformSpecificPath(IReadOnlyList<string> path) =>
        string.Join(Path.DirectorySeparatorChar.ToString(), path);

    /// <summary>
    /// https://github.com/libgit2/libgit2sharp/issues/769#issuecomment-198833179
    /// </summary>
    public static void DeleteLocalDirectoryRecursive(string directoryPath)
    {
        if (!Directory.Exists(directoryPath))
        {
            return;
        }

        var files = Directory.GetFiles(directoryPath);
        var directories = Directory.GetDirectories(directoryPath);

        foreach (var file in files)
        {
            File.SetAttributes(file, FileAttributes.Normal);
            File.Delete(file);
        }

        foreach (var dir in directories)
        {
            DeleteLocalDirectoryRecursive(dir);
        }

        File.SetAttributes(directoryPath, FileAttributes.Normal);

        Directory.Delete(directoryPath, false);
    }
}
