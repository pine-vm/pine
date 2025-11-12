using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;

namespace Pine.Core.IO;

/// <summary>
/// Filesystem helpers for reading and writing files, path manipulation and directory operations.
/// Provides utilities to enumerate files with filtering, compute relative paths, and safely delete directories.
/// </summary>
public static class Filesystem
{
    /// <summary>
    /// Gets the platform-specific cache directory path for this application.
    /// Uses <c>LOCALAPPDATA</c> on Windows and <c>HOME</c> on Unix-like systems and appends <c>pine/.cache</c>.
    /// </summary>
    public static string CacheDirectory =>
        //  https://stackoverflow.com/questions/39224518/path-to-localappdata-in-asp-net-core-application#comment83608153_39225227
        Path.Combine(
            Environment.GetEnvironmentVariable(RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "LOCALAPPDATA" : "HOME")!,
            "pine", ".cache");

    /// <summary>
    /// Recursively reads all files from <paramref name="directoryPath"/> returning relative paths and contents.
    /// </summary>
    /// <param name="directoryPath">Root directory to enumerate.</param>
    /// <param name="ignoreFileOnIOException">
    /// Optional callback invoked when reading a file throws <see cref="IOException"/>.
    /// Return <c>true</c> to skip the file, <c>false</c> to rethrow the exception.
    /// </param>
    /// <returns>Collection of tuples containing relative path segments and file contents.</returns>
    public static IReadOnlyCollection<(IReadOnlyList<string> path, ReadOnlyMemory<byte> content)> GetAllFilesFromDirectory(
        string directoryPath,
        Func<IReadOnlyList<string>, IOException, bool>? ignoreFileOnIOException = null) =>
        GetFilesFromDirectory(
            directoryPath: directoryPath,
            filterByRelativeName: _ => true,
            ignoreFileOnIOException: ignoreFileOnIOException);

    /// <summary>
    /// Recursively reads files from <paramref name="directoryPath"/> filtered by <paramref name="filterByRelativeName"/>.
    /// For each file, returns its relative path segments and contents. Files that cause an <see cref="IOException"/>
    /// may be skipped via <paramref name="ignoreFileOnIOException"/>.
    /// </summary>
    /// <param name="directoryPath">Root directory to enumerate.</param>
    /// <param name="filterByRelativeName">Predicate that receives relative path segments; return <c>true</c> to include.</param>
    /// <param name="ignoreFileOnIOException">
    /// Optional callback invoked when reading a file throws <see cref="IOException"/>.
    /// Return <c>true</c> to skip the file, <c>false</c> to rethrow the exception.
    /// </param>
    /// <returns>Collection of tuples containing relative path segments and file contents.</returns>
    public static IReadOnlyCollection<(IReadOnlyList<string> path, ReadOnlyMemory<byte> content)> GetFilesFromDirectory(
        string directoryPath,
        Func<IReadOnlyList<string>, bool> filterByRelativeName,
        Func<IReadOnlyList<string>, IOException, bool>? ignoreFileOnIOException = null) =>
        [.. Directory.GetFiles(directoryPath, "*", SearchOption.AllDirectories)
        .Select(filePath =>
            (absolutePath: filePath,
            relativePath: (IReadOnlyList<string>)GetRelativePath(directoryPath, filePath).Split(Path.DirectorySeparatorChar)))
        .Where(filePath => filterByRelativeName(filePath.relativePath))
        .SelectMany(filePath =>
        {
            try
            {
                return
                (IReadOnlyCollection<(IReadOnlyList<string> path, ReadOnlyMemory<byte> content)>)
                [(filePath.relativePath, (ReadOnlyMemory<byte>)File.ReadAllBytes(filePath.absolutePath))];
            }
            catch (IOException exception) when (ignoreFileOnIOException?.Invoke(filePath.relativePath, exception) ?? false)
            {
                return [];
            }
        })];

    /// <summary>
    /// Computes the relative path of <paramref name="path"/> with respect to <paramref name="relativeTo"/>.
    /// If <paramref name="path"/> does not start with <paramref name="relativeTo"/>, returns <paramref name="path"/> unchanged.
    /// </summary>
    /// <param name="relativeTo">Base path to remove from the start of <paramref name="path"/>.</param>
    /// <param name="path">Absolute or base-relative path to convert.</param>
    /// <param name="comparisonType">String comparison for prefix check; defaults to case-insensitive invariant.</param>
    /// <returns>The path relative to <paramref name="relativeTo"/>, or the original path if not a prefix match.</returns>
    public static string GetRelativePath(
        string relativeTo, string path, StringComparison comparisonType = StringComparison.InvariantCultureIgnoreCase)
    {
        if (!path.StartsWith(relativeTo, comparisonType) || !(0 < relativeTo?.Length))
            return path;

        return path[relativeTo.Length..].TrimStart(Path.DirectorySeparatorChar);
    }

    /// <summary>
    /// Creates a new randomly named directory under the system temporary directory and returns its path.
    /// </summary>
    public static string CreateRandomDirectoryInTempDirectory()
    {
        var directory = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
        Directory.CreateDirectory(directory);
        return directory;
    }

    /// <summary>
    /// Joins path segments using the platform-specific directory separator character.
    /// </summary>
    /// <param name="path">Ordered segments that compose a path.</param>
    /// <returns>The combined path string.</returns>
    public static string MakePlatformSpecificPath(IReadOnlyList<string> path) =>
        string.Join(Path.DirectorySeparatorChar.ToString(), path);

    /// <summary>
    /// Recursively deletes a local directory, clearing file attributes to allow deletion on Windows.
    /// See: https://github.com/libgit2/libgit2sharp/issues/769#issuecomment-198833179
    /// </summary>
    /// <param name="directoryPath">Path to the directory to delete.</param>
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
