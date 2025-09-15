using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.IO;

/// <summary>
/// Extension helpers for <see cref="IFileStoreReader"/> and <see cref="IFileStoreWriter"/>.
/// </summary>
/// <remarks>
/// Provides utilities to:
/// - List files from the root directory
/// - Map/transform logical paths before delegating to an underlying store
/// - Scope a store to a subdirectory (path prefix)
/// - Merge readers so that one store can fall back to another, with optional promotion on reads
/// </remarks>
public static class FileStoreExtension
{
    /// <summary>
    /// Lists files relative to the root directory.
    /// </summary>
    /// <param name="fileStore">The file store to list from.</param>
    /// <returns>
    /// A sequence of paths (each as <see cref="IImmutableList{T}"/> of <see cref="string"/>)
    /// relative to the root directory.
    /// </returns>
    public static IEnumerable<IImmutableList<string>> ListFiles(this IFileStoreReader fileStore) =>
        fileStore.ListFilesInDirectory([]);

    /// <summary>
    /// Returns a reader that maps paths via <paramref name="pathMap"/> before delegating
    /// to <paramref name="originalFileStore"/>.
    /// </summary>
    /// <param name="originalFileStore">The underlying reader to delegate to.</param>
    /// <param name="pathMap">Function that transforms the incoming logical path.</param>
    /// <returns>A reader that applies <paramref name="pathMap"/> on every operation.</returns>
    public static IFileStoreReader WithMappedPath(
        this IFileStoreReader originalFileStore, Func<IImmutableList<string>, IImmutableList<string>> pathMap) =>
        new DelegatingFileStoreReader
        (
            GetFileContentDelegate: originalPath => originalFileStore.GetFileContent(pathMap(originalPath)),
            ListFilesInDirectoryDelegate: originalPath => originalFileStore.ListFilesInDirectory(pathMap(originalPath))
        );

    /// <summary>
    /// Returns a reader scoped to the given subdirectory.
    /// </summary>
    /// <param name="originalFileStore">The underlying reader to scope.</param>
    /// <param name="directoryName">Single path segment to use as the subdirectory.</param>
    /// <returns>A reader where all operations are relative to <paramref name="directoryName"/>.</returns>
    public static IFileStoreReader ForSubdirectory(this IFileStoreReader originalFileStore, string directoryName) =>
        originalFileStore.ForSubdirectory([directoryName]);

    /// <summary>
    /// Returns a reader scoped to the given subdirectory path prefix.
    /// </summary>
    /// <param name="originalFileStore">The underlying reader to scope.</param>
    /// <param name="directoryPath">The path prefix to prepend to all operations.</param>
    /// <returns>A reader where all operations are relative to <paramref name="directoryPath"/>.</returns>
    public static IFileStoreReader ForSubdirectory(
        this IFileStoreReader originalFileStore, IEnumerable<string> directoryPath) =>
        originalFileStore.WithMappedPath(originalPath => originalPath.InsertRange(0, directoryPath));

    /// <summary>
    /// Returns a writer that maps paths via <paramref name="pathMap"/> before delegating
    /// to <paramref name="originalFileStore"/>.
    /// </summary>
    /// <param name="originalFileStore">The underlying writer to delegate to.</param>
    /// <param name="pathMap">Function that transforms the incoming logical path.</param>
    /// <returns>A writer that applies <paramref name="pathMap"/> on every operation.</returns>
    public static IFileStoreWriter WithMappedPath(
        this IFileStoreWriter originalFileStore, Func<IImmutableList<string>, IImmutableList<string>> pathMap) =>
        new DelegatingFileStoreWriter
        (
            SetFileContentDelegate:
            pathAndFileContent =>
            originalFileStore.SetFileContent(pathMap(pathAndFileContent.path), pathAndFileContent.fileContent),

            AppendFileContentDelegate:
            pathAndFileContent =>
            originalFileStore.AppendFileContent(pathMap(pathAndFileContent.path), pathAndFileContent.fileContent),

            DeleteFileDelegate:
            originalPath => originalFileStore.DeleteFile(pathMap(originalPath))
        );

    /// <summary>
    /// Returns a writer scoped to the given subdirectory.
    /// </summary>
    /// <param name="originalFileStore">The underlying writer to scope.</param>
    /// <param name="directoryName">Single path segment to use as the subdirectory.</param>
    /// <returns>A writer where all operations are relative to <paramref name="directoryName"/>.</returns>
    public static IFileStoreWriter ForSubdirectory(this IFileStoreWriter originalFileStore, string directoryName) =>
        originalFileStore.ForSubdirectory([directoryName]);

    /// <summary>
    /// Returns a writer scoped to the given subdirectory path prefix.
    /// </summary>
    /// <param name="originalFileStore">The underlying writer to scope.</param>
    /// <param name="directoryPath">The path prefix to prepend to all operations.</param>
    /// <returns>A writer where all operations are relative to <paramref name="directoryPath"/>.</returns>
    public static IFileStoreWriter ForSubdirectory(
        this IFileStoreWriter originalFileStore, IEnumerable<string> directoryPath) =>
        originalFileStore.WithMappedPath(originalPath => originalPath.InsertRange(0, directoryPath));

    /// <summary>
    /// Creates a new store that writes to <paramref name="primary"/> and reads from a merge of
    /// <paramref name="primary"/> and <paramref name="secondary"/>.
    /// </summary>
    /// <param name="primary">Primary store used for all write operations and preferred for reads.</param>
    /// <param name="secondary">Secondary store used as a fallback for reads.</param>
    /// <param name="promoteOnReadFileContentFromSecondary">
    /// If <c>true</c>, when a read misses the primary but hits the secondary, the content is written
    /// into <paramref name="primary"/> (read-through caching/promotion).
    /// </param>
    /// <returns>
    /// An <see cref="IFileStore"/> combining <paramref name="primary"/> (writer) with a merged reader.
    /// The merged reader prefers <paramref name="primary"/> for content, falling back to <paramref name="secondary"/>;
    /// file listings are the concatenation of secondary followed by primary.
    /// </returns>
    public static IFileStore MergeReader(
        this IFileStore primary,
        IFileStoreReader secondary,
        bool promoteOnReadFileContentFromSecondary)
    {
        ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path)
        {
            var primaryContent = primary.GetFileContent(path);

            if (primaryContent is not null)
            {
                return primaryContent;
            }

            var secondaryContent = secondary.GetFileContent(path);

            if (secondaryContent.HasValue && promoteOnReadFileContentFromSecondary)
            {
                primary.SetFileContent(path, secondaryContent.Value);
            }

            return secondaryContent;
        }

        var mergedReader =
            new DelegatingFileStoreReader
            (
                GetFileContentDelegate:
                GetFileContent,

                ListFilesInDirectoryDelegate:
                path => [.. secondary.ListFilesInDirectory(path), .. primary.ListFilesInDirectory(path)]
            );

        return new FileStoreFromWriterAndReader(primary, mergedReader);
    }

    /// <summary>
    /// Creates a reader that merges <paramref name="primary"/> and <paramref name="secondary"/>.
    /// Reads prefer <paramref name="primary"/>, falling back to <paramref name="secondary"/>.
    /// Listings are the concatenation of secondary then primary.
    /// </summary>
    /// <param name="primary">Primary reader to prefer for content.</param>
    /// <param name="secondary">Secondary reader used as a fallback.</param>
    /// <returns>A reader that merges the two sources.</returns>
    public static IFileStoreReader MergeReader(
        this IFileStoreReader primary,
        IFileStoreReader secondary) =>
        new DelegatingFileStoreReader
        (
            GetFileContentDelegate:
            path => primary.GetFileContent(path) ?? secondary.GetFileContent(path),

            ListFilesInDirectoryDelegate:
            path => [.. secondary.ListFilesInDirectory(path), .. primary.ListFilesInDirectory(path)]
        );
}
