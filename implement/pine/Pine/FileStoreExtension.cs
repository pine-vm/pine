using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine;

public static class FileStoreExtension
{
    public static IEnumerable<IImmutableList<string>> ListFiles(this IFileStoreReader fileStore) =>
        fileStore.ListFilesInDirectory([]);

    public static IFileStoreReader WithMappedPath(
        this IFileStoreReader originalFileStore, Func<IImmutableList<string>, IImmutableList<string>> pathMap) =>
        new DelegatingFileStoreReader
        (
            GetFileContentDelegate: originalPath => originalFileStore.GetFileContent(pathMap(originalPath)),
            ListFilesInDirectoryDelegate: originalPath => originalFileStore.ListFilesInDirectory(pathMap(originalPath))
        );

    public static IFileStoreReader ForSubdirectory(this IFileStoreReader originalFileStore, string directoryName) =>
        ForSubdirectory(originalFileStore, ImmutableList.Create(directoryName));

    public static IFileStoreReader ForSubdirectory(
        this IFileStoreReader originalFileStore, IEnumerable<string> directoryPath) =>
        WithMappedPath(originalFileStore, originalPath => originalPath.InsertRange(0, directoryPath));

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

    public static IFileStoreWriter ForSubdirectory(this IFileStoreWriter originalFileStore, string directoryName) =>
        ForSubdirectory(originalFileStore, ImmutableList.Create(directoryName));

    public static IFileStoreWriter ForSubdirectory(
        this IFileStoreWriter originalFileStore, IEnumerable<string> directoryPath) =>
        WithMappedPath(originalFileStore, originalPath => originalPath.InsertRange(0, directoryPath));

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
