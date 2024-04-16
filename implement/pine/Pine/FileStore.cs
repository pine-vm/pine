using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Pine;

public interface IFileStoreReader
{
    ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path);

    IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath);
}

public interface IFileStoreWriter
{
    void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent);

    // TODO: Simplify IFileStoreWriter: Do we still need AppendFileContent there?
    void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent);

    void DeleteFile(IImmutableList<string> path);
}

public record DelegatingFileStoreReader(
    Func<IImmutableList<string>, ReadOnlyMemory<byte>?> GetFileContentDelegate,
    Func<IImmutableList<string>, IEnumerable<IImmutableList<string>>> ListFilesInDirectoryDelegate)
    : IFileStoreReader
{
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path) => GetFileContentDelegate(path);

    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
        ListFilesInDirectoryDelegate(directoryPath);
}

public record DelegatingFileStoreWriter(
    Action<(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)> AppendFileContentDelegate,
    Action<IImmutableList<string>> DeleteFileDelegate,
    Action<(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)> SetFileContentDelegate)
    : IFileStoreWriter
{
    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        AppendFileContentDelegate((path, fileContent));

    public void DeleteFile(IImmutableList<string> path) =>
        DeleteFileDelegate(path);

    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        SetFileContentDelegate((path, fileContent));
}

public interface IFileStore : IFileStoreReader, IFileStoreWriter
{
}

public class FileStoreFromSystemIOFile(string directoryPath) : IFileStore
{
    private readonly string directoryPath = directoryPath ?? throw new ArgumentNullException(nameof(directoryPath));

    private string CombinePath(IImmutableList<string> path)
    {
        foreach (var pathComponent in path)
        {
            if (pathComponent.Contains('\\') || pathComponent.Contains('/'))
                throw new ArgumentException("Invalid character in path component '" + pathComponent + "'.");
        }

        return Path.Combine([directoryPath, .. path]);
    }

    private static void EnsureDirectoryExists(string directoryPath)
    {
        if (!(0 < directoryPath?.Length))
            return;

        var parentDirectory = Path.GetDirectoryName(directoryPath);

        if (parentDirectory != null)
            EnsureDirectoryExists(parentDirectory);

        if (!Directory.Exists(directoryPath))
            Directory.CreateDirectory(directoryPath);
    }

    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        var filePath = CombinePath(path);

        var directoryPath = Path.GetDirectoryName(filePath);

        if (directoryPath != null)
            EnsureDirectoryExists(directoryPath);

        using var fileStream = new FileStream(filePath, FileMode.Create, FileAccess.Write);

        fileStream.Write(fileContent.Span);

        fileStream.Flush();
    }

    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        var filePath = CombinePath(path);

        var directoryPath = Path.GetDirectoryName(filePath);

        if (directoryPath != null)
            EnsureDirectoryExists(directoryPath);

        using var fileStream = new FileStream(filePath, FileMode.Append, FileAccess.Write);

        fileStream.Write(fileContent.Span);

        fileStream.Flush();
    }

    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path)
    {
        var filePath = CombinePath(path);

        if (!File.Exists(filePath))
            return null;

        return File.ReadAllBytes(filePath);
    }

    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath)
    {
        var fileSystemDirectoryPath = CombinePath(directoryPath);

        if (!Directory.Exists(fileSystemDirectoryPath))
            return [];

        return
            Directory.GetFiles(fileSystemDirectoryPath, "*", SearchOption.AllDirectories)
            .Order()
            .Select(filePath => Path.GetRelativePath(fileSystemDirectoryPath, filePath).Split(Path.DirectorySeparatorChar).ToImmutableList());
    }

    public void DeleteFile(IImmutableList<string> path)
    {
        var fileSystemPath = CombinePath(path);

        if (!File.Exists(fileSystemPath))
            return;

        File.Delete(fileSystemPath);
    }
}

public class FileStoreFromWriterAndReader(
    IFileStoreWriter writer,
    IFileStoreReader reader)
    : IFileStore
{
    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        writer.AppendFileContent(path, fileContent);

    public void DeleteFile(IImmutableList<string> path) =>
        writer.DeleteFile(path);

    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        writer.SetFileContent(path, fileContent);

    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path) =>
        reader.GetFileContent(path);

    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
        reader.ListFilesInDirectory(directoryPath);
}

public class RecordingFileStoreWriter : IFileStoreWriter
{
    private readonly ConcurrentQueue<WriteOperation> history = new();

    public IEnumerable<WriteOperation> History => history;

    public IFileStoreReader Apply(IFileStoreReader fileStoreReader) =>
        WriteOperation.Apply(history, fileStoreReader);

    public record WriteOperation(
        (IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)? SetFileContent = null,
        (IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)? AppendFileContent = null,
        IImmutableList<string>? DeleteFile = null)
    {
        public static IFileStoreReader Apply(IEnumerable<WriteOperation> writeOperations, IFileStoreReader fileStoreReader) =>
            writeOperations.Aggregate(fileStoreReader, (previousState, writeOperation) => writeOperation.Apply(previousState));

        public IFileStoreReader Apply(IFileStoreReader previousState)
        {
            if (SetFileContent?.path != null)
            {
                return new DelegatingFileStoreReader
                (
                    GetFileContentDelegate: filePath =>
                    {
                        var previousFileContent = previousState.GetFileContent(filePath);

                        if (filePath.SequenceEqual(SetFileContent.Value.path))
                        {
                            return SetFileContent.Value.fileContent;
                        }

                        return previousFileContent;
                    },
                    ListFilesInDirectoryDelegate: directoryPath =>
                    {
                        var previousFilesInDirectory = previousState.ListFilesInDirectory(directoryPath);

                        if (SetFileContent.Value.path.Take(directoryPath.Count).SequenceEqual(directoryPath))
                        {
                            return previousFilesInDirectory.Append(SetFileContent.Value.path.Skip(directoryPath.Count).ToImmutableList()).Distinct();
                        }

                        return previousFilesInDirectory;
                    }
                );
            }

            if (AppendFileContent?.path != null)
            {
                return new DelegatingFileStoreReader
                (
                    GetFileContentDelegate: filePath =>
                    {
                        var previousFileContent = previousState.GetFileContent(filePath);

                        if (filePath.SequenceEqual(AppendFileContent.Value.path))
                        {
                            return CommonConversion.Concat(
                                (previousFileContent ?? ReadOnlyMemory<byte>.Empty).Span,
                                AppendFileContent.Value.fileContent.Span);
                        }

                        return previousFileContent;
                    },
                    ListFilesInDirectoryDelegate: directoryPath =>
                    {
                        var previousFilesInDirectory = previousState.ListFilesInDirectory(directoryPath);

                        if (AppendFileContent.Value.path.Take(directoryPath.Count).SequenceEqual(directoryPath))
                        {
                            return previousFilesInDirectory.Append(AppendFileContent.Value.path.Skip(directoryPath.Count).ToImmutableList()).Distinct();
                        }

                        return previousFilesInDirectory;
                    }
                );
            }

            if (DeleteFile != null)
            {
                return new DelegatingFileStoreReader
                (
                    GetFileContentDelegate: filePath =>
                    {
                        if (filePath.SequenceEqual(DeleteFile))
                            return null;

                        return previousState.GetFileContent(filePath);
                    },
                    ListFilesInDirectoryDelegate: directoryPath =>
                       previousState
                       .ListFilesInDirectory(directoryPath)
                       .Where(filePathInDirectory => !directoryPath.AddRange(filePathInDirectory).SequenceEqual(DeleteFile))
                );
            }

            throw new Exception("Invalid construction");
        }
    }

    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        history.Enqueue(new WriteOperation { SetFileContent = (path, fileContent) });

    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        history.Enqueue(new WriteOperation { AppendFileContent = (path, fileContent) });

    public void DeleteFile(IImmutableList<string> path) =>
        history.Enqueue(new WriteOperation { DeleteFile = path });
}

public class EmptyFileStoreReader : IFileStoreReader
{
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path) => null;

    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
        [];
}

public static class FileStoreExtension
{
    public static IEnumerable<IImmutableList<string>> ListFiles(this IFileStoreReader fileStore) =>
        fileStore.ListFilesInDirectory(ImmutableList<string>.Empty);

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
            SetFileContentDelegate: pathAndFileContent => originalFileStore.SetFileContent(pathMap(pathAndFileContent.path), pathAndFileContent.fileContent),
            AppendFileContentDelegate: pathAndFileContent => originalFileStore.AppendFileContent(pathMap(pathAndFileContent.path), pathAndFileContent.fileContent),
            DeleteFileDelegate: originalPath => originalFileStore.DeleteFile(pathMap(originalPath))
        );

    public static IFileStoreWriter ForSubdirectory(this IFileStoreWriter originalFileStore, string directoryName) =>
        ForSubdirectory(originalFileStore, ImmutableList.Create(directoryName));

    public static IFileStoreWriter ForSubdirectory(
        this IFileStoreWriter originalFileStore, IEnumerable<string> directoryPath) =>
        WithMappedPath(originalFileStore, originalPath => originalPath.InsertRange(0, directoryPath));
}
