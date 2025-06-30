using Pine.Core;
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

public class FileStoreFromSystemIOFile(
    string directoryPath,
    FileStoreFromSystemIOFile.FileStoreRetryOptions retryOptions) : IFileStore
{
    private readonly string directoryPath =
        directoryPath
        ??
        throw new ArgumentNullException(nameof(directoryPath));

    private readonly FileStoreRetryOptions retryOptions =
        retryOptions
        ??
        throw new ArgumentNullException(nameof(retryOptions));

    public FileStoreFromSystemIOFile(string directoryPath) : this(directoryPath, FileStoreRetryOptions.NoRetry)
    {
    }

    public record FileStoreRetryOptions(
        int MaxRetryAttempts,
        TimeSpan InitialRetryDelay,
        TimeSpan MaxRetryDelay)
    {
        /// <summary>
        /// Gets a configuration that disables retries
        /// </summary>
        public static readonly FileStoreRetryOptions NoRetry =
            new(MaxRetryAttempts: 0, TimeSpan.Zero, TimeSpan.Zero);
    }

    private string CombinePath(IImmutableList<string> path)
    {
        for (var i = 0; i < path.Count; i++)
        {
            var pathComponent = path[i];

            if (pathComponent.Contains('\\') || pathComponent.Contains('/'))
            {
                throw new ArgumentException(
                    "Invalid character in path component '" + pathComponent + "' at index " + i);
            }
        }

        return Path.Combine([directoryPath, .. path]);
    }

    public void SetFileContent(
        IImmutableList<string> path,
        ReadOnlyMemory<byte> fileContent)
    {
        var filePath = CombinePath(path);

        var directoryPath = Path.GetDirectoryName(filePath);

        if (directoryPath is not null)
            Directory.CreateDirectory(directoryPath);

        ExecuteWithRetry(() =>
        {
            using var fileStream = new FileStream(filePath, FileMode.Create, FileAccess.Write);

            fileStream.Write(fileContent.Span);

            fileStream.Flush();
            return true;
        });
    }

    public void AppendFileContent(
        IImmutableList<string> path,
        ReadOnlyMemory<byte> fileContent)
    {
        var filePath = CombinePath(path);

        var directoryPath = Path.GetDirectoryName(filePath);

        if (directoryPath is not null)
            Directory.CreateDirectory(directoryPath);

        using var fileStream = new FileStream(filePath, FileMode.Append, FileAccess.Write);

        fileStream.Write(fileContent.Span);

        fileStream.Flush();
    }

    public ReadOnlyMemory<byte>? GetFileContent(
        IImmutableList<string> path)
    {
        var filePath = CombinePath(path);

        return ExecuteWithRetry<ReadOnlyMemory<byte>?>(() =>
        {
            try
            {
                return File.ReadAllBytes(filePath);
            }
            catch (FileNotFoundException)
            {
                return null;
            }
            catch (DirectoryNotFoundException)
            {
                return null;
            }
        });
    }

    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(
        IImmutableList<string> directoryPath)
    {
        var fileSystemDirectoryPath = CombinePath(directoryPath);

        return
            ExecuteWithRetry<IEnumerable<IImmutableList<string>>>(() =>
            {
                if (!Directory.Exists(fileSystemDirectoryPath))
                    return [];

                return
                    Directory.GetFiles(fileSystemDirectoryPath, "*", SearchOption.AllDirectories)
                    .Order()
                    .Select(filePath =>
                    Path.GetRelativePath(fileSystemDirectoryPath, filePath).Split(Path.DirectorySeparatorChar)
                    .ToImmutableList());
            });
    }

    public void DeleteFile(IImmutableList<string> path)
    {
        var fileSystemPath = CombinePath(path);

        ExecuteWithRetry(() =>
        {
            if (!File.Exists(fileSystemPath))
                return true;

            File.Delete(fileSystemPath);
            return true;
        });
    }

    private T ExecuteWithRetry<T>(Func<T> operation) =>
        ExecuteWithRetry(operation, retryOptions);

    public static T ExecuteWithRetry<T>(
        Func<T> operation,
        FileStoreRetryOptions retryOptions)
    {
        // If retries are disabled, execute the operation directly
        if (retryOptions.MaxRetryAttempts <= 0)
        {
            return operation();
        }

        int attempts = 0;
        TimeSpan delay = retryOptions.InitialRetryDelay;

        while (true)
        {
            try
            {
                attempts++;
                return operation();
            }
            catch (IOException ex)
            when (ex.HResult is unchecked((int)0x80070020)) // ERROR_SHARING_VIOLATION
            {
                if (!(attempts <= retryOptions.MaxRetryAttempts))
                    throw; // Rethrow if we've reached max attempts

                System.Threading.Thread.Sleep(delay);

                // Increase delay with exponential backoff (up to max)
                delay =
                    TimeSpan.FromMilliseconds(
                        Math.Min(
                            delay.TotalMilliseconds * 2,
                            retryOptions.MaxRetryDelay.TotalMilliseconds));
            }
        }
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
    private readonly System.Threading.Lock @lock = new();

    private (ImmutableList<WriteOperation> history, TreeNodeWithStringPath latestVersion) historyAndReduction =
        ([], TreeNodeWithStringPath.EmptyTree);

    public IEnumerable<WriteOperation> History =>
        historyAndReduction.history;

    public IFileStoreReader Apply(IFileStoreReader fileStoreReader) =>
        WriteOperation.Apply(History, fileStoreReader);

    public IFileStoreReader ReaderFromAppliedOperationsOnEmptyStore() =>
        new FileStoreReaderFromTreeNodeWithStringPath(historyAndReduction.latestVersion);

    public record WriteOperation(
        (IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)? SetFileContent = null,
        (IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)? AppendFileContent = null,
        IImmutableList<string>? DeleteFile = null)
    {
        public static IFileStoreReader Apply(IEnumerable<WriteOperation> writeOperations, IFileStoreReader fileStoreReader) =>
            writeOperations.Aggregate(fileStoreReader, (previousState, writeOperation) => writeOperation.Apply(previousState));

        public static IFileStoreReader ReaderFromAppliedOperationsOnEmptyStore(
            IEnumerable<WriteOperation> writeOperations)
        {
            var store = new FileStoreFromConcurrentDictionary();

            ApplyOperationsOnWriter(writeOperations, store);

            return store;
        }

        public static void ApplyOperationsOnWriter(
            IEnumerable<WriteOperation> writeOperations,
            IFileStoreWriter fileStoreWriter)
        {
            foreach (var writeOperation in writeOperations)
            {
                if (writeOperation.SetFileContent is { } setFileContent)
                {
                    fileStoreWriter.SetFileContent(setFileContent.path, setFileContent.fileContent);
                }

                if (writeOperation.AppendFileContent is { } appendFileContent)
                {
                    fileStoreWriter.AppendFileContent(appendFileContent.path, appendFileContent.fileContent);
                }

                if (writeOperation.DeleteFile is { } deleteFile)
                {
                    fileStoreWriter.DeleteFile(deleteFile);
                }
            }
        }

        public TreeNodeWithStringPath Apply(TreeNodeWithStringPath previousState)
        {
            if (SetFileContent is { } setFileContent)
            {
                return
                    previousState.SetNodeAtPathSorted(
                        setFileContent.path,
                        TreeNodeWithStringPath.Blob(setFileContent.fileContent));
            }

            if (AppendFileContent is { } appendFileContent)
            {
                var previousNode =
                    previousState.GetNodeAtPath(appendFileContent.path);

                if (previousNode is null)
                {
                    return
                        previousState.SetNodeAtPathSorted(
                            appendFileContent.path,
                            TreeNodeWithStringPath.Blob(appendFileContent.fileContent));
                }

                if (previousNode is not TreeNodeWithStringPath.BlobNode previousBlob)
                {
                    throw new InvalidOperationException(
                        "Invalid operation: Cannot append to non-blob node");
                }

                return
                    previousState.SetNodeAtPathSorted(
                        appendFileContent.path,
                        TreeNodeWithStringPath.Blob(
                            BytesConversions.Concat(previousBlob.Bytes.Span, appendFileContent.fileContent.Span)));
            }

            if (DeleteFile is { } deleteFile)
            {
                return
                    previousState.RemoveNodeAtPath(deleteFile);
            }

            throw new Exception("Invalid construction");
        }

        public IFileStoreReader Apply(IFileStoreReader previousState)
        {
            if (SetFileContent?.path is { } setFilePath)
            {
                return new DelegatingFileStoreReader
                (
                    GetFileContentDelegate: filePath =>
                    {
                        if (filePath.SequenceEqual(setFilePath))
                        {
                            return SetFileContent.Value.fileContent;
                        }

                        var previousFileContent = previousState.GetFileContent(filePath);

                        return previousFileContent;
                    },
                    ListFilesInDirectoryDelegate: directoryPath =>
                    {
                        var previousFilesInDirectory = previousState.ListFilesInDirectory(directoryPath);

                        if (setFilePath.Take(directoryPath.Count).SequenceEqual(directoryPath))
                        {
                            return previousFilesInDirectory.Append(setFilePath.Skip(directoryPath.Count).ToImmutableList()).Distinct();
                        }

                        return previousFilesInDirectory;
                    }
                );
            }

            if (AppendFileContent?.path is { } appendPath)
            {
                return new DelegatingFileStoreReader
                (
                    GetFileContentDelegate: filePath =>
                    {
                        var previousFileContent = previousState.GetFileContent(filePath);

                        if (filePath.SequenceEqual(appendPath))
                        {
                            return BytesConversions.Concat(
                                (previousFileContent ?? ReadOnlyMemory<byte>.Empty).Span,
                                AppendFileContent.Value.fileContent.Span);
                        }

                        return previousFileContent;
                    },
                    ListFilesInDirectoryDelegate: directoryPath =>
                    {
                        var previousFilesInDirectory = previousState.ListFilesInDirectory(directoryPath);

                        if (appendPath.Take(directoryPath.Count).SequenceEqual(directoryPath))
                        {
                            return previousFilesInDirectory.Append(appendPath.Skip(directoryPath.Count).ToImmutableList()).Distinct();
                        }

                        return previousFilesInDirectory;
                    }
                );
            }

            if (DeleteFile is not null)
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

    private void AppendToHistory(WriteOperation writeOperation)
    {
        lock (@lock)
        {
            var newTree = writeOperation.Apply(historyAndReduction.latestVersion);

            historyAndReduction =
                (historyAndReduction.history.Add(writeOperation), newTree);
        }
    }

    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        AppendToHistory(new WriteOperation { SetFileContent = (path, fileContent) });

    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        AppendToHistory(new WriteOperation { AppendFileContent = (path, fileContent) });

    public void DeleteFile(IImmutableList<string> path) =>
        AppendToHistory(new WriteOperation { DeleteFile = path });
}

public class EmptyFileStoreReader : IFileStoreReader
{
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path) => null;

    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
        [];
}

public class FileStoreReaderFromTreeNodeWithStringPath(
    TreeNodeWithStringPath root)
    : IFileStoreReader
{
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path)
    {
        var node = root.GetNodeAtPath(path);

        if (node is TreeNodeWithStringPath.BlobNode blobNode)
        {
            return blobNode.Bytes;
        }

        return null;
    }

    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath)
    {
        var directoryNode = root.GetNodeAtPath(directoryPath);

        if (directoryNode is null)
        {
            return [];
        }

        if (directoryNode is TreeNodeWithStringPath.TreeNode directory)
        {
            return
                directory.EnumerateBlobsTransitive()
                .Select(blob => blob.path);
        }

        return [[]];
    }
}

public class FileStoreFromConcurrentDictionary : IFileStoreWriter, IFileStoreReader
{
    private readonly ConcurrentDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> files =
        new(EnumerableExtension.EqualityComparer<IImmutableList<string>>());

    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        files.AddOrUpdate(
            path,
            addValueFactory:
            _ => fileContent,
            updateValueFactory:
            (_, fileBefore) => BytesConversions.Concat(fileBefore.Span, fileContent.Span));
    }

    public void DeleteFile(IImmutableList<string> path)
    {
        files.TryRemove(path, out _);
    }

    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path)
    {
        if (!files.TryGetValue(path, out var fileContent))
        {
            return null;
        }

        return fileContent;
    }

    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath)
    {
        foreach (var file in files)
        {
            if (file.Key.Count < directoryPath.Count)
            {
                continue;
            }

            bool isMismatch = false;

            for (var i = 0; i < directoryPath.Count; i++)
            {
                if (file.Key[i] != directoryPath[i])
                {
                    isMismatch = true;
                    break;
                }
            }

            if (isMismatch)
            {
                continue;
            }

            var relativePath = file.Key.Skip(directoryPath.Count).ToImmutableList();

            yield return relativePath;
        }
    }

    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        files[path] = fileContent;
    }
}
