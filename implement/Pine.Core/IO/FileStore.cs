using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.IO;

/// <summary>
/// Read-only view over a hierarchical file store where paths are
/// lists of components.
/// </summary>
public interface IFileStoreReader
{
    /// <summary>
    /// Gets the file contents at <paramref name="path"/>, or <c>null</c> if the file does not exist.
    /// </summary>
    ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path);

    /// <summary>
    /// Lists files under <paramref name="directoryPath"/> (non-recursive path prefix),
    /// returning each file's path relative to <paramref name="directoryPath"/>.
    /// </summary>
    IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath);
}


/// <summary>
/// Write-only abstraction over a hierarchical file store where paths are lists of components
/// (no path separators inside a component) and file contents are raw bytes.
/// </summary>
/// <remarks>
/// <para>
/// This interface defines the small, cross-platform <em>common denominator</em> for writing files:
/// create/replace, append, and delete. Paths are expressed as <see cref="IImmutableList{T}"/> of
/// <see cref="string"/> segments; implementations translate these into their native representation.
/// </para>
/// <para><b>Why this abstraction?</b> It stays intentionally minimal so it works uniformly across
/// backends (local file system, in-memory, remote, transactional overlays), enabling:
/// </para>
/// <list type="bullet">
///   <item><description><b>Decoupling &amp; testability</b>: swap in-memory or recording writers in unit tests without touching real I/O.</description></item>
///   <item><description><b>Portability</b>: avoid platform path quirks; componentized paths keep separators/roots backend-specific.</description></item>
///   <item><description><b>Composability</b>: pair with <see cref="IFileStoreReader"/> to form <see cref="IFileStore"/> or to layer writes over a snapshot.</description></item>
///   <item><description><b>Deterministic semantics</b>: create/replace, byte-append, and idempotent delete behave consistently across implementations.</description></item>
///   <item><description><b>Auditing / replay</b>: a writer can be wrapped to record operations and later reconstruct state.</description></item>
/// </list>
/// <para>
/// Implementations may add concerns like retries, locking, or persistence, but must preserve
/// the semantics below. Calls are single logical operations; do not assume multi-call transactions.
/// Directories are implicit (derived from path prefixes).
/// </para>
/// </remarks>
public interface IFileStoreWriter
{
    /// <summary>
    /// Creates or replaces the file at <paramref name="path"/> with <paramref name="fileContent"/>.
    /// </summary>
    /// <param name="path">Sequence of path components;</param>
    /// <param name="fileContent">Bytes to write.</param>
    void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent);

    /// <summary>
    /// Appends <paramref name="fileContent"/> to the file at <paramref name="path"/>.
    /// Creates the file if it does not exist.
    /// </summary>
    /// <param name="path">Sequence of path components;</param>
    /// <param name="fileContent">Bytes to append; append is defined as byte-concatenation.</param>
    void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent);

    /// <summary>
    /// Deletes the file at <paramref name="path"/> if it exists.
    /// </summary>
    /// <param name="path">Sequence of path components;</param>
    /// <remarks>This operation is idempotent: deleting a non-existent file is a no-op.</remarks>
    void DeleteFile(IImmutableList<string> path);
}


/// <summary>
/// Combined file store abstraction that supports both reads and writes.
/// </summary>
public interface IFileStore : IFileStoreReader, IFileStoreWriter
{
}


/// <summary>
/// An <see cref="IFileStoreReader"/> implementation delegating calls to provided function instances.
/// </summary>
/// <param name="GetFileContentDelegate">Delegate invoked to resolve file content at a given path.</param>
/// <param name="ListFilesInDirectoryDelegate">Delegate invoked to enumerate files under a given directory path.</param>
public record DelegatingFileStoreReader(
    Func<IImmutableList<string>, ReadOnlyMemory<byte>?> GetFileContentDelegate,
    Func<IImmutableList<string>, IEnumerable<IImmutableList<string>>> ListFilesInDirectoryDelegate)
    : IFileStoreReader
{
    /// <inheritdoc />
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path) => GetFileContentDelegate(path);

    /// <inheritdoc />
    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
        ListFilesInDirectoryDelegate(directoryPath);
}

/// <summary>
/// An <see cref="IFileStoreWriter"/> implementation delegating calls to provided action instances.
/// </summary>
/// <param name="AppendFileContentDelegate">Action invoked when appending to a file.</param>
/// <param name="DeleteFileDelegate">Action invoked when deleting a file.</param>
/// <param name="SetFileContentDelegate">Action invoked when creating or replacing a file.</param>
public record DelegatingFileStoreWriter(
    Action<(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)> AppendFileContentDelegate,
    Action<IImmutableList<string>> DeleteFileDelegate,
    Action<(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)> SetFileContentDelegate)
    : IFileStoreWriter
{
    /// <inheritdoc />
    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        AppendFileContentDelegate((path, fileContent));

    /// <inheritdoc />
    public void DeleteFile(IImmutableList<string> path) =>
        DeleteFileDelegate(path);

    /// <inheritdoc />
    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        SetFileContentDelegate((path, fileContent));
}

/// <summary>
/// An <see cref="IFileStore"/> that composes distinct reader and writer implementations into a single instance.
/// </summary>
/// <param name="writer">Underlying writer used for mutating operations.</param>
/// <param name="reader">Underlying reader used for query operations.</param>
public class FileStoreFromWriterAndReader(
    IFileStoreWriter writer,
    IFileStoreReader reader)
    : IFileStore
{
    /// <inheritdoc />
    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        writer.AppendFileContent(path, fileContent);

    /// <inheritdoc />
    public void DeleteFile(IImmutableList<string> path) =>
        writer.DeleteFile(path);

    /// <inheritdoc />
    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        writer.SetFileContent(path, fileContent);

    /// <inheritdoc />
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path) =>
        reader.GetFileContent(path);

    /// <inheritdoc />
    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
        reader.ListFilesInDirectory(directoryPath);
}

/// <summary>
/// <see cref="IFileStoreWriter"/> that records all write operations and can replay them or materialize a reader snapshot.
/// </summary>
public class RecordingFileStoreWriter : IFileStoreWriter
{
    /// <summary>
    /// Synchronization primitive guarding updates to the internal history and reduction state.
    /// </summary>
    private readonly System.Threading.Lock _lock = new();

    /// <summary>
    /// Tuple of the complete history and the latest reduced tree state obtained by applying the history.
    /// </summary>
    private (ImmutableList<WriteOperation> history, BlobTreeWithStringPath latestVersion) _historyAndReduction =
        ([], BlobTreeWithStringPath.EmptyTree);

    /// <summary>
    /// Gets the sequence of write operations performed on this instance in order of application.
    /// </summary>
    public IEnumerable<WriteOperation> History =>
        _historyAndReduction.history;

    /// <summary>
    /// Applies the recorded history to the given <paramref name="fileStoreReader"/> and returns a new reader
    /// reflecting the resulting state as an overlay.
    /// </summary>
    /// <param name="fileStoreReader">The base reader to apply the history to.</param>
    /// <returns>A reader that resolves reads as if the recorded writes were applied.</returns>
    public IFileStoreReader Apply(IFileStoreReader fileStoreReader) =>
        WriteOperation.Apply(History, fileStoreReader);

    /// <summary>
    /// Builds a reader that exposes the state resulting from applying the recorded operations onto an empty store.
    /// </summary>
    /// <returns>A reader representing the materialized state of the recorded operations.</returns>
    public IFileStoreReader ReaderFromAppliedOperationsOnEmptyStore() =>
        new FileStoreReaderFromTreeNodeWithStringPath(_historyAndReduction.latestVersion);

    /// <summary>
    /// Immutable representation of a single write operation (set/append/delete) with helpers to apply sequences.
    /// </summary>
    /// <param name="SetFileContent">Optional create/replace operation.</param>
    /// <param name="AppendFileContent">Optional append operation.</param>
    /// <param name="DeleteFile">Optional delete operation.</param>
    public record WriteOperation(
        (IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)? SetFileContent = null,
        (IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)? AppendFileContent = null,
        IImmutableList<string>? DeleteFile = null)
    {
        /// <summary>
        /// Applies a sequence of write operations over the provided <paramref name="fileStoreReader"/> producing
        /// a reader that reflects the resulting state.
        /// </summary>
        /// <param name="writeOperations">The operations to apply in order.</param>
        /// <param name="fileStoreReader">The base reader over which to apply operations.</param>
        /// <returns>A reader representing the combined effect of the operations over the base.</returns>
        public static IFileStoreReader Apply(IEnumerable<WriteOperation> writeOperations, IFileStoreReader fileStoreReader) =>
            writeOperations.Aggregate(fileStoreReader, (previousState, writeOperation) => writeOperation.Apply(previousState));

        /// <summary>
        /// Applies <paramref name="writeOperations"/> onto an empty file store and returns a reader for the result.
        /// </summary>
        /// <param name="writeOperations">The operations to apply in order.</param>
        /// <returns>A reader exposing the resulting state.</returns>
        public static IFileStoreReader ReaderFromAppliedOperationsOnEmptyStore(
            IEnumerable<WriteOperation> writeOperations)
        {
            var store = new FileStoreFromConcurrentDictionary();

            ApplyOperationsOnWriter(writeOperations, store);

            return store;
        }

        /// <summary>
        /// Applies <paramref name="writeOperations"/> to the provided <paramref name="fileStoreWriter"/>.
        /// </summary>
        /// <param name="writeOperations">Operations to apply in order.</param>
        /// <param name="fileStoreWriter">Writer to receive the operations.</param>
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

        /// <summary>
        /// Applies this operation to an immutable tree state and returns the resulting tree.
        /// </summary>
        /// <param name="previousState">The input tree state.</param>
        /// <returns>The new tree reflecting this operation.</returns>
        /// <exception cref="InvalidOperationException">Thrown when appending to a non-blob node.</exception>
        public BlobTreeWithStringPath Apply(BlobTreeWithStringPath previousState)
        {
            if (SetFileContent is { } setFileContent)
            {
                return
                    previousState.SetNodeAtPathSorted(
                        setFileContent.path,
                        BlobTreeWithStringPath.Blob(setFileContent.fileContent));
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
                            BlobTreeWithStringPath.Blob(appendFileContent.fileContent));
                }

                if (previousNode is not BlobTreeWithStringPath.BlobNode previousBlob)
                {
                    throw new InvalidOperationException(
                        "Invalid operation: Cannot append to non-blob node");
                }

                return
                    previousState.SetNodeAtPathSorted(
                        appendFileContent.path,
                        BlobTreeWithStringPath.Blob(
                            BytesConversions.Concat(previousBlob.Bytes.Span, appendFileContent.fileContent.Span)));
            }

            if (DeleteFile is { } deleteFile)
            {
                return
                    previousState.RemoveNodeAtPath(deleteFile);
            }

            throw new Exception("Invalid construction");
        }

        /// <summary>
        /// Applies this operation as a logical overlay on top of <paramref name="previousState"/> and returns a reader
        /// that resolves reads accordingly.
        /// </summary>
        /// <param name="previousState">Base reader to overlay.</param>
        /// <returns>A reader reflecting the effect of this write operation.</returns>
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

    /// <summary>
    /// Appends a write operation to the internal history and updates the reduced tree state.
    /// </summary>
    /// <param name="writeOperation">The operation to add.</param>
    private void AppendToHistory(WriteOperation writeOperation)
    {
        lock (_lock)
        {
            var newTree = writeOperation.Apply(_historyAndReduction.latestVersion);

            _historyAndReduction =
                (_historyAndReduction.history.Add(writeOperation), newTree);
        }
    }

    /// <inheritdoc />
    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        AppendToHistory(new WriteOperation { SetFileContent = (path, fileContent) });

    /// <inheritdoc />
    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
        AppendToHistory(new WriteOperation { AppendFileContent = (path, fileContent) });

    /// <inheritdoc />
    public void DeleteFile(IImmutableList<string> path) =>
        AppendToHistory(new WriteOperation { DeleteFile = path });
}

/// <summary>
/// Reader that exposes an empty file store (no files, all queries resolve to missing).
/// </summary>
public class EmptyFileStoreReader : IFileStoreReader
{
    /// <inheritdoc />
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path) => null;

    /// <inheritdoc />
    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
        [];
}

/// <summary>
/// <see cref="IFileStoreReader"/> backed by an immutable <see cref="BlobTreeWithStringPath"/>.
/// </summary>
/// <param name="root">Root tree node representing the file hierarchy.</param>
public class FileStoreReaderFromTreeNodeWithStringPath(
    BlobTreeWithStringPath root)
    : IFileStoreReader
{
    /// <inheritdoc />
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path)
    {
        var node = root.GetNodeAtPath(path);

        if (node is BlobTreeWithStringPath.BlobNode blobNode)
        {
            return blobNode.Bytes;
        }

        return null;
    }

    /// <inheritdoc />
    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath)
    {
        var directoryNode = root.GetNodeAtPath(directoryPath);

        if (directoryNode is null)
        {
            return [];
        }

        if (directoryNode is BlobTreeWithStringPath.TreeNode directory)
        {
            return
                directory.EnumerateBlobsTransitive()
                .Select(blob => blob.path);
        }

        return [[]];
    }
}

/// <summary>
/// In-memory <see cref="IFileStore"/> implementation using a concurrent dictionary for file storage.
/// Useful for tests or ephemeral scenarios.
/// </summary>
public class FileStoreFromConcurrentDictionary : IFileStoreWriter, IFileStoreReader
{
    /// <summary>
    /// Underlying map from componentized path to blob content.
    /// </summary>
    private readonly ConcurrentDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> _files =
        new(EnumerableExtension.EqualityComparer<IImmutableList<string>>());

    /// <inheritdoc />
    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        _files.AddOrUpdate(
            path,
            addValueFactory:
            _ => fileContent,
            updateValueFactory:
            (_, fileBefore) => BytesConversions.Concat(fileBefore.Span, fileContent.Span));
    }

    /// <inheritdoc />
    public void DeleteFile(IImmutableList<string> path)
    {
        _files.TryRemove(path, out _);
    }

    /// <inheritdoc />
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path)
    {
        if (!_files.TryGetValue(path, out var fileContent))
        {
            return null;
        }

        return fileContent;
    }

    /// <inheritdoc />
    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath)
    {
        foreach (var file in _files)
        {
            if (file.Key.Count < directoryPath.Count)
            {
                continue;
            }

            var isMismatch = false;

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

    /// <inheritdoc />
    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        _files[path] = fileContent;
    }
}
