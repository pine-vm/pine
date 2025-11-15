using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.IO;


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
    private (ImmutableList<WriteOperation> history, FileTree latestVersion) _historyAndReduction =
        ([], FileTree.EmptyTree);

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
        new FileStoreReaderFromBlobTreeWithStringPath(_historyAndReduction.latestVersion);

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
        public FileTree Apply(FileTree previousState)
        {
            if (SetFileContent is { } setFileContent)
            {
                return
                    previousState.SetNodeAtPathSorted(
                        setFileContent.path,
                        FileTree.File(setFileContent.fileContent));
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
                            FileTree.File(appendFileContent.fileContent));
                }

                if (previousNode is not FileTree.FileNode previousBlob)
                {
                    throw new InvalidOperationException(
                        "Invalid operation: Cannot append to non-blob node");
                }

                return
                    previousState.SetNodeAtPathSorted(
                        appendFileContent.path,
                        FileTree.File(
                            BytesConversions.Concat(previousBlob.Bytes.Span, appendFileContent.fileContent.Span)));
            }

            if (DeleteFile is { } deleteFile)
            {
                return
                    previousState.RemoveNodeAtPath(deleteFile);
            }

            throw new NotImplementedException(
                "Operation variant not implemented: " + this);
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
