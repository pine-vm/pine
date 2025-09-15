using System;
using System.Collections.Generic;
using System.Collections.Immutable;

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

