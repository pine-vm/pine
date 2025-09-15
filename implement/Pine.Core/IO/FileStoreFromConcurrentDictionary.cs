using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.IO;


/// <summary>
/// In-memory <see cref="IFileStore"/> implementation using a concurrent dictionary for file storage.
/// Useful for tests or ephemeral scenarios.
/// The implementation is thread-safe, supporting concurrent access.
/// </summary>
public class FileStoreFromConcurrentDictionary : IFileStore
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
