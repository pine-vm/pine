using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.IO;


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
