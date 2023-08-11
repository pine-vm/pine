using System;
using System.Collections.Immutable;

namespace Pine;

public record CacheByFileName(IFileStore FileStore)
{
    public ReadOnlyMemory<byte> GetOrUpdate(string fileName, Func<ReadOnlyMemory<byte>> getNew) =>
        GetOrTryAdd(fileName, () => getNew())!.Value;

    public ReadOnlyMemory<byte>? GetOrTryAdd(string fileName, Func<ReadOnlyMemory<byte>?> tryBuild)
    {
        var entryPath = ImmutableList.Create(fileName);

        var fromCache = FileStore.GetFileContent(entryPath);

        if (fromCache is not null)
            return fromCache;

        var file = tryBuild();

        if (file.HasValue)
        {
            FileStore.SetFileContent(entryPath, file.Value.ToArray());
        }

        return file;
    }
}
