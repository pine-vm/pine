using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

namespace Pine;

public class DotNetAssembly
{
    static public Result<string, IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>>> LoadFromAssemblyManifestResourceStreamContents(
        IReadOnlyList<IReadOnlyList<string>> filePaths,
        string resourceNameCommonPrefix,
        Assembly assembly)
    {
        var seed =
            Result<string, IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>>>.ok(
                ImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>>.Empty
                .WithComparers(EnumerableExtension.EqualityComparer<IImmutableList<string>>()));

        return
            filePaths
            .Aggregate(
                seed: seed,
                func: (aggregate, filePath) =>
                {
                    if (aggregate.Ok == null)
                        return aggregate;

                    var resourceName = resourceNameCommonPrefix + string.Join(".", filePath);

                    var fileContent = GetManifestResourceStreamContentAsBytes(assembly, resourceName);

                    if (fileContent == null)
                        return Result<string, IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>>>.err(
                            "Failed to get content for resource: " + resourceName);

                    return
                        aggregate.map(dict => dict.SetItem(filePath.ToImmutableList(), fileContent.Value));
                });
    }

    static IImmutableDictionary<KeyT, ValueT>? SetItemOrReturnNull<KeyT, ValueT>(
        IImmutableDictionary<KeyT, ValueT>? dict,
        KeyT key,
        ValueT? value)
    {
        if (dict == null || value == null)
            return null;

        return dict.SetItem(key, value);
    }

    static public ReadOnlyMemory<byte>? GetManifestResourceStreamContentAsBytes(Assembly assembly, string name)
    {
        using var stream = assembly.GetManifestResourceStream(name);

        if (stream == null)
            return null;

        using var memoryStream = new System.IO.MemoryStream();

        stream.CopyTo(memoryStream);

        return memoryStream.ToArray();
    }
}
