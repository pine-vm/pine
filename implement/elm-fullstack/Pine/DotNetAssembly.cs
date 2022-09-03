using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

namespace Pine;

public class DotNetAssembly
{
    static public Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadFromAssemblyManifestResourceStreamContents(
        IReadOnlyList<IReadOnlyList<string>> filePaths,
        string resourceNameCommonPrefix,
        Assembly assembly)
    {
        var seed =
            Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.ok(
                ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                .WithComparers(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>()));

        return
            filePaths
            .Aggregate(
                seed: seed,
                func: (aggregate, filePath) =>
                aggregate.AndThen(
                    dict =>
                    {
                        var resourceName = resourceNameCommonPrefix + string.Join(".", filePath);

                        var fileContent = GetManifestResourceStreamContentAsBytes(assembly, resourceName);

                        if (fileContent == null)
                            return Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.err(
                                "Failed to get content for resource: " + resourceName);

                        return Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.ok(
                            dict.SetItem(filePath.ToImmutableList(), fileContent.Value));
                    }));
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
