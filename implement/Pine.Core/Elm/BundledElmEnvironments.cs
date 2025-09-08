using Pine.Core.Addressing;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO.Compression;
using System.Linq;

namespace Pine.Core.Elm;

public partial class BundledElmEnvironments
{
    public static PineValue? BundledElmEnvironmentFromFileTree(BlobTreeWithStringPath fileTree)
    {
        if (ReusedInstances.Instance?.BundledDeclarations?.EmbeddedDeclarations is { } embeddedDecls)
        {
            var expectedKey = DictionaryKeyFromFileTree(fileTree);

            if (embeddedDecls.TryGetValue(expectedKey, out var bundledEnv))
                return bundledEnv;
        }

        return null;
    }

    public static PineValue? BundledElmCompilerCompiledEnvValue()
    {
        var compiledEnvDict = ReusedInstances.Instance?.BundledDeclarations?.EmbeddedDeclarations;

        return
            compiledEnvDict
            .Where(kv => kv.Key.StartsWith(CompiledEnvDictionaryKeyPrefix))
            .Select(kv => kv.Value)
            .OrderByDescending(compiledEnv => compiledEnv is PineValue.ListValue listValue ? listValue.NodesCount : 0)
            .FirstOrDefault();
    }

    public static string DictionaryKeyFromFileTree(BlobTreeWithStringPath fileTree) =>
        CompiledEnvDictionaryKeyPrefix +
        DictionaryKeyHashPartFromFileTree(fileTree);

    private static string DictionaryKeyHashPartFromFileTree(BlobTreeWithStringPath fileTree)
    {
        var pineValue =
            PineValueComposition.FromTreeWithStringPath(fileTree);

        var hash = PineValueHashTree.ComputeHash(pineValue);

        return Convert.ToHexStringLower(hash[..16].Span);
    }

    const string CompiledEnvDictionaryKeyPrefix = "compiled-env-";

    [System.Text.RegularExpressions.GeneratedRegex(@"^" + CompiledEnvDictionaryKeyPrefix + @"-([\d\w]+)$")]
    private static partial System.Text.RegularExpressions.Regex CompiledEnvKeyRegex { get; }

    public static Result<string, IReadOnlyDictionary<string, PineValue>> LoadBundledDeclarations(
        System.IO.Stream readStream,
        bool gzipDecompress)
    {
        if (gzipDecompress)
        {
            using var gzipStream = new GZipStream(readStream, CompressionMode.Decompress);

            return LoadBundledDeclarations(gzipStream);
        }

        return LoadBundledDeclarations(readStream);
    }

    private static Result<string, IReadOnlyDictionary<string, PineValue>> LoadBundledDeclarations(
        System.IO.Stream readStream)
    {
        var asMemory = ReadOnlyMemoryFromWholeStream(readStream);

        try
        {
            return Result<string, IReadOnlyDictionary<string, PineValue>>.ok(
                PopularEncodings.StringNamedPineValueBinaryEncoding.Decode(asMemory).decls);
        }
        catch (Exception e)
        {
            return "Failed with runtime exception: " + e.Message;
        }
    }

    private static ReadOnlyMemory<byte> ReadOnlyMemoryFromWholeStream(System.IO.Stream stream)
    {
        using var grow = new System.IO.MemoryStream();

        stream.CopyTo(grow);

        if (grow.TryGetBuffer(out var seg2))
            return new ReadOnlyMemory<byte>(seg2.Array!, seg2.Offset, seg2.Count);

        return grow.ToArray().AsMemory();
    }

    public static (IReadOnlyList<PineValueCompactBuild.ListEntry>, ReadOnlyMemory<byte>) BuildBundleResourceFileJsonUtf8(
        IReadOnlyDictionary<BlobTreeWithStringPath, PineValue> compiledEnvironments)
    {
        var allEntries =
            BuildBundleResourceFileListItems(compiledEnvironments);

        return (allEntries, System.Text.Json.JsonSerializer.SerializeToUtf8Bytes(allEntries));
    }

    public static IReadOnlyList<PineValueCompactBuild.ListEntry> BuildBundleResourceFileListItems(
        IReadOnlyDictionary<BlobTreeWithStringPath, PineValue> compiledEnvironments)
    {
        var (listValues, blobValues) =
            PineValue.CollectAllComponentsFromRoots(compiledEnvironments.Values);

        var sharedValuesDict =
            PineValueCompactBuild.PrebuildListEntries(
                blobValues: blobValues,
                listValues: listValues);

        var environmentEntries =
            compiledEnvironments
            .Select(compiledEnvironment =>
            {
                var key = DictionaryKeyFromFileTree(compiledEnvironment.Key);

                if (compiledEnvironment.Value is not PineValue.ListValue compiledListValue)
                {
                    throw new NotImplementedException(
                        "Unexpected value type for compiled Elm environment: " + compiledEnvironment.GetType());
                }

                return new PineValueCompactBuild.ListEntry(
                    Key: key,
                    Value: sharedValuesDict.entryValueFromListItems(compiledListValue.Items));
            })
            .ToImmutableArray();

        return
            [..sharedValuesDict.listEntries
            , ..environmentEntries
            ];
    }
}
