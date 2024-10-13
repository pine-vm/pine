using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO.Compression;
using System.Linq;

namespace Pine.Core.Elm;

public class BundledElmEnvironments
{
    public static PineValue? BundledElmEnvironmentFromFileTree(TreeNodeWithStringPath fileTree)
    {
        CompiledEnvDict.TryGetValue(DictionaryKeyHashPartFromFileTree(fileTree), out var bundledEnv);

        return bundledEnv;
    }

    public static string DictionaryKeyHashPartFromFileTree(TreeNodeWithStringPath fileTree)
    {
        var pineValue =
            PineValueComposition.FromTreeWithStringPath(fileTree);

        var hash = PineValueHashTree.ComputeHash(pineValue);

        return CommonConversion.StringBase16(hash[..16]);
    }

    public const string EmbeddedResourceFilePath = "prebuilt-artifact/compiled-elm-environments.json.gzip";

    private static readonly System.Text.RegularExpressions.Regex compiledEnvKeyRegex =
        new System.Text.RegularExpressions.Regex("^" + CompiledEnvDictionaryKeyPrefix + "([\\d\\w]+)$");

    const string CompiledEnvDictionaryKeyPrefix = "compiled-env-";

    private readonly static IReadOnlyDictionary<string, PineValue> CompiledEnvDict =
        LoadBundledCompiledEnvironments(
            embeddedResourceFilePath: EmbeddedResourceFilePath,
            assembly: typeof(BundledElmEnvironments).Assembly)
        .Extract(err =>
        {
            System.Console.WriteLine("Failed loading bundled Elm environments from embedded resource: " + err);

            return ImmutableDictionary<string, PineValue>.Empty;
        });

    public static Result<string, IReadOnlyDictionary<string, PineValue>> LoadBundledCompiledEnvironments(
        string embeddedResourceFilePath,
        System.Reflection.Assembly assembly)
    {
        /*
        var inspect =
            DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["prebuilt-artifact"],
            assembly: assembly);
        */

        var manifestEmbeddedProvider =
            new Microsoft.Extensions.FileProviders.ManifestEmbeddedFileProvider(assembly);

        var embeddedFileInfo = manifestEmbeddedProvider.GetFileInfo(embeddedResourceFilePath);

        if (!embeddedFileInfo.Exists)
        {
            return "Did not find file " + embeddedResourceFilePath + " in assembly " + assembly.FullName;
        }

        if (embeddedFileInfo.Length is 0)
        {
            return "File " + embeddedResourceFilePath + " in assembly " + assembly.FullName + " is empty";
        }

        using var readStream = embeddedFileInfo.CreateReadStream();

        using var gzipStream = new GZipStream(readStream, CompressionMode.Decompress);

        var dictionaryEntries =
            System.Text.Json.JsonSerializer.Deserialize<IReadOnlyList<PineValueCompactBuild.ListEntry>>(gzipStream);

        var sharedDictionary = PineValueCompactBuild.BuildDictionaryFromEntries(dictionaryEntries);

        var mutatedDictionary = new Dictionary<string, PineValue>();

        foreach (var item in sharedDictionary)
        {
            var keyMatch = compiledEnvKeyRegex.Match(item.Key);

            if (!keyMatch.Success)
            {
                continue;
            }

            var environmentId = keyMatch.Groups[1].Value;

            mutatedDictionary[environmentId] = item.Value;
        }

        return Result<string, IReadOnlyDictionary<string, PineValue>>.ok(mutatedDictionary);
    }

    public const string ResourceFilePath = "./Pine.Core/" + EmbeddedResourceFilePath;

    public static void CompressAndWriteBundleFile(
        IReadOnlyDictionary<TreeNodeWithStringPath, PineValue> compiledEnvironments)
    {
        var fileContent = CompressResourceFile(BuildBundleResourceFileJsonUtf8(compiledEnvironments));

        CompressAndWriteBundleFile(fileContent);
    }

    public static void CompressAndWriteBundleFile(
        System.ReadOnlyMemory<byte> fileContent)
    {
        System.Console.WriteLine(
        "Current working directory: " + System.Environment.CurrentDirectory);

        var absolutePath = System.IO.Path.GetFullPath(ResourceFilePath);

        System.Console.WriteLine(
            "Resolved the destination path of " + ResourceFilePath +
            " to " + absolutePath);

        System.IO.Directory.CreateDirectory(
            System.IO.Path.GetDirectoryName(absolutePath));

        System.IO.File.WriteAllBytes(
            absolutePath,
            fileContent.ToArray());

        System.Console.WriteLine(
            "Saved the prebuilt dictionary to " + absolutePath);
    }

    public static System.ReadOnlyMemory<byte> CompressResourceFile(
        System.ReadOnlyMemory<byte> beforeCompression)
    {
        using var memoryStream = new System.IO.MemoryStream();

        using var gzipStream = new GZipStream(memoryStream, CompressionLevel.Optimal);

        gzipStream.Write(beforeCompression.Span);

        gzipStream.Flush();

        return memoryStream.ToArray();
    }

    public static System.ReadOnlyMemory<byte> BuildBundleResourceFileJsonUtf8(
        IReadOnlyDictionary<TreeNodeWithStringPath, PineValue> compiledEnvironments)
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
                var key =
                CompiledEnvDictionaryKeyPrefix +
                DictionaryKeyHashPartFromFileTree(compiledEnvironment.Key);

                if (compiledEnvironment.Value is not PineValue.ListValue compiledListValue)
                {
                    throw new System.NotImplementedException(
                        "Unexpected value type for compiled Elm environment: " + compiledEnvironment.GetType());
                }

                return new PineValueCompactBuild.ListEntry(
                    Key: key,
                    Value: sharedValuesDict.entryValueFromListItems(compiledListValue.Elements));
            })
            .ToImmutableArray();

        IReadOnlyList<PineValueCompactBuild.ListEntry> allEntries =
            [..sharedValuesDict.listEntries
            , ..environmentEntries
            ];

        return System.Text.Json.JsonSerializer.SerializeToUtf8Bytes(allEntries);
    }
}
