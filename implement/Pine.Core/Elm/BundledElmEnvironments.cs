using System;
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

    public static PineValue? BundledElmCompilerCompiledEnvValue()
    {
        var compiledEnvDict = CompiledEnvDict;

        return
            compiledEnvDict
            .Select(kv => kv.Value)
            .OrderByDescending(compiledEnv => compiledEnv is PineValue.ListValue listValue ? listValue.NodesCount : 0)
            .FirstOrDefault();
    }

    public static string DictionaryKeyHashPartFromFileTree(TreeNodeWithStringPath fileTree)
    {
        var pineValue =
            PineValueComposition.FromTreeWithStringPath(fileTree);

        var hash = PineValueHashTree.ComputeHash(pineValue);

        return Convert.ToHexStringLower(hash[..16].Span);
    }

    public const string EmbeddedResourceFilePath = "prebuilt-artifact/compiled-elm-environments.json.gzip";

    private static readonly System.Text.RegularExpressions.Regex compiledEnvKeyRegex =
        new("^" + CompiledEnvDictionaryKeyPrefix + "([\\d\\w]+)$");

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

        try
        {
            return LoadBundledCompiledEnvironments(readStream, gzipDecompress: true);
        }
        catch (Exception e)
        {
            return "Failed to load bundled Elm environments from embedded resource: " + e.Message;
        }
    }

    public static Result<string, IReadOnlyDictionary<string, PineValue>> LoadBundledCompiledEnvironments(
        System.IO.Stream readStream,
        bool gzipDecompress)
    {
        if (gzipDecompress)
        {
            using var gzipStream = new GZipStream(readStream, CompressionMode.Decompress);

            return LoadBundledCompiledEnvironments(gzipStream);
        }

        return LoadBundledCompiledEnvironments(readStream);
    }

    private static Result<string, IReadOnlyDictionary<string, PineValue>> LoadBundledCompiledEnvironments(
        System.IO.Stream readStream)
    {
        var dictionaryEntries =
            System.Text.Json.JsonSerializer.Deserialize<IReadOnlyList<PineValueCompactBuild.ListEntry>>(readStream);

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

        return mutatedDictionary;
    }

    public const string ResourceFilePath = "./Pine.Core/" + EmbeddedResourceFilePath;

    public static void CompressAndWriteBundleFile(
        IReadOnlyDictionary<TreeNodeWithStringPath, PineValue> compiledEnvironments)
    {
        var (allListEntries, uncompressed) =
            BuildBundleResourceFileJsonUtf8(compiledEnvironments);

        System.Console.WriteLine(
            "Built " + CommandLineInterface.FormatIntegerForDisplay(allListEntries.Count) + " list entries for " +
            compiledEnvironments.Count + " compiled environments with an uncompressed size of " +
            CommandLineInterface.FormatIntegerForDisplay(uncompressed.Length) + " bytes.");

        var fileContent = CompressResourceFile(uncompressed);

        CompressAndWriteBundleFile(fileContent);
    }

    public static void CompressAndWriteBundleFile(
        ReadOnlyMemory<byte> fileContent)
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

    public static ReadOnlyMemory<byte> CompressResourceFile(
        ReadOnlyMemory<byte> beforeCompression)
    {
        using var memoryStream = new System.IO.MemoryStream();

        using var gzipStream = new GZipStream(memoryStream, CompressionLevel.Optimal);

        gzipStream.Write(beforeCompression.Span);

        gzipStream.Flush();

        return memoryStream.ToArray();
    }

    public static (IReadOnlyList<PineValueCompactBuild.ListEntry>, ReadOnlyMemory<byte>) BuildBundleResourceFileJsonUtf8(
        IReadOnlyDictionary<TreeNodeWithStringPath, PineValue> compiledEnvironments)
    {
        IReadOnlyList<PineValueCompactBuild.ListEntry> allEntries =
            BuildBundleResourceFileListItems(compiledEnvironments);

        return (allEntries, System.Text.Json.JsonSerializer.SerializeToUtf8Bytes(allEntries));
    }

    public static IReadOnlyList<PineValueCompactBuild.ListEntry> BuildBundleResourceFileListItems(
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
                    throw new NotImplementedException(
                        "Unexpected value type for compiled Elm environment: " + compiledEnvironment.GetType());
                }

                return new PineValueCompactBuild.ListEntry(
                    Key: key,
                    Value: sharedValuesDict.entryValueFromListItems(compiledListValue.Elements));
            })
            .ToImmutableArray();

        return
            [..sharedValuesDict.listEntries
            , ..environmentEntries
            ];
    }
}
