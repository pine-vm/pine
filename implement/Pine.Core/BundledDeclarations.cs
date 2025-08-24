using Pine.Core.Elm;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.IO.Compression;
using System.Linq;

namespace Pine.Core;

/// <summary>
/// Named declarations of <see cref="PineValue"/> which are bundled with the assembly.
/// </summary>
public record BundledDeclarations(
    IReadOnlyList<PineValue> DecodedSequence,
    IReadOnlyDictionary<string, PineValue> EmbeddedDeclarations)
{
    public const string ResourceFilePath = "prebuilt-artifact/reused-pine-values-and-builds.gzip";

    public static BundledDeclarations? LoadFromEmbedded(
        System.Reflection.Assembly assembly) =>
         LoadEmbedded(assembly)
        .Extract(
             err =>
             {
                 Console.WriteLine("Failed loading from embedded resource: " + err);

                 return null;
             });

    public static Result<string, BundledDeclarations> LoadEmbedded(
        System.Reflection.Assembly assembly,
        string embeddedResourceFilePath = ResourceFilePath)
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

        using var memoryStream = new System.IO.MemoryStream();

        gzipStream.CopyTo(memoryStream);

        var asMemory = memoryStream.ToArray();

        var decoded = StringNamedPineValueBinaryEncoding.Decode(asMemory);

        return new BundledDeclarations(decoded.decodedSequence, decoded.decls);
    }

    public static void CompressAndWriteBundleFile(
        IReadOnlyDictionary<TreeNodeWithStringPath, PineValue> compiledEnvironments,
        IReadOnlyDictionary<string, PineValue> otherReusedValues,
        string? destinationDirectory)
    {
        var declarations = otherReusedValues.ToDictionary();

        foreach (var kvp in compiledEnvironments)
        {
            var keyString = BundledElmEnvironments.DictionaryKeyFromFileTree(kvp.Key);

            if (declarations.ContainsKey(keyString))
            {
                throw new InvalidOperationException(
                    "Key collision when building bundled declarations: " + keyString);
            }

            declarations[keyString] = kvp.Value;
        }

        using var memoryStream = new System.IO.MemoryStream();

        StringNamedPineValueBinaryEncoding.Encode(memoryStream, declarations);

        var uncompressed = memoryStream.ToArray();

        Console.WriteLine(
            "Built " + CommandLineInterface.FormatIntegerForDisplay(declarations.Count) + " declarations for " +
            compiledEnvironments.Count + " compiled environments with an uncompressed size of " +
            CommandLineInterface.FormatIntegerForDisplay(uncompressed.Length) + " bytes.");

        var compressed = CompressResourceFile(uncompressed);

        Console.WriteLine(
            "Compressed from " +
            CommandLineInterface.FormatIntegerForDisplay(uncompressed.Length) + " bytes to " +
            CommandLineInterface.FormatIntegerForDisplay(compressed.Length) + " bytes.");

        WriteBundleFile(
            compressed,
            destinationDirectory);
    }

    private static void WriteBundleFile(
        ReadOnlyMemory<byte> fileContent,
        string? destinationDirectory = null)
    {
        Console.WriteLine(
            "Current working directory: " + Environment.CurrentDirectory);

        if (string.IsNullOrEmpty(destinationDirectory))
        {
            destinationDirectory = Environment.CurrentDirectory;
        }

        Console.WriteLine(
            "Using destination directory: " + destinationDirectory);

        var resourceFilePathRel =
            System.IO.Path.Combine(destinationDirectory, ResourceFilePath);

        var absolutePath = System.IO.Path.GetFullPath(resourceFilePathRel);

        Console.WriteLine(
            "Resolved the destination path of " + resourceFilePathRel +
            " to " + absolutePath);

        System.IO.Directory.CreateDirectory(
            System.IO.Path.GetDirectoryName(absolutePath));

        System.IO.File.WriteAllBytes(
            absolutePath,
            fileContent.ToArray());

        Console.WriteLine(
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
}

