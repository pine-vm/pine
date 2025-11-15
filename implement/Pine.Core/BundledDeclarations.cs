using Pine.Core.Elm;
using Pine.Core.Files;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.IO.Compression;
using System.Linq;

namespace Pine.Core;

/// <summary>
/// Named declarations of <see cref="PineValue"/> which are bundled with the assembly.
/// </summary>
/// <param name="DecodedSequence">
/// The sequence of decoded <see cref="PineValue"/> instances contained in the bundle. This is typically used
/// to preserve ordering information alongside the named declarations.
/// </param>
/// <param name="EmbeddedDeclarations">
/// A dictionary mapping stable names to <see cref="PineValue"/> declarations contained in the bundle.
/// This includes compiled Elm environments and other reused values.
/// </param>
public record BundledDeclarations(
    IReadOnlyList<PineValue> DecodedSequence,
    IReadOnlyDictionary<string, PineValue> EmbeddedDeclarations)
{
    /// <summary>
    /// Default relative path of the embedded resource file that contains bundled declarations.
    /// </summary>
    public const string ResourceFilePath = "prebuilt-artifact/reused-pine-values-and-builds.gzip";

    /// <summary>
    /// Loads <see cref="BundledDeclarations"/> from an assembly's embedded resource using <see cref="ResourceFilePath"/>.
    /// Returns <c>null</c> on failure and logs a message to <see cref="Console"/>.
    /// </summary>
    /// <param name="assembly">The assembly containing the embedded resource.</param>
    /// <returns>The loaded <see cref="BundledDeclarations"/>, or <c>null</c> if loading failed.</returns>
    public static BundledDeclarations? LoadFromEmbedded(
        System.Reflection.Assembly assembly) =>
         LoadEmbedded(assembly)
        .Extract(
             err =>
             {
                 Console.WriteLine("Failed loading from embedded resource: " + err);

                 return null;
             });

    /// <summary>
    /// Loads <see cref="BundledDeclarations"/> from an assembly's embedded resource at the specified path.
    /// The resource is expected to be a GZip-compressed binary produced by <see cref="CompressResourceFile(ReadOnlyMemory{byte})"/>
    /// and encoded via <see cref="StringNamedPineValueBinaryEncoding"/>.
    /// </summary>
    /// <param name="assembly">The assembly containing the embedded resource.</param>
    /// <param name="embeddedResourceFilePath">The relative resource path inside the assembly. Defaults to <see cref="ResourceFilePath"/>.</param>
    /// <returns>
    /// A <see cref="Result{TError, TOk}"/> containing either an error message or the decoded <see cref="BundledDeclarations"/>.
    /// </returns>
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

    /// <summary>
    /// Compresses and writes a bundle file containing compiled environments and other reused values.
    /// The result is written to <see cref="ResourceFilePath"/> under <paramref name="destinationDirectory"/>.
    /// </summary>
    /// <param name="compiledEnvironments">A mapping of source trees to their compiled environment values.</param>
    /// <param name="otherReusedValues">Additional named <see cref="PineValue"/>s to include in the bundle.</param>
    /// <param name="destinationDirectory">The destination directory. If null or empty, uses the current working directory.</param>
    public static void CompressAndWriteBundleFile(
        IReadOnlyDictionary<FileTree, PineValue> compiledEnvironments,
        IReadOnlyDictionary<string, PineValue> otherReusedValues,
        string? destinationDirectory)
    {
        var (_, uncompressed) =
            BuildBundleFile(
                compiledEnvironments,
                otherReusedValues);

        var compressed = CompressResourceFile(uncompressed);

        Console.WriteLine(
            "Compressed from " +
            CommandLineInterface.FormatIntegerForDisplay(uncompressed.Length) + " bytes to " +
            CommandLineInterface.FormatIntegerForDisplay(compressed.Length) + " bytes.");

        WriteBundleFile(
            compressed,
            destinationDirectory);
    }

    /// <summary>
    /// Builds the uncompressed bundle payload for the given compiled environments and other reused values.
    /// Keys for compiled environments are computed via <see cref="BundledElmEnvironments.DictionaryKeyFromFileTree(FileTree)"/>.
    /// </summary>
    /// <param name="compiledEnvironments">A mapping of source trees to their compiled environment values.</param>
    /// <param name="otherReusedValues">Additional named <see cref="PineValue"/>s to include in the bundle.</param>
    /// <returns>The uncompressed binary payload to embed or compress.</returns>
    /// <exception cref="InvalidOperationException">Thrown if a key collision occurs when building the dictionary.</exception>
    public static (IReadOnlyList<PineValue> componentsWritten, ReadOnlyMemory<byte> fileContent) BuildBundleFile(
        IReadOnlyDictionary<FileTree, PineValue> compiledEnvironments,
        IReadOnlyDictionary<string, PineValue> otherReusedValues)
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

        var declarationsWritten = new List<PineValue>();

        StringNamedPineValueBinaryEncoding.Encode(
            memoryStream,
            declarations,
            componentDeclWritten: (declValue, _) => declarationsWritten.Add(declValue));

        var uncompressed = memoryStream.ToArray();

        Console.WriteLine(
            "Built " + CommandLineInterface.FormatIntegerForDisplay(declarations.Count) + " declarations for " +
            compiledEnvironments.Count + " compiled environments with an uncompressed size of " +
            CommandLineInterface.FormatIntegerForDisplay(uncompressed.Length) + " bytes.");

        return
            (declarationsWritten, uncompressed);
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

    /// <summary>
    /// Compresses the provided bundle payload using GZip.
    /// </summary>
    /// <param name="beforeCompression">The uncompressed payload to compress.</param>
    /// <returns>The compressed payload bytes.</returns>
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

