using Pine.Core.Addressing;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.IO.Compression;
using System.Linq;

namespace Pine.Core.Elm;

/// <summary>
/// Utilities for working with compiled Elm environments that are bundled with an assembly or produced at build time.
/// Provides helpers to look up embedded (precompiled) environments, compute stable dictionary keys from file trees,
/// load bundled declaration blobs, and build compact bundle resources.
/// </summary>
public partial class BundledElmEnvironments
{
    /// <summary>
    /// Attempts to retrieve a bundled (embedded) compiled Elm environment corresponding to the provided file tree.
    /// The lookup uses a deterministic key derived from <paramref name="fileTree"/> via <see cref="DictionaryKeyFromFileTree(FileTree)"/>.
    /// </summary>
    /// <param name="fileTree">The source tree describing the Elm project files.</param>
    /// <returns>
    /// The compiled environment as a <see cref="PineValue"/> when available in the embedded declarations; otherwise <c>null</c>.
    /// </returns>
    public static PineValue? BundledElmEnvironmentFromFileTree(FileTree fileTree)
    {
        if (ReusedInstances.Instance?.BundledDeclarations?.EmbeddedDeclarations is { } embeddedDecls)
        {
            var expectedKey = DictionaryKeyFromFileTree(fileTree);

            if (embeddedDecls.TryGetValue(expectedKey, out var bundledEnv))
                return bundledEnv;
        }

        return null;
    }

    /// <summary>
    /// Returns the most complex (by node count) bundled compiled Elm environment among the embedded declarations
    /// whose keys start with the compiled environment prefix.
    /// </summary>
    /// <returns>
    /// A compiled environment value or <c>null</c> if none are bundled.
    /// </returns>
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

    /// <summary>
    /// Computes the dictionary key used for looking up a compiled environment derived from the given file tree.
    /// The key is the concatenation of a prefix and a stable hash of the tree content.
    /// </summary>
    /// <param name="fileTree">The file tree to derive the key from.</param>
    /// <returns>A stable key string suitable for indexing embedded declarations.</returns>
    public static string DictionaryKeyFromFileTree(FileTree fileTree) =>
        CompiledEnvDictionaryKeyPrefix +
        DictionaryKeyHashPartFromFileTree(fileTree);

    private static string DictionaryKeyHashPartFromFileTree(FileTree fileTree)
    {
        var pineValue =
            PineValueComposition.FromTreeWithStringPath(fileTree);

        var hash = PineValueHashTree.ComputeHash(pineValue);

        return Convert.ToHexStringLower(hash[..16].Span);
    }

    const string CompiledEnvDictionaryKeyPrefix = "compiled-env-";

    [System.Text.RegularExpressions.GeneratedRegex(@"^" + CompiledEnvDictionaryKeyPrefix + @"-([\d\w]+)$")]
    private static partial System.Text.RegularExpressions.Regex CompiledEnvKeyRegex { get; }

    /// <summary>
    /// Loads bundled declarations from a stream, optionally applying GZip decompression.
    /// </summary>
    /// <param name="readStream">The input stream containing the bundled declarations payload.</param>
    /// <param name="gzipDecompress">Whether to wrap the stream in a <see cref="GZipStream"/> for decompression.</param>
    /// <returns>
    /// A <see cref="Result{TError, TOk}"/> containing either an error description or the dictionary of embedded declarations.
    /// </returns>
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
}
