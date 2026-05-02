using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Pine.Elm;

/// <summary>
/// Builds the Elm source tree containing the language service implementation
/// (<c>LanguageService.elm</c> and its dependencies from the bundled
/// elm-in-elm sources) and compiles it via
/// <see cref="Pine.Core.Elm.ElmCompilerInDotnet.ElmCompiler.CompileInteractiveEnvironment"/>.
/// <para>
/// Compiling the language service from scratch is expensive (hundreds of
/// modules through the full optimization pipeline). To recover the
/// performance previously provided by the precompiled environment shipped
/// in <see cref="Pine.Core.Elm.BundledElmEnvironments"/>, the result is
/// cached on disk, keyed by a hash of the input file tree, scoped to the
/// Pine application version id.
/// </para>
/// </summary>
internal static class LanguageServiceCompilation
{
    /// <summary>
    /// Builds the merged Elm source tree containing the bundled elm-in-elm
    /// language service implementation, its <c>elm-syntax</c> dependency,
    /// the bundled kernel modules, and the bundled "other library" modules.
    /// This is the exact tree that is fed to
    /// <see cref="Pine.Core.Elm.ElmCompilerInDotnet.ElmCompiler.CompileInteractiveEnvironment"/>.
    /// </summary>
    public static FileTree BuildLanguageServiceSourceTree()
    {
        var bundledTree =
            BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var kernelModulesTree =
            BundledFiles.ElmKernelModulesDefault.Value;

        var elmSyntaxSrcTree =
            bundledTree.GetNodeAtPath(["elm-syntax", "src"])
            ?? throw new Exception("Did not find elm-syntax/src in bundled compiler tree");

        var elmInElmSrcTree =
            bundledTree.GetNodeAtPath(["src"])
            ?? throw new Exception("Did not find src in bundled compiler tree");

        var otherLibraryModulesTree =
            bundledTree.GetNodeAtPath(["other-library-modules"]);

        var mergedTree = kernelModulesTree;

        foreach (var (path, file) in elmSyntaxSrcTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        foreach (var (path, file) in elmInElmSrcTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        if (otherLibraryModulesTree is not null)
        {
            foreach (var (path, file) in otherLibraryModulesTree.EnumerateFilesTransitive())
            {
                mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
            }
        }

        return mergedTree;
    }

    /// <summary>
    /// Root file path used for the compilation closure.
    /// <c>LanguageService.elm</c> transitively imports
    /// <c>LanguageServiceInterface</c>, <c>Elm.Parser</c>, the
    /// <c>Elm.Syntax.*</c> modules, <c>Frontend.MonacoEditor</c>, etc.
    /// </summary>
    public static System.Collections.Generic.IReadOnlyList<string> LanguageServiceRootFilePath { get; } =
        ["LanguageService.elm"];

    /// <summary>
    /// Computes a deterministic cache key for the given source tree.
    /// </summary>
    public static string CacheKeyFromSourceTree(FileTree sourceTree)
    {
        var encoded = FileTreeEncoding.Encode(sourceTree);

        var hash = PineValueHashTree.ComputeHash(encoded);

        return Convert.ToHexStringLower(hash.Span);
    }

    /// <summary>
    /// Returns the default persistent cache for compiled language service
    /// environments, scoped to the Pine application version id.
    /// </summary>
    public static IFileStore DefaultPersistentCache(string pineAppVersionId)
    {
        var directory =
            Path.Combine(
                Filesystem.CacheDirectory,
                "lang-service-compile",
                pineAppVersionId);

        return new FileStoreFromSystemIOFile(directory);
    }

    /// <summary>
    /// Compiles the given source tree to a Pine value containing the
    /// language service environment, using the persistent cache when
    /// available.
    /// </summary>
    /// <param name="sourceTree">
    /// The merged Elm source tree, typically obtained from
    /// <see cref="BuildLanguageServiceSourceTree"/>.
    /// </param>
    /// <param name="cache">
    /// Optional persistent cache. When non-null, a cache hit returns the
    /// cached value without invoking the compiler; a cache miss compiles
    /// and writes the result.
    /// </param>
    /// <param name="logDelegate">Optional logging callback for diagnostic output.</param>
    public static Result<string, PineValue> CompileLanguageServiceEnv(
        FileTree sourceTree,
        IFileStore? cache,
        Action<string>? logDelegate = null)
    {
        var cacheKey = CacheKeyFromSourceTree(sourceTree);

        var cacheFilePath =
            ImmutableList.Create(cacheKey + ".bin");

        if (cache is not null)
        {
            var cached = cache.GetFileContent(cacheFilePath);

            if (cached is { } cachedBytes)
            {
                try
                {
                    var decoded = ValueBinaryEncodingClassic.DecodeRoot(cachedBytes);

                    logDelegate?.Invoke(
                        "Loaded compiled language service environment from cache (key " +
                        cacheKey + ", " + cachedBytes.Length + " bytes)");

                    return decoded;
                }
                catch (Exception e)
                {
                    logDelegate?.Invoke(
                        "Failed decoding cached language service environment (key " +
                        cacheKey + "): " + e.Message + " - recompiling.");
                }
            }
        }

        logDelegate?.Invoke(
            "Compiling language service environment (cache key " + cacheKey + ") ...");

        var rootFilePaths =
            sourceTree.EnumerateFilesTransitive()
            .Where(
                b =>
                b.path.Count == LanguageServiceRootFilePath.Count &&
                b.path.SequenceEqual(LanguageServiceRootFilePath, StringComparer.Ordinal))
            .Select(b => (System.Collections.Generic.IReadOnlyList<string>)b.path)
            .ToList();

        if (rootFilePaths.Count is 0)
        {
            return
                "Source tree does not contain " +
                string.Join("/", LanguageServiceRootFilePath);
        }

        var compileResult =
            Core.Elm.ElmCompilerInDotnet.ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree: sourceTree,
                rootFilePaths: rootFilePaths,
                disableInlining: true,
                maxOptimizationRounds: 1);

        if (compileResult.IsErrOrNull() is { } compileErr)
        {
            return "Failed compiling language service environment: " + compileErr;
        }

        if (compileResult.IsOkOrNullable() is not { } compileOk)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + compileResult.GetType());
        }

        var compiledEnv = compileOk.compiledEnvValue;

        if (cache is not null)
        {
            try
            {
                using var stream = new MemoryStream();

                ValueBinaryEncodingClassic.Encode(stream, compiledEnv);

                cache.SetFileContent(
                    cacheFilePath,
                    stream.GetBuffer().AsMemory()[..(int)stream.Length]);

                logDelegate?.Invoke(
                    "Wrote compiled language service environment to cache (key " +
                    cacheKey + ", " + stream.Length + " bytes)");
            }
            catch (Exception e)
            {
                logDelegate?.Invoke(
                    "Failed writing cached language service environment: " + e.Message);
            }
        }

        return compiledEnv;
    }
}
