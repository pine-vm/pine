using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Reflection;

namespace Pine.Core.DotNet;

using FileDictionary = IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>;

/// <summary>
/// Helpers for working with .NET assemblies.
/// <list type="bullet">
/// <item>Loading embedded resources (manifest or <see cref="Microsoft.Extensions.FileProviders.ManifestEmbeddedFileProvider"/>)</item>
/// <item>Resolving the current process executable path.</item>
/// </list>
/// </summary>
public class DotNetAssembly
{
    /// <summary>
    /// Loads a set of manifest embedded resources from <paramref name="assembly"/> into a flat dictionary
    /// keyed by their logical path segments.
    /// </summary>
    /// <param name="filePaths">Logical file paths expressed as lists of segments; each path is concatenated with dots for the resource lookup.</param>
    /// <param name="resourceNameCommonPrefix">Prefix added before the dot-joined path segments to form the manifest resource name (e.g. namespace + folder).</param>
    /// <param name="assembly">Assembly containing the manifest resources.</param>
    /// <returns>A <see cref="Result{TError, TOk}"/> with either an error message or the loaded immutable dictionary.</returns>
    public static Result<string, FileDictionary> LoadFromAssemblyManifestResourceStreamContents(
        IReadOnlyList<IReadOnlyList<string>> filePaths,
        string resourceNameCommonPrefix,
        Assembly assembly)
    {
        var seed =
            Result<string, FileDictionary>.ok(
                ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>()));

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

                        if (fileContent is null)
                        {
                            return Result<string, FileDictionary>.err(
                                "Failed to get content for resource: " + resourceName);
                        }

                        return Result<string, FileDictionary>.ok(
                            dict.SetItem([.. filePath], fileContent.Value));
                    }));
    }

    /// <summary>
    /// Loads all files under the embedded directory identified by <paramref name="directoryPath"/>
    /// using <see cref="Microsoft.Extensions.FileProviders.ManifestEmbeddedFileProvider"/> and flattens
    /// them into a dictionary keyed by path segments.
    /// </summary>
    /// <param name="directoryPath">Directory path inside the assembly (segments, joined with '/').</param>
    /// <param name="assembly">Assembly containing the embedded directory.</param>
    /// <returns>Result with a flat dictionary of file contents or an error description.</returns>
    public static Result<string, FileDictionary> LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
        IReadOnlyList<string> directoryPath,
        Assembly assembly) =>
        LoadTreeFromManifestEmbeddedFileProvider(directoryPath, assembly)
        .Map(PineValueComposition.TreeToFlatDictionaryWithPathComparer);

    /// <summary>
    /// Recursively loads a directory tree from the assembly's embedded file provider, retaining hierarchy.
    /// </summary>
    /// <param name="directoryPath">Directory path segments inside the assembly.</param>
    /// <param name="assembly">Assembly to inspect.</param>
    /// <returns>A hierarchical <see cref="FileTree"/> wrapped in a result.</returns>
    public static Result<string, FileTree> LoadTreeFromManifestEmbeddedFileProvider(
        IReadOnlyList<string> directoryPath,
        Assembly assembly)
    {
        var manifestEmbeddedProvider =
            new Microsoft.Extensions.FileProviders.ManifestEmbeddedFileProvider(assembly);

        var directoryPathString = string.Join("/", directoryPath);

        var directoryContents = manifestEmbeddedProvider.GetDirectoryContents(directoryPathString);

        if (!directoryContents.Exists)
            return "Did not find a directory at path '" + directoryPathString + "'";

        return
            LoadTreeFromManifestEmbeddedFileProviderFileInfo(
                directoryContents,
                loadSubdirectory:
                subdirectory =>
                LoadTreeFromManifestEmbeddedFileProvider([.. directoryPath, subdirectory], assembly));
    }

    /// <summary>
    /// Builds a <see cref="FileTree"/> from an <see cref="Microsoft.Extensions.FileProviders.IDirectoryContents"/> collection,
    /// invoking <paramref name="loadSubdirectory"/> for nested directories.
    /// </summary>
    /// <param name="directoryContents">The directory contents abstraction from the embedded file provider.</param>
    /// <param name="loadSubdirectory">Callback to load a named subdirectory recursively.</param>
    /// <returns>The composed file tree or an error.</returns>
    public static Result<string, FileTree> LoadTreeFromManifestEmbeddedFileProviderFileInfo(
        Microsoft.Extensions.FileProviders.IDirectoryContents directoryContents,
        Func<string, Result<string, FileTree>> loadSubdirectory)
    {
        static FileTree FromFile(Microsoft.Extensions.FileProviders.IFileInfo file)
        {
            using var stream = file.CreateReadStream();

            using var memoryStream = new System.IO.MemoryStream();

            stream.CopyTo(memoryStream);

            return FileTree.File(memoryStream.ToArray());
        }

        var treeElements = new List<(string name, FileTree component)>();

        foreach (var item in directoryContents)
        {
            if (item.IsDirectory)
            {
                var subdirectoryResult = loadSubdirectory(item.Name);

                if (subdirectoryResult.IsErrOrNull() is { } err)
                {
                    return "Failed for directory " + item.Name + ": " + err;
                }

                if (subdirectoryResult.IsOkOrNull() is not { } subdirOk)
                {
                    throw new NotImplementedException(
                        "LoadTreeFromManifestEmbeddedFileProviderFileInfo: Unexpected result type: " +
                        subdirectoryResult.GetType());
                }

                treeElements.Add((item.Name, subdirOk));
            }
            else
            {
                treeElements.Add((item.Name, FromFile(item)));
            }
        }

        return FileTree.SortedDirectory([.. treeElements]);
    }

    /// <summary>
    /// Reads the raw bytes of a manifest resource stream, returning <c>null</c> if not found.
    /// </summary>
    /// <param name="assembly">Assembly containing the resource.</param>
    /// <param name="name">Fully qualified manifest resource name.</param>
    /// <returns>Byte content or <c>null</c> if the resource does not exist.</returns>
    public static ReadOnlyMemory<byte>? GetManifestResourceStreamContentAsBytes(Assembly assembly, string name)
    {
        using var stream = assembly.GetManifestResourceStream(name);

        if (stream is null)
            return null;

        using var memoryStream = new System.IO.MemoryStream();

        stream.CopyTo(memoryStream);

        return memoryStream.ToArray();
    }

    /// <summary>
    /// Lazily resolves the executable file path for the current process in a robust way that works
    /// in contexts like test runners where <see cref="Assembly.GetExecutingAssembly()"/> or
    /// <see cref="Environment.ProcessPath"/> may be empty.
    /// </summary>
    public static readonly Lazy<string> ProgramExecutableFileName =
        new(() =>
        // Assembly.GetExecutingAssembly().Location for cases where process comes from `dotnet test`
        Assembly.GetExecutingAssembly().Location switch
        {
            { Length: > 0 } executingAssemblyLocation =>
            executingAssemblyLocation,

            _ =>
            Environment.ProcessPath ??
            /*
             * Do not rely on Environment.ProcessPath because it is often null:
             * https://github.com/dotnet/runtime/issues/66323
             * */
            Process.GetCurrentProcess().MainModule!.FileName
        });
}
