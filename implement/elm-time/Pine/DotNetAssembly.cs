using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Reflection;

namespace Pine;

public class DotNetAssembly
{
    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadFromAssemblyManifestResourceStreamContents(
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

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
        IReadOnlyList<string> directoryPath,
        Assembly assembly) =>
        LoadTreeFromManifestEmbeddedFileProvider(directoryPath, assembly)
        .Map(PineValueComposition.TreeToFlatDictionaryWithPathComparer);

    public static Result<string, TreeNodeWithStringPath> LoadTreeFromManifestEmbeddedFileProvider(
        IReadOnlyList<string> directoryPath,
        Assembly assembly)
    {
        var manifestEmbeddedProvider = new Microsoft.Extensions.FileProviders.ManifestEmbeddedFileProvider(assembly);

        var directoryPathString = string.Join("/", directoryPath);

        var directoryContents = manifestEmbeddedProvider.GetDirectoryContents(directoryPathString);

        if (!directoryContents.Exists)
            return Result<string, TreeNodeWithStringPath>.err(
                "Did not find a directory at path '" + directoryPathString + "'");

        return
            LoadTreeFromManifestEmbeddedFileProviderFileInfo(
                directoryContents,
                loadSubdirectory:
                subdirectory =>
                LoadTreeFromManifestEmbeddedFileProvider([.. directoryPath, subdirectory], assembly));
    }

    public static Result<string, TreeNodeWithStringPath> LoadTreeFromManifestEmbeddedFileProviderFileInfo(
        Microsoft.Extensions.FileProviders.IDirectoryContents directoryContents,
        Func<string, Result<string, TreeNodeWithStringPath>> loadSubdirectory)
    {
        static TreeNodeWithStringPath fromFile(Microsoft.Extensions.FileProviders.IFileInfo file)
        {
            using var stream = file.CreateReadStream();

            using var memoryStream = new System.IO.MemoryStream();

            stream.CopyTo(memoryStream);

            return TreeNodeWithStringPath.Blob(memoryStream.ToArray());
        }

        var treeElementsResults =
            directoryContents
            .Select(item =>
            {
                var itemResult =
                item.IsDirectory ?
                loadSubdirectory(item.Name)
                :
                Result<string, TreeNodeWithStringPath>.ok(fromFile(item));

                return (item.Name, itemResult);
            })
            .ToImmutableList();

        return
            treeElementsResults
            .Select(item =>
            item.itemResult
            .MapError(err => "Failed for directory " + item.Name + ": " + err)
            .Map(success => (item.Name, success)))
            .ListCombine()
            .Map(treeElements => TreeNodeWithStringPath.SortedTree(treeElements.ToImmutableList()));
    }

    public static ReadOnlyMemory<byte>? GetManifestResourceStreamContentAsBytes(Assembly assembly, string name)
    {
        using var stream = assembly.GetManifestResourceStream(name);

        if (stream == null)
            return null;

        using var memoryStream = new System.IO.MemoryStream();

        stream.CopyTo(memoryStream);

        return memoryStream.ToArray();
    }

    public static readonly Lazy<string> ProgramExecutableFileName = new(() =>
    // Assembly.GetExecutingAssembly().Location for cases where process comes from `dotnet test`
    Assembly.GetExecutingAssembly().Location switch
    {
        { Length: > 0 } executingAssemblyLocation =>
        executingAssemblyLocation,

        _ =>
        Environment.ProcessPath ??
        /*
         * Do not rely on Environment.ProcessPath because it is often null on Linux:
         * https://github.com/dotnet/runtime/issues/66323
         * */
        Process.GetCurrentProcess().MainModule!.FileName
    });
}
