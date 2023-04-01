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

    static public Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
        IReadOnlyList<string> directoryPath,
        Assembly assembly) =>
        LoadTreeFromManifestEmbeddedFileProvider(directoryPath, assembly)
        .Map(PineValueComposition.TreeToFlatDictionaryWithPathComparer);

    static public Result<string, TreeNodeWithStringPath> LoadTreeFromManifestEmbeddedFileProvider(
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
                LoadTreeFromManifestEmbeddedFileProvider(ImmutableList.CreateRange(directoryPath).Add(subdirectory), assembly));
    }

    static public Result<string, TreeNodeWithStringPath> LoadTreeFromManifestEmbeddedFileProviderFileInfo(
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
