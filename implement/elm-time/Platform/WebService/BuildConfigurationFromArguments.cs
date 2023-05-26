using Pine;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace ElmTime.Platform.WebService;

public static class BuildConfigurationFromArguments
{
    public static
        (TreeNodeWithStringPath sourceTree,
        string filteredSourceCompositionId,
        byte[] configZipArchive)
        BuildConfigurationZipArchiveFromPath(string sourcePath)
    {
        var loadCompositionResult =
            LoadComposition.LoadFromPathResolvingNetworkDependencies(sourcePath)
            .LogToActions(Console.WriteLine)
            .Extract(error => throw new Exception("Failed to load from path '" + sourcePath + "': " + error));

        var sourceTree = loadCompositionResult.tree;

        /*
        TODO: Provide a better way to avoid unnecessary files ending up in the config: Get the source files from git.
        */
        var filteredSourceTree =
            loadCompositionResult.origin?.FromLocalFileSystem is not null
            ?
            RemoveNoiseFromTreeComingFromLocalFileSystem(sourceTree)
            :
            sourceTree;

        var filteredSourceComposition = PineValueComposition.FromTreeWithStringPath(filteredSourceTree);

        var filteredSourceCompositionId = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(filteredSourceComposition));

        Console.WriteLine("Loaded source composition " + filteredSourceCompositionId + " from '" + sourcePath + "'.");

        var configZipArchive =
            BuildConfigurationZipArchive(sourceComposition: filteredSourceComposition);

        return (sourceTree, filteredSourceCompositionId, configZipArchive);
    }

    public static TreeNodeWithStringPath RemoveNoiseFromTreeComingFromLocalFileSystem(
        TreeNodeWithStringPath originalTree)
    {
        if (originalTree is not TreeNodeWithStringPath.TreeNode tree)
            return originalTree;

        TreeNodeWithStringPath? getValueFromStringName(string name) =>
            tree.Elements.FirstOrDefault(c => c.name == name).component;

        var elmJson = getValueFromStringName("elm.json");

        bool keepNode((string name, TreeNodeWithStringPath component) node)
        {
            if (elmJson != null && node.name == "elm-stuff")
                return false;

            return true;
        }

        return TreeNodeWithStringPath.SortedTree(
            treeContent:
                tree.Elements
                .Where(keepNode)
                .Select(child => (child.name, RemoveNoiseFromTreeComingFromLocalFileSystem(child.component))).ToImmutableList());
    }

    public static byte[] BuildConfigurationZipArchive(PineValue sourceComposition)
    {
        var parseSourceAsTree =
            PineValueComposition.ParseAsTreeWithStringPath(sourceComposition)
            .Extract(_ => throw new Exception("Failed to map source to tree."));

        var sourceFiles = PineValueComposition.TreeToFlatDictionaryWithPathComparer(parseSourceAsTree);

        return ZipArchive.ZipArchiveFromEntries(sourceFiles);
    }
}
