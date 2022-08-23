using System;
using System.Collections.Immutable;
using System.Linq;
using Pine;

namespace ElmFullstack.WebHost;

static public class BuildConfigurationFromArguments
{
    static public
        (TreeNodeWithStringPath sourceTree,
        string filteredSourceCompositionId,
        byte[] configZipArchive)
        BuildConfigurationZipArchiveFromPath(string sourcePath)
    {
        var loadCompositionResult =
            LoadComposition.LoadFromPathResolvingNetworkDependencies(sourcePath)
            .LogToActions(Console.WriteLine)
            .extract(error => throw new Exception("Failed to load from path '" + sourcePath + "': " + error));

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

        var filteredSourceComposition = Composition.FromTreeWithStringPath(filteredSourceTree);

        var filteredSourceCompositionId = CommonConversion.StringBase16(Composition.GetHash(filteredSourceComposition));

        Console.WriteLine("Loaded source composition " + filteredSourceCompositionId + " from '" + sourcePath + "'.");

        var configZipArchive =
            BuildConfigurationZipArchive(sourceComposition: filteredSourceComposition);

        return (sourceTree, filteredSourceCompositionId, configZipArchive);
    }

    static public TreeNodeWithStringPath RemoveNoiseFromTreeComingFromLocalFileSystem(
        TreeNodeWithStringPath originalTree)
    {
        if (originalTree.TreeContent == null)
            return originalTree;

        TreeNodeWithStringPath getComponentFromStringName(string name) =>
            originalTree.TreeContent.FirstOrDefault(c => c.name == name).component;

        var elmJson = getComponentFromStringName("elm.json");

        bool keepNode((string name, TreeNodeWithStringPath component) node)
        {
            if (elmJson != null && node.name == "elm-stuff")
                return false;

            return true;
        }

        return TreeNodeWithStringPath.SortedTree(
            treeContent:
                originalTree.TreeContent
                .Where(keepNode)
                .Select(child => (child.name, RemoveNoiseFromTreeComingFromLocalFileSystem(child.component))).ToImmutableList());
    }

    static public byte[] BuildConfigurationZipArchive(PineValue sourceComposition)
    {
        var parseSourceAsTree =
            Composition.ParseAsTreeWithStringPath(sourceComposition)
            .extract(error => throw new Exception("Failed to map source to tree."));

        var sourceFiles = Composition.TreeToFlatDictionaryWithPathComparer(parseSourceAsTree);

        return ZipArchive.ZipArchiveFromEntries(sourceFiles);
    }
}
