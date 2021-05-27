using System;
using System.Collections.Immutable;
using System.Linq;
using Pine;

namespace ElmFullstack.WebHost
{
    static public class BuildConfigurationFromArguments
    {
        static public (
            Composition.TreeWithStringPath sourceTree,
            string filteredSourceCompositionId,
            byte[] configZipArchive)
            BuildConfigurationZipArchiveFromPath(string sourcePath)
        {
            var loadFromPathResult = LoadFromPath.LoadTreeFromPath(sourcePath);

            if (loadFromPathResult?.Ok == null)
            {
                throw new Exception("Failed to load from path '" + sourcePath + "': " + loadFromPathResult?.Err);
            }

            var sourceTree = loadFromPathResult.Ok.tree;

            /*
            TODO: Provide a better way to avoid unnecessary files ending up in the config: Get the source files from git.
            */
            var filteredSourceTree =
                loadFromPathResult.Ok.comesFromLocalFilesystem
                ?
                RemoveNoiseFromTreeComingFromLocalFileSystem(sourceTree)
                :
                sourceTree;

            var filteredSourceComposition = Composition.FromTreeWithStringPath(filteredSourceTree);

            var filteredSourceCompositionId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(filteredSourceComposition));

            Console.WriteLine("Loaded source composition " + filteredSourceCompositionId + " from '" + sourcePath + "'.");

            var configZipArchive =
                BuildConfigurationZipArchive(sourceComposition: filteredSourceComposition);

            return (sourceTree, filteredSourceCompositionId, configZipArchive);
        }

        static public Composition.TreeWithStringPath RemoveNoiseFromTreeComingFromLocalFileSystem(
            Composition.TreeWithStringPath originalTree)
        {
            if (originalTree.TreeContent == null)
                return originalTree;

            Composition.TreeWithStringPath getComponentFromStringName(string name) =>
                originalTree.TreeContent.FirstOrDefault(c => c.name == name).component;

            var elmJson = getComponentFromStringName("elm.json");

            bool keepNode((string name, Composition.TreeWithStringPath component) node)
            {
                if (elmJson != null && node.name == "elm-stuff")
                    return false;

                return true;
            }

            return Composition.TreeWithStringPath.Tree(
                treeContent:
                    originalTree.TreeContent
                    .Where(keepNode)
                    .Select(child => (child.name, RemoveNoiseFromTreeComingFromLocalFileSystem(child.component))).ToImmutableList());
        }

        static public byte[] BuildConfigurationZipArchive(Composition.Component sourceComposition)
        {
            var parseSourceAsTree = Composition.ParseAsTreeWithStringPath(sourceComposition);

            if (parseSourceAsTree.Ok == null)
                throw new Exception("Failed to map source to tree.");

            var sourceFiles =
                Composition.TreeToFlatDictionaryWithPathComparer(parseSourceAsTree.Ok);

            return ZipArchive.ZipArchiveFromEntries(sourceFiles);
        }
    }
}
