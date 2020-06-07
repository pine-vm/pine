using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;

namespace Kalmit.PersistentProcess.WebHost
{
    static public class BuildConfigurationFromArguments
    {
        public const string ElmAppSubdirectoryName = "elm-app";

        static public (
            string sourceCompositionId,
            byte[] configZipArchive)
            BuildConfigurationZipArchiveFromPath(string sourcePath)
        {
            var loadFromPathResult = LoadFromPath.LoadTreeFromPath(sourcePath);

            if (loadFromPathResult?.Ok == null)
            {
                throw new Exception("Failed to load from path '" + sourcePath + "': " + loadFromPathResult?.Err);
            }

            var sourceComposition = Composition.FromTree(loadFromPathResult.Ok);

            var sourceCompositionId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(sourceComposition));

            Console.WriteLine("Loaded source composition " + sourceCompositionId + " from '" + sourcePath + "'.");

            var configZipArchive =
                BuildConfigurationZipArchive(sourceComposition: sourceComposition);

            return
                (sourceCompositionId: sourceCompositionId,
                configZipArchive: configZipArchive);
        }

        static public byte[] BuildConfigurationZipArchive(Composition.Component sourceComposition)
        {
            var parseSourceAsTree = Composition.ParseAsTree(sourceComposition);

            if (parseSourceAsTree.Ok == null)
                throw new Exception("Failed to map source to tree.");

            var sourceFiles =
                ElmApp.ToFlatDictionaryWithPathComparer(
                    parseSourceAsTree.Ok.EnumerateBlobsTransitive()
                    .Select(sourceFilePathAndContent =>
                        (path: (IImmutableList<string>)sourceFilePathAndContent.path.Select(pathComponent => Encoding.UTF8.GetString(pathComponent.ToArray())).ToImmutableList(),
                        sourceFilePathAndContent.blobContent))
                        .ToImmutableList());

            return ZipArchive.ZipArchiveFromEntries(sourceFiles);
        }

        static string FindDirectoryUpwardContainingElmJson(string searchBeginDirectory)
        {
            var currentDirectory = searchBeginDirectory;

            while (true)
            {
                if (!(0 < currentDirectory?.Length))
                    return null;

                if (File.Exists(Path.Combine(currentDirectory, "elm.json")))
                    return currentDirectory;

                currentDirectory = Path.GetDirectoryName(currentDirectory);
            }
        }
    }
}
