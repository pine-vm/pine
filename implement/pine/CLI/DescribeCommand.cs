using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using System;
using System.CommandLine;
using System.IO;
using System.Linq;

namespace Pine.CLI;

public static class DescribeCommand
{
    public static Command Create()
    {
        var command =
            new Command(
                "describe",
                "Describe the artifact at the given location. Valid locations can also be URLs into git repositories or paths in the local file system.");

        var sourcePathParameter = new Argument<string>("source-path");

        var listBlobsOption = new Option<bool>("--list-blobs");

        var compileZipArchiveOption =
            new Option<string?>("--compile-zip-archive")
            {
                Arity = ArgumentArity.ZeroOrOne
            };

        command.Add(sourcePathParameter);
        command.Add(listBlobsOption);
        command.Add(compileZipArchiveOption);

        command.SetAction(
            (parseResult) =>
            {
                var sourcePath = parseResult.GetValue(sourcePathParameter);
                var listBlobs = parseResult.GetValue(listBlobsOption);
                var compileZipArchive = parseResult.GetValue(compileZipArchiveOption);

                var loadCompositionResult =
                    LoadComposition.LoadFromPathResolvingNetworkDependencies(sourcePath)
                    .LogToActions(Console.WriteLine)
                    .Extract(error => throw new Exception("Failed to load from path '" + sourcePath + "': " + error));

                var composition = FileTreeEncoding.Encode(loadCompositionResult.tree);

                var compositionId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(composition).Span);

                Console.WriteLine("Loaded composition " + compositionId + " from '" + sourcePath + "'.");

                var compositionDescription =
                    string.Join(
                        "\n",
                        FileTreeExtensions.DescribeFileTreeForHumans(
                            loadCompositionResult.tree,
                            listFiles: listBlobs,
                            extractFileName: sourcePath.Split('\\', '/').Last()));

                Console.WriteLine("Composition " + compositionId + " is " + compositionDescription);

                if (compileZipArchive != null)
                {
                    var asZipArchive =
                        ZipArchive.ZipArchiveFromFiles(
                            loadCompositionResult.tree.EnumerateFilesTransitive()
                            .Select(entry => (string.Join("/", entry.path), entry.fileContent)));

                    var defaultFileName = compositionId + ".zip";

                    var destinationPath = compileZipArchive.Length > 0 ? compileZipArchive : defaultFileName;

                    if (Directory.Exists(destinationPath))
                        destinationPath = Path.Combine(destinationPath, defaultFileName);

                    File.WriteAllBytes(destinationPath, asZipArchive);
                    Console.WriteLine("Saved " + compositionId[..10] + " to " + destinationPath);
                }

                return 0;
            });

        return command;
    }
}
