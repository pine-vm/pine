using Pine.Core.Addressing;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Files;

/// <summary>
/// Extension methods for <see cref="FileTree"/>.
/// </summary>
public static class FileTreeExtensions
{
    /// <summary>
    /// Generates human-readable overviews of a file tree or blob, for use in command-line interfaces or logs.
    /// </summary>
    public static IEnumerable<string> DescribeFileTreeForHumans(
        FileTree composition,
        bool listFiles,
        string? extractFileName)
    {
        if (composition is FileTree.DirectoryNode tree)
        {
            var blobs = composition.EnumerateFilesTransitive().ToImmutableList();

            yield return
                "a directory containing " + blobs.Count + " files with an aggregate size of " +
                CommandLineInterface.FormatIntegerForDisplay(blobs.Sum(blob => (long)blob.fileContent.Length)) + " bytes.";

            if (listFiles)
            {
                yield return
                    "file paths, sizes and hashes:\n" +
                    string.Join(
                        "\n",
                        blobs.Select(blobAtPath =>
                        string.Join("/", blobAtPath.path) + " : " +
                        blobAtPath.fileContent.Length + " bytes, " +
                        Convert.ToHexStringLower(PineValueHashTree.ComputeHash(PineValue.Blob(blobAtPath.fileContent)).Span)[..10]));
            }

            yield break;
        }

        if (composition is FileTree.FileNode blob)
        {
            yield return "a blob containing " + blob.Bytes.Length + " bytes";

            if (extractFileName is not null)
            {
                foreach (var extractedTree in CommonMappings.ExtractTreesFromNamedBlob(extractFileName, blob.Bytes))
                {
                    var extractedTreeCompositionId =
                        Convert.ToHexStringLower(PineValueHashTree.ComputeHashNotSorted(extractedTree).Span);

                    var compositionDescription =
                        string.Join(
                            "\n",
                            DescribeFileTreeForHumans(
                                extractedTree,
                                listFiles: listFiles,
                                extractFileName: null));

                    yield return "Extracted composition " + extractedTreeCompositionId + ", which is " + compositionDescription;
                }
            }
        }
    }
}
