using Pine.Core.Addressing;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Files;

/// <summary>
/// Extension methods for <see cref="BlobTreeWithStringPath"/>.
/// </summary>
public static class FileTreeExtensions
{
    /// <summary>
    /// Generates human-readable overviews of a file tree or blob, for use in command-line interfaces or logs.
    /// </summary>
    public static IEnumerable<string> DescribeFileTreeForHumans(
        BlobTreeWithStringPath composition,
        bool listFiles,
        string? extractFileName)
    {
        if (composition is BlobTreeWithStringPath.TreeNode tree)
        {
            var blobs = composition.EnumerateBlobsTransitive().ToImmutableList();

            yield return
                "a directory containing " + blobs.Count + " files with an aggregate size of " +
                CommandLineInterface.FormatIntegerForDisplay(blobs.Sum(blob => (long)blob.blobContent.Length)) + " bytes.";

            if (listFiles)
            {
                yield return
                    "file paths, sizes and hashes:\n" +
                    string.Join(
                        "\n",
                        blobs.Select(blobAtPath =>
                        string.Join("/", blobAtPath.path) + " : " +
                        blobAtPath.blobContent.Length + " bytes, " +
                        Convert.ToHexStringLower(PineValueHashTree.ComputeHash(PineValue.Blob(blobAtPath.blobContent)).Span)[..10]));
            }

            yield break;
        }

        if (composition is BlobTreeWithStringPath.BlobNode blob)
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
