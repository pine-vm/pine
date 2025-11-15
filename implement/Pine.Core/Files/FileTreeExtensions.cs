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
    /// Generates human-readable overviews of a file tree, for use in command-line interfaces or logs.
    /// </summary>
    public static IEnumerable<string> DescribeFileTreeForHumans(
        FileTree treeNode,
        bool listFiles,
        string? extractFileName)
    {
        if (treeNode is FileTree.DirectoryNode directory)
        {
            var files = treeNode.EnumerateFilesTransitive().ToImmutableList();

            yield return
                "a directory containing " + files.Count + " files with an aggregate size of " +
                CommandLineInterface.FormatIntegerForDisplay(files.Sum(file => (long)file.fileContent.Length)) + " bytes.";

            if (listFiles)
            {
                yield return
                    "file paths, sizes and hashes:\n" +
                    string.Join(
                        "\n",
                        files.Select(fileAtPath =>
                        string.Join("/", fileAtPath.path) + " : " +
                        fileAtPath.fileContent.Length + " bytes, " +
                        Convert.ToHexStringLower(PineValueHashTree.ComputeHash(PineValue.Blob(fileAtPath.fileContent)).Span)[..10]));
            }

            yield break;
        }

        if (treeNode is FileTree.FileNode file)
        {
            yield return "a file containing " + file.Bytes.Length + " bytes";

            if (extractFileName is not null)
            {
                foreach (var extractedTree in CommonMappings.ExtractTreesFromNamedBlob(extractFileName, file.Bytes))
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

    /// <summary>
    /// Computes the union of multiple file trees. Later trees overwrite files from earlier trees at the same path.
    /// </summary>
    /// <param name="trees">Sequence of trees to merge.</param>
    public static FileTree Union(IEnumerable<FileTree> trees) =>
        trees
        .Aggregate(
            seed: FileTree.EmptyTree,
            func: Union);

    /// <summary>
    /// Computes the union of two file trees. Files in <paramref name="treeA"/> overwrite files in <paramref name="treeB"/> when paths collide.
    /// </summary>
    /// <param name="treeA">First tree whose files take precedence.</param>
    /// <param name="treeB">Second tree whose files are used when not overridden.</param>
    public static FileTree Union(
        FileTree treeA,
        FileTree treeB) =>
        treeA.EnumerateFilesTransitive()
        .Aggregate(
            seed: treeB,
            (tree, filePathAndContent) =>
            tree.SetNodeAtPathSorted(
                filePathAndContent.path,
                FileTree.File(filePathAndContent.fileContent)));

    /// <summary>
    /// Flattens a tree into a dictionary mapping file paths to contents using a structural path comparer.
    /// </summary>
    /// <param name="tree">The tree to flatten.</param>
    /// <returns>Immutable dictionary keyed by path element lists.</returns>
    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>
        ToFlatDictionaryWithPathComparer(this FileTree tree) =>
        ToFlatDictionaryWithPathComparer(tree.EnumerateFilesTransitive());

    /// <summary>
    /// Builds a dictionary from a sequence of files (path, content) using a path comparer. Paths are provided as immutable lists.
    /// </summary>
    /// <param name="filesBeforeSorting">Sequence of files with immutable list paths.</param>
    /// <returns>Immutable dictionary keyed by path element lists.</returns>
    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> ToFlatDictionaryWithPathComparer(
        IEnumerable<(IImmutableList<string> filePath, ReadOnlyMemory<byte> fileContent)> filesBeforeSorting) =>
        ToFlatDictionaryWithPathComparer(
            filesBeforeSorting
                .Select(file => ((IReadOnlyList<string>)file.filePath, file.fileContent)).ToImmutableList());

    /// <summary>
    /// Builds a dictionary from a sequence of files (path, content) using a path comparer. Paths are provided as read-only lists.
    /// </summary>
    /// <param name="filesBeforeSorting">Sequence of files with path and content.</param>
    /// <returns>Immutable dictionary keyed by path element lists.</returns>
    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> ToFlatDictionaryWithPathComparer(
        IEnumerable<(IReadOnlyList<string> filePath, ReadOnlyMemory<byte> fileContent)> filesBeforeSorting) =>
        filesBeforeSorting.ToImmutableSortedDictionary(
            entry => entry.filePath,
            entry => entry.fileContent,
            keyComparer: EnumerableExtensions.Comparer<IReadOnlyList<string>>());
}
