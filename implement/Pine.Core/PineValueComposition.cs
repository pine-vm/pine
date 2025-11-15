using Pine.Core.Files;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core;

public static class PineValueComposition
{
    public static Result<IReadOnlyList<(int index, string name)>, FileTree> ParseAsTreeWithStringPath(
        PineValue composition) =>
        FileTreeEncoding.Parse(composition);


    public static PineValue FromTreeWithStringPath(FileTree node) =>
        FileTreeEncoding.Encode(node);


    public static FileTree SortedTreeFromSetOfBlobsWithCommonFilePath(
        IEnumerable<(string path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        SortedTreeFromSetOfBlobs(
            blobsWithPath.Select(blobWithPath =>
            {
                var pathElements =
                    blobWithPath.path.Split("/").SelectMany(pathElement => pathElement.Split(@"\"))
                        .ToImmutableList();

                return (path: (IReadOnlyList<string>)pathElements, blobWithPath.blobContent);
            })
        );

    public static FileTree SortedTreeFromSetOfBlobsWithStringPath<PathT>(
        IEnumerable<(PathT path, ReadOnlyMemory<byte> blobContent)> blobsWithPath)
        where PathT : IReadOnlyList<string>
        =>
        SortedTreeFromSetOfBlobs(blobsWithPath);

    public static FileTree SortedTreeFromSetOfBlobsWithStringPath<PathT>(
        IReadOnlyDictionary<PathT, ReadOnlyMemory<byte>> blobsWithPath)
        where PathT : IReadOnlyList<string>
        =>
        SortedTreeFromSetOfBlobsWithStringPath(
            blobsWithPath.Select(pathAndBlobContent =>
                (path: pathAndBlobContent.Key, blobContent: pathAndBlobContent.Value)));

    public static FileTree SortedTreeFromTree(
        FileTree tree) => FileTree.Sort(tree);

    public static FileTree SortedTreeFromSetOfBlobs<PathT>(
        IEnumerable<(PathT path, ReadOnlyMemory<byte> blobContent)> blobsWithPath)
        where PathT : IReadOnlyList<string>
        =>
        blobsWithPath.Aggregate(
            seed: FileTree.EmptyTree,
            func: (tree, blobPathAndContent) =>
                tree.SetNodeAtPathSorted(blobPathAndContent.path,
                    FileTree.File(blobPathAndContent.blobContent)));

    public static FileTree Union(IEnumerable<FileTree> trees) =>
        trees
        .Aggregate(
            seed: FileTree.EmptyTree,
            func: Union);

    public static FileTree Union(
        FileTree treeA,
        FileTree treeB) =>
        treeA.EnumerateFilesTransitive()
        .Aggregate(
            seed: treeB,
            (tree, blobPathAndContent) =>
            tree.SetNodeAtPathSorted(
                blobPathAndContent.path,
                FileTree.File(blobPathAndContent.fileContent)));

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>
        TreeToFlatDictionaryWithPathComparer(
            FileTree tree) =>
        ToFlatDictionaryWithPathComparer(tree.EnumerateFilesTransitive());

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> ToFlatDictionaryWithPathComparer(
        IEnumerable<(IImmutableList<string> filePath, ReadOnlyMemory<byte> fileContent)> filesBeforeSorting) =>
        ToFlatDictionaryWithPathComparer(
            filesBeforeSorting
                .Select(file => ((IReadOnlyList<string>)file.filePath, file.fileContent)).ToImmutableList());

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> ToFlatDictionaryWithPathComparer(
        IEnumerable<(IReadOnlyList<string> filePath, ReadOnlyMemory<byte> fileContent)> filesBeforeSorting) =>
        filesBeforeSorting.ToImmutableSortedDictionary(
            entry => entry.filePath,
            entry => entry.fileContent,
            keyComparer: EnumerableExtensions.Comparer<IReadOnlyList<string>>());

    public static IEqualityComparer<T> FromDictionaryComparer<T>()
        where T : IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> =>
        new DictionaryAsTreeEqualityComparer<T>();

    private class DictionaryAsTreeEqualityComparer<T> : IEqualityComparer<T>
        where T : IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>
    {
        public bool Equals(T? x, T? y)
        {
            if (ReferenceEquals(x, y))
                return true;

            if (x is null || y is null)
                return false;

            return
                SortedTreeFromSetOfBlobsWithStringPath(x)
                .Equals(SortedTreeFromSetOfBlobsWithStringPath(y));
        }

        public int GetHashCode(T obj) =>
            obj?.GetHashCode() ?? 0;
    }
}
