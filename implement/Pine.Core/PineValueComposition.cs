using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core;

public static class PineValueComposition
{
    public static Result<IReadOnlyList<(int index, string name)>, BlobTreeWithStringPath> ParseAsTreeWithStringPath(
        PineValue composition) =>
        FileTreeEncoding.Parse(composition);


    public static PineValue FromTreeWithStringPath(BlobTreeWithStringPath node) =>
        FileTreeEncoding.Encode(node);


    public static BlobTreeWithStringPath SortedTreeFromSetOfBlobsWithCommonFilePath(
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

    public static BlobTreeWithStringPath SortedTreeFromSetOfBlobsWithStringPath<PathT>(
        IEnumerable<(PathT path, ReadOnlyMemory<byte> blobContent)> blobsWithPath)
        where PathT : IReadOnlyList<string>
        =>
        SortedTreeFromSetOfBlobs(blobsWithPath);

    public static BlobTreeWithStringPath SortedTreeFromSetOfBlobsWithStringPath<PathT>(
        IReadOnlyDictionary<PathT, ReadOnlyMemory<byte>> blobsWithPath)
        where PathT : IReadOnlyList<string>
        =>
        SortedTreeFromSetOfBlobsWithStringPath(
            blobsWithPath.Select(pathAndBlobContent =>
                (path: pathAndBlobContent.Key, blobContent: pathAndBlobContent.Value)));

    public static BlobTreeWithStringPath SortedTreeFromTree(
        BlobTreeWithStringPath tree) => BlobTreeWithStringPath.Sort(tree);

    public static BlobTreeWithStringPath SortedTreeFromSetOfBlobs<PathT>(
        IEnumerable<(PathT path, ReadOnlyMemory<byte> blobContent)> blobsWithPath)
        where PathT : IReadOnlyList<string>
        =>
        blobsWithPath.Aggregate(
            seed: BlobTreeWithStringPath.EmptyTree,
            func: (tree, blobPathAndContent) =>
                tree.SetNodeAtPathSorted(blobPathAndContent.path,
                    BlobTreeWithStringPath.Blob(blobPathAndContent.blobContent)));

    public static BlobTreeWithStringPath Union(IEnumerable<BlobTreeWithStringPath> trees) =>
        trees
        .Aggregate(
            seed: BlobTreeWithStringPath.EmptyTree,
            func: Union);

    public static BlobTreeWithStringPath Union(
        BlobTreeWithStringPath treeA,
        BlobTreeWithStringPath treeB) =>
        treeA.EnumerateBlobsTransitive()
        .Aggregate(
            seed: treeB,
            (tree, blobPathAndContent) =>
            tree.SetNodeAtPathSorted(
                blobPathAndContent.path,
                BlobTreeWithStringPath.Blob(blobPathAndContent.blobContent)));

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>
        TreeToFlatDictionaryWithPathComparer(
            BlobTreeWithStringPath tree) =>
        ToFlatDictionaryWithPathComparer(tree.EnumerateBlobsTransitive());

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
