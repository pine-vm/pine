using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine;

public static class PineValueComposition
{
    public static Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath> ParseAsTreeWithStringPath(
        PineValue composition)
    {
        static Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath> continueForListComposition(
            PineValue.ListValue compositionAsList)
        {
            var compositionResults =
                compositionAsList.Elements
                    .Select((element, elementIndex) =>
                    {
                        if (element is not PineValue.ListValue elementAsList || elementAsList.Elements.Count != 2)
                        {
                            return Result<IReadOnlyList<(int index, string name)>, (string name, TreeNodeWithStringPath
                                element)>.err([]);
                        }

                        if (PineValueAsString.StringFromValue(elementAsList.Elements.ElementAt(0)) is not
                            Result<string, string>.Ok nameOk)
                        {
                            return Result<IReadOnlyList<(int index, string name)>, (string name, TreeNodeWithStringPath
                                element)>.err([]);
                        }

                        var currentIndexAndName =
                            (index: elementIndex, name: nameOk.Value);

                        return
                            ParseAsTreeWithStringPath(elementAsList.Elements.ElementAt(1)) switch
                            {
                                Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath>.Err err =>
                                    Result<IReadOnlyList<(int index, string name)>, (string name,
                                        TreeNodeWithStringPath element)>.err(
                                        [currentIndexAndName, .. err.Value]),

                                Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath>.Ok ok =>
                                    Result<IReadOnlyList<(int index, string name)>, (string name,
                                        TreeNodeWithStringPath element)>.ok(
                                        (currentIndexAndName.name, ok.Value)),

                                _ => throw new NotImplementedException()
                            };
                    })
                    .ToImmutableList();

            return
                compositionResults
                    .ListCombine()
                    .Map(compositionOk => TreeNodeWithStringPath.SortedTree(compositionOk.ToImmutableList()));
        }

        return
            composition switch
            {
                PineValue.BlobValue compositionAsBlob =>
                    Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath>.ok(
                        TreeNodeWithStringPath.Blob(compositionAsBlob.Bytes)),

                PineValue.ListValue compositionAsList =>
                    continueForListComposition(compositionAsList),

                _ => throw new NotImplementedException("Incomplete match on sum type.")
            };
    }

    public static PineValue FromTreeWithStringPath(TreeNodeWithStringPath node) =>
        node switch
        {
            TreeNodeWithStringPath.BlobNode blob => PineValue.Blob(blob.Bytes),

            TreeNodeWithStringPath.TreeNode tree =>
                PineValue.List(
                    tree.Elements
                        .Select(treeElement =>
                            PineValue.List(
                                [
                                    PineValueAsString.ValueFromString(treeElement.name),
                                    FromTreeWithStringPath(treeElement.component)
                                ]
                            ))
                        .ToImmutableList()),

            _ => throw new NotImplementedException("Incomplete match on sum type.")
        };

    public static TreeNodeWithStringPath SortedTreeFromSetOfBlobsWithCommonFilePath(
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

    public static TreeNodeWithStringPath SortedTreeFromSetOfBlobs<PathT>(
        IEnumerable<(IReadOnlyList<PathT> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath,
        Func<PathT, string> mapPathElement) =>
        SortedTreeFromSetOfBlobs(
            blobsWithPath.Select(blobWithPath =>
                (path: (IReadOnlyList<string>)blobWithPath.path.Select(mapPathElement).ToImmutableList(),
                    blobWithPath.blobContent)));

    public static TreeNodeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
        IEnumerable<(IReadOnlyList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        SortedTreeFromSetOfBlobs(blobsWithPath, pathComponent => pathComponent);

    public static TreeNodeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> blobsWithPath) =>
        SortedTreeFromSetOfBlobsWithStringPath(
            blobsWithPath.Select(pathAndBlobContent =>
                (path: pathAndBlobContent.Key, blobContent: pathAndBlobContent.Value)));

    public static TreeNodeWithStringPath SortedTreeFromTree(
        TreeNodeWithStringPath tree) => TreeNodeWithStringPath.Sort(tree);

    public static TreeNodeWithStringPath SortedTreeFromSetOfBlobs(
        IEnumerable<(IReadOnlyList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        blobsWithPath.Aggregate(
            seed: TreeNodeWithStringPath.EmptyTree,
            func: (tree, blobPathAndContent) =>
                tree.SetNodeAtPathSorted(blobPathAndContent.path,
                    TreeNodeWithStringPath.Blob(blobPathAndContent.blobContent)));

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>
        TreeToFlatDictionaryWithPathComparer(
            TreeNodeWithStringPath tree) =>
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
            keyComparer: EnumerableExtension.Comparer<IReadOnlyList<string>>());

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
