using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;

namespace Pine;

public static class PineValueComposition
{
    static public Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath> ParseAsTreeWithStringPath(PineValue composition)
    {
        static Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath> continueForListComposition(PineValue.ListValue compositionAsList)
        {
            var compositionResults =
                compositionAsList.Elements
                .Select((element, elementIndex) =>
                {
                    if (element is not PineValue.ListValue elementAsList || elementAsList.Elements.Count != 2)
                    {
                        return Result<IImmutableList<(int index, string name)>, (string name, TreeNodeWithStringPath element)>.err(
                            ImmutableList<(int index, string name)>.Empty);
                    }

                    if (PineValueAsString.StringFromValue(elementAsList.Elements.ElementAt(0)) is not Result<string, string>.Ok nameOk)
                    {
                        return Result<IImmutableList<(int index, string name)>, (string name, TreeNodeWithStringPath element)>.err(
                            ImmutableList<(int index, string name)>.Empty);
                    }

                    var currentIndexAndName =
                        (index: elementIndex, name: nameOk.Value);

                    return
                        ParseAsTreeWithStringPath(elementAsList.Elements.ElementAt(1)) switch
                        {
                            Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath>.Err err =>
                            Result<IImmutableList<(int index, string name)>, (string name, TreeNodeWithStringPath element)>.err(
                                ImmutableList.Create(currentIndexAndName).AddRange(err.Value)),

                            Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath>.Ok ok =>
                            Result<IImmutableList<(int index, string name)>, (string name, TreeNodeWithStringPath element)>.ok(
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
                Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath>.ok(
                    TreeNodeWithStringPath.Blob(compositionAsBlob.Bytes)),

                PineValue.ListValue compositionAsList =>
                continueForListComposition(compositionAsList),

                _ => throw new NotImplementedException("Incomplete match on sum type.")
            };
    }

    static public PineValue FromTreeWithStringPath(TreeNodeWithStringPath node) =>
        node switch
        {
            TreeNodeWithStringPath.BlobNode blob => PineValue.Blob(blob.Bytes),

            TreeNodeWithStringPath.TreeNode tree =>
            PineValue.List(
                tree.Elements
                .Select(treeElement =>
                    PineValue.List(
                        ImmutableList.Create(
                            PineValueAsString.ValueFromString(treeElement.name),
                            FromTreeWithStringPath(treeElement.component))
                    ))
                .ToImmutableList()),

            _ => throw new NotImplementedException("Incomplete match on sum type.")
        };

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobsWithCommonFilePath(
        IEnumerable<(string path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        SortedTreeFromSetOfBlobs(
            blobsWithPath.Select(blobWithPath =>
            {
                var pathElements =
                    blobWithPath.path.Split("/").SelectMany(pathElement => pathElement.Split(@"\"))
                    .ToImmutableList();

                return (path: (IImmutableList<string>)pathElements, blobWithPath.blobContent);
            })
        );

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobs<PathT>(
        IEnumerable<(IReadOnlyList<PathT> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath,
        Func<PathT, string> mapPathElement) =>
        SortedTreeFromSetOfBlobs(
            blobsWithPath.Select(blobWithPath =>
                (path: (IImmutableList<string>)blobWithPath.path.Select(mapPathElement).ToImmutableList(),
                blobWithPath.blobContent)));

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
        IEnumerable<(IReadOnlyList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        SortedTreeFromSetOfBlobs(blobsWithPath, pathComponent => pathComponent);

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> blobsWithPath) =>
        SortedTreeFromSetOfBlobsWithStringPath(
            blobsWithPath.Select(pathAndBlobContent => (path: pathAndBlobContent.Key, blobContent: pathAndBlobContent.Value)));

    static public TreeNodeWithStringPath SortedTreeFromTree(
        TreeNodeWithStringPath tree) => TreeNodeWithStringPath.Sort(tree);

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobs(
        IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        blobsWithPath.Aggregate(
            seed: TreeNodeWithStringPath.EmptyTree,
            func: (tree, blobPathAndContent) =>
            tree.SetNodeAtPathSorted(blobPathAndContent.path, TreeNodeWithStringPath.Blob(blobPathAndContent.blobContent)));

    static public Result<string, PineValue> Deserialize(
        ReadOnlyMemory<byte> serializedValue,
        Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>> loadSerializedValueByHash) =>
        Deserialize(serializedValue, loadSerializedValueByHash);

    static public Result<string, PineValue> Deserialize(
        ReadOnlyMemory<byte> serializedValue,
        Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>?> loadSerializedValueByHash)
    {
        var asciiStringUpToNull =
            System.Text.Encoding.ASCII.GetString(serializedValue.Span).Split('\0').First();

        var asciiStringUpToFirstSpace =
            asciiStringUpToNull.Split(' ').First();

        if (asciiStringUpToFirstSpace == "blob")
        {
            var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

            var expectedCount = serializedValue.Length - beginningToRemoveLength;

            var count = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

            if (count != expectedCount)
                return Result<string, PineValue>.err("Unexpected count: got " + count + ", but I expected " + expectedCount);

            return Result<string, PineValue>.ok(PineValue.Blob(serializedValue[beginningToRemoveLength..]));
        }

        if (asciiStringUpToFirstSpace == "list")
        {
            var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

            var remainingBytes = serializedValue.Slice(beginningToRemoveLength);

            var parsedElementCount = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

            var elementHashLength = 32;

            var expectedRemainingLength = parsedElementCount * elementHashLength;

            if (remainingBytes.Length != expectedRemainingLength)
                return Result<string, PineValue>.err(
                    "Unexpected remaining length: " + remainingBytes.Length + " instead of " + expectedRemainingLength);

            var elementsHashes =
                Enumerable.Range(0, parsedElementCount)
                .Select(elementIndex => remainingBytes.Slice(elementIndex * elementHashLength, elementHashLength))
                .ToImmutableList();

            Result<string, PineValue> TryLoadElementForHash(ReadOnlyMemory<byte> elementHash)
            {
                var loadedElementSerialRepresentation = loadSerializedValueByHash(elementHash);

                if (loadedElementSerialRepresentation == null)
                    return Result<string, PineValue>.err(
                        "Failed to load list element " + CommonConversion.StringBase16(elementHash));

                if (!SHA256.HashData(loadedElementSerialRepresentation.Value.Span).AsSpan().SequenceEqual(elementHash.Span))
                    return Result<string, PineValue>.err(
                        "Hash for loaded element does not match " + CommonConversion.StringBase16(elementHash));

                return Deserialize(loadedElementSerialRepresentation.Value, loadSerializedValueByHash);
            }

            var loadElementsResults =
                elementsHashes
                .Select(elementHash => (elementHash, loadResult: TryLoadElementForHash(elementHash)))
                .ToImmutableList();

            var firstFailed =
                loadElementsResults
                .FirstOrDefault(elementResult => elementResult.loadResult is Result<string, PineValue>.Err err);

            if (firstFailed.loadResult != null)
                return Result<string, PineValue>.err(
                    "Failed to load element " +
                    CommonConversion.StringBase16(firstFailed.elementHash) + ": " +
                    (firstFailed.loadResult as Result<string, PineValue>.Err)!.Value);

            return Result<string, PineValue>.ok(
                PineValue.List(
                    loadElementsResults
                    .Select(elementResult => elementResult.loadResult.Extract(error => throw new Exception(error))).ToImmutableList()));
        }

        return Result<string, PineValue>.err("Invalid prefix: '" + asciiStringUpToFirstSpace + "'.");
    }

    static public ReadOnlyMemory<byte> GetSerialRepresentation(PineValue pineValue) =>
        GetSerialRepresentationAndDependencies(pineValue).serialRepresentation;

    static public (ReadOnlyMemory<byte> serialRepresentation, IReadOnlyCollection<PineValue> dependencies)
        GetSerialRepresentationAndDependencies(PineValue pineValue)
    {
        if (pineValue is PineValue.BlobValue blobValue)
        {
            var prefix = System.Text.Encoding.ASCII.GetBytes("blob " + blobValue.Bytes.Length + "\0");

            var serialRepresentation = new byte[prefix.Length + blobValue.Bytes.Length];

            var blobValueArray = blobValue.Bytes.ToArray();

            Buffer.BlockCopy(prefix, 0, serialRepresentation, 0, prefix.Length);
            Buffer.BlockCopy(blobValueArray, 0, serialRepresentation, prefix.Length, blobValueArray.Length);

            return (serialRepresentation, ImmutableHashSet<PineValue>.Empty);
        }

        if (pineValue is PineValue.ListValue listValue)
        {
            var elementsHashes =
                listValue.Elements.Select(GetHash).ToList();

            var prefix = System.Text.Encoding.ASCII.GetBytes("list " + elementsHashes.Count + "\0");

            return
                (serialRepresentation: CommonConversion.Concat(new[] { (ReadOnlyMemory<byte>)prefix }.Concat(elementsHashes).ToList()).ToArray(),
                dependencies: listValue.Elements);
        }

        throw new Exception("Incomplete match on sum type.");
    }

    static public ReadOnlyMemory<byte> GetHashSorted(TreeNodeWithStringPath treeNode) =>
        GetHashNotSorted(TreeNodeWithStringPath.Sort(treeNode));

    static public ReadOnlyMemory<byte> GetHashNotSorted(TreeNodeWithStringPath treeNode) =>
        GetHash(FromTreeWithStringPath(treeNode));

    static public ReadOnlyMemory<byte> GetHash(PineValue pineValue) =>
        GetHashAndDependencies(pineValue).hash;

    static public (ReadOnlyMemory<byte> hash, IReadOnlyCollection<PineValue> dependencies)
        GetHashAndDependencies(PineValue pineValue)
    {
        var (serialRepresentation, dependencies) = GetSerialRepresentationAndDependencies(pineValue);

        return (hash: CommonConversion.HashSHA256(serialRepresentation), dependencies);
    }

    static public PineValue? FindComponentByHash(PineValue pineValue, ReadOnlyMemory<byte> hash)
    {
        if (GetHash(pineValue).Span.SequenceEqual(hash.Span))
            return pineValue;

        if (pineValue is PineValue.ListValue listValue)
        {
            foreach (var item in listValue.Elements)
            {
                var matchInItem = FindComponentByHash(item, hash);

                if (matchInItem != null)
                    return matchInItem;
            }
        }

        return null;
    }

    static public IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> TreeToFlatDictionaryWithPathComparer(
        TreeNodeWithStringPath tree) =>
        ToFlatDictionaryWithPathComparer(tree.EnumerateBlobsTransitive());

    static public IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> ToFlatDictionaryWithPathComparer(
        IEnumerable<(IImmutableList<string> filePath, ReadOnlyMemory<byte> fileContent)> filesBeforeSorting) =>
        ToFlatDictionaryWithPathComparer(
            filesBeforeSorting
            .Select(file => ((IReadOnlyList<string>)file.filePath, file.fileContent)).ToImmutableList());

    static public IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> ToFlatDictionaryWithPathComparer(
        IEnumerable<(IReadOnlyList<string> filePath, ReadOnlyMemory<byte> fileContent)> filesBeforeSorting) =>
        filesBeforeSorting.ToImmutableSortedDictionary(
            entry => entry.filePath,
            entry => entry.fileContent,
            keyComparer: EnumerableExtension.Comparer<IReadOnlyList<string>>());
}
