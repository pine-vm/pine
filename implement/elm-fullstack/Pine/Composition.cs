using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;

namespace Pine;

public static class Composition
{
    static public PineValue ComponentFromString(string str) =>
        PineValue.List(ListValueFromString(str));

    static public IImmutableList<PineValue> ListValueFromString(string str) =>
        ToCodePoints(str)!
        .Select(charAsInteger => new System.Numerics.BigInteger(charAsInteger))
        .Select(ComponentFromUnsignedInteger)
        .Select(charResult => charResult.Extract(error => throw new Exception(error)))
        .ToImmutableList();


    // https://stackoverflow.com/questions/687359/how-would-you-get-an-array-of-unicode-code-points-from-a-net-string/28155130#28155130
    static public int[]? ToCodePoints(string str)
    {
        if (str == null)
            return null;

        var codePoints = new List<int>(str.Length);
        for (int i = 0; i < str.Length; i++)
        {
            codePoints.Add(char.ConvertToUtf32(str, i));
            if (char.IsHighSurrogate(str[i]))
                i += 1;
        }

        return codePoints.ToArray();
    }

    static public Result<string, string> StringFromComponent(PineValue component)
    {
        if (component is not PineValue.ListValue list)
            return Result<string, string>.err("Only a ListValue can represent a string.");

        var charsIntegersResults =
            list.Elements
            .Select(UnsignedIntegerFromComponent)
            .ToImmutableList();

        return
            charsIntegersResults
            .ListCombine()
            .Map(charsIntegersResults => string.Join("", charsIntegersResults.Select(toIntResult => char.ConvertFromUtf32((int)toIntResult))));
    }

    static public Result<string, PineValue> ComponentFromUnsignedInteger(System.Numerics.BigInteger integer) =>
        BlobValueFromUnsignedInteger(integer)
        .Map(PineValue.Blob);

    static public Result<string, ReadOnlyMemory<byte>> BlobValueFromUnsignedInteger(System.Numerics.BigInteger integer)
    {
        var signedBlobValue = BlobValueFromSignedInteger(integer);

        if (signedBlobValue.Span[0] != 4)
            return Result<string, ReadOnlyMemory<byte>>.err("Argument is a negative integer.");

        return Result<string, ReadOnlyMemory<byte>>.ok(signedBlobValue[1..]);
    }

    static public PineValue ComponentFromSignedInteger(System.Numerics.BigInteger integer) =>
        PineValue.Blob(BlobValueFromSignedInteger(integer));

    static public ReadOnlyMemory<byte> BlobValueFromSignedInteger(System.Numerics.BigInteger integer)
    {
        var absoluteValue = System.Numerics.BigInteger.Abs(integer);

        var signByte =
            (byte)(absoluteValue == integer ? 4 : 2);

        var absoluteArray = absoluteValue.ToByteArray(isUnsigned: true, isBigEndian: true);

        var memory = new byte[1 + absoluteArray.Length];

        memory[0] = signByte;
        absoluteArray.CopyTo(memory, 1);

        return memory;
    }

    static public Result<string, System.Numerics.BigInteger> SignedIntegerFromComponent(PineValue component)
    {
        if (component is not PineValue.BlobValue blob)
            return Result<string, System.Numerics.BigInteger>.err(
                "Only a BlobValue can represent an integer.");

        return SignedIntegerFromBlobValue(blob.Bytes.Span);
    }

    static public Result<string, System.Numerics.BigInteger> SignedIntegerFromBlobValue(ReadOnlySpan<byte> blobValue)
    {
        if (blobValue.Length < 1)
            return Result<string, System.Numerics.BigInteger>.err(
                "Empty blob is not a valid integer because the sign byte is missing. Did you mean to use an unsigned integer?");

        var signByte = blobValue[0];

        if (signByte != 4 && signByte != 2)
            return Result<string, System.Numerics.BigInteger>.err(
                "Unexpected value for sign byte of integer: " + signByte);

        var isNegative = signByte != 4;

        var integerValue = UnsignedIntegerFromBlobValue(blobValue[1..]);

        return
            Result<string, System.Numerics.BigInteger>.ok(
                integerValue * new System.Numerics.BigInteger(isNegative ? -1 : 1));
    }

    static public Result<string, System.Numerics.BigInteger> UnsignedIntegerFromComponent(PineValue component) =>
        component switch
        {
            PineValue.BlobValue blob => Result<string, System.Numerics.BigInteger>.ok(UnsignedIntegerFromBlobValue(blob.Bytes.Span)),
            _ => Result<string, System.Numerics.BigInteger>.err("Only a BlobValue can represent an integer.")
        };

    static public System.Numerics.BigInteger UnsignedIntegerFromBlobValue(ReadOnlySpan<byte> blobValue) =>
        new(blobValue, isUnsigned: true, isBigEndian: true);

    static public Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath> ParseAsTreeWithStringPath(PineValue composition)
    {
        static Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath> continueForListComposition(PineValue.ListValue compositionAsList)
        {
            var compositionResults =
                compositionAsList.Elements
                .Select((component, componentIndex) =>
                {
                    if (component is not PineValue.ListValue componentAsList || componentAsList.Elements.Count != 2)
                    {
                        return Result<IImmutableList<(int index, string name)>, (string name, TreeNodeWithStringPath component)>.err(
                            ImmutableList<(int index, string name)>.Empty);
                    }

                    if (StringFromComponent(componentAsList.Elements.ElementAt(0)) is not Result<string, string>.Ok nameOk)
                    {
                        return Result<IImmutableList<(int index, string name)>, (string name, TreeNodeWithStringPath component)>.err(
                            ImmutableList<(int index, string name)>.Empty);
                    }

                    var currentIndexAndName =
                        (index: componentIndex, name: nameOk.Value);

                    return
                        ParseAsTreeWithStringPath(componentAsList.Elements.ElementAt(1)) switch
                        {
                            Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath>.Err err =>
                            Result<IImmutableList<(int index, string name)>, (string name, TreeNodeWithStringPath component)>.err(
                                ImmutableList.Create(currentIndexAndName).AddRange(err.Value)),

                            Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath>.Ok ok =>
                            Result<IImmutableList<(int index, string name)>, (string name, TreeNodeWithStringPath component)>.ok(
                                (name: currentIndexAndName.name, ok.Value)),

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

    static public PineValue FromTreeWithStringPath(TreeNodeWithStringPath tree)
    {
        if (tree.BlobContent != null)
            return PineValue.Blob(tree.BlobContent.Value);

        if (tree.TreeContent != null)
        {
            var listContent =
                tree.TreeContent
                .Select(treeComponent =>
                    PineValue.List(
                        ImmutableList.Create(
                            ComponentFromString(treeComponent.name),
                            FromTreeWithStringPath(treeComponent.component))
                    ))
                .ToImmutableList();

            return PineValue.List(listContent);
        }

        throw new Exception("Incomplete match on sum type.");
    }

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobsWithCommonFilePath(
        IEnumerable<(string path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        SortedTreeFromSetOfBlobs(
            blobsWithPath.Select(blobWithPath =>
            {
                var pathComponents =
                    blobWithPath.path.Split("/").SelectMany(pathComponent => pathComponent.Split(@"\"))
                    .ToImmutableList();

                return (path: (IImmutableList<string>)pathComponents, blobContent: blobWithPath.blobContent);
            })
        );

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobs<PathT>(
        IEnumerable<(IImmutableList<PathT> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath,
        Func<PathT, string> mapPathComponent) =>
        SortedTreeFromSetOfBlobs(
            blobsWithPath.Select(blobWithPath =>
                (path: (IImmutableList<string>)blobWithPath.path.Select(mapPathComponent).ToImmutableList(),
                blobContent: blobWithPath.blobContent)));

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
        IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        SortedTreeFromSetOfBlobs(blobsWithPath, pathComponent => pathComponent);

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
        IReadOnlyDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> blobsWithPath) =>
        SortedTreeFromSetOfBlobsWithStringPath(
            blobsWithPath.Select(pathAndBlobContent => (path: pathAndBlobContent.Key, blobContent: pathAndBlobContent.Value)));

    static public TreeNodeWithStringPath SortedTreeFromTree(
        TreeNodeWithStringPath tree) =>
        tree.BlobContent != null
        ?
        tree
        :
        SortedTreeFromSetOfBlobs(tree.EnumerateBlobsTransitive());

    static public TreeNodeWithStringPath SortedTreeFromSetOfBlobs(
        IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        TreeNodeWithStringPath.SortedTree(treeContent: SortedTreeContentFromSetOfBlobs(blobsWithPath));

    static public IImmutableList<(string name, TreeNodeWithStringPath obj)> SortedTreeContentFromSetOfBlobs(
        IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        blobsWithPath
        .Aggregate(
            (IImmutableList<(string name, TreeNodeWithStringPath obj)>)
            ImmutableList<(string name, TreeNodeWithStringPath obj)>.Empty,
            (intermediateResult, nextBlob) => SetBlobAtPathSorted(intermediateResult, nextBlob.path, nextBlob.blobContent));

    static public IImmutableList<(string name, TreeNodeWithStringPath obj)> SetBlobAtPathSorted(
        IImmutableList<(string name, TreeNodeWithStringPath obj)> treeContentBefore,
        IImmutableList<string> path,
        ReadOnlyMemory<byte> blobContent)
    {
        var pathFirstElement = path.First();

        var componentBefore =
            treeContentBefore.FirstOrDefault(c => c.name.SequenceEqual(pathFirstElement)).obj;

        var component =
            path.Count < 2
            ?
            TreeNodeWithStringPath.Blob(blobContent: blobContent)
            :
            TreeNodeWithStringPath.SortedTree(
                treeContent:
                    SetBlobAtPathSorted(
                        componentBefore?.TreeContent ?? ImmutableList<(string name, TreeNodeWithStringPath obj)>.Empty,
                        path.RemoveAt(0),
                        blobContent));

        return
            treeContentBefore
            .RemoveAll(c => c.name == pathFirstElement)
            .Add((pathFirstElement, component))
            .OrderBy(c => c.name, TreeNodeWithStringPath.TreeEntryNameComparer)
            .ToImmutableList();
    }

    static public Result<string, PineValue> Deserialize(
        ReadOnlyMemory<byte> serializedComponent,
        Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>> loadSerializedComponentByHash) =>
        Deserialize(serializedComponent, loadSerializedComponentByHash);

    static public Result<string, PineValue> Deserialize(
        ReadOnlyMemory<byte> serializedComponent,
        Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>?> loadSerializedComponentByHash)
    {
        var asciiStringUpToNull =
            System.Text.Encoding.ASCII.GetString(serializedComponent.Span).Split('\0').First();

        var asciiStringUpToFirstSpace =
            asciiStringUpToNull.Split(' ').First();

        if (asciiStringUpToFirstSpace == "blob")
        {
            var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

            var expectedCount = serializedComponent.Length - beginningToRemoveLength;

            var count = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

            if (count != expectedCount)
                return Result<string, PineValue>.err("Unexpected count: got " + count + ", but I expected " + expectedCount);

            return Result<string, PineValue>.ok(PineValue.Blob(serializedComponent.Slice(beginningToRemoveLength)));
        }

        if (asciiStringUpToFirstSpace == "list")
        {
            var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

            var remainingBytes = serializedComponent.Slice(beginningToRemoveLength);

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
                var loadedElementSerialRepresentation = loadSerializedComponentByHash(elementHash);

                if (loadedElementSerialRepresentation == null)
                    return Result<string, PineValue>.err(
                        "Failed to load list element " + CommonConversion.StringBase16(elementHash));

                if (!SHA256.HashData(loadedElementSerialRepresentation.Value.Span).AsSpan().SequenceEqual(elementHash.Span))
                    return Result<string, PineValue>.err(
                        "Hash for loaded element does not match " + CommonConversion.StringBase16(elementHash));

                return Deserialize(loadedElementSerialRepresentation.Value, loadSerializedComponentByHash);
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

    static public ReadOnlyMemory<byte> GetSerialRepresentation(PineValue component) =>
        GetSerialRepresentationAndDependencies(component).serialRepresentation;

    static public (ReadOnlyMemory<byte> serialRepresentation, IReadOnlyCollection<PineValue> dependencies)
        GetSerialRepresentationAndDependencies(PineValue component)
    {
        if (component is PineValue.BlobValue blobComponent)
        {
            var prefix = System.Text.Encoding.ASCII.GetBytes("blob " + blobComponent.Bytes.Length.ToString() + "\0");

            var serialRepresentation = new byte[prefix.Length + blobComponent.Bytes.Length];

            var componentBlobContentArray = blobComponent.Bytes.ToArray();

            Buffer.BlockCopy(prefix, 0, serialRepresentation, 0, prefix.Length);
            Buffer.BlockCopy(componentBlobContentArray, 0, serialRepresentation, prefix.Length, componentBlobContentArray.Length);

            return (serialRepresentation, ImmutableHashSet<PineValue>.Empty);
        }

        if (component is PineValue.ListValue listComponent)
        {
            var componentsHashes =
                listComponent.Elements.Select(GetHash).ToList();

            var prefix = System.Text.Encoding.ASCII.GetBytes("list " + componentsHashes.Count.ToString() + "\0");

            return
                (serialRepresentation: CommonConversion.Concat(new[] { (ReadOnlyMemory<byte>)prefix }.Concat(componentsHashes).ToList()).ToArray(),
                dependencies: listComponent.Elements);
        }

        throw new System.Exception("Incomplete match on sum type.");
    }

    static public ReadOnlyMemory<byte> GetHash(PineValue component) =>
        GetHashAndDependencies(component).hash;

    static public (ReadOnlyMemory<byte> hash, IReadOnlyCollection<PineValue> dependencies)
        GetHashAndDependencies(PineValue component)
    {
        var (serialRepresentation, dependencies) = GetSerialRepresentationAndDependencies(component);

        return (hash: CommonConversion.HashSHA256(serialRepresentation), dependencies: dependencies);
    }

    static public ReadOnlyMemory<byte> GetHash(TreeNodeWithStringPath component) =>
        CommonConversion.HashSHA256(GetSerialRepresentation(FromTreeWithStringPath(component)!));

    static public PineValue? FindComponentByHash(PineValue component, ReadOnlyMemory<byte> hash)
    {
        if (GetHash(component).Span.SequenceEqual(hash.Span))
            return component;

        if (component is PineValue.ListValue listComponent)
        {
            foreach (var item in listComponent.Elements)
            {
                var matchInItem = FindComponentByHash(item, hash);

                if (matchInItem != null)
                    return matchInItem;
            }
        }

        return null;
    }

    static public IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> TreeToFlatDictionaryWithPathComparer(
        TreeNodeWithStringPath tree) =>
        ToFlatDictionaryWithPathComparer(tree.EnumerateBlobsTransitive());

    static public IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> ToFlatDictionaryWithPathComparer(
        IEnumerable<(IImmutableList<string> filePath, ReadOnlyMemory<byte> fileContent)> filesBeforeSorting) =>
        filesBeforeSorting.ToImmutableSortedDictionary(
            entry => entry.filePath,
            entry => entry.fileContent,
            EnumerableExtension.Comparer<IImmutableList<string>>());
}
