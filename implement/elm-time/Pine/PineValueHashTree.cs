using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;

namespace Pine;

static public class PineValueHashTree
{
    static public ReadOnlyMemory<byte> ComputeHash(PineValue pineValue) =>
        ComputeHashAndDependencies(pineValue).hash;

    static public (ReadOnlyMemory<byte> hash, IReadOnlyCollection<PineValue> dependencies)
        ComputeHashAndDependencies(PineValue pineValue)
    {
        var (serialRepresentation, dependencies) = ComputeHashTreeNodeSerialRepresentation(pineValue);

        return (hash: CommonConversion.HashSHA256(serialRepresentation), dependencies);
    }

    static public (ReadOnlyMemory<byte> serialRepresentation, IReadOnlyCollection<PineValue> dependencies)
        ComputeHashTreeNodeSerialRepresentation(PineValue pineValue)
    {
        switch (pineValue)
        {
            case PineValue.BlobValue blobValue:
                {
                    var prefix = System.Text.Encoding.ASCII.GetBytes("blob " + blobValue.Bytes.Length + "\0");

                    var serialRepresentation = new byte[prefix.Length + blobValue.Bytes.Length];

                    var blobValueArray = blobValue.Bytes.ToArray();

                    Buffer.BlockCopy(prefix, 0, serialRepresentation, 0, prefix.Length);
                    Buffer.BlockCopy(blobValueArray, 0, serialRepresentation, prefix.Length, blobValueArray.Length);

                    return (serialRepresentation, ImmutableHashSet<PineValue>.Empty);
                }

            case PineValue.ListValue listValue:
                {
                    var elementsHashes =
                        listValue.Elements.Select(ComputeHash).ToList();

                    var prefix = System.Text.Encoding.ASCII.GetBytes("list " + elementsHashes.Count + "\0");

                    return
                        (serialRepresentation: CommonConversion.Concat(new[] { (ReadOnlyMemory<byte>)prefix }.Concat(elementsHashes).ToList()).ToArray(),
                            dependencies: listValue.Elements);
                }

            default:
                throw new NotImplementedException("Not implemented for value type: " + pineValue.GetType().FullName);
        }
    }

    static public Result<string, PineValue> DeserializeFromHashTree(
        ReadOnlyMemory<byte> serializedValue,
        Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>> loadSerializedValueByHash) =>
        DeserializeFromHashTree(serializedValue, loadSerializedValueByHash);

    static public Result<string, PineValue> DeserializeFromHashTree(
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

            var remainingBytes = serializedValue[beginningToRemoveLength..];

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

                if (!SHA256.HashData(loadedElementSerialRepresentation.Value.Span).AsSpan()
                        .SequenceEqual(elementHash.Span))
                    return Result<string, PineValue>.err(
                        "Hash for loaded element does not match " + CommonConversion.StringBase16(elementHash));

                return DeserializeFromHashTree(loadedElementSerialRepresentation.Value, loadSerializedValueByHash);
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
                        .Select(elementResult => elementResult.loadResult.Extract(error => throw new Exception(error)))
                        .ToImmutableList()));
        }

        return Result<string, PineValue>.err("Invalid prefix: '" + asciiStringUpToFirstSpace + "'.");
    }

    static public ReadOnlyMemory<byte> ComputeHashSorted(TreeNodeWithStringPath treeNode) =>
        ComputeHashNotSorted(TreeNodeWithStringPath.Sort(treeNode));

    static public ReadOnlyMemory<byte> ComputeHashNotSorted(TreeNodeWithStringPath treeNode) =>
        ComputeHash(PineValueComposition.FromTreeWithStringPath(treeNode));

    static public PineValue? FindNodeByHash(PineValue pineValue, ReadOnlyMemory<byte> hash)
    {
        if (ComputeHash(pineValue).Span.SequenceEqual(hash.Span))
            return pineValue;

        if (pineValue is not PineValue.ListValue listValue)
            return null;

        return
            listValue.Elements
                .Select(item => FindNodeByHash(item, hash))
                .FirstOrDefault(matchInItem => matchInItem is not null);
    }
}