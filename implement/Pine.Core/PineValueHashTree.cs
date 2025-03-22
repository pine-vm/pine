using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;

namespace Pine.Core;

public static class PineValueHashTree
{
    public static ReadOnlyMemory<byte> ComputeHash(
        PineValue pineValue,
        Func<PineValue, ReadOnlyMemory<byte>?>? delegateGetHashOfComponent = null) =>
        ComputeHashAndDependencies(pineValue, delegateGetHashOfComponent).hash;


    public static (ReadOnlyMemory<byte> hash, IReadOnlyCollection<PineValue> dependencies)
        ComputeHashAndDependencies(
        PineValue pineValue,
        Func<PineValue, ReadOnlyMemory<byte>?>? delegateGetHashOfComponent = null)
    {
        var (serialRepresentation, dependencies) =
            ComputeHashTreeNodeSerialRepresentation(pineValue, delegateGetHashOfComponent);

        return (hash: SHA256.HashData(serialRepresentation.Span), dependencies);
    }

    public static (ReadOnlyMemory<byte> serialRepresentation, IReadOnlyCollection<PineValue> dependencies)
        ComputeHashTreeNodeSerialRepresentation(
        PineValue pineValue,
        Func<PineValue, ReadOnlyMemory<byte>?>? delegateGetHashOfComponent = null)
    {
        switch (pineValue)
        {
            case PineValue.BlobValue blobValue:
                {
                    ReadOnlySpan<byte> blobPrefix = "blob "u8;

                    var countEncoded = CountEncoding((uint)blobValue.Bytes.Length);

                    var serialRepresentationLength =
                        blobPrefix.Length + countEncoded.Length + 1 + blobValue.Bytes.Length;

                    var serialRepresentation = new byte[serialRepresentationLength];

                    blobPrefix.CopyTo(serialRepresentation);
                    countEncoded.CopyTo(serialRepresentation.AsMemory()[blobPrefix.Length..]);

                    serialRepresentation[blobPrefix.Length + countEncoded.Length] = 0;

                    blobValue.Bytes.CopyTo(
                        serialRepresentation.AsMemory()[(blobPrefix.Length + countEncoded.Length + 1)..]);

                    return (serialRepresentation, []);
                }

            case PineValue.ListValue listValue:
                {
                    var elementsSpan = listValue.Elements.Span;

                    var elementsHashes = new ReadOnlyMemory<byte>[elementsSpan.Length];

                    for (var i = 0; i < elementsSpan.Length; i++)
                    {
                        var element = elementsSpan[i];

                        var elementHash =
                            delegateGetHashOfComponent?.Invoke(element) ??
                            ComputeHash(element, delegateGetHashOfComponent);

                        elementsHashes[i] = elementHash;
                    }

                    var prefix = System.Text.Encoding.ASCII.GetBytes("list " + elementsHashes.Length + "\0");

                    return
                        (serialRepresentation: BytesConversions.Concat([(ReadOnlyMemory<byte>)prefix, .. elementsHashes]),
                            dependencies: listValue.Elements.ToArray());
                }

            default:
                throw new NotImplementedException("Not implemented for value type: " + pineValue.GetType().FullName);
        }
    }

    private static ReadOnlyMemory<byte> CountEncoding(uint count) =>
        System.Text.Encoding.ASCII.GetBytes(count.ToString());

    public static Result<string, PineValue> DeserializeFromHashTree(
        ReadOnlyMemory<byte> serializedValue,
        Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>> loadSerializedValueByHash) =>
        DeserializeFromHashTree(serializedValue, loadSerializedValueByHash);

    public static Result<string, PineValue> DeserializeFromHashTree(
        ReadOnlyMemory<byte> serializedValue,
        Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>?> loadSerializedValueByHash)
    {
        var asciiStringUpToNull =
            System.Text.Encoding.ASCII.GetString(serializedValue.Span).Split('\0').First();

        var asciiStringUpToFirstSpace =
            asciiStringUpToNull.Split(' ').First();

        if (asciiStringUpToFirstSpace is "blob")
        {
            var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

            var expectedCount = serializedValue.Length - beginningToRemoveLength;

            var count = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

            if (count != expectedCount)
                return "Unexpected count: got " + count + ", but I expected " + expectedCount;

            return PineValue.Blob(serializedValue[beginningToRemoveLength..]);
        }

        if (asciiStringUpToFirstSpace is "list")
        {
            var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

            var remainingBytes = serializedValue[beginningToRemoveLength..];

            var parsedElementCount = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

            var elementHashLength = 32;

            var expectedRemainingLength = parsedElementCount * elementHashLength;

            if (remainingBytes.Length != expectedRemainingLength)
                return
                    "Unexpected remaining length: " + remainingBytes.Length + " instead of " + expectedRemainingLength;

            var elementsHashes =
                Enumerable.Range(0, parsedElementCount)
                    .Select(elementIndex => remainingBytes.Slice(elementIndex * elementHashLength, elementHashLength))
                    .ToImmutableList();

            Result<string, PineValue> TryLoadElementForHash(ReadOnlyMemory<byte> elementHash)
            {
                var loadedElementSerialRepresentation = loadSerializedValueByHash(elementHash);

                if (loadedElementSerialRepresentation is null)
                    return
                        "Failed to load list element " + Convert.ToHexStringLower(elementHash.Span);

                if (!SHA256.HashData(loadedElementSerialRepresentation.Value.Span).AsSpan()
                        .SequenceEqual(elementHash.Span))
                    return
                        "Hash for loaded element does not match " + Convert.ToHexStringLower(elementHash.Span);

                return DeserializeFromHashTree(loadedElementSerialRepresentation.Value, loadSerializedValueByHash);
            }

            var loadElementsResults = new PineValue[elementsHashes.Count];

            for (var i = 0; i < elementsHashes.Count; i++)
            {
                var elementHash = elementsHashes[i];

                var loadResult = TryLoadElementForHash(elementHash);

                if (loadResult.IsErrOrNull() is { } err)
                {
                    return
                        "Failed to load element " +
                        Convert.ToHexStringLower(elementHash.Span) + ": " + err;
                }

                if (loadResult.IsOkOrNull() is not { } ok)
                    throw new Exception("Unexpected result: " + loadResult);

                loadElementsResults[i] = ok;
            }

            return PineValue.List(loadElementsResults);
        }

        return "Invalid prefix: '" + asciiStringUpToFirstSpace + "'.";
    }

    public static ReadOnlyMemory<byte> ComputeHashSorted(TreeNodeWithStringPath treeNode) =>
        ComputeHashNotSorted(TreeNodeWithStringPath.Sort(treeNode));

    public static ReadOnlyMemory<byte> ComputeHashNotSorted(TreeNodeWithStringPath treeNode) =>
        ComputeHash(PineValueComposition.FromTreeWithStringPath(treeNode));

    public static PineValue? FindNodeByHash(PineValue pineValue, ReadOnlyMemory<byte> hash)
    {
        if (ComputeHash(pineValue).Span.SequenceEqual(hash.Span))
            return pineValue;

        if (pineValue is not PineValue.ListValue listValue)
            return null;

        var itemsSpan = listValue.Elements.Span;

        for (var i = 0; i < itemsSpan.Length; i++)
        {
            var matchInItem = FindNodeByHash(itemsSpan[i], hash);

            if (matchInItem is not null)
                return matchInItem;
        }

        return null;
    }
}