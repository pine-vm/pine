using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;

namespace Pine.Core;

public static class PineValueHashTree
{
    private static readonly ConcurrentDictionary<PineValue, ReadOnlyMemory<byte>> cachedHashes = new();

    public static ReadOnlyMemory<byte> ComputeHash(
        PineValue pineValue,
        Func<PineValue, ReadOnlyMemory<byte>?>? delegateGetHashOfComponent = null)
    {
        if (cachedHashes.TryGetValue(pineValue, out var fromCache))
        {
            return fromCache;
        }

        var hash =
            ComputeHashAndDependencies(pineValue, delegateGetHashOfComponent).hash;

        if (ReusedInstances.Instance.ReusedInstance(pineValue) is { } reusedInstance)
        {
            cachedHashes.TryAdd(reusedInstance, hash);
        }

        return hash;
    }


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
        Func<string, ReadOnlyMemory<byte>?> loadSerializedValueByHash,
        Func<string, PineValue?>? valueFromCache,
        Action<string, PineValue>? valueLoadedForHash)
    {
        int charSpaceIndex = 0;
        int charNullIndex = 0;

        for (var i = 0; i < serializedValue.Length; i++)
        {
            var currentByte = serializedValue.Span[i];

            if (charSpaceIndex is 0)
            {
                if (currentByte is 32)
                {
                    charSpaceIndex = i;
                }
            }
            else if (currentByte is 0)
            {
                charNullIndex = i;
                break;
            }
        }

        var asciiStringUpToFirstSpace =
            System.Text.Encoding.ASCII.GetString(serializedValue.Span[..charSpaceIndex]);

        var asciiStringDigits =
            System.Text.Encoding.ASCII.GetString(
                serializedValue.Span[(charSpaceIndex + 1)..charNullIndex]);

        if (asciiStringUpToFirstSpace is "blob")
        {
            var expectedCount = serializedValue.Length - charNullIndex - 1;

            var count = int.Parse(asciiStringDigits);

            if (count != expectedCount)
                return "Unexpected count: got " + count + ", but I expected " + expectedCount;

            return PineValue.Blob(serializedValue[(charNullIndex + 1)..]);
        }

        if (asciiStringUpToFirstSpace is "list")
        {
            var remainingBytes = serializedValue[(charNullIndex + 1)..];

            var parsedElementCount = int.Parse(asciiStringDigits);

            var elementHashLength = 32;

            var expectedRemainingLength = parsedElementCount * elementHashLength;

            if (remainingBytes.Length != expectedRemainingLength)
            {
                return
                    "Unexpected remaining length: " + remainingBytes.Length +
                    " instead of " + expectedRemainingLength;
            }

            var elementsHashes = new ReadOnlyMemory<byte>[parsedElementCount];

            for (var i = 0; i < parsedElementCount; i++)
            {
                elementsHashes[i] =
                    remainingBytes.Slice(i * elementHashLength, elementHashLength);
            }

            Result<string, PineValue> TryLoadElementForHash(ReadOnlyMemory<byte> elementHash)
            {
                var elementHashBase16 = Convert.ToHexStringLower(elementHash.Span);

                if (valueFromCache?.Invoke(elementHashBase16) is { } cachedValue)
                {
                    return cachedValue;
                }

                var loadedElementSerialRepresentation =
                    loadSerializedValueByHash(elementHashBase16);

                if (loadedElementSerialRepresentation is null)
                {
                    return
                        "Failed to load list element " + elementHashBase16;
                }

                if (!SHA256.HashData(loadedElementSerialRepresentation.Value.Span).AsSpan()
                    .SequenceEqual(elementHash.Span))
                {
                    return
                        "Hash for loaded element does not match " + elementHashBase16;
                }

                var loadItemResult =
                    DeserializeFromHashTree(
                        loadedElementSerialRepresentation.Value,
                        loadSerializedValueByHash,
                        valueFromCache,
                        valueLoadedForHash);

                if (loadItemResult.IsOkOrNull() is { } loadedItem)
                {
                    valueLoadedForHash?.Invoke(elementHashBase16, loadedItem);
                }

                return loadItemResult;
            }

            var loadElementsResults = new PineValue[elementsHashes.Length];

            for (var i = 0; i < elementsHashes.Length; i++)
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