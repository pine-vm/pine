using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;

namespace Pine.Core.Addressing;

/// <summary>
/// Utilities for computing and working with content-addressed hashes of <see cref="PineValue"/> trees.
/// </summary>
/// <remarks>
/// <para>
/// This module defines a stable, canonical serialization for <see cref="PineValue.BlobValue"/> and
/// <see cref="PineValue.ListValue"/> and computes a SHA-256 hash over that serialization.
/// </para>
/// <para>
/// Serialization format (ASCII prefixes):
/// <list type="bullet">
///   <item><description><c>blob [byteCount]\0[bytes]</c></description></item>
///   <item><description><c>list [elementCount]\0[32B_hash_of_item_0][32B_hash_of_item_1]…</c></description></item>
/// </list>
/// Hashes are computed as <see cref="SHA256"/> over the serialization bytes.
/// </para>
/// </remarks>
public static class PineValueHashTree
{
    private static readonly ConcurrentDictionary<PineValue, ReadOnlyMemory<byte>> s_cachedHashes = new();

    // Pre-encode ASCII representations for small counts to avoid per-call allocations.
    private const uint PreencodedCountMax = 1000u;
    private static readonly ReadOnlyMemory<byte>[] s_preencodedCounts = CreatePreencodedCounts();

    private static ReadOnlyMemory<byte>[] CreatePreencodedCounts()
    {
        var arr = new ReadOnlyMemory<byte>[(int)PreencodedCountMax + 1];

        for (var i = 0; i <= (int)PreencodedCountMax; i++)
        {
            arr[i] = System.Text.Encoding.ASCII.GetBytes(i.ToString());
        }

        return arr;
    }

    /// <summary>
    /// Computes the SHA-256 hash for the given <paramref name="pineValue"/> according to the canonical hash-tree serialization.
    /// </summary>
    /// <param name="pineValue">The value to hash.</param>
    /// <param name="delegateGetHashOfComponent">
    /// Optional delegate to supply a precomputed hash for components (e.g., memoization or external cache).
    /// If it returns a non-null hash for a component, that hash is used instead of recomputing.
    /// </param>
    /// <param name="reportComputedHash">
    /// Optional callback invoked whenever this method computes a hash for a node.
    /// Use this to capture the computed hashes for external caches.
    /// </param>
    /// <returns>The 32-byte hash as <see cref="ReadOnlyMemory{Byte}"/>.</returns>
    /// <remarks>
    /// Results are cached for reused instances via <see cref="ReusedInstances"/> to avoid recomputation.
    /// </remarks>
    public static ReadOnlyMemory<byte> ComputeHash(
        PineValue pineValue,
        Func<PineValue, ReadOnlyMemory<byte>?>? delegateGetHashOfComponent = null,
        Action<PineValue, ReadOnlyMemory<byte>>? reportComputedHash = null)
    {
        if (s_cachedHashes.TryGetValue(pineValue, out var fromCache))
        {
            return fromCache;
        }

        var result =
            ComputeHashIterative(
                pineValue,
                delegateGetHashOfComponent,
                includeDependencies: false,
                captureRootSerialization: false,
                reportComputedHash);

        return result.RootHash;
    }


    /// <summary>
    /// Computes the hash for <paramref name="pineValue"/> and returns the set of direct dependencies.
    /// </summary>
    /// <param name="pineValue">The value to hash.</param>
    /// <param name="delegateGetHashOfComponent">
    /// Optional delegate to supply a precomputed hash for components.
    /// </param>
    /// <param name="reportComputedHash">
    /// Optional callback invoked whenever this method computes a hash for a node.
    /// </param>
    /// <returns>
    /// A tuple containing:
    /// <list type="bullet">
    ///   <item><description><c>hash</c>: the 32-byte SHA-256 hash.</description></item>
    ///   <item><description><c>dependencies</c>: the direct child values referenced by this node (for lists: the items; for blobs: empty).</description></item>
    /// </list>
    /// </returns>
    public static (ReadOnlyMemory<byte> hash, IReadOnlyCollection<PineValue> dependencies)
        ComputeHashAndDependencies(
        PineValue pineValue,
        Func<PineValue, ReadOnlyMemory<byte>?>? delegateGetHashOfComponent = null,
        Action<PineValue, ReadOnlyMemory<byte>>? reportComputedHash = null)
    {
        var result =
            ComputeHashIterative(
                pineValue,
                delegateGetHashOfComponent,
                includeDependencies: true,
                captureRootSerialization: false,
                reportComputedHash);

        return (result.RootHash, result.RootDependencies);
    }

    /// <summary>
    /// Builds the canonical serialization for the given <paramref name="pineValue"/> and reports its direct dependencies.
    /// </summary>
    /// <param name="pineValue">The value to serialize.</param>
    /// <param name="delegateGetHashOfComponent">
    /// Optional delegate to supply a precomputed hash for components.
    /// </param>
    /// <param name="reportComputedHash">
    /// Optional callback invoked whenever this method computes a hash for a node.
    /// </param>
    /// <returns>
    /// A tuple containing:
    /// <list type="bullet">
    ///   <item><description><c>serialRepresentation</c>: the canonical bytes used for hashing.</description></item>
    ///   <item><description><c>dependencies</c>: the direct child values referenced by this node.</description></item>
    /// </list>
    /// </returns>
    /// <exception cref="NotImplementedException">Thrown if an unknown <see cref="PineValue"/> variant is encountered.</exception>
    public static (ReadOnlyMemory<byte> serialRepresentation, IReadOnlyCollection<PineValue> dependencies)
        ComputeHashTreeNodeSerialRepresentation(
        PineValue pineValue,
        Func<PineValue, ReadOnlyMemory<byte>?>? delegateGetHashOfComponent = null,
        Action<PineValue, ReadOnlyMemory<byte>>? reportComputedHash = null)
    {
        var result =
            ComputeHashIterative(
                pineValue,
                delegateGetHashOfComponent,
                includeDependencies: true,
                captureRootSerialization: true,
                reportComputedHash);

        return (result.RootSerialization, result.RootDependencies);
    }

    internal static HashComputationResult ComputeHashIterative(
        PineValue root,
        Func<PineValue, ReadOnlyMemory<byte>?>? delegateGetHashOfComponent,
        bool includeDependencies,
        bool captureRootSerialization,
        Action<PineValue, ReadOnlyMemory<byte>>? reportComputedHash)
    {
        var computedHashes = new Dictionary<PineValue, ReadOnlyMemory<byte>>();

        var stack = new Stack<(PineValue Node, bool ChildrenScheduled)>();
        stack.Push((root, false));

        var rootSerialization = ReadOnlyMemory<byte>.Empty;
        var rootSerializationInitialized = !captureRootSerialization;

        IReadOnlyCollection<PineValue> rootDependencies = [];
        var rootDependenciesInitialized = !includeDependencies;

        while (stack.Count > 0)
        {
            var (node, childrenScheduled) = stack.Pop();

            if (childrenScheduled)
            {
                if (computedHashes.ContainsKey(node))
                {
                    continue;
                }

                ReadOnlyMemory<byte> serialization;
                ReadOnlyMemory<byte> hash;

                switch (node)
                {
                    case PineValue.BlobValue blobValue:
                        serialization = SerializeBlobValue(blobValue);
                        hash = SHA256.HashData(serialization.Span);
                        break;

                    case PineValue.ListValue listValue:
                        {
                            var itemsSpan = listValue.Items.Span;

                            var elementHashes = new ReadOnlyMemory<byte>[itemsSpan.Length];

                            for (var i = 0; i < itemsSpan.Length; i++)
                            {
                                var child = itemsSpan[i];

                                if (!computedHashes.TryGetValue(child, out var childHash))
                                {
                                    throw new InvalidOperationException("Missing hash for child value.");
                                }

                                elementHashes[i] = childHash;
                            }

                            serialization = SerializeListValue(elementHashes);
                            hash = SHA256.HashData(serialization.Span);

                            if (includeDependencies && ReferenceEquals(node, root))
                            {
                                rootDependencies = listValue.Items.ToArray();
                                rootDependenciesInitialized = true;
                            }

                            break;
                        }

                    default:
                        throw new NotImplementedException(
                            "Not implemented for value type: " + node.GetType().FullName);
                }

                computedHashes[node] = hash;

                if (ReferenceEquals(node, root) && captureRootSerialization && !rootSerializationInitialized)
                {
                    rootSerialization = serialization;
                    rootSerializationInitialized = true;
                }

                TryCacheComputedHash(node, hash);
                reportComputedHash?.Invoke(node, hash);

                continue;
            }

            if (computedHashes.ContainsKey(node))
            {
                continue;
            }

            if (!ReferenceEquals(node, root) &&
                delegateGetHashOfComponent?.Invoke(node) is { } delegatedHash)
            {
                computedHashes[node] = delegatedHash;
                reportComputedHash?.Invoke(node, delegatedHash);
                continue;
            }

            if (!ReferenceEquals(node, root) &&
                s_cachedHashes.TryGetValue(node, out var cachedHash))
            {
                computedHashes[node] = cachedHash;
                continue;
            }

            switch (node)
            {
                case PineValue.BlobValue:
                    stack.Push((node, true));
                    break;

                case PineValue.ListValue listValue:
                    stack.Push((node, true));

                    var itemsSpan = listValue.Items.Span;

                    for (var i = itemsSpan.Length - 1; i >= 0; i--)
                    {
                        stack.Push((itemsSpan[i], false));
                    }

                    break;

                default:
                    throw new NotImplementedException(
                        "Not implemented for value type: " + node.GetType().FullName);
            }
        }

        if (!computedHashes.TryGetValue(root, out var rootHash))
        {
            throw new InvalidOperationException("Failed to compute hash for root value.");
        }

        if (!rootDependenciesInitialized)
        {
            rootDependencies =
                root is PineValue.ListValue listValue
                ?
                listValue.Items.ToArray()
                :
                [];
        }

        if (!includeDependencies)
        {
            rootDependencies = [];
        }

        if (captureRootSerialization && !rootSerializationInitialized)
        {
            switch (root)
            {
                case PineValue.BlobValue blobValue:
                    rootSerialization = SerializeBlobValue(blobValue);
                    break;

                case PineValue.ListValue listValue:
                    {
                        var itemsSpan = listValue.Items.Span;
                        var elementHashes = new ReadOnlyMemory<byte>[itemsSpan.Length];

                        for (var i = 0; i < itemsSpan.Length; i++)
                        {
                            var child = itemsSpan[i];

                            if (!computedHashes.TryGetValue(child, out var childHash))
                            {
                                throw new InvalidOperationException("Missing hash for child value.");
                            }

                            elementHashes[i] = childHash;
                        }

                        rootSerialization = SerializeListValue(elementHashes);
                        break;
                    }

                default:
                    throw new NotImplementedException(
                        "Not implemented for value type: " + root.GetType().FullName);
            }

            rootSerializationInitialized = true;
        }

        return new HashComputationResult(
            rootHash,
            rootSerializationInitialized ? rootSerialization : ReadOnlyMemory<byte>.Empty,
            rootDependencies);
    }

    private static ReadOnlyMemory<byte> SerializeBlobValue(PineValue.BlobValue blobValue)
    {
        var blobPrefix = "blob "u8;
        var countEncoded = CountEncoding((uint)blobValue.Bytes.Length);

        var totalLength = blobPrefix.Length + countEncoded.Length + 1 + blobValue.Bytes.Length;
        var buffer = new byte[totalLength];

        blobPrefix.CopyTo(buffer);
        countEncoded.CopyTo(buffer.AsMemory(blobPrefix.Length));

        buffer[blobPrefix.Length + countEncoded.Length] = 0;
        blobValue.Bytes.CopyTo(buffer.AsMemory(blobPrefix.Length + countEncoded.Length + 1));

        return buffer;
    }

    private static ReadOnlyMemory<byte> SerializeListValue(ReadOnlyMemory<byte>[] elementHashes)
    {
        var listPrefix = "list "u8;
        var countEncoded = CountEncoding((uint)elementHashes.Length);

        var prefixLength = listPrefix.Length + countEncoded.Length + 1;

        var totalLength = prefixLength;

        for (var i = 0; i < elementHashes.Length; i++)
        {
            totalLength += elementHashes[i].Length;
        }

        var buffer = new byte[totalLength];

        listPrefix.CopyTo(buffer);
        countEncoded.CopyTo(buffer.AsMemory(listPrefix.Length));
        buffer[listPrefix.Length + countEncoded.Length] = 0;

        var offset = prefixLength;

        for (var i = 0; i < elementHashes.Length; i++)
        {
            var elementHash = elementHashes[i];
            elementHash.CopyTo(buffer.AsMemory(offset));
            offset += elementHash.Length;
        }

        return buffer;
    }

    private static void TryCacheComputedHash(PineValue pineValue, ReadOnlyMemory<byte> hash)
    {
        if (ReusedInstances.Instance.ReusedInstance(pineValue) is { } reusedInstance)
        {
            s_cachedHashes.TryAdd(reusedInstance, hash);
        }
    }

    internal readonly record struct HashComputationResult(
        ReadOnlyMemory<byte> RootHash,
        ReadOnlyMemory<byte> RootSerialization,
        IReadOnlyCollection<PineValue> RootDependencies);

    private static ReadOnlyMemory<byte> CountEncoding(uint count) =>
        count < s_preencodedCounts.Length
            ? s_preencodedCounts[(int)count]
            : System.Text.Encoding.ASCII.GetBytes(count.ToString());

    /// <summary>
    /// Deserializes a value from its canonical hash-tree serialization, using the provided lookup to load list elements by hash.
    /// </summary>
    /// <param name="serializedValue">The serialized bytes for a single node (blob or list) following the canonical format.</param>
    /// <param name="loadSerializedValueByHash">Callback to load a serialized node by its lowercase hex SHA-256 hash.</param>
    /// <param name="valueFromCache">Optional callback to resolve a previously materialized value from a lowercase hex hash.</param>
    /// <param name="valueLoadedForHash">Optional callback invoked after successfully materializing a value for a hash.</param>
    /// <returns>
    /// A <see cref="Result{TError, TOk}"/> with an error message on failure, or the reconstructed <see cref="PineValue"/> on success.
    /// </returns>
    public static Result<string, PineValue> DeserializeFromHashTree(
        ReadOnlyMemory<byte> serializedValue,
        Func<string, ReadOnlyMemory<byte>?> loadSerializedValueByHash,
        Func<string, PineValue?>? valueFromCache,
        Action<string, PineValue>? valueLoadedForHash)
    {
        var charSpaceIndex = 0;
        var charNullIndex = 0;

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

    /// <summary>
    /// Computes a content hash for a <see cref="FileTree"/>, after recursively sorting all siblings to obtain a canonical order.
    /// </summary>
    /// <param name="treeNode">The tree to hash.</param>
    /// <returns>The 32-byte SHA-256 hash as <see cref="ReadOnlyMemory{Byte}"/>.</returns>
    public static ReadOnlyMemory<byte> ComputeHashSorted(FileTree treeNode) =>
        ComputeHashNotSorted(FileTree.Sort(treeNode));

    /// <summary>
    /// Computes a content hash for a <see cref="FileTree"/> without changing sibling order.
    /// </summary>
    /// <param name="treeNode">The tree to hash.</param>
    /// <returns>The 32-byte SHA-256 hash as <see cref="ReadOnlyMemory{Byte}"/>.</returns>
    /// <remarks>
    /// Use <see cref="ComputeHashSorted(FileTree)"/> to obtain order-insensitive hashing.
    /// </remarks>
    public static ReadOnlyMemory<byte> ComputeHashNotSorted(FileTree treeNode) =>
        ComputeHash(FileTreeEncoding.Encode(treeNode));

    /// <summary>
    /// Finds the first node in <paramref name="pineValue"/> whose hash equals <paramref name="hash"/>.
    /// </summary>
    /// <param name="pineValue">The root value to search.</param>
    /// <param name="hash">The 32-byte hash of the node to find.</param>
    /// <returns>The matching node if found; otherwise, <c>null</c>.</returns>
    public static PineValue? FindNodeByHash(PineValue pineValue, ReadOnlyMemory<byte> hash)
    {
        if (ComputeHash(pineValue).Span.SequenceEqual(hash.Span))
            return pineValue;

        if (pineValue is not PineValue.ListValue listValue)
            return null;

        var itemsSpan = listValue.Items.Span;

        for (var i = 0; i < itemsSpan.Length; i++)
        {
            var matchInItem = FindNodeByHash(itemsSpan[i], hash);

            if (matchInItem is not null)
                return matchInItem;
        }

        return null;
    }
}
