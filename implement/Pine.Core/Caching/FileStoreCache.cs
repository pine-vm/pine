using Pine.Core.IO;
using System;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Security.Cryptography;

namespace Pine.Core.Caching;

/// <summary>Lossless encoding for one cached data model. Malformed input must throw.</summary>
public interface ICacheCodec<T>
{
    /// <summary>Encodes a complete value.</summary>
    ReadOnlyMemory<byte> Encode(T value);

    /// <summary>Decodes a complete value, reporting malformed or incompatible data.</summary>
    T Decode(ReadOnlyMemory<byte> bytes);
}

/// <summary>
/// Content-addressed storage beneath a caller-selected file-store subtree.
/// The hash function must return a deterministic SHA-256 digest of every input affecting the result,
/// including an algorithm/format version. File-store and decoding failures propagate to the caller.
/// </summary>
public sealed class FileStoreCache<TKey, TValue>(
    IFileStore store,
    IImmutableList<string> directory,
    Func<TKey, ReadOnlyMemory<byte>> hashInput,
    ICacheCodec<TValue> codec) : ICache<TKey, TValue>
    where TKey : notnull
{
    private (IImmutableList<string> Path, ReadOnlyMemory<byte> Hash) Address(TKey key)
    {
        var hash = hashInput(key);

        if (hash.Length != SHA256.HashSizeInBytes)
            throw new ArgumentException("Cache input hashes must be SHA-256 digests.", nameof(hashInput));

        var hex = Convert.ToHexStringLower(hash.Span);
        return (directory.Add(hex[..2]).Add(hex[2..]), hash);
    }

    /// <inheritdoc/>
    public bool TryGetValue(TKey key, [MaybeNullWhen(false)] out TValue value)
    {
        var (path, hash) = Address(key);

        if (store.GetFileContent(path) is not { } bytes)
        {
            value = default;
            return false;
        }

        const int HeaderLength = 2 * SHA256.HashSizeInBytes;

        if (bytes.Length < HeaderLength ||
            !bytes.Span[..SHA256.HashSizeInBytes].SequenceEqual(hash.Span) ||
            !CryptographicOperations.FixedTimeEquals(
                bytes.Span.Slice(SHA256.HashSizeInBytes, SHA256.HashSizeInBytes),
                SHA256.HashData(bytes.Span[HeaderLength..])))
        {
            throw new InvalidDataException("Invalid content-addressed cache entry: " + string.Join("/", path));
        }

        value = codec.Decode(bytes[HeaderLength..]);
        return true;
    }

    /// <inheritdoc/>
    public void Set(TKey key, TValue value)
    {
        var (path, hash) = Address(key);
        var payload = codec.Encode(value);
        var bytes = new byte[2 * SHA256.HashSizeInBytes + payload.Length];
        hash.Span.CopyTo(bytes);
        SHA256.HashData(payload.Span, bytes.AsSpan(SHA256.HashSizeInBytes, SHA256.HashSizeInBytes));
        payload.Span.CopyTo(bytes.AsSpan(2 * SHA256.HashSizeInBytes));
        store.SetFileContent(path, bytes);
    }
}

/// <summary>A read-through/write-through layer. Lifetime management remains on the individual layers.</summary>
public sealed class LayeredCache<TKey, TValue>(ICache<TKey, TValue> front, ICache<TKey, TValue> backing)
    : ICache<TKey, TValue> where TKey : notnull
{
    /// <inheritdoc/>
    public bool TryGetValue(TKey key, [MaybeNullWhen(false)] out TValue value)
    {
        if (front.TryGetValue(key, out value))
            return true;

        if (!backing.TryGetValue(key, out value))
            return false;

        front.Set(key, value);
        return true;
    }

    /// <inheritdoc/>
    public void Set(TKey key, TValue value)
    {
        backing.Set(key, value);
        front.Set(key, value);
    }
}
