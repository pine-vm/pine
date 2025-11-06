using System;
using System.Security.Cryptography;

namespace Pine.Core.Addressing;

/// <summary>
/// Utilities for computing content-addressable hashes for <see cref="PineValue"/> instances
/// using a deterministic value encoding.
/// </summary>
public class PineValueHashFlat
{
    /// <summary>
    /// Computes a cryptographic hash for the specified <paramref name="value"/> based on a deterministic encoding.
    /// </summary>
    public static (ReadOnlyMemory<byte> hashBytes, int encodingBytesLength)
        ComputeHashForValue(
        PineValue value)
    {
        using var stream = new System.IO.MemoryStream();

        PopularEncodings.ValueEncodingFlatDeterministic.Encode(stream, value);

        var encodingBytes = stream.ToArray();

        var hashBytes = SHA256.HashData(encodingBytes);

        return (hashBytes, encodingBytes.Length);
    }
}
