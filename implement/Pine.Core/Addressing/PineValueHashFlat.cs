using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Buffers;
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
        const int BufferSize = 32 * 1024;

        using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);
        var buffer = ArrayPool<byte>.Shared.Rent(BufferSize);
        var buffered = 0;
        var maxBuffered = 0;
        var encodingLength = 0;

        void Flush()
        {
            if (buffered > 0)
            {
                hash.AppendData(buffer.AsSpan(0, buffered));
                buffered = 0;
            }
        }

        void Write(ReadOnlySpan<byte> bytes)
        {
            encodingLength = checked(encodingLength + bytes.Length);

            if (bytes.Length >= BufferSize)
            {
                Flush();
                hash.AppendData(bytes);
                return;
            }

            if (bytes.Length > BufferSize - buffered)
                Flush();

            bytes.CopyTo(buffer.AsSpan(buffered));
            buffered += bytes.Length;
            maxBuffered = Math.Max(maxBuffered, buffered);
        }

        try
        {
            PineTreeBinaryEncoding.Encode(Write, value);
            Flush();
            return (hash.GetHashAndReset(), encodingLength);
        }
        finally
        {
            buffer.AsSpan(0, maxBuffered).Clear();
            ArrayPool<byte>.Shared.Return(buffer);
        }
    }
}
