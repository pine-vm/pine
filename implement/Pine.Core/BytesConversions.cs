using System;
using System.Collections.Generic;

namespace Pine.Core;

/// <summary>
/// Functions for common conversions around streams or sequences of bytes.
/// </summary>
public class BytesConversions
{
    /// <summary>
    /// Compresses a memory segment using the <see href="https://en.wikipedia.org/wiki/Gzip">GZIP</see> algorithm.
    /// </summary>
    public static ReadOnlyMemory<byte> CompressGzip(ReadOnlyMemory<byte> original)
    {
        using var compressedStream = new System.IO.MemoryStream();

        using var compressStream = new System.IO.Compression.GZipStream(
            compressedStream, System.IO.Compression.CompressionMode.Compress);

        compressStream.Write(original.Span);
        compressStream.Flush();
        return compressedStream.ToArray();
    }

    /// <summary>
    /// Decompresses a memory segment that was compressed using the <see href="https://en.wikipedia.org/wiki/Gzip">GZIP</see> algorithm.
    /// </summary>
    /// <param name="compressed"></param>
    /// <returns></returns>
    public static ReadOnlyMemory<byte> DecompressGzip(ReadOnlyMemory<byte> compressed)
    {
        using var decompressStream = new System.IO.Compression.GZipStream(
            new System.IO.MemoryStream(compressed.ToArray()), System.IO.Compression.CompressionMode.Decompress);

        using var decompressedStream = new System.IO.MemoryStream();

        decompressStream.CopyTo(decompressedStream);

        return decompressedStream.ToArray();
    }

    /// <summary>
    /// Compresses a memory segment using the <see href="https://en.wikipedia.org/wiki/Deflate">DEFLATE</see> algorithm.
    /// </summary>
    public static ReadOnlyMemory<byte> Deflate(ReadOnlyMemory<byte> input)
    {
        using var deflatedStream = new System.IO.MemoryStream();

        using var compressor = new System.IO.Compression.DeflateStream(
            deflatedStream, System.IO.Compression.CompressionMode.Compress);

        compressor.Write(input.Span);

        compressor.Close();

        return deflatedStream.ToArray();
    }

    /// <summary>
    /// Decompresses a memory segment using the <see href="https://en.wikipedia.org/wiki/Deflate">DEFLATE</see> algorithm.
    /// </summary>
    public static ReadOnlyMemory<byte> Inflate(ReadOnlyMemory<byte> input)
    {
        using var inflatedStream = new System.IO.MemoryStream();

        using var deflateStream = new System.IO.Compression.DeflateStream(
            new System.IO.MemoryStream(input.ToArray()), System.IO.Compression.CompressionMode.Decompress);

        deflateStream.CopyTo(inflatedStream);

        return inflatedStream.ToArray();
    }

    public static string TimeStringViewForReport(DateTimeOffset time) =>
        time.ToString("yyyy-MM-ddTHH-mm-ss");

    /// <summary>
    /// Concatenates two memory segments into a single memory segment.
    /// </summary>
    public static ReadOnlyMemory<T> Concat<T>(ReadOnlySpan<T> s1, ReadOnlySpan<T> s2)
    {
        var array = new T[s1.Length + s2.Length];

        s1.CopyTo(array);
        s2.CopyTo(array.AsSpan(s1.Length));

        return array;
    }

    /// <summary>
    /// Concatenates a list of memory segments into a single memory segment.
    /// </summary>
    public static ReadOnlyMemory<T> Concat<T>(IReadOnlyList<ReadOnlyMemory<T>> list)
    {
        var aggregateLength = 0;

        for (var i = 0; i < list.Count; ++i)
            aggregateLength += list[i].Length;

        var destArray = new T[aggregateLength];

        var destMemory = destArray.AsMemory();

        var offset = 0;

        for (var i = 0; i < list.Count; ++i)
        {
            var segment = list[i];
            segment.CopyTo(destMemory[offset..]);
            offset += segment.Length;
        }

        return destArray;
    }
}
