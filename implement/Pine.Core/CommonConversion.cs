using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;

namespace Pine.Core;

public class CommonConversion
{
    public static byte[] ByteArrayFromStringBase16(string base16) =>
        [.. Enumerable.Range(0, base16.Length / 2).Select(octetIndex => Convert.ToByte(base16.Substring(octetIndex * 2, 2), 16))];

    public static string StringBase16FromByteArray(IReadOnlyList<byte> bytes) =>
        BitConverter.ToString(bytes as byte[] ?? [.. bytes]).Replace("-", "").ToLowerInvariant();

    public static string StringBase16(ReadOnlyMemory<byte> bytes) =>
        BitConverter.ToString(bytes.ToArray()).Replace("-", "").ToLowerInvariant();

    public static ReadOnlyMemory<byte> HashSHA256(ReadOnlyMemory<byte> input) => SHA256.HashData(input.Span);

    public static ReadOnlyMemory<byte> CompressGzip(ReadOnlyMemory<byte> original)
    {
        using var compressedStream = new System.IO.MemoryStream();

        using var compressStream = new System.IO.Compression.GZipStream(
            compressedStream, System.IO.Compression.CompressionMode.Compress);

        compressStream.Write(original.Span);
        compressStream.Flush();
        return compressedStream.ToArray();
    }

    public static ReadOnlyMemory<byte> DecompressGzip(ReadOnlyMemory<byte> compressed)
    {
        using var decompressStream = new System.IO.Compression.GZipStream(
            new System.IO.MemoryStream(compressed.ToArray()), System.IO.Compression.CompressionMode.Decompress);

        var decompressedStream = new System.IO.MemoryStream();
        decompressStream.CopyTo(decompressedStream);
        return decompressedStream.ToArray();
    }

    public static ReadOnlyMemory<byte> Deflate(ReadOnlyMemory<byte> input)
    {
        using var deflatedStream = new System.IO.MemoryStream();

        using var compressor = new System.IO.Compression.DeflateStream(
            deflatedStream, System.IO.Compression.CompressionMode.Compress);

        compressor.Write(input.Span);
        compressor.Close();
        return deflatedStream.ToArray();
    }

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

    public static ReadOnlyMemory<T> Concat<T>(ReadOnlySpan<T> s1, ReadOnlySpan<T> s2)
    {
        var array = new T[s1.Length + s2.Length];
        s1.CopyTo(array);
        s2.CopyTo(array.AsSpan(s1.Length));
        return array;
    }

    public static ReadOnlyMemory<T> Concat<T>(IReadOnlyList<ReadOnlyMemory<T>> list)
    {
        var aggregateLength = 0;

        for (int i = 0; i < list.Count; ++i)
            aggregateLength += list[i].Length;

        var destArray = new T[aggregateLength];

        var destMemory = destArray.AsMemory();

        var offset = 0;

        for (int i = 0; i < list.Count; ++i)
        {
            var segment = list[i];
            segment.CopyTo(destMemory[offset..]);
            offset += segment.Length;
        }

        return destArray;
    }

    public static Result<ExceptionT, OkT> CatchExceptionAsResultErr<ExceptionT, OkT>(Func<OkT> func)
        where ExceptionT : Exception
    {
        try
        {
            return func();
        }
        catch (ExceptionT e)
        {
            return e;
        }
    }
}
