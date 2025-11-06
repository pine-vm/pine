using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.IO.Compression;
using System.Linq;

namespace GitCore;

public static class PackFile
{
    public enum ObjectType
    {
        Commit = 1,
        Tree = 2,
        Blob = 3,
        Tag = 4,
        OfsDelta = 6,
        RefDelta = 7
    }

    public record PackFileHeader(
        uint Version,
        uint ObjectCount);

    public record PackObject(
        ObjectType Type,
        long Size,
        ReadOnlyMemory<byte> Data,
        string SHA1base16);

    public static PackFileHeader ParsePackFileHeader(ReadOnlyMemory<byte> packFileData)
    {
        if (packFileData.Length < 12)
        {
            throw new ArgumentException("Pack file is too small to contain a valid header");
        }

        var span = packFileData.Span;

        // Verify PACK signature
        if (!span[..4].SequenceEqual("PACK"u8))
        {
            throw new ArgumentException("Invalid pack file signature");
        }

        // Read version (big-endian)
        var version = BinaryPrimitives.ReadUInt32BigEndian(span.Slice(4, 4));

        // Read object count (big-endian)
        var objectCount = BinaryPrimitives.ReadUInt32BigEndian(span.Slice(8, 4));

        return new PackFileHeader(version, objectCount);
    }

    public static ReadOnlyMemory<byte> GetPackFileChecksum(ReadOnlyMemory<byte> packFileData)
    {
        if (packFileData.Length < 20)
        {
            throw new ArgumentException("Pack file is too small to contain a checksum");
        }

        // Last 20 bytes are the SHA-1 checksum
        return packFileData[^20..];
    }

    public static bool VerifyPackFileChecksum(ReadOnlyMemory<byte> packFileData)
    {
        if (packFileData.Length < 20)
        {
            return false;
        }

        // Get the stored checksum (last 20 bytes)
        var storedChecksum = packFileData[^20..];

        // Calculate checksum of everything except the last 20 bytes
        var dataToHash = packFileData[..^20];
        var calculatedChecksum = System.Security.Cryptography.SHA1.HashData(dataToHash.Span);

        // Compare checksums
        return storedChecksum.Span.SequenceEqual(calculatedChecksum);
    }

    /// <summary>
    /// Parses all objects directly from a pack file without requiring a pre-generated index.
    /// This is more efficient for in-memory operations where we don't need to generate an index file.
    /// </summary>
    /// <param name="packFileData">The bytes of the pack file including its trailing 20-byte checksum.</param>
    /// <returns>A read-only list of parsed pack objects.</returns>
    public static IReadOnlyList<PackObject> ParseAllObjectsDirectly(ReadOnlyMemory<byte> packFileData)
    {
        var header = ParsePackFileHeader(packFileData);
        var objectCount = (int)header.ObjectCount;
        var dataWithoutChecksum = packFileData[..^20];

        /*
         * Since Inflater.SetInput only takes arrays, we convert the span to an array once.
         * An earlier version did convert to an array per object, but that is expensive for large pack files.
         * */
        var sourceArray = dataWithoutChecksum.Span.ToArray();

        // We'll parse objects sequentially and build up the list
        // Track objects by both offset and SHA1 for efficient delta resolution
        var objectsByOffset = new Dictionary<long, (ObjectType Type, byte[] Data, string SHA1)>();
        var objectsBySHA1 = new Dictionary<string, (ObjectType Type, byte[] Data)>();
        var objects = new List<PackObject>();

        var offset = 12; // Start after pack header

        // Helper to calculate SHA1 of an object with reduced allocations
        string CalculateObjectSHA1(ObjectType objectType, byte[] data)
        {
            var typeString = objectType.ToString().ToLower();
            var headerString = $"{typeString} {data.Length}\0";
            var headerBytes = System.Text.Encoding.UTF8.GetByteCount(headerString);

            var buffer = new byte[headerBytes + data.Length];
            System.Text.Encoding.UTF8.GetBytes(headerString, buffer.AsSpan()[..headerBytes]);
            data.AsSpan().CopyTo(buffer.AsSpan()[headerBytes..]);

            var sha1 = System.Security.Cryptography.SHA1.HashData(buffer);
            return Convert.ToHexStringLower(sha1);
        }

        // Parse objects sequentially
        for (var i = 0; i < objectCount; i++)
        {
            var startOffset = offset;

            // Read object header (type and size)
            var currentByte = sourceArray[offset++];
            var objectType = (ObjectType)((currentByte >> 4) & 0x7);
            long size = currentByte & 0xF;
            var shift = 4;

            while ((currentByte & 0x80) is not 0)
            {
                currentByte = sourceArray[offset++];
                size |= (long)(currentByte & 0x7F) << shift;
                shift += 7;
            }

            // Handle different object types
            if (objectType is ObjectType.OfsDelta)
            {
                // Read negative offset
                var negativeOffset = 0L;
                currentByte = sourceArray[offset++];
                negativeOffset = currentByte & 0x7F;

                while ((currentByte & 0x80) is not 0)
                {
                    currentByte = sourceArray[offset++];
                    negativeOffset = ((negativeOffset + 1) << 7) | ((long)currentByte & 0x7F);
                }

                var baseOffset = startOffset - negativeOffset;

                // Get base object
                if (!objectsByOffset.TryGetValue(baseOffset, out var baseObj))
                {
                    throw new InvalidOperationException($"Base object at offset {baseOffset} not found for OfsDelta at {startOffset}");
                }

                // Decompress delta data
                var compressedLength = FindCompressedLength(sourceArray, offset, (int)size);
                var compressedData = sourceArray.AsSpan().Slice(offset, compressedLength);
                var deltaData = DecompressZlib(compressedData, (int)size);

                // Apply delta to reconstruct object
                var reconstructedData = ApplyDelta(baseObj.Data, deltaData);
                var sha1 = CalculateObjectSHA1(baseObj.Type, reconstructedData);

                // Store for potential future delta references
                objectsByOffset[startOffset] = (baseObj.Type, reconstructedData, sha1);
                objectsBySHA1[sha1] = (baseObj.Type, reconstructedData);

                // Create pack object
                var packObject = new PackObject(baseObj.Type, reconstructedData.Length, reconstructedData, sha1);
                objects.Add(packObject);

                offset += compressedLength;
            }
            else if (objectType is ObjectType.RefDelta)
            {
                // Read base SHA1
                var baseSHA1Bytes = sourceArray.AsSpan().Slice(offset, 20);
                var baseSHA1 = Convert.ToHexStringLower(baseSHA1Bytes);
                offset += 20;

                // Find base object by SHA1 using O(1) lookup
                if (!objectsBySHA1.TryGetValue(baseSHA1, out var baseObj))
                {
                    throw new InvalidOperationException($"Base object {baseSHA1} not found for RefDelta at {startOffset}");
                }

                // Decompress delta data
                var compressedLength = FindCompressedLength(sourceArray, offset, (int)size);
                var compressedData = sourceArray.AsSpan().Slice(offset, compressedLength);
                var deltaData = DecompressZlib(compressedData, (int)size);

                // Apply delta to reconstruct object
                var reconstructedData = ApplyDelta(baseObj.Data, deltaData);
                var sha1 = CalculateObjectSHA1(baseObj.Type, reconstructedData);

                // Store for potential future delta references
                objectsByOffset[startOffset] = (baseObj.Type, reconstructedData, sha1);
                objectsBySHA1[sha1] = (baseObj.Type, reconstructedData);

                // Create pack object
                var packObject = new PackObject(baseObj.Type, reconstructedData.Length, reconstructedData, sha1);
                objects.Add(packObject);

                offset += compressedLength;
            }
            else
            {
                // Regular object (commit, tree, blob, tag)
                var compressedLength = FindCompressedLength(sourceArray, offset, (int)size);
                var compressedData = sourceArray.AsSpan().Slice(offset, compressedLength);
                var decompressedData = DecompressZlib(compressedData, (int)size);

                var sha1 = CalculateObjectSHA1(objectType, decompressedData);

                // Store for potential future delta references
                objectsByOffset[startOffset] = (objectType, decompressedData, sha1);
                objectsBySHA1[sha1] = (objectType, decompressedData);

                // Create pack object
                var packObject = new PackObject(objectType, decompressedData.Length, decompressedData, sha1);
                objects.Add(packObject);

                offset += compressedLength;
            }
        }

        return objects;
    }

    public static IReadOnlyList<PackObject> ParseAllObjects(
        ReadOnlyMemory<byte> packFileData,
        IReadOnlyList<PackIndex.IndexEntry> indexEntries)
    {
        var header = ParsePackFileHeader(packFileData);
        var objects = new List<PackObject>();

        var dataWithoutChecksum = packFileData[..^20];

        // Build lookup maps for quick access
        var entriesByOffset = indexEntries.ToDictionary(e => e.Offset, e => e);
        var entriesBySHA1 = indexEntries.ToDictionary(e => e.SHA1base16, e => e);

        // First pass: parse regular objects and store them for delta reconstruction
        var objectsByOffset = new Dictionary<long, (ObjectType Type, byte[] Data)>();
        var objectsBySHA1 = new Dictionary<string, (ObjectType Type, byte[] Data)>();

        // Helper to parse an object at a given offset
        (ObjectType Type, byte[] Data) ParseObjectAt(int objOffset)
        {
            if (objectsByOffset.TryGetValue(objOffset, out var cached))
            {
                return cached;
            }

            var sourceArray = dataWithoutChecksum.Span.ToArray();
            var pos = objOffset;
            var startPos = pos;

            // Read object type and size from variable-length encoding
            var currentByte = sourceArray[pos++];
            var objectType = (ObjectType)((currentByte >> 4) & 0x7);
            long size = currentByte & 0xF;
            var shift = 4;

            // Continue reading size if MSB is set
            while ((currentByte & 0x80) is not 0)
            {
                currentByte = sourceArray[pos++];
                size |= (long)(currentByte & 0x7F) << shift;
                shift += 7;
            }

            // Handle delta objects
            if (objectType is ObjectType.OfsDelta)
            {
                // Read negative offset
                var negativeOffset = 0L;
                currentByte = sourceArray[pos++];
                negativeOffset = currentByte & 0x7F;

                while ((currentByte & 0x80) is not 0)
                {
                    currentByte = sourceArray[pos++];
                    negativeOffset = ((negativeOffset + 1) << 7) | ((long)currentByte & 0x7F);
                }

                var baseOffset = startPos - negativeOffset;

                // Get base object
                var (baseType, baseData) = ParseObjectAt((int)baseOffset);

                // Decompress delta data
                // Find compressed length by trying to decompress
                var compressedLength = FindCompressedLength(sourceArray, pos, (int)size);
                var compressedData = sourceArray.AsSpan().Slice(pos, compressedLength);
                var deltaData = DecompressZlib(compressedData, (int)size);

                // Apply delta
                var reconstructedData = ApplyDelta(baseData, deltaData);

                // Cache and return
                var result = (baseType, reconstructedData);
                objectsByOffset[objOffset] = result;
                return result;
            }
            else if (objectType is ObjectType.RefDelta)
            {
                // Read base SHA1
                var baseSHA1Bytes = sourceArray.AsSpan().Slice(pos, 20);
                var baseSHA1 = Convert.ToHexStringLower(baseSHA1Bytes);
                pos += 20;

                // Get base object
                if (!objectsBySHA1.TryGetValue(baseSHA1, out var baseObj))
                {
                    // Need to find the base object by SHA1
                    if (!entriesBySHA1.TryGetValue(baseSHA1, out var baseEntry))
                    {
                        throw new InvalidOperationException($"Base object {baseSHA1} not found for RefDelta");
                    }
                    baseObj = ParseObjectAt((int)baseEntry.Offset);
                }

                // Decompress delta data
                var compressedLength = FindCompressedLength(sourceArray, pos, (int)size);
                var compressedData = sourceArray.AsSpan().Slice(pos, compressedLength);
                var deltaData = DecompressZlib(compressedData, (int)size);

                // Apply delta
                var reconstructedData = ApplyDelta(baseObj.Data, deltaData);

                // Cache and return
                var result = (baseObj.Type, reconstructedData);
                objectsByOffset[objOffset] = result;
                return result;
            }
            else
            {
                // Regular object
                // Find compressed length
                var compressedLength = FindCompressedLength(sourceArray, pos, (int)size);
                var compressedData = sourceArray.AsSpan().Slice(pos, compressedLength);
                var decompressedData = DecompressZlib(compressedData, (int)size);

                var result = (objectType, decompressedData);
                objectsByOffset[objOffset] = result;
                return result;
            }
        }

        // Parse all objects using the index
        for (var i = 0; i < indexEntries.Count; i++)
        {
            var entry = indexEntries[i];
            var offset = (int)entry.Offset;

            var (objectType, decompressedData) = ParseObjectAt(offset);

            // Store in SHA1 map for RefDelta lookups
            objectsBySHA1[entry.SHA1base16] = (objectType, decompressedData);

            // Create PackObject
            var packObject = new PackObject(objectType, decompressedData.Length, decompressedData, entry.SHA1base16);

            // Verify SHA1 matches
            var objectHeader = System.Text.Encoding.UTF8.GetBytes($"{objectType.ToString().ToLower()} {decompressedData.Length}\0");
            var dataForHash = new byte[objectHeader.Length + decompressedData.Length];
            Array.Copy(objectHeader, 0, dataForHash, 0, objectHeader.Length);
            Array.Copy(decompressedData, 0, dataForHash, objectHeader.Length, decompressedData.Length);
            var sha1 = System.Security.Cryptography.SHA1.HashData(dataForHash);
            var sha1Hex = Convert.ToHexStringLower(sha1);

            if (sha1Hex != entry.SHA1base16)
            {
                throw new InvalidOperationException($"SHA1 mismatch: expected {entry.SHA1base16}, got {sha1Hex}");
            }

            objects.Add(packObject);
        }

        return objects;
    }

    internal static int FindCompressedLength(byte[] data, int startOffset, int expectedDecompressedSize)
    {
        // Use SharpZipLib's Inflater which tracks bytes consumed via TotalIn property

        var inflater =
            new ICSharpCode.SharpZipLib.Zip.Compression.Inflater(
                noHeader: false); // false = expect zlib header

        try
        {
            // Provide all available data to the inflater
            inflater.SetInput(data, index: startOffset, count: data.Length - startOffset);

            // Decompress to a buffer - use a slightly larger buffer to ensure the inflater
            // reads the entire compressed stream and sets IsFinished
            var outputBuffer = new byte[expectedDecompressedSize + 1];

            var decompressedBytes = inflater.Inflate(outputBuffer);

            if (decompressedBytes != expectedDecompressedSize)
            {
                throw new InvalidOperationException(
                    $"Decompression produced {decompressedBytes} bytes but expected " +
                    $"{expectedDecompressedSize} bytes at offset {startOffset}");
            }

            // TotalIn tells us exactly how many bytes were consumed from the input
            // This should now be accurate since the inflater has finished the stream
            return (int)inflater.TotalIn;
        }
        catch (Exception ex)
        {
            throw new InvalidOperationException($"Could not decompress object at offset {startOffset}: {ex.Message}", ex);
        }
        finally
        {
            inflater.Reset();
        }
    }

    public static byte[] DecompressZlib(ReadOnlySpan<byte> compressedData, int expectedSize)
    {
        using var inputStream = new System.IO.MemoryStream(compressedData.ToArray());
        using var zlibStream = new ZLibStream(inputStream, CompressionMode.Decompress);

        var result = new byte[expectedSize];
        zlibStream.ReadExactly(result);

        return result;
    }

    public static IReadOnlyDictionary<string, PackObject> GetObjectsBySHA1(IReadOnlyList<PackObject> objects)
    {
        return objects.ToDictionary(obj => obj.SHA1base16, obj => obj);
    }

    /// <summary>
    /// Applies delta instructions to reconstruct an object from a base object.
    /// </summary>
    /// <param name="baseData">The base object data</param>
    /// <param name="deltaData">The delta instructions</param>
    /// <returns>The reconstructed object data</returns>
    public static byte[] ApplyDelta(ReadOnlySpan<byte> baseData, ReadOnlySpan<byte> deltaData)
    {
        var offset = 0;

        // Read base object size (variable-length encoding)
        var baseSize = ReadDeltaSize(deltaData, ref offset);

        if (baseSize != baseData.Length)
        {
            throw new InvalidOperationException($"Base size mismatch: expected {baseSize}, got {baseData.Length}");
        }

        // Read result object size (variable-length encoding)
        var resultSize = ReadDeltaSize(deltaData, ref offset);

        // Build the result
        var result = new byte[resultSize];
        var resultOffset = 0;

        while (offset < deltaData.Length)
        {
            var cmd = deltaData[offset++];

            if ((cmd & 0x80) != 0)
            {
                // Copy command
                var copyOffset = 0;
                var copySize = 0;

                // Read offset (up to 4 bytes)
                if ((cmd & 0x01) != 0) copyOffset = deltaData[offset++];
                if ((cmd & 0x02) != 0) copyOffset |= deltaData[offset++] << 8;
                if ((cmd & 0x04) != 0) copyOffset |= deltaData[offset++] << 16;
                if ((cmd & 0x08) != 0) copyOffset |= deltaData[offset++] << 24;

                // Read size (up to 3 bytes)
                if ((cmd & 0x10) != 0) copySize = deltaData[offset++];
                if ((cmd & 0x20) != 0) copySize |= deltaData[offset++] << 8;
                if ((cmd & 0x40) != 0) copySize |= deltaData[offset++] << 16;

                // Size 0 means 0x10000
                if (copySize == 0) copySize = 0x10000;

                // Copy from base
                baseData.Slice(copyOffset, copySize).CopyTo(result.AsSpan(resultOffset, copySize));
                resultOffset += copySize;
            }
            else if (cmd != 0)
            {
                // Insert command - cmd bytes follow
                var insertSize = cmd;
                deltaData.Slice(offset, insertSize).CopyTo(result.AsSpan(resultOffset, insertSize));
                offset += insertSize;
                resultOffset += insertSize;
            }
            else
            {
                throw new InvalidOperationException("Invalid delta instruction: zero byte");
            }
        }

        if (resultOffset != resultSize)
        {
            throw new InvalidOperationException($"Delta reconstruction size mismatch: expected {resultSize}, got {resultOffset}");
        }

        return result;
    }

    private static int ReadDeltaSize(ReadOnlySpan<byte> data, ref int offset)
    {
        var size = 0;
        var shift = 0;
        byte b;

        do
        {
            b = data[offset++];
            size |= (b & 0x7F) << shift;
            shift += 7;
        } while ((b & 0x80) != 0);

        return size;
    }
}
