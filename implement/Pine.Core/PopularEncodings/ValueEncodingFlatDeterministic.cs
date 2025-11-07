using System;
using System.Buffers.Binary;
using System.Collections.Frozen;
using System.Collections.Generic;

namespace Pine.Core.PopularEncodings;

/// <summary>
/// Functions to encode and decode <see cref="PineValue"/> instances 
/// to and from a compact binary representation.
/// </summary>
public static class ValueEncodingFlatDeterministic
{
    /// <summary>
    /// Size in bytes of the tag identifier used for differentiating entry types.
    /// </summary>
    public const int TagSize = 4;

    /// <summary>
    /// Tag identifier indicating the PineValue represents a Blob.
    /// </summary>
    public const int TagBlob = 1;

    /// <summary>
    /// Tag identifier indicating the PineValue represents a List.
    /// </summary>
    public const int TagList = 3;

    /// <summary>
    /// Tag identifier indicating the PineValue represents a Reference to a previously encoded value.
    /// </summary>
    public const int TagReference = 4;

    /// <summary>
    /// Encodes a <see cref="PineValue"/> to the specified stream using 32-bit IDs.
    /// </summary>
    /// <param name="stream">The output stream to write the encoded bytes to.</param>
    /// <param name="composition">The PineValue instance to encode.</param>
    public static void Encode(
        System.IO.Stream stream,
        PineValue composition)
    {
        void Write(ReadOnlySpan<byte> bytes)
        {
            stream.Write(bytes);
        }

        Encode(Write, composition);
    }

    /// <summary>
    /// Encodes a <see cref="PineValue"/> using a provided span-writing delegate.
    /// </summary>
    /// <param name="write">An action that writes the encoded bytes.</param>
    /// <param name="composition">The PineValue instance to encode.</param>
    public static void Encode(
        Action<ReadOnlySpan<byte>> write,
        PineValue composition)
    {
        var blobsStartAddresses = new Dictionary<PineValue.BlobValue, long>();

        var listsStartAddresses = new Dictionary<PineValue.ListValue, long>();

        var bytesWritten = 0L;

        void WriteAndCount(ReadOnlySpan<byte> bytes)
        {
            write(bytes);
            bytesWritten += bytes.Length;
        }

        void WriteReferenceToEarlier(int writtenEarlierAddress)
        {
            WriteAndCount(s_tagReferenceEncoded.Span);

            var relativeAddress = writtenEarlierAddress - (int)bytesWritten;

            Span<byte> encodedInt32 = stackalloc byte[4];

            BinaryPrimitives.WriteInt32BigEndian(
                encodedInt32,
                relativeAddress);

            WriteAndCount(encodedInt32);
        }

        void EncodeNext(PineValue current)
        {
            if (current is PineValue.ListValue list)
            {
                if (listsStartAddresses.TryGetValue(list, out var earlierAddress))
                {
                    // Already encoded this list earlier - write a reference.
                    WriteReferenceToEarlier((int)earlierAddress);
                    return;
                }

                listsStartAddresses[list] = bytesWritten;

                WriteAndCount(s_tagListEncoded.Span);

                Span<byte> encodedInt32 = stackalloc byte[4];

                BinaryPrimitives.WriteInt32BigEndian(encodedInt32, list.Items.Length);

                WriteAndCount(encodedInt32);

                for (var i = 0; i < list.Items.Length; i++)
                {
                    EncodeNext(list.Items.Span[i]);
                }

                return;
            }

            if (current is PineValue.BlobValue blob)
            {
                if (blobsStartAddresses.TryGetValue(blob, out var earlierAddress))
                {
                    // Already encoded this blob earlier - write a reference.
                    WriteReferenceToEarlier((int)earlierAddress);
                    return;
                }

                blobsStartAddresses[blob] = bytesWritten;

                WriteAndCount(s_tagBlobEncoded.Span);

                Span<byte> encodedInt32 = stackalloc byte[4];

                BinaryPrimitives.WriteInt32BigEndian(encodedInt32, blob.Bytes.Length);

                WriteAndCount(encodedInt32);

                WriteAndCount(blob.Bytes.Span);

                // encode blobs aligned to blocks of four bytes.
                switch (blob.Bytes.Length % 4)
                {
                    case 1:
                        WriteAndCount(s_paddingBytes_3.Span);
                        break;
                    case 2:
                        WriteAndCount(s_paddingBytes_2.Span);
                        break;
                    case 3:
                        WriteAndCount(s_paddingBytes_1.Span);
                        break;
                }

                return;
            }

            throw new NotImplementedException(
                "Encoding of this PineValue type is not implemented: " +
                current.GetType());
        }

        EncodeNext(composition);
    }

    /// <summary>
    /// Decodes the root <see cref="PineValue"/> from its binary representation.
    /// </summary>
    /// <param name="sourceBytes">The binary-encoded data as memory.</param>
    /// <param name="blobInstancesToReuse">Optional dictionary of blob instances to reuse during decoding.</param>
    /// <param name="listInstancesToReuse">Optional dictionary of list instances to reuse during decoding.</param>
    /// <returns>The decoded PineValue instance.</returns>
    public static PineValue DecodeRoot(
        ReadOnlyMemory<byte> sourceBytes,
        IReadOnlyDictionary<int, FrozenSet<PineValue.BlobValue>>? blobInstancesToReuse = null,
        IReadOnlyDictionary<int, FrozenSet<PineValue.ListValue>>? listInstancesToReuse = null)
    {
        var decoded = new Dictionary<long, PineValue>();

        var readPosition = 0;

        int ReadNextInt32()
        {
            var value =
                BinaryPrimitives.ReadInt32BigEndian(
                    sourceBytes.Span.Slice(readPosition, 4));

            readPosition += 4;

            return value;
        }

        PineValue DecodeNext()
        {
            var startAddress = readPosition;

            var tag = ReadNextInt32();

            if (tag is TagReference)
            {
                var addressBase = readPosition;

                var relativeAddress = ReadNextInt32();

                var referencedAddress = addressBase + relativeAddress;

                return decoded[referencedAddress];
            }

            if (tag is TagList)
            {
                var itemCount = ReadNextInt32();

                var items = new PineValue[itemCount];

                for (var i = 0; i < itemCount; i++)
                {
                    items[i] = DecodeNext();
                }

                var listValue = PineValue.List(items);

                if (listInstancesToReuse is not null &&
                    listInstancesToReuse.TryGetValue(itemCount, out var reusableLists))
                {
                    reusableLists.TryGetValue(
                        listValue,
                        out var reusedListValue);

                    listValue = reusedListValue ?? listValue;
                }

                decoded[startAddress] = listValue;

                return listValue;
            }

            if (tag is TagBlob)
            {
                var byteCount = ReadNextInt32();

                var blobBytes =
                    sourceBytes.Slice(readPosition, byteCount);

                var paddedBytesCount =
                    (byteCount + 3) & ~3;

                readPosition += paddedBytesCount;

                var blobValue = PineValue.Blob(blobBytes);

                if (blobInstancesToReuse is not null &&
                    blobInstancesToReuse.TryGetValue(byteCount, out var reusableBlobs))
                {
                    reusableBlobs.TryGetValue(
                        blobValue,
                        out var reusedBlobValue);

                    blobValue = reusedBlobValue ?? blobValue;
                }

                decoded[startAddress] = blobValue;

                return blobValue;
            }

            throw new NotImplementedException(
                "Decoding of this PineValue type is not implemented for tag: " +
                tag);
        }

        return DecodeNext();
    }

    private readonly static ReadOnlyMemory<byte> s_tagBlobEncoded =
        new([0, 0, 0, TagBlob]);

    private readonly static ReadOnlyMemory<byte> s_tagListEncoded =
        new([0, 0, 0, TagList]);

    private readonly static ReadOnlyMemory<byte> s_tagReferenceEncoded =
        new([0, 0, 0, TagReference]);

    private readonly static ReadOnlyMemory<byte> s_paddingBytes_1 =
        new([0]);

    private readonly static ReadOnlyMemory<byte> s_paddingBytes_2 =
        new([0, 0]);

    private readonly static ReadOnlyMemory<byte> s_paddingBytes_3 =
        new([0, 0, 0]);
}
