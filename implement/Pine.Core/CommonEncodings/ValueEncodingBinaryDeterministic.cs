using System;
using System.Buffers.Binary;
using System.Collections.Frozen;
using System.Collections.Generic;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// Functions to encode and decode <see cref="PineValue"/> instances 
/// to and from a compact binary representation.
/// </summary>
public static partial class ValueEncodingBinaryDeterministic
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
    public const int TagReferenceInteral = 4;

    /// <summary>
    /// Tag identifier indicating the PineValue is given by a reference to a value external to the encoded data.
    /// The tag is followed by <see cref="ReferenceExternal256Length"/> bytes identifying the referenced value.
    /// Decoding resolves the reference via a delegate supplied by the caller.
    /// Encoding does not emit this tag.
    /// </summary>
    public const int TagReferenceExternal256 = 5;

    /// <summary>
    /// Number of bytes identifying the referenced value in an entry tagged with <see cref="TagReferenceExternal256"/>.
    /// </summary>
    public const int ReferenceExternal256Length = 32;

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

        void WriteReferenceToEarlier(long writtenEarlierAddress)
        {
            var relativeAddress = writtenEarlierAddress - (bytesWritten + TagSize);

            if (relativeAddress < int.MinValue)
            {
                throw new InvalidOperationException(
                    "Relative address of reference at offset " + bytesWritten +
                    " exceeds the range of a 32-bit integer: " + relativeAddress);
            }

            WriteAndCount(s_tagReferenceEncoded.Span);

            Span<byte> encodedInt32 = stackalloc byte[4];

            BinaryPrimitives.WriteInt32BigEndian(
                encodedInt32,
                (int)relativeAddress);

            WriteAndCount(encodedInt32);
        }

        // Use an explicit stack rather than recursion so that deeply nested
        // values do not overflow the call stack. Items are pushed in reverse
        // order so that they are processed (and thus written) in their original
        // order, preserving the exact pre-order byte layout and back-reference
        // addressing of the recursive implementation.
        var pending = new Stack<PineValue>();

        pending.Push(composition);

        Span<byte> encodedInt32 = stackalloc byte[4];

        while (pending.Count is not 0)
        {
            var current = pending.Pop();

            if (current is PineValue.ListValue list)
            {
                if (listsStartAddresses.TryGetValue(list, out var earlierAddress))
                {
                    // Already encoded this list earlier - write a reference.
                    WriteReferenceToEarlier(earlierAddress);

                    continue;
                }

                listsStartAddresses[list] = bytesWritten;

                WriteAndCount(s_tagListEncoded.Span);

                BinaryPrimitives.WriteInt32BigEndian(encodedInt32, list.Items.Length);

                WriteAndCount(encodedInt32);

                for (var i = list.Items.Length - 1; i >= 0; i--)
                {
                    pending.Push(list.Items.Span[i]);
                }

                continue;
            }

            if (current is PineValue.BlobValue blob)
            {
                if (blobsStartAddresses.TryGetValue(blob, out var earlierAddress))
                {
                    // Already encoded this blob earlier - write a reference.
                    WriteReferenceToEarlier(earlierAddress);

                    continue;
                }

                blobsStartAddresses[blob] = bytesWritten;

                WriteAndCount(s_tagBlobEncoded.Span);

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

                continue;
            }

            throw new NotImplementedException(
                "Encoding of this PineValue type is not implemented: " +
                current.GetType().FullName);
        }
    }

    /// <summary>
    /// Decodes the root <see cref="PineValue"/> from its binary representation.
    /// </summary>
    /// <param name="sourceBytes">The binary-encoded data as memory.</param>
    /// <param name="blobInstancesToReuse">Optional dictionary of blob instances to reuse during decoding.</param>
    /// <param name="listInstancesToReuse">Optional dictionary of list instances to reuse during decoding.</param>
    /// <param name="resolveExternalReference">
    /// Optional delegate to resolve entries tagged with <see cref="TagReferenceExternal256"/>.
    /// It receives the <see cref="ReferenceExternal256Length"/> bytes of the reference and returns the referenced value,
    /// or null if not found. When this delegate is not given, every external reference is treated as not found.
    /// </param>
    /// <returns>
    /// The decoded PineValue instance, or a description of the error if decoding failed.
    /// </returns>
    public static Result<DecodeError, PineValue> DecodeRoot(
        ReadOnlyMemory<byte> sourceBytes,
        IReadOnlyDictionary<int, FrozenSet<PineValue.BlobValue>>? blobInstancesToReuse = null,
        IReadOnlyDictionary<int, FrozenSet<PineValue.ListValue>>? listInstancesToReuse = null,
        Func<ReadOnlyMemory<byte>, PineValue?>? resolveExternalReference = null)
    {
        var decoded = new Dictionary<long, PineValue>();

        var readPosition = 0;

        bool TryReadNextInt32(out int value)
        {
            if (sourceBytes.Length - readPosition < 4)
            {
                value = 0;
                return false;
            }

            value =
                BinaryPrimitives.ReadInt32BigEndian(
                    sourceBytes.Span.Slice(readPosition, 4));

            readPosition += 4;

            return true;
        }

        DecodeError UnexpectedEnd(string expected) =>
            new DecodeError.UnexpectedEndOfInput(Offset: readPosition, Expected: expected);

        // Use an explicit stack rather than recursion so that deeply nested
        // values do not overflow the call stack.
        var pendingLists = new Stack<PendingList>();

        while (true)
        {
            var startAddress = readPosition;

            if (!TryReadNextInt32(out var tag))
            {
                return UnexpectedEnd("tag");
            }

            PineValue completedValue;

            if (tag is TagReferenceInteral)
            {
                var addressBase = readPosition;

                if (!TryReadNextInt32(out var relativeAddress))
                {
                    return UnexpectedEnd("relative address of reference");
                }

                var referencedAddress = (long)addressBase + relativeAddress;

                if (!decoded.TryGetValue(referencedAddress, out var referencedValue))
                {
                    return
                        new DecodeError.InvalidInternalReference(
                            Offset: startAddress,
                            ReferencedAddress: referencedAddress);
                }

                completedValue = referencedValue;
            }
            else if (tag is TagReferenceExternal256)
            {
                if (sourceBytes.Length - readPosition < ReferenceExternal256Length)
                {
                    return UnexpectedEnd("reference of external reference");
                }

                var reference = sourceBytes.Slice(readPosition, ReferenceExternal256Length);

                readPosition += ReferenceExternal256Length;

                if (resolveExternalReference?.Invoke(reference) is not { } referencedValue)
                {
                    return
                        new DecodeError.ExternalReferenceNotFound(
                            Offset: startAddress,
                            Reference: reference);
                }

                decoded[startAddress] = referencedValue;

                completedValue = referencedValue;
            }
            else if (tag is TagList)
            {
                if (!TryReadNextInt32(out var itemCount))
                {
                    return UnexpectedEnd("item count of list");
                }

                if (itemCount < 0)
                {
                    return new DecodeError.NegativeLength(Offset: startAddress, Tag: tag, Length: itemCount);
                }

                // Each item occupies at least a tag and a 32-bit integer.
                if ((sourceBytes.Length - readPosition) / (TagSize + 4) < itemCount)
                {
                    return
                        new DecodeError.LengthExceedsRemainingInput(
                            Offset: startAddress,
                            Tag: tag,
                            Length: itemCount,
                            RemainingBytes: sourceBytes.Length - readPosition);
                }

                if (itemCount is not 0)
                {
                    pendingLists.Push(new PendingList(startAddress, new PineValue[itemCount]));

                    continue;
                }

                completedValue = CompleteList(startAddress, []);
            }
            else if (tag is TagBlob)
            {
                if (!TryReadNextInt32(out var byteCount))
                {
                    return UnexpectedEnd("byte count of blob");
                }

                if (byteCount < 0)
                {
                    return new DecodeError.NegativeLength(Offset: startAddress, Tag: tag, Length: byteCount);
                }

                var paddedBytesCount =
                    ((long)byteCount + 3) & ~3L;

                if (sourceBytes.Length - readPosition < paddedBytesCount)
                {
                    return
                        new DecodeError.LengthExceedsRemainingInput(
                            Offset: startAddress,
                            Tag: tag,
                            Length: byteCount,
                            RemainingBytes: sourceBytes.Length - readPosition);
                }

                var blobBytes =
                    sourceBytes.Slice(readPosition, byteCount);

                readPosition += (int)paddedBytesCount;

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

                completedValue = blobValue;
            }
            else
            {
                return new DecodeError.UnknownTag(Offset: startAddress, Tag: tag);
            }

            // Deliver the completed value to the enclosing lists, completing them in turn when full.
            while (true)
            {
                if (!pendingLists.TryPeek(out var parent))
                {
                    return completedValue;
                }

                parent.Items[parent.NextIndex] = completedValue;

                parent.NextIndex++;

                if (parent.NextIndex < parent.Items.Length)
                {
                    break;
                }

                pendingLists.Pop();

                completedValue = CompleteList(parent.StartAddress, parent.Items);
            }
        }

        PineValue CompleteList(int startAddress, PineValue[] items)
        {
            var listValue = PineValue.List(items);

            if (listInstancesToReuse is not null &&
                listInstancesToReuse.TryGetValue(items.Length, out var reusableLists))
            {
                reusableLists.TryGetValue(
                    listValue,
                    out var reusedListValue);

                listValue = reusedListValue ?? listValue;
            }

            decoded[startAddress] = listValue;

            return listValue;
        }
    }

    private sealed class PendingList(int startAddress, PineValue[] items)
    {
        public int StartAddress { get; } = startAddress;

        public PineValue[] Items { get; } = items;

        public int NextIndex { get; set; }
    }

    private readonly static ReadOnlyMemory<byte> s_tagBlobEncoded =
        new([0, 0, 0, TagBlob]);

    private readonly static ReadOnlyMemory<byte> s_tagListEncoded =
        new([0, 0, 0, TagList]);

    private readonly static ReadOnlyMemory<byte> s_tagReferenceEncoded =
        new([0, 0, 0, TagReferenceInteral]);

    private readonly static ReadOnlyMemory<byte> s_paddingBytes_1 =
        new([0]);

    private readonly static ReadOnlyMemory<byte> s_paddingBytes_2 =
        new([0, 0]);

    private readonly static ReadOnlyMemory<byte> s_paddingBytes_3 =
        new([0, 0, 0]);
}
