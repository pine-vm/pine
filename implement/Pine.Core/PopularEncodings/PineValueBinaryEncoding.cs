using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.PopularEncodings;

/// <summary>
/// Functions to encode and decode <see cref="PineValue"/> instances 
/// to and from a compact binary representation.
/// </summary>
public static class PineValueBinaryEncoding
{
    /// <summary>
    /// Size in bytes of the component ID prefix used in binary encoding.
    /// </summary>
    public const int componentIdSize = 8;

    /// <summary>
    /// Size in bytes of the tag identifier used for differentiating PineValue types.
    /// </summary>
    public const int tagSize = 4;

    /// <summary>
    /// Tag identifier indicating the PineValue represents a Blob.
    /// </summary>
    public const int tagBlob = 1;

    /// <summary>
    /// Tag identifier indicating the PineValue represents a List.
    /// </summary>
    public const int tagList = 3;

    /// <summary>
    /// Tag identifier indicating the PineValue represents a Reference to a previously encoded value.
    /// </summary>
    public const int tagReference = 4;

    /// <summary>
    /// Encodes a <see cref="PineValue"/> to the specified stream.
    /// </summary>
    /// <param name="stream">The output stream to write the encoded bytes to.</param>
    /// <param name="composition">The PineValue instance to encode.</param>
    public static void Encode(
        System.IO.Stream stream,
        PineValue composition)
    {
        void write(ReadOnlySpan<byte> bytes)
        {
            stream.Write(bytes);
        }

        Encode(write, composition);
    }

    /// <summary>
    /// Encodes a <see cref="PineValue"/> using a provided span-writing delegate.
    /// </summary>
    /// <param name="write">An action that writes the encoded bytes.</param>
    /// <param name="composition">The PineValue instance to encode.</param>
    public static void Encode(
        Action<ReadOnlySpan<byte>> write,
        PineValue composition) =>
        Encode(composition, write, componentIdOffset: 0);

    private static void Encode(
        PineValue root,
        Action<ReadOnlySpan<byte>> write,
        long componentIdOffset)
    {
        var seenOnce = new HashSet<PineValue>();

        var seenTwice = new HashSet<PineValue>();

        var stack = new Stack<PineValue>([root]);

        while (stack.Count is not 0)
        {
            var value = stack.Pop();

            if (seenOnce.Contains(value))
            {
                seenTwice.Add(value);
                continue;
            }

            seenOnce.Add(value);

            if (value is PineValue.ListValue list)
            {
                for (var i = 0; i < list.Elements.Length; i++)
                {
                    stack.Push(list.Elements.Span[i]);
                }
            }
        }

        var blobDeclarations =
            seenTwice
            .OfType<PineValue.BlobValue>()
            .OrderBy(b => b.Bytes.Length)
            .ToArray();

        var listsDeclarations =
            seenTwice
            .OfType<PineValue.ListValue>()
            .OrderBy(l => l.NodesCount)
            .ToArray();

        var componentId = componentIdOffset;

        var declarationsDict = new Dictionary<PineValue, long>();

        void writeDeclaration(PineValue declaration)
        {
            Span<byte> encodedId = stackalloc byte[8];

            BinaryPrimitives.WriteInt64BigEndian(encodedId, componentId);
            write(encodedId);

            EncodeExpression(
                declaration,
                write,
                declarations: declarationsDict);

            declarationsDict[declaration] = componentId;

            ++componentId;
        }

        void writeDeclarations(IReadOnlyList<PineValue> declarations)
        {
            for (var i = 0; i < declarations.Count; i++)
            {
                var declaration = declarations[i];
                writeDeclaration(declaration);
            }
        }

        writeDeclarations(blobDeclarations);

        writeDeclarations(listsDeclarations);

        writeDeclaration(root);
    }

    private static void EncodeExpression(
        PineValue composition,
        Action<ReadOnlySpan<byte>> write,
        IReadOnlyDictionary<PineValue, long> declarations)
    {
        if (declarations.TryGetValue(composition, out var refId))
        {
            write(tagReferenceEncoded.Span);

            Span<byte> encodedId = stackalloc byte[8];
            BinaryPrimitives.WriteInt64BigEndian(encodedId, refId);
            write(encodedId);

            return;
        }

        if (composition is PineValue.BlobValue blob)
        {
            write(tagBlobEncoded.Span);

            Span<byte> encodedLength = stackalloc byte[8];
            BinaryPrimitives.WriteInt64BigEndian(encodedLength, blob.Bytes.Length);
            write(encodedLength);

            write(blob.Bytes.Span);

            // encode blobs aligned to blocks of four bytes.

            switch (blob.Bytes.Length % 4)
            {
                case 1:
                    write(paddingBytes_3.Span);
                    break;

                case 2:
                    write(paddingBytes_2.Span);
                    break;

                case 3:
                    write(paddingBytes_1.Span);
                    break;
            }

            return;
        }

        if (composition is PineValue.ListValue list)
        {
            write(tagListEncoded.Span);

            Span<byte> encodedLength = stackalloc byte[8];
            BinaryPrimitives.WriteInt64BigEndian(encodedLength, list.Elements.Length);
            write(encodedLength);

            for (var i = 0; i < list.Elements.Length; i++)
            {
                var item = list.Elements.Span[i];

                EncodeExpression(item, write, declarations);
            }

            return;
        }

        throw new NotImplementedException(
            "Unexpected value type: " + composition.GetType());
    }

    /// <summary>
    /// Decodes the root <see cref="PineValue"/> from its binary representation.
    /// </summary>
    /// <param name="sourceBytes">The binary-encoded data as memory.</param>
    /// <returns>The decoded PineValue instance.</returns>
    public static PineValue DecodeRoot(ReadOnlyMemory<byte> sourceBytes) =>
        DecodeSequence(sourceBytes)
        .Last();

    private static IEnumerable<PineValue> DecodeSequence(ReadOnlyMemory<byte> sourceBytes)
    {
        long currentOffset = 0;

        var declarationsDict = new Dictionary<long, PineValue>();

        while (currentOffset < sourceBytes.Length)
        {
            var ((compositionId, composition), addedOffset) =
                DecodeDeclaration(
                    sourceBytes,
                    sourceBytesOffset: (int)currentOffset,
                    declarations: declarationsDict);

            yield return composition;

            declarationsDict[compositionId] = composition;

            currentOffset += addedOffset;
        }
    }

    private static ((long id, PineValue composition), long offset) DecodeDeclaration(
        ReadOnlyMemory<byte> sourceBytes,
        int sourceBytesOffset,
        IReadOnlyDictionary<long, PineValue> declarations)
    {
        var id = BinaryPrimitives.ReadInt64BigEndian(sourceBytes.Span[sourceBytesOffset..]);

        var (composition, exprOffset) =
            DecodeExpression(
                sourceBytes,
                sourceBytesOffset: sourceBytesOffset + componentIdSize,
                declarations);

        return ((id, composition), offset: componentIdSize + exprOffset);
    }

    private static (PineValue composition, int offset) DecodeExpression(
        ReadOnlyMemory<byte> sourceBytes,
        int sourceBytesOffset,
        IReadOnlyDictionary<long, PineValue> declarations)
    {
        var tagId = BinaryPrimitives.ReadInt32BigEndian(sourceBytes.Span[sourceBytesOffset..]);

        if (tagId is tagBlob)
        {
            var bytesCount =
                BinaryPrimitives.ReadInt64BigEndian(sourceBytes.Span[(sourceBytesOffset + 4)..]);

            var bytes = new byte[bytesCount];

            sourceBytes.Slice(start: sourceBytesOffset + 12, length: (int)bytesCount).CopyTo(bytes);

            var paddingBytesCount =
                 (bytesCount % 4) switch
                 {
                     1 => 3,
                     2 => 2,
                     3 => 1,
                     _ => 0
                 };

            return (PineValue.Blob(bytes), offset: 12 + (int)bytesCount + paddingBytesCount);
        }

        if (tagId is tagList)
        {
            var itemsCount = BinaryPrimitives.ReadInt64BigEndian(sourceBytes.Span[(sourceBytesOffset + 4)..]);

            var currentOffset = 12;

            var items = new PineValue[itemsCount];

            for (var i = 0; i < itemsCount; i++)
            {
                var (item, childOffset) =
                    DecodeExpression(
                        sourceBytes,
                        sourceBytesOffset: sourceBytesOffset + currentOffset,
                        declarations);

                items[i] = item;

                currentOffset += childOffset;
            }

            return (PineValue.List(items), offset: currentOffset);
        }

        if (tagId is tagReference)
        {
            var id = BinaryPrimitives.ReadInt64BigEndian(sourceBytes.Span[(sourceBytesOffset + 4)..]);

            if (declarations.TryGetValue(id, out var value))
            {
                return (value, offset: tagSize + componentIdSize);
            }

            throw new InvalidOperationException("Reference not found: " + id);
        }

        throw new NotImplementedException("Unexpected tag: " + tagId);
    }

    private readonly static ReadOnlyMemory<byte> tagBlobEncoded =
        new([0, 0, 0, tagBlob]);

    private readonly static ReadOnlyMemory<byte> tagListEncoded =
        new([0, 0, 0, tagList]);

    private readonly static ReadOnlyMemory<byte> tagReferenceEncoded =
        new([0, 0, 0, tagReference]);

    private readonly static ReadOnlyMemory<byte> paddingBytes_1 =
        new([0]);

    private readonly static ReadOnlyMemory<byte> paddingBytes_2 =
        new([0, 0]);

    private readonly static ReadOnlyMemory<byte> paddingBytes_3 =
        new([0, 0, 0]);
}
