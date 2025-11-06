using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.PopularEncodings;

/// <summary>
/// Functions to encode and decode <see cref="PineValue"/> instances 
/// to and from a compact binary representation.
/// </summary>
public static class ValueBinaryEncodingClassic
{
    /// <summary>
    /// Size in bytes of the component ID prefix used in binary encoding (64-bit version).
    /// </summary>
    public const int ComponentIdSize64 = 8;

    /// <summary>
    /// Size in bytes of the component ID prefix used in binary encoding (32-bit version).
    /// </summary>
    public const int ComponentIdSize32 = 4;

    /// <summary>
    /// Size in bytes of the component ID prefix used in binary encoding.
    /// This uses the 32-bit version for new encodings.
    /// </summary>
    public const int ComponentIdSize = ComponentIdSize32;

    /// <summary>
    /// Size in bytes of the tag identifier used for differentiating PineValue types.
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
    /// Encodes a <see cref="PineValue"/> using a provided span-writing delegate with 32-bit IDs.
    /// </summary>
    /// <param name="write">An action that writes the encoded bytes.</param>
    /// <param name="composition">The PineValue instance to encode.</param>
    /// <param name="onDeclarationWritten">An optional callback invoked after each declaration is written, with the declaration and its component ID.</param>
    public static void Encode(
        Action<ReadOnlySpan<byte>> write,
        PineValue composition,
        Action<PineValue, long>? onDeclarationWritten = null) =>
        Encode32(
            composition,
            write,
            componentIdOffset: 0,
            onDeclarationWritten);

    private static void Encode64(
        PineValue root,
        Action<ReadOnlySpan<byte>> write,
        long componentIdOffset,
        Action<PineValue, long>? onDeclarationWritten = null)
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
                for (var i = 0; i < list.Items.Length; i++)
                {
                    stack.Push(list.Items.Span[i]);
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

        void WriteDeclaration(PineValue declaration)
        {
            Span<byte> encodedId = stackalloc byte[8];

            BinaryPrimitives.WriteInt64BigEndian(encodedId, componentId);
            write(encodedId);

            EncodeExpression64(
                declaration,
                write,
                declarations: declarationsDict);

            declarationsDict[declaration] = componentId;

            onDeclarationWritten?.Invoke(declaration, componentId);

            ++componentId;
        }

        void WriteDeclarations(IReadOnlyList<PineValue> declarations)
        {
            for (var i = 0; i < declarations.Count; i++)
            {
                var declaration = declarations[i];
                WriteDeclaration(declaration);
            }
        }

        WriteDeclarations(blobDeclarations);

        WriteDeclarations(listsDeclarations);

        WriteDeclaration(root);
    }

    private static void EncodeExpression64(
        PineValue composition,
        Action<ReadOnlySpan<byte>> write,
        IReadOnlyDictionary<PineValue, long> declarations)
    {
        if (declarations.TryGetValue(composition, out var refId))
        {
            write(s_tagReferenceEncoded.Span);

            Span<byte> encodedId = stackalloc byte[8];
            BinaryPrimitives.WriteInt64BigEndian(encodedId, refId);
            write(encodedId);

            return;
        }

        if (composition is PineValue.BlobValue blob)
        {
            write(s_tagBlobEncoded.Span);

            Span<byte> encodedLength = stackalloc byte[8];
            BinaryPrimitives.WriteInt64BigEndian(encodedLength, blob.Bytes.Length);
            write(encodedLength);

            write(blob.Bytes.Span);

            // encode blobs aligned to blocks of four bytes.

            switch (blob.Bytes.Length % 4)
            {
                case 1:
                    write(s_paddingBytes_3.Span);
                    break;

                case 2:
                    write(s_paddingBytes_2.Span);
                    break;

                case 3:
                    write(s_paddingBytes_1.Span);
                    break;
            }

            return;
        }

        if (composition is PineValue.ListValue list)
        {
            write(s_tagListEncoded.Span);

            Span<byte> encodedLength = stackalloc byte[8];
            BinaryPrimitives.WriteInt64BigEndian(encodedLength, list.Items.Length);
            write(encodedLength);

            for (var i = 0; i < list.Items.Length; i++)
            {
                var item = list.Items.Span[i];

                EncodeExpression64(item, write, declarations);
            }

            return;
        }

        throw new NotImplementedException(
            "Unexpected value type: " + composition.GetType());
    }

    private static void Encode32(
        PineValue root,
        Action<ReadOnlySpan<byte>> write,
        long componentIdOffset,
        Action<PineValue, long>? onDeclarationWritten = null)
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
                for (var i = 0; i < list.Items.Length; i++)
                {
                    stack.Push(list.Items.Span[i]);
                }
            }
        }

        var blobDeclarationsByLength =
            seenTwice
            .OfType<PineValue.BlobValue>()
            .GroupBy(b => b.Bytes.Length);

        var listsDeclarationsByNodesCount =
            seenTwice
            .OfType<PineValue.ListValue>()
            .GroupBy(l => l.NodesCount);

        var componentId = componentIdOffset;

        var declarationsDict = new Dictionary<PineValue, long>();

        void WriteDeclaration(PineValue declaration)
        {
            Span<byte> encodedId = stackalloc byte[4];

            BinaryPrimitives.WriteInt32BigEndian(encodedId, (int)componentId);
            write(encodedId);

            EncodeExpression32(
                declaration,
                write,
                declarations: declarationsDict);

            declarationsDict[declaration] = componentId;

            onDeclarationWritten?.Invoke(declaration, componentId);

            ++componentId;
        }

        void OrderAndWriteDeclarations(IEnumerable<PineValue> declarationsBeforeOrdering)
        {
            var declarations =
                declarationsBeforeOrdering
                .Order(DotNet.CSharpDeclarationOrder.ValueDeclarationOrder.Instance)
                .ToArray();

            for (var i = 0; i < declarations.Length; i++)
            {
                var declaration = declarations[i];

                WriteDeclaration(declaration);
            }
        }

        foreach (var lengthGroup in blobDeclarationsByLength.OrderBy(kv => kv.Key))
        {
            OrderAndWriteDeclarations(lengthGroup);
        }

        foreach (var nodesCountGroup in listsDeclarationsByNodesCount.OrderBy(kv => kv.Key))
        {
            OrderAndWriteDeclarations(nodesCountGroup);
        }

        WriteDeclaration(root);
    }

    private static void EncodeExpression32(
        PineValue composition,
        Action<ReadOnlySpan<byte>> write,
        IReadOnlyDictionary<PineValue, long> declarations)
    {
        if (declarations.TryGetValue(composition, out var refId))
        {
            write(s_tagReferenceEncoded.Span);

            Span<byte> encodedId = stackalloc byte[4];
            BinaryPrimitives.WriteInt32BigEndian(encodedId, (int)refId);
            write(encodedId);

            return;
        }

        if (composition is PineValue.BlobValue blob)
        {
            write(s_tagBlobEncoded.Span);

            Span<byte> encodedLength = stackalloc byte[4];
            BinaryPrimitives.WriteInt32BigEndian(encodedLength, blob.Bytes.Length);
            write(encodedLength);

            write(blob.Bytes.Span);

            // encode blobs aligned to blocks of four bytes.

            switch (blob.Bytes.Length % 4)
            {
                case 1:
                    write(s_paddingBytes_3.Span);
                    break;

                case 2:
                    write(s_paddingBytes_2.Span);
                    break;

                case 3:
                    write(s_paddingBytes_1.Span);
                    break;
            }

            return;
        }

        if (composition is PineValue.ListValue list)
        {
            write(s_tagListEncoded.Span);

            Span<byte> encodedLength = stackalloc byte[4];
            BinaryPrimitives.WriteInt32BigEndian(encodedLength, list.Items.Length);
            write(encodedLength);

            for (var i = 0; i < list.Items.Length; i++)
            {
                var item = list.Items.Span[i];

                EncodeExpression32(item, write, declarations);
            }

            return;
        }

        throw new NotImplementedException(
            "Unexpected value type: " + composition.GetType());
    }

    /// <summary>
    /// Decodes the root <see cref="PineValue"/> from its binary representation.
    /// Tries 32-bit format first, falls back to 64-bit format for backward compatibility.
    /// </summary>
    /// <param name="sourceBytes">The binary-encoded data as memory.</param>
    /// <returns>The decoded PineValue instance.</returns>
    public static PineValue DecodeRoot(ReadOnlyMemory<byte> sourceBytes)
    {
        try
        {
            return DecodeSequence32(sourceBytes)
                .Last().declValue;
        }
        catch
        {
            // Fall back to 64-bit format for backward compatibility
            return DecodeSequence64(sourceBytes)
                .Last().declValue;
        }
    }

    /// <summary>
    /// Decodes a sequence of <see cref="PineValue"/> declarations from their binary representation.
    /// Tries 32-bit format first, falls back to 64-bit format for backward compatibility.
    /// Each declaration is returned as a tuple containing its component ID and the decoded <see cref="PineValue"/>.
    /// </summary>
    /// <param name="sourceBytes">The binary-encoded data as memory.</param>
    /// <returns>
    /// An enumerable of tuples, each containing the component ID and the corresponding decoded <see cref="PineValue"/>.
    /// </returns>
    public static IEnumerable<(long declId, PineValue declValue)>
        DecodeSequence(ReadOnlyMemory<byte> sourceBytes)
    {
        Exception? firstException = null;

        try
        {
            // Try 32-bit format first
            var result32 = DecodeSequence32(sourceBytes).ToList();
            return result32;
        }
        catch (Exception ex)
        {
            firstException = ex;
        }

        // Fall back to 64-bit format for backward compatibility
        try
        {
            var result64 = DecodeSequence64(sourceBytes).ToList();
            return result64;
        }
        catch (Exception ex)
        {
            throw new InvalidOperationException(
                "Failed to decode with both 32-bit and 64-bit formats. First exception: " + firstException?.Message,
                ex);
        }
    }

    private static IEnumerable<(long declId, PineValue declValue)>
        DecodeSequence64(ReadOnlyMemory<byte> sourceBytes)
    {
        long currentOffset = 0;

        var declarationsDict = new Dictionary<long, PineValue>();

        while (currentOffset < sourceBytes.Length)
        {
            var ((compositionId, composition), addedOffset) =
                DecodeDeclaration64(
                    sourceBytes,
                    sourceBytesOffset: (int)currentOffset,
                    declarations: declarationsDict);

            yield return (compositionId, composition);

            declarationsDict[compositionId] = composition;

            currentOffset += addedOffset;
        }
    }

    private static ((long id, PineValue composition), long offset) DecodeDeclaration64(
        ReadOnlyMemory<byte> sourceBytes,
        int sourceBytesOffset,
        IReadOnlyDictionary<long, PineValue> declarations)
    {
        var id = BinaryPrimitives.ReadInt64BigEndian(sourceBytes.Span[sourceBytesOffset..]);

        var (composition, exprOffset) =
            DecodeExpression64(
                sourceBytes,
                sourceBytesOffset: sourceBytesOffset + ComponentIdSize64,
                declarations);

        return ((id, composition), offset: ComponentIdSize64 + exprOffset);
    }

    private static (PineValue composition, int offset) DecodeExpression64(
        ReadOnlyMemory<byte> sourceBytes,
        int sourceBytesOffset,
        IReadOnlyDictionary<long, PineValue> declarations)
    {
        var tagId = BinaryPrimitives.ReadInt32BigEndian(sourceBytes.Span[sourceBytesOffset..]);

        if (tagId is TagBlob)
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

        if (tagId is TagList)
        {
            var itemsCount = BinaryPrimitives.ReadInt64BigEndian(sourceBytes.Span[(sourceBytesOffset + 4)..]);

            var currentOffset = 12;

            var items = new PineValue[itemsCount];

            for (var i = 0; i < itemsCount; i++)
            {
                var (item, childOffset) =
                    DecodeExpression64(
                        sourceBytes,
                        sourceBytesOffset: sourceBytesOffset + currentOffset,
                        declarations);

                items[i] = item;

                currentOffset += childOffset;
            }

            return (PineValue.List(items), offset: currentOffset);
        }

        if (tagId is TagReference)
        {
            var id = BinaryPrimitives.ReadInt64BigEndian(sourceBytes.Span[(sourceBytesOffset + 4)..]);

            if (declarations.TryGetValue(id, out var value))
            {
                return (value, offset: TagSize + ComponentIdSize64);
            }

            throw new InvalidOperationException("Reference not found: " + id);
        }

        throw new NotImplementedException("Unexpected tag: " + tagId);
    }

    private static IEnumerable<(long declId, PineValue declValue)>
        DecodeSequence32(ReadOnlyMemory<byte> sourceBytes)
    {
        long currentOffset = 0;

        var declarationsDict = new Dictionary<long, PineValue>();

        while (currentOffset < sourceBytes.Length)
        {
            var ((compositionId, composition), addedOffset) =
                DecodeDeclaration32(
                    sourceBytes,
                    sourceBytesOffset: (int)currentOffset,
                    declarations: declarationsDict);

            yield return (compositionId, composition);

            declarationsDict[compositionId] = composition;

            currentOffset += addedOffset;
        }
    }

    private static ((long id, PineValue composition), long offset) DecodeDeclaration32(
        ReadOnlyMemory<byte> sourceBytes,
        int sourceBytesOffset,
        IReadOnlyDictionary<long, PineValue> declarations)
    {
        var id = BinaryPrimitives.ReadInt32BigEndian(sourceBytes.Span[sourceBytesOffset..]);

        var (composition, exprOffset) =
            DecodeExpression32(
                sourceBytes,
                sourceBytesOffset: sourceBytesOffset + ComponentIdSize32,
                declarations);

        return ((id, composition), offset: ComponentIdSize32 + exprOffset);
    }

    private static (PineValue composition, int offset) DecodeExpression32(
        ReadOnlyMemory<byte> sourceBytes,
        int sourceBytesOffset,
        IReadOnlyDictionary<long, PineValue> declarations)
    {
        var tagId = BinaryPrimitives.ReadInt32BigEndian(sourceBytes.Span[sourceBytesOffset..]);

        if (tagId is TagBlob)
        {
            var bytesCount =
                BinaryPrimitives.ReadInt32BigEndian(sourceBytes.Span[(sourceBytesOffset + 4)..]);

            var bytes = new byte[bytesCount];

            sourceBytes.Slice(start: sourceBytesOffset + 8, length: bytesCount).CopyTo(bytes);

            var paddingBytesCount =
                 (bytesCount % 4) switch
                 {
                     1 => 3,
                     2 => 2,
                     3 => 1,
                     _ => 0
                 };

            return (PineValue.Blob(bytes), offset: 8 + bytesCount + paddingBytesCount);
        }

        if (tagId is TagList)
        {
            var itemsCount = BinaryPrimitives.ReadInt32BigEndian(sourceBytes.Span[(sourceBytesOffset + 4)..]);

            var currentOffset = 8;

            var items = new PineValue[itemsCount];

            for (var i = 0; i < itemsCount; i++)
            {
                var (item, childOffset) =
                    DecodeExpression32(
                        sourceBytes,
                        sourceBytesOffset: sourceBytesOffset + currentOffset,
                        declarations);

                items[i] = item;

                currentOffset += childOffset;
            }

            return (PineValue.List(items), offset: currentOffset);
        }

        if (tagId is TagReference)
        {
            var id = BinaryPrimitives.ReadInt32BigEndian(sourceBytes.Span[(sourceBytesOffset + 4)..]);

            if (declarations.TryGetValue(id, out var value))
            {
                return (value, offset: TagSize + ComponentIdSize32);
            }

            throw new InvalidOperationException("Reference not found: " + id);
        }

        throw new NotImplementedException("Unexpected tag: " + tagId);
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
