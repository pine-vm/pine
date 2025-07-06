using Pine.Core.PopularEncodings;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core;

/// <summary>
/// The <see cref="PineValue"/> type describes values processed by Pine programs.
/// It is a choice type with two cases, <see cref="ListValue"/> and <see cref="BlobValue"/>.
/// <para/>
/// Other kinds of data, like text, images, or a file system directory, are encoded based on these primitives.
/// For conversion between the generic value type and common data types, see <see cref="IntegerEncoding"/>, <see cref="StringEncoding"/>, and <see cref="PineValueComposition"/>.
/// <para/>
/// There is also a standard representation of program code and expressions as Pine values, 
/// and you can see a reference implementation of this encoding at <see cref="ExpressionEncoding.ParseExpressionFromValue(PineValue)"/>.
/// </summary>
public abstract record PineValue : IEquatable<PineValue>
{
    /*
     * C# 12 does not support mapping from collection expression to ReadOnlyMemory<byte>,
     * therefore provide an additional overload.
     * */
    /// <summary>
    /// Construct a blob value from a sequence of bytes.
    /// </summary>
    public static PineValue Blob(byte[] bytes) =>
        Blob((ReadOnlyMemory<byte>)bytes);

    /// <summary>
    /// Construct a blob value from a sequence of bytes.
    /// </summary>
    public static PineValue Blob(ReadOnlyMemory<byte> bytes) =>
        bytes.Length is 0
        ?
        EmptyBlob
        :
        bytes.Length is 1
        ?
        BlobSingleByte(bytes.Span[0])
        :
        bytes.Length is 2
        ?
        ReusedBlobTuple[bytes.Span[0] * 256 + bytes.Span[1]]
        :
        bytes.Length is 3 && bytes.Span[0] is 2
        ?
        ReusedBlobInteger3ByteNegative[bytes.Span[1] * 256 + bytes.Span[2]]
        :
        bytes.Length is 3 && bytes.Span[0] is 4
        ?
        ReusedBlobInteger3BytePositive[bytes.Span[1] * 256 + bytes.Span[2]]
        :
        bytes.Length is 4 && bytes.Span[0] is 0 && bytes.Span[1] is 0
        ?
        ReusedBlobChar4Byte[bytes.Span[2] * 256 + bytes.Span[3]]
        :
        new BlobValue(bytes);

    /// <summary>
    /// Blob value for a single byte.
    /// </summary>
    public static BlobValue BlobSingleByte(byte value) =>
        ReusedBlobSingle[value];

    /// <summary>
    /// Construct a list value from a sequence of other values.
    /// </summary>
    public static ListValue List(ReadOnlyMemory<PineValue> elements)
    {
        if (elements.Length is 0)
            return EmptyList;

        if (elements.Length is 1)
        {
            if (ReusedInstances.Instance.SingletonListValues is { } reusedSingletonListValues)
            {
                if (reusedSingletonListValues.TryGetValue(elements.Span[0], out var existing))
                {
                    return existing;
                }
            }
        }

        var asStruct = new ListValue.ListValueStruct(elements);

        if (ReusedInstances.Instance?.ListValues is { } reusedListValues)
        {
            if (reusedListValues.TryGetValue(asStruct, out var existing))
            {
                return existing;
            }
        }

        return new ListValue(asStruct);
    }

    public static ListValue List(params PineValue[] elements) =>
        List(elements.AsMemory());

    /// <summary>
    /// List value containing zero elements.
    /// </summary>
    public static readonly ListValue EmptyList = new(ReadOnlyMemory<PineValue>.Empty);

    /// <summary>
    /// Blob value containing zero bytes.
    /// </summary>
    public static readonly BlobValue EmptyBlob = new(ReadOnlyMemory<byte>.Empty);

    private static readonly BlobValue[] ReusedBlobSingle =
        [..Enumerable.Range(0, 256)
        .Select(i => new BlobValue(new byte[] { (byte)i }))];

    private static readonly BlobValue[] ReusedBlobTuple =
        [..Enumerable.Range(0, 256)
        .SelectMany(i => Enumerable.Range(0, 256).Select(j => new BlobValue(new byte[] { (byte)i, (byte)j })))];

    private static readonly BlobValue[] ReusedBlobInteger3ByteNegative =
        [..Enumerable.Range(0, 0x1_00_00)
        .Select(i => new BlobValue(new byte[] { 2, (byte)(i >> 8), (byte)(i & 0xff) }))];

    private static readonly BlobValue[] ReusedBlobInteger3BytePositive =
        [..Enumerable.Range(0, 0x1_00_00)
        .Select(i => new BlobValue(new byte[] { 4, (byte)(i >> 8), (byte)(i & 0xff) }))];

    private static readonly BlobValue[] ReusedBlobChar4Byte =
        [..Enumerable.Range(0, 0x1_00_00)
        .Select(i => new BlobValue(new byte[] { 0, (byte)(i >> 16), (byte)(i >> 8), (byte)(i & 0xff) }))];

    public static BlobValue ReusedBlobTupleFromBytes(byte first, byte second) =>
        ReusedBlobTuple[first * 256 + second];

    public static BlobValue ReusedBlobInteger3ByteNegativeFromBytes(byte second, byte third) =>
        ReusedBlobInteger3ByteNegative[second * 256 + third];

    public static BlobValue ReusedBlobInteger3BytePositiveFromBytes(byte second, byte third) =>
        ReusedBlobInteger3BytePositive[second * 256 + third];

    public static BlobValue ReusedBlobCharFourByte(byte third, byte fourth) =>
        ReusedBlobChar4Byte[third * 256 + fourth];

    public static readonly FrozenSet<BlobValue> ReusedBlobInstances =
        new HashSet<BlobValue>(
            [EmptyBlob,
            .. ReusedBlobSingle,
            .. ReusedBlobTuple,
            ..ReusedBlobInteger3ByteNegative,
            ..ReusedBlobInteger3BytePositive,
            ..ReusedBlobChar4Byte,
            ..PopularValues.PopularStrings.Select(StringEncoding.BlobValueFromString)])
        .ToFrozenSet();

    /// <summary>
    /// A <see cref="PineValue"/> that is a list of <see cref="PineValue"/>s.
    /// </summary>
    public record ListValue : PineValue, IEquatable<ListValue>
    {
        private readonly int slimHashCode;

        /// <summary>
        /// Gets a read-only memory region containing the items of the list.
        /// </summary>
        public ReadOnlyMemory<PineValue> Elements { get; }

        /// <summary>
        /// Aggregate number of nodes, including all nested lists.
        /// </summary>
        public readonly long NodesCount;

        /// <summary>
        /// Aggregate number of bytes in all <see cref="BlobValue"/>s contained within this list and its nested lists.
        /// </summary>
        public readonly long BlobsBytesCount;

        /// <summary>
        /// List value containing zero elements.
        /// </summary>
        public static ListValue Empty => EmptyList;

        /// <summary>
        /// Construct a list value from a sequence of other values.
        /// </summary>
        public ListValue(ReadOnlyMemory<PineValue> elements)
            : this(new ListValueStruct(elements))
        {
        }

        internal ListValue(ListValueStruct listValueStruct)
        {
            Elements = listValueStruct.Items;

            slimHashCode = listValueStruct.slimHashCode;
            NodesCount = listValueStruct.nodesCount;
            BlobsBytesCount = listValueStruct.blobsBytesCount;
        }

        private static void ComputeDerivations(
            ReadOnlySpan<PineValue> items,
            out int slimHashCode,
            out long nodesCount,
            out long blobsBytesCount)
        {
            var hash = new HashCode();

            nodesCount = items.Length;
            blobsBytesCount = 0;

            var length = items.Length;

            ref var itemsFirst =
                ref System.Runtime.InteropServices.MemoryMarshal.GetReference(items);

            for (var i = 0; i < length; i++)
            {
                var item = System.Runtime.CompilerServices.Unsafe.Add(ref itemsFirst, i);

                hash.Add(item.GetHashCode());

                switch (item)
                {
                    case ListValue listItem:
                        nodesCount += listItem.NodesCount;
                        blobsBytesCount += listItem.BlobsBytesCount;
                        break;

                    case BlobValue blobItem:
                        blobsBytesCount += blobItem.Bytes.Length;
                        break;
                }
            }

            slimHashCode = hash.ToHashCode();
        }

        /// <inheritdoc/>
        public virtual bool Equals(ListValue? other) =>
            EqualStatic(this, other);

        private static bool EqualStatic(ListValue self, ListValue? other)
        {
            if (ReferenceEquals(self, other))
                return true;

            if (other is null)
                return false;

            if (self.slimHashCode != other.slimHashCode ||
                self.NodesCount != other.NodesCount ||
                self.BlobsBytesCount != other.BlobsBytesCount)
            {
                return false;
            }

            var selfSpan = self.Elements.Span;
            var otherSpan = other.Elements.Span;

            if (selfSpan.Length != otherSpan.Length)
                return false;

            var length = selfSpan.Length;

            ref var selfItemFirst =
                ref System.Runtime.InteropServices.MemoryMarshal.GetReference(selfSpan);

            ref var otherItemFirst =
                ref System.Runtime.InteropServices.MemoryMarshal.GetReference(otherSpan);

            for (var i = 0; i < length; i++)
            {
                var selfItem = System.Runtime.CompilerServices.Unsafe.Add(ref selfItemFirst, i);
                var otherItem = System.Runtime.CompilerServices.Unsafe.Add(ref otherItemFirst, i);

                if (selfItem is ListValue selfItemList && otherItem is ListValue otherItemList)
                {
                    if (!EqualStatic(selfItemList, otherItemList))
                        return false;
                }
                else
                {
                    if (!selfItem.Equals(otherItem))
                        return false;
                }
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            slimHashCode;

        /// <inheritdoc/>
        public override string ToString()
        {
            return
                ToString(
                    itemsCount: Elements.Length,
                    nodesCount: NodesCount,
                    blobsBytesCount: BlobsBytesCount);
        }

        /// <summary>
        /// Value type variant of <see cref="ListValue"/>.
        /// </summary>
        public readonly record struct ListValueStruct
        {
            internal readonly int slimHashCode;

            internal ReadOnlyMemory<PineValue> Items { get; }

            internal readonly long nodesCount;

            internal readonly long blobsBytesCount;

            /// <summary>
            /// Construct a list value from a sequence of other values.
            /// </summary>
            public ListValueStruct(ReadOnlyMemory<PineValue> items)
            {
                Items = items;

                ComputeDerivations(
                    items.Span,
                    out slimHashCode,
                    out nodesCount,
                    out blobsBytesCount);
            }

            /// <summary>
            /// Copy from an existing <see cref="ListValue"/> instance.
            /// </summary>
            public ListValueStruct(ListValue instance)
            {
                Items = instance.Elements;

                slimHashCode = instance.slimHashCode;
                nodesCount = instance.NodesCount;
                blobsBytesCount = instance.BlobsBytesCount;
            }

            /// <inheritdoc/>
            public bool Equals(ListValueStruct other)
            {
                if (slimHashCode != other.slimHashCode)
                    return false;

                var selfSpan = Items.Span;
                var otherSpan = other.Items.Span;

                if (selfSpan.Length != otherSpan.Length)
                    return false;

                var length = selfSpan.Length;

                ref var selfItemFirst =
                    ref System.Runtime.InteropServices.MemoryMarshal.GetReference(selfSpan);

                ref var otherItemFirst =
                    ref System.Runtime.InteropServices.MemoryMarshal.GetReference(otherSpan);

                for (var i = 0; i < length; i++)
                {
                    var selfItem = System.Runtime.CompilerServices.Unsafe.Add(ref selfItemFirst, i);
                    var otherItem = System.Runtime.CompilerServices.Unsafe.Add(ref otherItemFirst, i);

                    if (selfItem is ListValue selfItemList && otherItem is ListValue otherItemList)
                    {
                        if (!selfItemList.Equals(otherItemList))
                            return false;
                    }
                    else
                    {
                        if (!selfItem.Equals(otherItem))
                            return false;
                    }
                }

                return true;
            }

            /// <inheritdoc/>
            public override int GetHashCode() => slimHashCode;

            /// <inheritdoc/>
            public override string ToString()
            {
                return
                    ListValue.ToString(
                        itemsCount: Items.Length,
                        nodesCount: nodesCount,
                        blobsBytesCount: blobsBytesCount);
            }
        }

        private static string ToString(
            long itemsCount,
            long nodesCount,
            long blobsBytesCount)
        {
            /*
             * Example:
             * 
             * ListValue { ItemsCount = 85, NodesCount = 6_523_426_362, BlobsBytesCount = 91_790_840_965 }
             * */

            return
                nameof(ListValue) +
                " { ItemsCount = " + CommandLineInterface.FormatIntegerForDisplay(itemsCount) +
                ", NodesCount = " + CommandLineInterface.FormatIntegerForDisplay(nodesCount) +
                ", BlobsBytesCount = " + CommandLineInterface.FormatIntegerForDisplay(blobsBytesCount) +
                " }";
        }
    }

    /// <summary>
    /// A <see cref="PineValue"/> that is a sequence of bytes.
    /// </summary>
    public record BlobValue : PineValue, IEquatable<BlobValue>
    {
        private readonly int slimHashCode;

        /// <summary>
        /// Gets the underlying data as a read-only sequence of bytes.
        /// </summary>
        public ReadOnlyMemory<byte> Bytes { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="BlobValue"/> class with the specified byte data.
        /// </summary>
        /// <remarks>The provided byte data is used to compute a hash code for the instance, which can be
        /// used for efficient comparisons or storage in hash-based collections.</remarks>
        /// <param name="bytes">The read-only memory containing the byte data to initialize the blob value.</param>
        public BlobValue(ReadOnlyMemory<byte> bytes)
        {
            Bytes = bytes;

            var hash = new HashCode();

            hash.AddBytes(bytes.Span);

            slimHashCode = hash.ToHashCode();
        }

        /// <inheritdoc/>
        public virtual bool Equals(BlobValue? other)
        {
            if (other is null)
                return false;

            if (ReferenceEquals(this, other))
                return true;

            if (slimHashCode != other.slimHashCode)
                return false;

            var selfSpan = Bytes.Span;
            var otherSpan = other.Bytes.Span;

            if (selfSpan.Length != otherSpan.Length)
                return false;

            return MemoryExtensions.SequenceEqual(selfSpan, otherSpan);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => slimHashCode;

        /// <inheritdoc/>
        public override string ToString()
        {
            return
                nameof(BlobValue) +
                " { BytesCount = " + CommandLineInterface.FormatIntegerForDisplay(Bytes.Length) + " }";
        }
    }

    /// <summary>
    /// Determines whether the given value is present within a nested ListValue, considering transitive containment.
    /// </summary>
    /// <param name="pineValue">The value to search for within the nested lists.</param>
    /// <returns>
    /// Returns true if the provided PineValue is found within the nested ListValue, including in any sublists;
    /// returns false if the current instance is a BlobValue, as it does not contain any elements.
    /// </returns>
    public bool ContainsInListTransitive(PineValue pineValue)
    {
        if (this is not ListValue list)
        {
            return false;
        }

        var elements = list.Elements.Span;

        for (var i = 0; i < elements.Length; i++)
        {
            var element = elements[i];

            if (element.Equals(pineValue) || element.ContainsInListTransitive(pineValue))
            {
                return true;
            }
        }

        return false;
    }

    public static (IReadOnlySet<ListValue>, IReadOnlySet<BlobValue>) CollectAllComponentsFromRoots(
        IEnumerable<PineValue> roots)
    {
        var collectedBlobs = new HashSet<BlobValue>(roots.OfType<BlobValue>());
        var collectedLists = new HashSet<ListValue>();

        var stack = new Stack<ListValue>(roots.OfType<ListValue>());

        while (stack.TryPop(out var listValue))
        {
            if (collectedLists.Contains(listValue))
                continue;

            collectedLists.Add(listValue);

            var elements = listValue.Elements.Span;

            for (var i = 0; i < elements.Length; i++)
            {
                if (elements[i] is ListValue childList)
                    stack.Push(childList);

                if (elements[i] is BlobValue childBlob)
                    collectedBlobs.Add(childBlob);
            }
        }

        return (collectedLists, collectedBlobs);
    }
}
