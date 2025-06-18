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
            Elements = listValueStruct.Elements;

            NodesCount = 0;
            BlobsBytesCount = 0;

            var elementsSpan = Elements.Span;

            for (int i = 0; i < elementsSpan.Length; i++)
            {
                var element = elementsSpan[i];

                switch (element)
                {
                    case ListValue listItem:
                        NodesCount += listItem.NodesCount;
                        BlobsBytesCount += listItem.BlobsBytesCount;
                        break;

                    case BlobValue blobItem:
                        BlobsBytesCount += blobItem.Bytes.Length;
                        break;
                }
            }

            NodesCount += Elements.Length;

            slimHashCode = listValueStruct.slimHashCode;
        }

        private static int ComputeSlimHashCode(ReadOnlySpan<PineValue> elements)
        {
            var hash = new HashCode();

            for (var i = 0; i < elements.Length; ++i)
            {
                hash.Add(elements[i].GetHashCode());
            }

            return hash.ToHashCode();
        }

        /// <inheritdoc/>
        public virtual bool Equals(ListValue? other)
        {
            if (other is null)
                return false;

            if (ReferenceEquals(this, other))
                return true;

            if (slimHashCode != other.slimHashCode ||
                Elements.Length != other.Elements.Length ||
                NodesCount != other.NodesCount)
                return false;

            var selfSpan = Elements.Span;
            var otherSpan = other.Elements.Span;

            for (int i = 0; i < selfSpan.Length; i++)
            {
                var selfElement = selfSpan[i];
                var otherElement = otherSpan[i];

                if (selfElement is ListValue selfList && otherElement is ListValue otherList)
                {
                    if (!selfList.Equals(otherList))
                        return false;
                }
                else
                {
                    if (!selfElement.Equals(otherElement))
                        return false;
                }
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            slimHashCode;

        /// <summary>
        /// Value type variant of <see cref="ListValue"/>.
        /// </summary>
        public readonly record struct ListValueStruct
        {
            internal readonly int slimHashCode;

            public ReadOnlyMemory<PineValue> Elements { get; }

            /// <summary>
            /// Construct a list value from a sequence of other values.
            /// </summary>
            public ListValueStruct(ReadOnlyMemory<PineValue> elements)
                :
                this(elements, ComputeSlimHashCode(elements.Span))
            {
            }

            public ListValueStruct(ListValue instance)
                :
                this(instance.Elements, instance.slimHashCode)
            {
            }

            private ListValueStruct(
                ReadOnlyMemory<PineValue> elements,
                int slimHashCode)
            {
                Elements = elements;

                this.slimHashCode = slimHashCode;
            }

            /// <inheritdoc/>
            public bool Equals(ListValueStruct other)
            {
                if (slimHashCode != other.slimHashCode || Elements.Length != other.Elements.Length)
                    return false;

                var selfSpan = Elements.Span;
                var otherSpan = other.Elements.Span;

                for (int i = 0; i < selfSpan.Length; i++)
                {
                    var selfElement = selfSpan[i];
                    var otherElement = otherSpan[i];

                    if (selfElement is ListValue selfList && otherElement is ListValue otherList)
                    {
                        if (!selfList.Equals(otherList))
                            return false;
                    }
                    else
                    {
                        if (!selfElement.Equals(otherElement))
                            return false;
                    }
                }

                return true;
            }

            /// <inheritdoc/>
            public override int GetHashCode() => slimHashCode;
        }
    }

    /// <summary>
    /// A <see cref="PineValue"/> that is a sequence of bytes.
    /// </summary>
    public record BlobValue : PineValue, IEquatable<BlobValue>
    {
        private readonly int slimHashCode;

        public ReadOnlyMemory<byte> Bytes { get; }

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

            return
                slimHashCode == other.slimHashCode &&
                Bytes.Span.SequenceEqual(other.Bytes.Span);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => slimHashCode;
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

