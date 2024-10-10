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
/// For conversion between the generic value type and common data types, see <see cref="PineValueAsInteger"/>, <see cref="PineValueAsString"/>, and <see cref="PineValueComposition"/>.
/// <para/>
/// There is also a standard representation of program code and expressions as Pine values, 
/// and you can see a reference implementation of this encoding at <see cref="ExpressionEncoding.ParseExpressionFromValueDefault(PineValue)"/>.
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
        ReusedBlobSingle[bytes.Span[0]]
        :
        bytes.Length is 2
        ?
        ReusedBlobTuple[bytes.Span[0] * 256 + bytes.Span[1]]
        :
        new BlobValue(bytes);

    /// <summary>
    /// Construct a list value from a sequence of other values.
    /// </summary>
    public static ListValue List(IReadOnlyList<PineValue> elements)
    {
        if (elements.Count is 0)
            return EmptyList;

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

    /// <summary>
    /// List value containing zero elements.
    /// </summary>
    public static readonly ListValue EmptyList = new([]);

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

    public static readonly FrozenSet<BlobValue> ReusedBlobs =
        new HashSet<BlobValue>([EmptyBlob, .. ReusedBlobSingle, .. ReusedBlobTuple])
        .ToFrozenSet();

    /// <summary>
    /// A <see cref="PineValue"/> that is a list of <see cref="PineValue"/>s.
    /// </summary>
    public record ListValue : PineValue, IEquatable<ListValue>
    {
        private readonly int slimHashCode;

        public IReadOnlyList<PineValue> Elements { get; }

        public readonly long NodesCount;

        public readonly long BlobsBytesCount;

        /// <summary>
        /// Construct a list value from a sequence of other values.
        /// </summary>
        public ListValue(IReadOnlyList<PineValue> elements)
            : this(new ListValueStruct(elements))
        {
        }

        internal ListValue(ListValueStruct listValueStruct)
        {
            Elements = listValueStruct.Elements;

            NodesCount = 0;
            BlobsBytesCount = 0;

            for (int i = 0; i < Elements.Count; i++)
            {
                var element = Elements[i];

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

            NodesCount += Elements.Count;

            slimHashCode = listValueStruct.slimHashCode;
        }

        private static int ComputeSlimHashCode(IReadOnlyList<PineValue> elements)
        {
            var hash = new HashCode();

            for (var i = 0; i < elements.Count; ++i)
            {
                hash.Add(elements[i].GetHashCode());
            }

            return hash.ToHashCode();
        }

        public virtual bool Equals(ListValue? other)
        {
            if (other is null)
                return false;

            if (ReferenceEquals(this, other))
                return true;

            if (slimHashCode != other.slimHashCode ||
                Elements.Count != other.Elements.Count ||
                NodesCount != other.NodesCount)
                return false;

            for (int i = 0; i < Elements.Count; i++)
            {
                var selfElement = Elements[i];
                var otherElement = other.Elements[i];

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

        public override int GetHashCode() =>
            slimHashCode;

        /// <summary>
        /// Value type variant of <see cref="ListValue"/>.
        /// </summary>
        public readonly record struct ListValueStruct
        {
            internal readonly int slimHashCode;

            public IReadOnlyList<PineValue> Elements { get; }

            /// <summary>
            /// Construct a list value from a sequence of other values.
            /// </summary>
            public ListValueStruct(IReadOnlyList<PineValue> elements)
                :
                this(elements, ComputeSlimHashCode(elements))
            {
            }

            public ListValueStruct(ListValue instance)
                :
                this(instance.Elements, instance.slimHashCode)
            {
            }

            private ListValueStruct(
                IReadOnlyList<PineValue> elements,
                int slimHashCode)
            {
                Elements = elements;

                this.slimHashCode = slimHashCode;
            }

            public bool Equals(ListValueStruct other)
            {
                if (slimHashCode != other.slimHashCode || Elements.Count != other.Elements.Count)
                    return false;

                for (int i = 0; i < Elements.Count; i++)
                {
                    var selfElement = Elements[i];
                    var otherElement = other.Elements[i];

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

        public virtual bool Equals(BlobValue? other)
        {
            if (other is null)
                return false;

            return ReferenceEquals(this, other)
                ||
                slimHashCode == other.slimHashCode &&
                Bytes.Span.SequenceEqual(other.Bytes.Span);
        }

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
    public bool ContainsInListTransitive(PineValue pineValue) =>
        this switch
        {
            BlobValue => false,

            ListValue list =>
            list.Elements.Any(e => e.Equals(pineValue) || e.ContainsInListTransitive(pineValue)),

            _ =>
            throw new NotImplementedException()
        };
}

