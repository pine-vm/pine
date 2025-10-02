using Pine.Core.Optimization;
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
    public static PineValue Blob(ReadOnlyMemory<byte> bytes)
    {
        if (bytes.Length is 0)
            return EmptyBlob;

        if (bytes.Length is 1)
            return BlobSingleByte(bytes.Span[0]);

        if (bytes.Length is 2)
            return s_reusedBlobTuple[bytes.Span[0] * 256 + bytes.Span[1]];

        if (bytes.Length is 3 && bytes.Span[0] is 2)
        {
            return s_reusedBlobInteger3ByteNegative[bytes.Span[1] * 256 + bytes.Span[2]];
        }

        if (bytes.Length is 3 && bytes.Span[0] is 4)
        {
            return s_reusedBlobInteger3BytePositive[bytes.Span[1] * 256 + bytes.Span[2]];
        }

        if (bytes.Length is 4 && bytes.Span[0] is 0 && bytes.Span[1] is 0)
        {
            return s_reusedBlobChar4Byte[bytes.Span[2] * 256 + bytes.Span[3]];
        }

        if (ReusedBlobInstances is { } reusedSet &&
            reusedSet.TryGetValue(new BlobValue(bytes), out var reused))
        {
            return reused;
        }

        var newInstance = new BlobValue(bytes);

        return newInstance;
    }

    /// <summary>
    /// Blob value for a single byte.
    /// </summary>
    public static BlobValue BlobSingleByte(byte value) =>
        s_reusedBlobSingle[value];

    /// <summary>
    /// Construct a list value from a sequence of other values.
    /// </summary>
    public static ListValue List(ReadOnlyMemory<PineValue> items)
    {
        if (items.Length is 0)
            return EmptyList;

        if (items.Length is 1)
        {
            if (ReusedInstances.Instance?.SingletonListValues is { } reusedSingletonListValues)
            {
                if (reusedSingletonListValues.TryGetValue(items.Span[0], out var existing))
                {
                    return existing;
                }
            }
        }

        var asStruct = new ListValue.ListValueStruct(items);

        if (ReusedInstances.Instance?.ListValues is { } reusedListValues)
        {
            if (reusedListValues.TryGetValue(asStruct, out var existing))
            {
                return existing;
            }
        }

        var newInstance = new ListValue(asStruct);

        return PineValueWeakInterner.GetOrAdd(newInstance);
    }

    /// <summary>
    /// Construct a list value from a sequence of other values.
    /// </summary>
    /// <param name="items">The items to include in the list.</param>
    /// <returns>A <see cref="ListValue"/> containing the specified items.</returns>
    public static ListValue List(params PineValue[] items) =>
        List(items.AsMemory());

    /// <summary>
    /// List value containing zero items.
    /// </summary>
    public static readonly ListValue EmptyList = new(ReadOnlyMemory<PineValue>.Empty);

    /// <summary>
    /// Blob value containing zero bytes.
    /// </summary>
    public static readonly BlobValue EmptyBlob = new(ReadOnlyMemory<byte>.Empty);

    private static readonly BlobValue[] s_reusedBlobSingle =
        [..Enumerable.Range(0, 256)
        .Select(i => new BlobValue(new byte[] { (byte)i }))];

    private static readonly BlobValue[] s_reusedBlobTuple =
        [..Enumerable.Range(0, 256)
        .SelectMany(i => Enumerable.Range(0, 256).Select(j => new BlobValue(new byte[] { (byte)i, (byte)j })))];

    private static readonly BlobValue[] s_reusedBlobInteger3ByteNegative =
        [..Enumerable.Range(0, 0x1_00_00)
        .Select(i => new BlobValue(new byte[] { 2, (byte)(i >> 8), (byte)(i & 0xff) }))];

    private static readonly BlobValue[] s_reusedBlobInteger3BytePositive =
        [..Enumerable.Range(0, 0x1_00_00)
        .Select(i => new BlobValue(new byte[] { 4, (byte)(i >> 8), (byte)(i & 0xff) }))];

    private static readonly BlobValue[] s_reusedBlobChar4Byte =
        [..Enumerable.Range(0, 0x1_00_00)
        .Select(i => new BlobValue(new byte[] { 0, (byte)(i >> 16), (byte)(i >> 8), (byte)(i & 0xff) }))];

    /// <summary>
    /// Returns a reused <see cref="BlobValue"/> instance representing a tuple of two bytes.
    /// </summary>
    /// <param name="first">The first byte of the tuple.</param>
    /// <param name="second">The second byte of the tuple.</param>
    /// <returns>
    /// A <see cref="BlobValue"/> instance corresponding to the specified two-byte tuple.
    /// </returns>
    public static BlobValue ReusedBlobTupleFromBytes(byte first, byte second) =>
        s_reusedBlobTuple[first * 256 + second];

    /// <summary>
    /// Returns a reused <see cref="BlobValue"/> instance representing a three-byte negative integer encoding.
    /// </summary>
    /// <param name="second">The second byte of the encoding.</param>
    /// <param name="third">The third byte of the encoding.</param>
    /// <returns>
    /// A <see cref="BlobValue"/> instance corresponding to the specified three-byte negative integer encoding.
    /// </returns>
    public static BlobValue ReusedBlobInteger3ByteNegativeFromBytes(byte second, byte third) =>
        s_reusedBlobInteger3ByteNegative[second * 256 + third];

    /// <summary>
    /// Returns a reused <see cref="BlobValue"/> instance representing a three-byte positive integer encoding.
    /// </summary>
    /// <param name="second">The second byte of the encoding.</param>
    /// <param name="third">The third byte of the encoding.</param>
    /// <returns>
    /// A <see cref="BlobValue"/> instance corresponding to the specified three-byte positive integer encoding.
    /// </returns>
    public static BlobValue ReusedBlobInteger3BytePositiveFromBytes(byte second, byte third) =>
        s_reusedBlobInteger3BytePositive[second * 256 + third];

    /// <summary>
    /// Returns a reused <see cref="BlobValue"/> instance representing a four-byte character encoding.
    /// </summary>
    /// <param name="third">The third byte of the encoding.</param>
    /// <param name="fourth">The fourth byte of the encoding.</param>
    /// <returns>
    /// A <see cref="BlobValue"/> instance corresponding to the specified four-byte character encoding.
    /// </returns>
    public static BlobValue ReusedBlobCharFourByte(byte third, byte fourth) =>
        s_reusedBlobChar4Byte[third * 256 + fourth];

    /// <summary>
    /// A set of commonly reused <see cref="BlobValue"/> instances, including empty, single-byte, two-byte, and other frequently used blobs.
    /// This set is used to optimize memory usage and performance by sharing instances for popular blob values.
    /// </summary>
    public static readonly FrozenSet<BlobValue> ReusedBlobInstances =
        new HashSet<BlobValue>(
            [EmptyBlob,
            .. s_reusedBlobSingle,
            .. s_reusedBlobTuple,
            ..s_reusedBlobInteger3ByteNegative,
            ..s_reusedBlobInteger3BytePositive,
            ..s_reusedBlobChar4Byte,
            ..PopularValues.PopularStrings.Select(StringEncoding.BlobValueFromString)])
        .ToFrozenSet();

    /// <summary>
    /// A <see cref="PineValue"/> that is a list of <see cref="PineValue"/>s.
    /// </summary>
    public record ListValue : PineValue, IEquatable<ListValue>
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// Gets a read-only memory region containing the items of the list.
        /// </summary>
        public ReadOnlyMemory<PineValue> Items { get; }

        /// <summary>
        /// Aggregate number of nodes, including all nested lists.
        /// </summary>
        public readonly long NodesCount;

        /// <summary>
        /// Aggregate number of bytes in all <see cref="BlobValue"/>s contained within this list and its nested lists.
        /// </summary>
        public readonly long BlobsBytesCount;

        /// <summary>
        /// List value containing zero items.
        /// </summary>
        public static ListValue Empty => EmptyList;

        /// <summary>
        /// Construct a list value from a sequence of other values.
        /// </summary>
        public ListValue(ReadOnlyMemory<PineValue> items)
            : this(new ListValueStruct(items))
        {
        }

        internal ListValue(ListValueStruct listValueStruct)
        {
            Items = listValueStruct.Items;

            _slimHashCode = listValueStruct.SlimHashCode;
            NodesCount = listValueStruct.NodesCount;
            BlobsBytesCount = listValueStruct.BlobsBytesCount;
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

            if (self._slimHashCode != other._slimHashCode ||
                self.NodesCount != other.NodesCount ||
                self.BlobsBytesCount != other.BlobsBytesCount)
            {
                return false;
            }

            var selfSpan = self.Items.Span;
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
            _slimHashCode;

        /// <inheritdoc/>
        public override string ToString()
        {
            return
                ToString(
                    itemsCount: Items.Length,
                    nodesCount: NodesCount,
                    blobsBytesCount: BlobsBytesCount);
        }

        /// <summary>
        /// Value type variant of <see cref="ListValue"/>.
        /// </summary>
        public readonly record struct ListValueStruct
        {
            internal readonly int SlimHashCode;

            internal ReadOnlyMemory<PineValue> Items { get; }

            internal readonly long NodesCount;

            internal readonly long BlobsBytesCount;

            /// <summary>
            /// Construct a list value from a sequence of other values.
            /// </summary>
            public ListValueStruct(ReadOnlyMemory<PineValue> items)
            {
                Items = items;

                ComputeDerivations(
                    items.Span,
                    out SlimHashCode,
                    out NodesCount,
                    out BlobsBytesCount);
            }

            /// <summary>
            /// Copy from an existing <see cref="ListValue"/> instance.
            /// </summary>
            public ListValueStruct(ListValue instance)
            {
                Items = instance.Items;

                SlimHashCode = instance._slimHashCode;
                NodesCount = instance.NodesCount;
                BlobsBytesCount = instance.BlobsBytesCount;
            }

            /// <inheritdoc/>
            public bool Equals(ListValueStruct other)
            {
                if (SlimHashCode != other.SlimHashCode)
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
            public override int GetHashCode() => SlimHashCode;

            /// <inheritdoc/>
            public override string ToString()
            {
                return
                    ListValue.ToString(
                        itemsCount: Items.Length,
                        nodesCount: NodesCount,
                        blobsBytesCount: BlobsBytesCount);
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
        private readonly int _slimHashCode;

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

            _slimHashCode = hash.ToHashCode();
        }

        /// <inheritdoc/>
        public virtual bool Equals(BlobValue? other)
        {
            if (other is null)
                return false;

            if (ReferenceEquals(this, other))
                return true;

            if (_slimHashCode != other._slimHashCode)
                return false;

            var selfSpan = Bytes.Span;
            var otherSpan = other.Bytes.Span;

            if (selfSpan.Length != otherSpan.Length)
                return false;

            return MemoryExtensions.SequenceEqual(selfSpan, otherSpan);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;

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
    /// returns false if the current instance is a BlobValue, as it does not contain any items.
    /// </returns>
    public bool ContainsInListTransitive(PineValue pineValue)
    {
        if (this is not ListValue list)
        {
            return false;
        }

        var items = list.Items.Span;

        for (var i = 0; i < items.Length; i++)
        {
            var item = items[i];

            if (item.Equals(pineValue) || item.ContainsInListTransitive(pineValue))
            {
                return true;
            }
        }

        return false;
    }

    /// <summary>
    /// Collects all unique <see cref="ListValue"/> and <see cref="BlobValue"/> components that are reachable from the given root values.
    /// Traverses the provided <paramref name="roots"/> and recursively collects all nested <see cref="ListValue"/> and <see cref="BlobValue"/> instances.
    /// </summary>
    /// <param name="roots">The root <see cref="PineValue"/> instances from which to start collecting components.</param>
    /// <returns>
    /// A tuple containing two sets:
    /// <list type="bullet">
    /// <item>
    /// <description><c>lists</c>: All unique <see cref="ListValue"/> instances found in the transitive closure of the roots.</description>
    /// </item>
    /// <item>
    /// <description><c>blobs</c>: All unique <see cref="BlobValue"/> instances found in the transitive closure of the roots.</description>
    /// </item>
    /// </list>
    /// </returns>
    public static (IReadOnlySet<ListValue> lists, IReadOnlySet<BlobValue> blobs) CollectAllComponentsFromRoots(
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

            var items = listValue.Items.Span;

            for (var i = 0; i < items.Length; i++)
            {
                if (items[i] is ListValue childList)
                    stack.Push(childList);

                if (items[i] is BlobValue childBlob)
                    collectedBlobs.Add(childBlob);
            }
        }

        return (collectedLists, collectedBlobs);
    }
}
