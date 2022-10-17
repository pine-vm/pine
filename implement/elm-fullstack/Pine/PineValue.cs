using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine;

/// <summary>
/// This type describes values processes by Pine programs.
/// It is a choice type with two cases, <see cref="ListValue"/> and <see cref="BlobValue"/>.
/// Other kinds of data, like text or a file system directory, are encoded based on these primitives.
/// For the encoding of expressions and programs as Pine values, see <see cref="PineVM"/>.
/// </summary>
public abstract record PineValue : IEquatable<PineValue>
{
    static public PineValue Blob(ReadOnlyMemory<byte> bytes) =>
        new BlobValue(bytes);

    static public PineValue List(IReadOnlyList<PineValue> elements) =>
        new ListValue(elements);

    static public readonly PineValue EmptyList = List(ImmutableList<PineValue>.Empty);

    /// <summary>
    /// A <see cref="PineValue"/> that is a list of <see cref="PineValue"/>s.
    /// </summary>
    public record ListValue : PineValue, IEquatable<ListValue>
    {
        readonly int slimHashCode;

        public IReadOnlyList<PineValue> Elements { private init; get; }

        public ListValue(IReadOnlyList<PineValue> elements)
        {
            Elements = elements;

            var hash = new HashCode();

            foreach (var item in elements)
            {
                hash.Add(item.GetHashCode());
            }

            slimHashCode = hash.ToHashCode();
        }

        public virtual bool Equals(ListValue? other)
        {
            if (other is null)
                return false;

            return ReferenceEquals(this, other)
                ||
                (slimHashCode == other.slimHashCode &&
                Elements.Count == other.Elements.Count &&
                Elements.SequenceEqual(other.Elements));
        }

        public override int GetHashCode() => slimHashCode;
    }

    /// <summary>
    /// A <see cref="PineValue"/> that is a sequence of bytes.
    /// </summary>
    public record BlobValue : PineValue, IEquatable<BlobValue>
    {
        readonly int slimHashCode;

        public ReadOnlyMemory<byte> Bytes { private init; get; }

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
                (slimHashCode == other.slimHashCode &&
                Bytes.Span.SequenceEqual(other.Bytes.Span));
        }

        public override int GetHashCode() => slimHashCode;
    }
}

