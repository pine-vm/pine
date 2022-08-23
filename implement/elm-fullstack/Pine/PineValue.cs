using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine;

/// <summary>
/// This type describes values processes by Pine programs.
/// It is a discriminated union type with two cases, <see cref="ListValue"/> and <see cref="BlobValue"/>.
/// Other kinds of data, like text or a file system directory, are encoded based on these primitives.
/// For the encoding of expressions and programs as Pine values, see <see cref="PineVM"/>.
/// </summary>
public abstract record PineValue : IEquatable<PineValue>
{
    static public PineValue Blob(ReadOnlyMemory<byte> blobContent) =>
        new BlobValue(blobContent);

    static public PineValue List(IReadOnlyList<PineValue> listContent) =>
        new ListValue(listContent);

    static public readonly PineValue EmptyList = List(ImmutableList<PineValue>.Empty);

    /// <summary>
    /// A <see cref="PineValue"/> that is a list of <see cref="PineValue"/>s.
    /// </summary>
    public record ListValue : PineValue, IEquatable<ListValue>
    {
        readonly int slimHashCode;

        public IReadOnlyList<PineValue> ListContent { private init; get; }

        public ListValue(IReadOnlyList<PineValue> ListContent)
        {
            this.ListContent = ListContent;

            var hash = new HashCode();

            foreach (var item in ListContent)
            {
                hash.Add(item.GetHashCode());
            }

            slimHashCode = hash.ToHashCode();
        }

        public virtual bool Equals(ListValue? other)
        {
            if (other is null)
                return false;

            return
                slimHashCode == other.slimHashCode &&
                ListContent.Count == other.ListContent.Count &&
                ListContent.SequenceEqual(other.ListContent);
        }

        public override int GetHashCode() => slimHashCode;
    }

    /// <summary>
    /// A <see cref="PineValue"/> that is a sequence of bytes.
    /// </summary>
    public record BlobValue : PineValue, IEquatable<BlobValue>
    {
        readonly int slimHashCode;

        public ReadOnlyMemory<byte> BlobContent { private init; get; }

        public BlobValue(ReadOnlyMemory<byte> BlobContent)
        {
            this.BlobContent = BlobContent;

            var hash = new HashCode();

            hash.AddBytes(BlobContent.Span);

            slimHashCode = hash.ToHashCode();
        }

        public virtual bool Equals(BlobValue? other)
        {
            if (other is null)
                return false;

            return
                slimHashCode == other.slimHashCode &&
                BlobContent.Span.SequenceEqual(other.BlobContent.Span);
        }

        public override int GetHashCode() => slimHashCode;
    }
}

