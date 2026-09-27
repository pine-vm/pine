using System;

namespace Pine.Core.CommonEncodings;

public static partial class ValueEncodingBinaryDeterministic
{
    /// <summary>
    /// Errors that can occur when decoding a <see cref="PineValue"/> using <see cref="DecodeRoot"/>.
    /// </summary>
    public abstract record DecodeError
    {
        /// <summary>
        /// The input ended before the complete value was read.
        /// </summary>
        /// <param name="Offset">Offset at which more input was expected.</param>
        /// <param name="Expected">Description of the expected element.</param>
        public sealed record UnexpectedEndOfInput(
            long Offset,
            string Expected)
            : DecodeError
        {
            /// <inheritdoc/>
            public override string ToString() =>
                "Unexpected end of input at offset " + Offset + ": Expected " + Expected + ".";
        }

        /// <summary>
        /// The entry starting at <paramref name="Offset"/> has a tag that is not supported.
        /// </summary>
        public sealed record UnknownTag(
            long Offset,
            int Tag)
            : DecodeError
        {
            /// <inheritdoc/>
            public override string ToString() =>
                "Decoding of this PineValue type is not implemented for tag " + Tag +
                " at offset " + Offset + ".";
        }

        /// <summary>
        /// The entry starting at <paramref name="Offset"/> declares a negative length (item count or byte count).
        /// </summary>
        public sealed record NegativeLength(
            long Offset,
            int Tag,
            int Length)
            : DecodeError
        {
            /// <inheritdoc/>
            public override string ToString() =>
                "Invalid negative length for tag " + Tag + " at offset " + Offset + ": " + Length;
        }

        /// <summary>
        /// The entry starting at <paramref name="Offset"/> declares a length that does not fit in the remaining input.
        /// </summary>
        public sealed record LengthExceedsRemainingInput(
            long Offset,
            int Tag,
            int Length,
            long RemainingBytes)
            : DecodeError
        {
            /// <inheritdoc/>
            public override string ToString() =>
                "Length for tag " + Tag + " at offset " + Offset + " (" + Length +
                ") exceeds the remaining input length (" + RemainingBytes + " bytes).";
        }

        /// <summary>
        /// An internal reference starting at <paramref name="Offset"/> does not point to the start of a
        /// completely decoded value.
        /// </summary>
        public sealed record InvalidInternalReference(
            long Offset,
            long ReferencedAddress)
            : DecodeError
        {
            /// <inheritdoc/>
            public override string ToString() =>
                "Invalid reference at offset " + Offset +
                ": No completely decoded value starts at offset " + ReferencedAddress + ".";
        }

        /// <summary>
        /// The delegate for resolving external references returned no value for the reference
        /// in the entry starting at <paramref name="Offset"/>.
        /// </summary>
        public sealed record ExternalReferenceNotFound(
            long Offset,
            ReadOnlyMemory<byte> Reference)
            : DecodeError
        {
            /// <inheritdoc/>
            public override string ToString() =>
                "External reference at offset " + Offset + " not found: " +
                Convert.ToHexStringLower(Reference.Span);

            /// <inheritdoc/>
            public bool Equals(ExternalReferenceNotFound? other) =>
                other is not null &&
                Offset == other.Offset &&
                Reference.Span.SequenceEqual(other.Reference.Span);

            /// <inheritdoc/>
            public override int GetHashCode()
            {
                var hash = new HashCode();

                hash.Add(Offset);
                hash.AddBytes(Reference.Span);

                return hash.ToHashCode();
            }
        }
    }
}
