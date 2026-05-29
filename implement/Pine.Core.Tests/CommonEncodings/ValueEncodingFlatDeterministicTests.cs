using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using System;
using System.Buffers.Binary;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

/// <summary>
/// Regression tests for <see cref="ValueEncodingFlatDeterministic"/> intended to lock down
/// the on-the-wire format and observable behaviour, so that future refactors (or migration
/// to a more generalized implementation) can be validated against the existing format.
///
/// The tests cover three orthogonal aspects:
///   1. The constants of the public API (tag identifiers, tag size).
///   2. The exact byte layout of the encoded form for representative small inputs
///      (header layout, big-endian length encoding, padding bytes, reference offsets).
///   3. Round-trip equality across a broad variety of inputs (empty, deeply nested,
///      mixed, large, all byte values, sharing of components, etc.) along with the
///      decode-time interning behaviour.
/// </summary>
public class ValueEncodingFlatDeterministicTests
{
    [Fact]
    public void Reuses_components()
    {
        var largeComponent =
            StringEncoding.ValueFromString(
                "building a value of size large enough so that non-duplicate encoding would become obvious");

        var compositionAlfa =
            PineValue.List(
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(71),
                    largeComponent),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(91)));

        var compositionBeta =
            PineValue.List(
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(71),
                    largeComponent),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(91),
                    largeComponent),
                largeComponent);

        using var compositionAlfaEncodedBytes = new System.IO.MemoryStream();

        ValueEncodingFlatDeterministic.Encode(compositionAlfaEncodedBytes, compositionAlfa);

        compositionAlfaEncodedBytes.Seek(
            offset: 0,
            SeekOrigin.Begin);

        var reproducedAlfa =
            ValueEncodingFlatDeterministic.DecodeRoot(compositionAlfaEncodedBytes.ToArray());

        using var compositionBetaEncodedBytes = new System.IO.MemoryStream();

        ValueEncodingFlatDeterministic.Encode(compositionBetaEncodedBytes, compositionBeta);

        compositionBetaEncodedBytes.Seek(
            offset: 0,
            SeekOrigin.Begin);

        var reproducedBeta =
            ValueEncodingFlatDeterministic.DecodeRoot(compositionBetaEncodedBytes.ToArray());

        reproducedAlfa.Should().Be(compositionAlfa);

        reproducedBeta.Should().Be(compositionBeta);

        compositionBetaEncodedBytes.Length.Should().BeLessThan(compositionAlfaEncodedBytes.Length * 2);
    }

    [Fact]
    public void Roundtrips()
    {
        IReadOnlyList<PineValue> testCases =
            [
            PineValue.EmptyBlob,
            PineValue.EmptyList,

            PineValue.List(PineValue.EmptyList),

            PineValue.List(PineValue.EmptyBlob),

            PineValue.List(
                PineValue.EmptyList,
                PineValue.EmptyList),

            PineValue.List(
                PineValue.EmptyList,
                PineValue.EmptyBlob),

            IntegerEncoding.EncodeSignedInteger(71),
            IntegerEncoding.EncodeSignedInteger(4171),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(4171),
                IntegerEncoding.EncodeSignedInteger(134171),
                IntegerEncoding.EncodeSignedInteger(43134171),
                IntegerEncoding.EncodeSignedInteger(8143134171)),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(131),
                IntegerEncoding.EncodeSignedInteger(71)),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(47),
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(131),
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(47)),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(47),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(71)),
                IntegerEncoding.EncodeSignedInteger(19),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(71))),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(47),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(43)),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(91)),
                IntegerEncoding.EncodeSignedInteger(21),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(91)),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(43))),

            Pine.PineVM.PopularExpression.BuildPopularValueDictionary().Values
            .OfType<PineValue.ListValue>()
            .OrderByDescending(l => l.NodesCount)
            .First()
            ];

        for (var i = 0; i < testCases.Count; i++)
        {
            var testCase = testCases[i];

            try
            {
                using var encodedStream = new System.IO.MemoryStream();

                ValueEncodingFlatDeterministic.Encode(encodedStream, testCase);

                var encodedFlat = encodedStream.ToArray();

                var decoded =
                    ValueEncodingFlatDeterministic.DecodeRoot(encodedFlat);

                decoded.Should().Be(testCase);
            }
            catch (System.Exception ex)
            {
                throw new System.Exception(
                    "Failed for test case [" + i + "] (" + testCase + ")",
                    innerException: ex);
            }
        }
    }

    /* ------------------------------------------------------------------ */
    /*  Constants of the public API                                        */
    /* ------------------------------------------------------------------ */

    [Fact]
    public void Public_tag_constants_have_the_expected_values()
    {
        // These constants are part of the on-the-wire format. Changing any of them
        // would break compatibility with previously encoded data.
        ValueEncodingFlatDeterministic.TagSize.Should().Be(4);
        ValueEncodingFlatDeterministic.TagBlob.Should().Be(1);
        ValueEncodingFlatDeterministic.TagList.Should().Be(3);
        ValueEncodingFlatDeterministic.TagReference.Should().Be(4);
    }

    /* ------------------------------------------------------------------ */
    /*  Exact byte layout for small inputs                                 */
    /* ------------------------------------------------------------------ */

    [Fact]
    public void Encodes_empty_blob_as_eight_bytes_with_big_endian_zero_length()
    {
        var encoded = EncodeToBytes(PineValue.EmptyBlob);

        encoded.Should().Equal(
        [
            0, 0, 0, 1, // tag = TagBlob
            0, 0, 0, 0, // length = 0
        ]);
    }

    [Fact]
    public void Encodes_empty_list_as_eight_bytes_with_big_endian_zero_length()
    {
        var encoded = EncodeToBytes(PineValue.EmptyList);

        encoded.Should().Equal(
        [
            0, 0, 0, 3, // tag = TagList
            0, 0, 0, 0, // item count = 0
        ]);
    }

    [Fact]
    public void Encodes_single_byte_blob_with_three_trailing_padding_zero_bytes()
    {
        var blob = PineValue.Blob([0x42]);

        var encoded = EncodeToBytes(blob);

        encoded.Should().Equal(
        [
            0, 0, 0, 1,    // tag = TagBlob
            0, 0, 0, 1,    // length = 1
            0x42,          // payload
            0, 0, 0,       // 3 padding bytes (len % 4 == 1)
        ]);
    }

    [Fact]
    public void Encodes_two_byte_blob_with_two_trailing_padding_zero_bytes()
    {
        var blob = PineValue.Blob([0xAA, 0xBB]);

        var encoded = EncodeToBytes(blob);

        encoded.Should().Equal(
        [
            0, 0, 0, 1,
            0, 0, 0, 2,
            0xAA, 0xBB,
            0, 0,
        ]);
    }

    [Fact]
    public void Encodes_three_byte_blob_with_one_trailing_padding_zero_byte()
    {
        var blob = PineValue.Blob([0x10, 0x20, 0x30]);

        var encoded = EncodeToBytes(blob);

        encoded.Should().Equal(
        [
            0, 0, 0, 1,
            0, 0, 0, 3,
            0x10, 0x20, 0x30,
            0,
        ]);
    }

    [Fact]
    public void Encodes_four_byte_blob_without_any_trailing_padding()
    {
        var blob = PineValue.Blob([0x01, 0x02, 0x03, 0x04]);

        var encoded = EncodeToBytes(blob);

        encoded.Should().Equal(
        [
            0, 0, 0, 1,
            0, 0, 0, 4,
            0x01, 0x02, 0x03, 0x04,
        ]);
    }

    [Fact]
    public void Encodes_blob_length_as_big_endian_int32()
    {
        // A length of 0x0102 (258) must be encoded as the bytes 00 00 01 02, not 02 01 00 00.
        var blob = PineValue.Blob(new byte[0x0102]);

        var encoded = EncodeToBytes(blob);

        encoded.AsSpan(0, 4).ToArray().Should().Equal([0, 0, 0, 1]);
        encoded.AsSpan(4, 4).ToArray().Should().Equal([0, 0, 0x01, 0x02]);
    }

    [Fact]
    public void Encodes_list_item_count_as_big_endian_int32()
    {
        // 260 = 0x00000104 - exposes the byte order of the length field.
        var items = Enumerable.Range(0, 260).Select(_ => (PineValue)PineValue.EmptyList).ToArray();

        var encoded = EncodeToBytes(PineValue.List(items));

        encoded.AsSpan(0, 4).ToArray().Should().Equal([0, 0, 0, 3]);
        encoded.AsSpan(4, 4).ToArray().Should().Equal([0, 0, 0x01, 0x04]);
    }

    [Fact]
    public void Encodes_duplicate_empty_blob_as_a_back_reference_with_negative_relative_offset()
    {
        // List of two identical empty blobs - the second occurrence must be encoded
        // as a TagReference pointing back to the first.
        var composition = PineValue.List(PineValue.EmptyBlob, PineValue.EmptyBlob);

        var encoded = EncodeToBytes(composition);

        // Layout:
        //   offset  0: list header   (8 bytes)  -> tag(3) + count(2)
        //   offset  8: first blob    (8 bytes)  -> tag(1) + len(0)
        //   offset 16: reference     (8 bytes)  -> tag(4) + rel(-12)
        //
        // Reference relative offset is computed against the read position AFTER
        // consuming the 4-byte tag (decoder side), which equals 20 on the encoder side.
        // Therefore rel = 8 - 20 = -12.
        encoded.Length.Should().Be(24);

        encoded.AsSpan(0, 8).ToArray().Should().Equal([0, 0, 0, 3, 0, 0, 0, 2]);
        encoded.AsSpan(8, 8).ToArray().Should().Equal([0, 0, 0, 1, 0, 0, 0, 0]);
        encoded.AsSpan(16, 4).ToArray().Should().Equal([0, 0, 0, 4]);
        BinaryPrimitives.ReadInt32BigEndian(encoded.AsSpan(20, 4)).Should().Be(-12);
    }

    [Fact]
    public void Encodes_duplicate_empty_list_as_a_back_reference_with_negative_relative_offset()
    {
        var composition = PineValue.List(PineValue.EmptyList, PineValue.EmptyList);

        var encoded = EncodeToBytes(composition);

        encoded.Length.Should().Be(24);

        encoded.AsSpan(0, 8).ToArray().Should().Equal([0, 0, 0, 3, 0, 0, 0, 2]);
        encoded.AsSpan(8, 8).ToArray().Should().Equal([0, 0, 0, 3, 0, 0, 0, 0]);
        encoded.AsSpan(16, 4).ToArray().Should().Equal([0, 0, 0, 4]);
        BinaryPrimitives.ReadInt32BigEndian(encoded.AsSpan(20, 4)).Should().Be(-12);
    }

    /* ------------------------------------------------------------------ */
    /*  Encoder/decoder API contracts                                      */
    /* ------------------------------------------------------------------ */

    [Fact]
    public void Encode_via_stream_and_via_action_delegate_produce_identical_bytes()
    {
        var composition =
            PineValue.List(
                PineValue.Blob([1, 2, 3]),
                PineValue.List(
                    PineValue.EmptyBlob,
                    PineValue.Blob([9, 8, 7, 6, 5])),
                PineValue.EmptyList);

        using var streamEncoded = new MemoryStream();
        ValueEncodingFlatDeterministic.Encode(streamEncoded, composition);

        var delegateBuffer = new MemoryStream();

        ValueEncodingFlatDeterministic.Encode(
            bytes => delegateBuffer.Write(bytes),
            composition);

        streamEncoded.ToArray().Should().Equal(delegateBuffer.ToArray());
    }

    [Fact]
    public void Encoding_the_same_value_repeatedly_produces_identical_bytes()
    {
        var composition =
            PineValue.List(
                PineValue.Blob([1, 2, 3, 4, 5]),
                PineValue.Blob([1, 2, 3, 4, 5]),
                PineValue.List(
                    PineValue.Blob([0xFF]),
                    PineValue.Blob([0xFF])));

        var first = EncodeToBytes(composition);
        var second = EncodeToBytes(composition);
        var third = EncodeToBytes(composition);

        second.Should().Equal(first);
        third.Should().Equal(first);
    }

    [Fact]
    public void Decoding_back_reference_returns_a_value_equal_to_the_referenced_value()
    {
        var blob = PineValue.Blob([1, 2, 3, 4, 5, 6, 7]);
        var composition = PineValue.List(blob, blob, blob);

        var encoded = EncodeToBytes(composition);
        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded);

        decoded.Should().Be(composition);

        var decodedList = decoded.Should().BeOfType<PineValue.ListValue>().Subject;
        decodedList.Items.Length.Should().Be(3);
        decodedList.Items.Span[0].Should().Be(blob);
        decodedList.Items.Span[1].Should().Be(blob);
        decodedList.Items.Span[2].Should().Be(blob);
    }

    [Fact]
    public void Decoding_back_reference_to_a_nested_earlier_list_resolves_correctly()
    {
        var sharedInner =
            PineValue.List(
                PineValue.Blob([0x11, 0x22]),
                PineValue.Blob([0x33, 0x44, 0x55]));

        var composition =
            PineValue.List(
                PineValue.List(sharedInner, PineValue.EmptyBlob),
                sharedInner);

        var encoded = EncodeToBytes(composition);
        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded);

        decoded.Should().Be(composition);
    }

    [Fact]
    public void Sharing_a_large_component_reduces_the_encoded_size_proportionally()
    {
        // A non-trivial blob, large enough that any duplication would dominate the encoded size.
        var largePayload = Enumerable.Range(0, 1000).Select(i => (byte)(i & 0xFF)).ToArray();
        var largeBlob = PineValue.Blob(largePayload);

        var singleOccurrence = PineValue.List(largeBlob);

        var manyOccurrences =
            PineValue.List(largeBlob, largeBlob, largeBlob, largeBlob, largeBlob);

        var singleEncoded = EncodeToBytes(singleOccurrence);
        var manyEncoded = EncodeToBytes(manyOccurrences);

        // The four additional references should each contribute only 8 bytes
        // (tag + relative offset), regardless of the blob's size.
        var expectedExtraBytes = 4 * (ValueEncodingFlatDeterministic.TagSize + 4);
        manyEncoded.Length.Should().Be(singleEncoded.Length + expectedExtraBytes);
    }

    [Fact]
    public void Triple_occurrence_of_a_list_encodes_first_in_full_and_subsequent_as_references()
    {
        var inner =
            PineValue.List(
                PineValue.Blob([1, 2, 3, 4]),
                PineValue.Blob([5, 6, 7, 8]));

        var composition = PineValue.List(inner, inner, inner);

        var encoded = EncodeToBytes(composition);
        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded);

        decoded.Should().Be(composition);

        // The encoded stream should contain exactly two reference markers
        // (one for the 2nd and one for the 3rd occurrence of "inner").
        CountReferenceMarkers(encoded).Should().Be(2);
    }

    [Fact]
    public void Reference_relative_offset_is_negative_so_back_references_only_point_to_earlier_bytes()
    {
        var blob = PineValue.Blob([0xDE, 0xAD]);
        var composition = PineValue.List(blob, blob, blob);

        var encoded = EncodeToBytes(composition);

        // Scan the stream and ensure every reference points strictly backwards.
        var span = encoded.AsSpan();
        var offset = 0;
        var foundAnyReference = false;

        while (offset + 4 <= span.Length)
        {
            var tag = BinaryPrimitives.ReadInt32BigEndian(span.Slice(offset, 4));

            if (tag == ValueEncodingFlatDeterministic.TagReference)
            {
                foundAnyReference = true;

                var relative = BinaryPrimitives.ReadInt32BigEndian(span.Slice(offset + 4, 4));

                relative.Should().BeLessThan(
                    0,
                    because: "back references must always point to earlier bytes");

                // The absolute target address must lie within the already-written prefix.
                var addressBase = offset + 4;
                var target = addressBase + relative;
                target.Should().BeGreaterThanOrEqualTo(0);
                target.Should().BeLessThan(offset);

                offset += 8;
                continue;
            }

            // Skip past non-reference entries by their on-the-wire length.
            offset += AdvancePastNonReferenceEntry(span, offset, tag);
        }

        foundAnyReference.Should().BeTrue();
    }

    /* ------------------------------------------------------------------ */
    /*  Decode-time interning                                              */
    /* ------------------------------------------------------------------ */

    [Fact]
    public void Decode_returns_the_provided_blob_instance_when_present_in_the_reuse_dictionary()
    {
        var sharedBytes = new byte[] { 9, 8, 7, 6, 5 };
        var reusableBlob = (PineValue.BlobValue)PineValue.Blob(sharedBytes);

        // Encode a fresh, structurally-equal blob (not the exact instance above).
        var encoded = EncodeToBytes(PineValue.Blob([.. sharedBytes]));

        var reuseDict =
            new Dictionary<int, FrozenSet<PineValue.BlobValue>>
            {
                [sharedBytes.Length] = new HashSet<PineValue.BlobValue> { reusableBlob }.ToFrozenSet(),
            };

        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded, blobInstancesToReuse: reuseDict);

        decoded.Should().BeSameAs(reusableBlob);
    }

    [Fact]
    public void Decode_returns_the_provided_list_instance_when_present_in_the_reuse_dictionary()
    {
        var reusableList =
            PineValue.List(
                PineValue.Blob([1]),
                PineValue.Blob([2]));

        var equivalent =
            PineValue.List(
                PineValue.Blob([1]),
                PineValue.Blob([2]));

        var encoded = EncodeToBytes(equivalent);

        var reuseDict =
            new Dictionary<int, FrozenSet<PineValue.ListValue>>
            {
                [reusableList.Items.Length] =
                new HashSet<PineValue.ListValue> { reusableList }.ToFrozenSet(),
            };

        var decoded =
            ValueEncodingFlatDeterministic.DecodeRoot(
                encoded,
                listInstancesToReuse: reuseDict);

        decoded.Should().BeSameAs(reusableList);
    }

    [Fact]
    public void Decode_returns_a_structurally_equal_value_when_the_reuse_dictionary_has_no_matching_entry()
    {
        var composition =
            PineValue.List(
                PineValue.Blob([0xAB, 0xCD]),
                PineValue.EmptyList);

        var encoded = EncodeToBytes(composition);

        // Provide reuse dictionaries that do not contain any matching entries
        // (different item-count / blob length buckets, or empty buckets).
        var emptyBlobDict =
            new Dictionary<int, FrozenSet<PineValue.BlobValue>>
            {
                [999] = new HashSet<PineValue.BlobValue>().ToFrozenSet(),
            };

        var emptyListDict =
            new Dictionary<int, FrozenSet<PineValue.ListValue>>
            {
                [999] = new HashSet<PineValue.ListValue>().ToFrozenSet(),
            };

        var decoded =
            ValueEncodingFlatDeterministic.DecodeRoot(
                encoded,
                blobInstancesToReuse: emptyBlobDict,
                listInstancesToReuse: emptyListDict);

        decoded.Should().Be(composition);
    }

    /* ------------------------------------------------------------------ */
    /*  Broad round-trip coverage                                          */
    /* ------------------------------------------------------------------ */

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    [InlineData(3)]
    [InlineData(4)]
    [InlineData(5)]
    [InlineData(6)]
    [InlineData(7)]
    [InlineData(8)]
    [InlineData(15)]
    [InlineData(16)]
    [InlineData(17)]
    [InlineData(31)]
    [InlineData(32)]
    [InlineData(33)]
    [InlineData(1023)]
    [InlineData(1024)]
    [InlineData(1025)]
    public void Roundtrips_blob_of_every_alignment_case(int blobSize)
    {
        var bytes = Enumerable.Range(0, blobSize).Select(i => (byte)((i * 37) & 0xFF)).ToArray();
        var blob = PineValue.Blob(bytes);

        var encoded = EncodeToBytes(blob);

        // Total encoded length must be a multiple of 4 (alignment invariant).
        (encoded.Length % 4).Should().Be(0);

        // Header layout: 4 bytes tag + 4 bytes length + payload + padding to multiple of 4.
        var expectedLength = ValueEncodingFlatDeterministic.TagSize + 4 + ((blobSize + 3) & ~3);
        encoded.Length.Should().Be(expectedLength);

        // Any padding bytes (between payload end and encoded end) must be zero.
        for (var i = 8 + blobSize; i < encoded.Length; i++)
        {
            encoded[i].Should().Be(0, because: "padding bytes must be zero");
        }

        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded);
        decoded.Should().Be(blob);
    }

    [Fact]
    public void Roundtrips_blob_containing_every_possible_byte_value()
    {
        var bytes = Enumerable.Range(0, 256).Select(i => (byte)i).ToArray();
        var blob = PineValue.Blob(bytes);

        var encoded = EncodeToBytes(blob);
        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded);

        decoded.Should().Be(blob);
    }

    [Fact]
    public void Roundtrips_a_deeply_nested_list()
    {
        // Build a nested list of depth 32 by repeated wrapping.
        PineValue current = PineValue.EmptyList;

        for (var i = 0; i < 32; i++)
        {
            current = PineValue.List(current);
        }

        var encoded = EncodeToBytes(current);
        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded);

        decoded.Should().Be(current);
    }

    [Fact]
    public void Roundtrips_a_wide_list_of_many_distinct_blobs()
    {
        var items =
            Enumerable.Range(0, 200)
            .Select(i => PineValue.Blob(BitConverter.GetBytes(i)))
            .ToArray();

        var composition = PineValue.List(items);

        var encoded = EncodeToBytes(composition);
        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded);

        decoded.Should().Be(composition);
    }

    [Fact]
    public void Roundtrips_a_mixed_tree_of_blobs_and_lists_with_internal_sharing()
    {
        var sharedBlob = PineValue.Blob([0xCA, 0xFE, 0xBA, 0xBE]);

        var sharedList =
            PineValue.List(
                PineValue.Blob([0x01]),
                PineValue.Blob([0x02, 0x03]),
                PineValue.EmptyList);

        var composition =
            PineValue.List(
                sharedBlob,
                sharedList,
                PineValue.List(
                    sharedBlob,
                    sharedList,
                    PineValue.List(sharedBlob, sharedList)),
                PineValue.EmptyBlob,
                PineValue.EmptyList,
                sharedList,
                sharedBlob);

        var encoded = EncodeToBytes(composition);
        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded);

        decoded.Should().Be(composition);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    [InlineData(7)]
    [InlineData(64)]
    public void Roundtrips_a_list_of_n_empty_blobs_using_a_single_full_entry_plus_n_minus_one_references(int count)
    {
        var items = Enumerable.Range(0, count).Select(_ => (PineValue)PineValue.EmptyBlob).ToArray();
        var composition = PineValue.List(items);

        var encoded = EncodeToBytes(composition);
        var decoded = ValueEncodingFlatDeterministic.DecodeRoot(encoded);

        decoded.Should().Be(composition);

        if (count >= 2)
        {
            // Exactly one full encoding of EmptyBlob plus (count - 1) reference entries.
            CountReferenceMarkers(encoded).Should().Be(count - 1);
        }
    }

    /* ------------------------------------------------------------------ */
    /*  Helpers                                                            */
    /* ------------------------------------------------------------------ */

    private static byte[] EncodeToBytes(PineValue value)
    {
        using var stream = new MemoryStream();
        ValueEncodingFlatDeterministic.Encode(stream, value);
        return stream.ToArray();
    }

    /// <summary>
    /// Walks the encoded stream entry-by-entry and counts how many <see cref="ValueEncodingFlatDeterministic.TagReference"/>
    /// entries appear. The walk uses the on-the-wire layout, so it implicitly
    /// validates that all non-reference headers are well-formed.
    /// </summary>
    private static int CountReferenceMarkers(ReadOnlySpan<byte> encoded)
    {
        var count = 0;
        var offset = 0;

        while (offset + 4 <= encoded.Length)
        {
            var tag = BinaryPrimitives.ReadInt32BigEndian(encoded.Slice(offset, 4));

            if (tag == ValueEncodingFlatDeterministic.TagReference)
            {
                count++;
                offset += 8;
                continue;
            }

            offset += AdvancePastNonReferenceEntry(encoded, offset, tag);
        }

        offset.Should().Be(
            encoded.Length,
            because: "the stream must be fully consumed when stepping through entries");

        return count;
    }

    /// <summary>
    /// Returns the number of bytes occupied by a non-reference entry header (plus its inline payload),
    /// given the entry's tag. List headers are 8 bytes (their items are walked in subsequent steps);
    /// blob headers are 8 bytes plus the padded payload.
    /// </summary>
    private static int AdvancePastNonReferenceEntry(ReadOnlySpan<byte> encoded, int offset, int tag)
    {
        if (tag == ValueEncodingFlatDeterministic.TagList)
        {
            return 8;
        }

        if (tag == ValueEncodingFlatDeterministic.TagBlob)
        {
            var length = BinaryPrimitives.ReadInt32BigEndian(encoded.Slice(offset + 4, 4));
            var paddedLength = (length + 3) & ~3;
            return 8 + paddedLength;
        }

        throw new InvalidOperationException("Unexpected tag at offset " + offset + ": " + tag);
    }
}
