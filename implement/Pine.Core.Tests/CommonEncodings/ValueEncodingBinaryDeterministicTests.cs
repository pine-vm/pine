using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using System;
using System.Buffers.Binary;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Xunit;

using DecodeError = Pine.Core.CommonEncodings.ValueEncodingBinaryDeterministic.DecodeError;

namespace Pine.Core.Tests.CommonEncodings;

/// <summary>
/// Regression tests for <see cref="ValueEncodingBinaryDeterministic"/> intended to lock down
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
public class ValueEncodingBinaryDeterministicTests
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

        using var compositionAlfaEncodedBytes = new MemoryStream();

        ValueEncodingBinaryDeterministic.Encode(compositionAlfaEncodedBytes, compositionAlfa);

        compositionAlfaEncodedBytes.Seek(
            offset: 0,
            SeekOrigin.Begin);

        var reproducedAlfa =
            DecodeRootExpectingOk(compositionAlfaEncodedBytes.ToArray());

        using var compositionBetaEncodedBytes = new MemoryStream();

        ValueEncodingBinaryDeterministic.Encode(compositionBetaEncodedBytes, compositionBeta);

        compositionBetaEncodedBytes.Seek(
            offset: 0,
            SeekOrigin.Begin);

        var reproducedBeta =
            DecodeRootExpectingOk(compositionBetaEncodedBytes.ToArray());

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
                using var encodedStream = new MemoryStream();

                ValueEncodingBinaryDeterministic.Encode(encodedStream, testCase);

                var encodedFlat = encodedStream.ToArray();

                var decoded =
                    DecodeRootExpectingOk(encodedFlat);

                decoded.Should().Be(testCase);
            }
            catch (Exception ex)
            {
                throw new Exception(
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
        ValueEncodingBinaryDeterministic.TagSize.Should().Be(4);
        ValueEncodingBinaryDeterministic.TagBlob.Should().Be(1);
        ValueEncodingBinaryDeterministic.TagList.Should().Be(3);
        ValueEncodingBinaryDeterministic.TagReferenceInteral.Should().Be(4);
        ValueEncodingBinaryDeterministic.TagReferenceExternal256.Should().Be(5);
        ValueEncodingBinaryDeterministic.ReferenceExternal256Length.Should().Be(32);
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
        ValueEncodingBinaryDeterministic.Encode(streamEncoded, composition);

        var delegateBuffer = new MemoryStream();

        ValueEncodingBinaryDeterministic.Encode(
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
        var decoded = DecodeRootExpectingOk(encoded);

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
        var decoded = DecodeRootExpectingOk(encoded);

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
        var expectedExtraBytes = 4 * (ValueEncodingBinaryDeterministic.TagSize + 4);
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
        var decoded = DecodeRootExpectingOk(encoded);

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

            if (tag == ValueEncodingBinaryDeterministic.TagReferenceInteral)
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

        var decoded = DecodeRootExpectingOk(encoded, blobInstancesToReuse: reuseDict);

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
            DecodeRootExpectingOk(
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
            DecodeRootExpectingOk(
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
        var expectedLength = ValueEncodingBinaryDeterministic.TagSize + 4 + ((blobSize + 3) & ~3);
        encoded.Length.Should().Be(expectedLength);

        // Any padding bytes (between payload end and encoded end) must be zero.
        for (var i = 8 + blobSize; i < encoded.Length; i++)
        {
            encoded[i].Should().Be(0, because: "padding bytes must be zero");
        }

        var decoded = DecodeRootExpectingOk(encoded);
        decoded.Should().Be(blob);
    }

    [Fact]
    public void Roundtrips_blob_containing_every_possible_byte_value()
    {
        var bytes = Enumerable.Range(0, 256).Select(i => (byte)i).ToArray();
        var blob = PineValue.Blob(bytes);

        var encoded = EncodeToBytes(blob);
        var decoded = DecodeRootExpectingOk(encoded);

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
        var decoded = DecodeRootExpectingOk(encoded);

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
        var decoded = DecodeRootExpectingOk(encoded);

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
        var decoded = DecodeRootExpectingOk(encoded);

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
        var decoded = DecodeRootExpectingOk(encoded);

        decoded.Should().Be(composition);

        if (count >= 2)
        {
            // Exactly one full encoding of EmptyBlob plus (count - 1) reference entries.
            CountReferenceMarkers(encoded).Should().Be(count - 1);
        }
    }

    /* ------------------------------------------------------------------ */
    /*  Malformed input                                                    */
    /* ------------------------------------------------------------------ */

    [Fact]
    public void Decode_returns_err_for_empty_input()
    {
        DecodeRootExpectingErr(Array.Empty<byte>())
            .Should().Be(new DecodeError.UnexpectedEndOfInput(Offset: 0, Expected: "tag"));
    }

    [Fact]
    public void Decode_returns_err_for_truncated_tag()
    {
        DecodeRootExpectingErr(new byte[] { 0, 0, 0 })
            .Should().Be(new DecodeError.UnexpectedEndOfInput(Offset: 0, Expected: "tag"));
    }

    [Fact]
    public void Decode_returns_err_for_missing_length()
    {
        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagBlob))
            .Should().Be(new DecodeError.UnexpectedEndOfInput(Offset: 4, Expected: "byte count of blob"));

        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagList))
            .Should().Be(new DecodeError.UnexpectedEndOfInput(Offset: 4, Expected: "item count of list"));

        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagReferenceInteral))
            .Should().Be(new DecodeError.UnexpectedEndOfInput(Offset: 4, Expected: "relative address of reference"));
    }

    [Fact]
    public void Decode_returns_err_for_unknown_tag()
    {
        var error = DecodeRootExpectingErr(Int32Sequence(7, 0));

        error.Should().Be(new DecodeError.UnknownTag(Offset: 0, Tag: 7));

        error.ToString().Should().Contain("tag 7");
    }

    [Fact]
    public void Decode_returns_err_for_blob_length_exceeding_input()
    {
        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagBlob, 5, 0))
            .Should().Be(
            new DecodeError.LengthExceedsRemainingInput(
                Offset: 0,
                Tag: ValueEncodingBinaryDeterministic.TagBlob,
                Length: 5,
                RemainingBytes: 4));

        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagBlob, int.MaxValue))
            .Should().Be(
            new DecodeError.LengthExceedsRemainingInput(
                Offset: 0,
                Tag: ValueEncodingBinaryDeterministic.TagBlob,
                Length: int.MaxValue,
                RemainingBytes: 0));
    }

    [Fact]
    public void Decode_returns_err_for_negative_lengths()
    {
        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagBlob, -1))
            .Should().Be(
            new DecodeError.NegativeLength(Offset: 0, Tag: ValueEncodingBinaryDeterministic.TagBlob, Length: -1));

        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagList, -1))
            .Should().Be(
            new DecodeError.NegativeLength(Offset: 0, Tag: ValueEncodingBinaryDeterministic.TagList, Length: -1));
    }

    [Fact]
    public void Decode_returns_err_for_list_item_count_exceeding_input()
    {
        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagList, int.MaxValue))
            .Should().Be(
            new DecodeError.LengthExceedsRemainingInput(
                Offset: 0,
                Tag: ValueEncodingBinaryDeterministic.TagList,
                Length: int.MaxValue,
                RemainingBytes: 0));

        // Declares two items but contains only one.
        DecodeRootExpectingErr(
            Int32Sequence(
                ValueEncodingBinaryDeterministic.TagList,
                2,
                ValueEncodingBinaryDeterministic.TagBlob,
                0))
            .Should().Be(
            new DecodeError.LengthExceedsRemainingInput(
                Offset: 0,
                Tag: ValueEncodingBinaryDeterministic.TagList,
                Length: 2,
                RemainingBytes: 8));
    }

    [Fact]
    public void Decode_returns_err_for_truncated_nested_list()
    {
        var encoded =
            EncodeToBytes(
                PineValue.List(
                    PineValue.List(PineValue.Blob([1]), PineValue.Blob([2])),
                    PineValue.Blob([3])));

        for (var length = 0; length < encoded.Length; length++)
        {
            ValueEncodingBinaryDeterministic.DecodeRoot(encoded.AsMemory(0, length))
                .IsErr().Should().BeTrue("truncated to " + length + " bytes");
        }
    }

    [Fact]
    public void Decode_returns_err_for_invalid_reference()
    {
        // Reference pointing to itself (no completed value there).
        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagReferenceInteral, -4))
            .Should().Be(new DecodeError.InvalidInternalReference(Offset: 0, ReferencedAddress: 0));

        // Reference pointing outside of the input.
        DecodeRootExpectingErr(Int32Sequence(ValueEncodingBinaryDeterministic.TagReferenceInteral, int.MinValue))
            .Should().Be(
            new DecodeError.InvalidInternalReference(Offset: 0, ReferencedAddress: 4L + int.MinValue));

        // Reference from inside a list to the enclosing (not yet completed) list.
        DecodeRootExpectingErr(
            Int32Sequence(
                ValueEncodingBinaryDeterministic.TagList,
                1,
                ValueEncodingBinaryDeterministic.TagReferenceInteral,
                -12))
            .Should().Be(new DecodeError.InvalidInternalReference(Offset: 8, ReferencedAddress: 0));
    }

    [Fact]
    public void Encode_and_decode_deeply_nested_list_without_stack_overflow()
    {
        PineValue value = PineValue.EmptyList;

        for (var i = 0; i < 200_000; i++)
        {
            value = PineValue.List(value);
        }

        var encoded = EncodeToBytes(value);

        DecodeRootExpectingOk(encoded).Should().Be(value);
    }

    /* ------------------------------------------------------------------ */
    /*  External references                                                */
    /* ------------------------------------------------------------------ */

    [Fact]
    public void Decode_resolves_external_reference_at_root_via_delegate()
    {
        var reference = ExternalReferenceBytes(seed: 17);

        var externalValue = PineValue.List(PineValue.Blob([1, 2, 3]), PineValue.EmptyList);

        var receivedReferences = new List<byte[]>();

        var decoded =
            DecodeRootExpectingOk(
                EncodeExternalReference(reference),
                resolveExternalReference:
                requested =>
                {
                    receivedReferences.Add(requested.ToArray());

                    return externalValue;
                });

        decoded.Should().BeSameAs(externalValue);

        receivedReferences.Should().HaveCount(1);
        receivedReferences[0].Should().Equal(reference);
    }

    [Fact]
    public void Decode_resolves_external_references_nested_in_lists()
    {
        var referenceAlfa = ExternalReferenceBytes(seed: 1);
        var referenceBeta = ExternalReferenceBytes(seed: 2);

        var valueAlfa = PineValue.Blob([0xAA]);
        var valueBeta = PineValue.List(PineValue.Blob([0xBB]));

        var encoded =
            Concat(
                Int32Sequence(ValueEncodingBinaryDeterministic.TagList, 3),
                EncodeExternalReference(referenceAlfa),
                Int32Sequence(ValueEncodingBinaryDeterministic.TagBlob, 1),
                [0x07, 0, 0, 0],
                EncodeExternalReference(referenceBeta));

        var decoded =
            DecodeRootExpectingOk(
                encoded,
                resolveExternalReference:
                requested =>
                requested.Span.SequenceEqual(referenceAlfa)
                ?
                valueAlfa
                :
                requested.Span.SequenceEqual(referenceBeta)
                ?
                valueBeta
                :
                null);

        decoded.Should().Be(
            PineValue.List(
                valueAlfa,
                PineValue.Blob([0x07]),
                valueBeta));
    }

    [Fact]
    public void Decode_supports_internal_reference_to_external_reference_entry()
    {
        var reference = ExternalReferenceBytes(seed: 3);

        var externalValue = PineValue.Blob([4, 5, 6, 7, 8]);

        // List [ external, reference to the external entry at offset 8 ].
        // The internal reference is located at offset 8 + 4 + 32 = 44, its address base is 48.
        var encoded =
            Concat(
                Int32Sequence(ValueEncodingBinaryDeterministic.TagList, 2),
                EncodeExternalReference(reference),
                Int32Sequence(ValueEncodingBinaryDeterministic.TagReferenceInteral, 8 - 48));

        var resolveCount = 0;

        var decoded =
            DecodeRootExpectingOk(
                encoded,
                resolveExternalReference:
                _ =>
                {
                    resolveCount++;
                    return externalValue;
                });

        decoded.Should().Be(PineValue.List(externalValue, externalValue));

        resolveCount.Should().Be(1);
    }

    [Fact]
    public void Decode_returns_err_containing_reference_when_delegate_returns_null()
    {
        var reference = ExternalReferenceBytes(seed: 0x40);

        var encoded =
            Concat(
                Int32Sequence(ValueEncodingBinaryDeterministic.TagList, 1),
                EncodeExternalReference(reference));

        var error =
            ValueEncodingBinaryDeterministic.DecodeRoot(
                encoded,
                resolveExternalReference: _ => null);

        var notFound =
            error.Should().BeOfType<Result<DecodeError, PineValue>.Err>()
            .Which.Value.Should().BeOfType<DecodeError.ExternalReferenceNotFound>()
            .Subject;

        notFound.Offset.Should().Be(8);
        notFound.Reference.ToArray().Should().Equal(reference);

        notFound.Should().Be(new DecodeError.ExternalReferenceNotFound(Offset: 8, Reference: reference.ToArray()));

        notFound.ToString().Should().Contain(Convert.ToHexStringLower(reference));
    }

    [Fact]
    public void Decode_returns_err_for_external_reference_when_no_delegate_given()
    {
        var reference = ExternalReferenceBytes(seed: 9);

        DecodeRootExpectingErr(EncodeExternalReference(reference))
            .Should().Be(new DecodeError.ExternalReferenceNotFound(Offset: 0, Reference: reference));
    }

    [Fact]
    public void Decode_returns_err_for_truncated_external_reference()
    {
        var encoded = EncodeExternalReference(ExternalReferenceBytes(seed: 5));

        var resolveCount = 0;

        for (var length = 4; length < encoded.Length; length++)
        {
            ValueEncodingBinaryDeterministic.DecodeRoot(
                encoded.AsMemory(0, length),
                resolveExternalReference:
                _ =>
                {
                    resolveCount++;
                    return PineValue.EmptyList;
                })
                .Should().Be(
                Result<DecodeError, PineValue>.err(
                    new DecodeError.UnexpectedEndOfInput(Offset: 4, Expected: "reference of external reference")));
        }

        resolveCount.Should().Be(0);
    }

    [Fact]
    public void Encode_does_not_emit_external_references()
    {
        var composition =
            PineValue.List(
                PineValue.Blob([1, 2, 3]),
                PineValue.List(PineValue.Blob([1, 2, 3]), PineValue.EmptyList),
                PineValue.EmptyList);

        var encoded = EncodeToBytes(composition);

        // The walk only accepts blob, list, and internal reference entries.
        CountReferenceMarkers(encoded).Should().BeGreaterThan(0);
    }

    /* ------------------------------------------------------------------ */
    /*  Helpers                                                            */
    /* ------------------------------------------------------------------ */

    private static byte[] EncodeToBytes(PineValue value)
    {
        using var stream = new MemoryStream();

        ValueEncodingBinaryDeterministic.Encode(stream, value);

        return stream.ToArray();
    }

    private static PineValue DecodeRootExpectingOk(
        ReadOnlyMemory<byte> sourceBytes,
        IReadOnlyDictionary<int, FrozenSet<PineValue.BlobValue>>? blobInstancesToReuse = null,
        IReadOnlyDictionary<int, FrozenSet<PineValue.ListValue>>? listInstancesToReuse = null,
        Func<ReadOnlyMemory<byte>, PineValue?>? resolveExternalReference = null)
    {
        var decodeResult =
            ValueEncodingBinaryDeterministic.DecodeRoot(
                sourceBytes,
                blobInstancesToReuse: blobInstancesToReuse,
                listInstancesToReuse: listInstancesToReuse,
                resolveExternalReference: resolveExternalReference);

        if (decodeResult is Result<DecodeError, PineValue>.Ok ok)
        {
            return ok.Value;
        }

        throw new Exception("Expected decoding to succeed, but got: " + decodeResult);
    }

    private static DecodeError DecodeRootExpectingErr(ReadOnlyMemory<byte> sourceBytes)
    {
        var decodeResult = ValueEncodingBinaryDeterministic.DecodeRoot(sourceBytes);

        if (decodeResult is Result<DecodeError, PineValue>.Err err)
        {
            return err.Value;
        }

        throw new Exception("Expected decoding to fail, but got: " + decodeResult);
    }

    private static byte[] ExternalReferenceBytes(byte seed)
    {
        var bytes = new byte[ValueEncodingBinaryDeterministic.ReferenceExternal256Length];

        for (var i = 0; i < bytes.Length; i++)
        {
            bytes[i] = (byte)(seed + i * 7);
        }

        return bytes;
    }

    private static byte[] EncodeExternalReference(byte[] reference) =>
        Concat(Int32Sequence(ValueEncodingBinaryDeterministic.TagReferenceExternal256), reference);

    private static byte[] Concat(params byte[][] parts) =>
        [.. parts.SelectMany(part => part)];

    private static byte[] Int32Sequence(params int[] values)
    {
        var bytes = new byte[values.Length * 4];

        for (var i = 0; i < values.Length; i++)
        {
            BinaryPrimitives.WriteInt32BigEndian(bytes.AsSpan(i * 4, 4), values[i]);
        }

        return bytes;
    }

    /// <summary>
    /// Walks the encoded stream entry-by-entry and counts how many <see cref="ValueEncodingBinaryDeterministic.TagReferenceInteral"/>
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

            if (tag == ValueEncodingBinaryDeterministic.TagReferenceInteral)
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
        if (tag == ValueEncodingBinaryDeterministic.TagList)
        {
            return 8;
        }

        if (tag == ValueEncodingBinaryDeterministic.TagBlob)
        {
            var length = BinaryPrimitives.ReadInt32BigEndian(encoded.Slice(offset + 4, 4));
            var paddedLength = (length + 3) & ~3;
            return 8 + paddedLength;
        }

        throw new InvalidOperationException("Unexpected tag at offset " + offset + ": " + tag);
    }
}
