using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System.Collections.Generic;
using Xunit;

namespace Pine.IntegrationTests;

public class ElmValueTests
{
    [Fact]
    public void Elm_value_encoding_roundtrips()
    {
        var testCases = (IReadOnlyList<ElmValue>)[
            ElmValue.CharInstance('a'),
            ElmValue.Integer(42),
            ElmValue.StringInstance("Hello, world!"),
            new ElmValue.ElmList([
                ElmValue.Integer(31),
                ElmValue.Integer(37),
                ElmValue.Integer(39)]),
            new ElmValue.ElmRecord([
                ("alfa", ElmValue.Integer(1)),
                ("beta", ElmValue.Integer(2)),
                ("gamma", ElmValue.Integer(3)),
            ]),

            ElmValue.StringInstance("Hello, world 👋"),

            ElmValue.TagInstance("True", []),
            ElmValue.TagInstance("False", []),

            new ElmValue.ElmList([
                new ElmValue.ElmList([
                    ElmValue.Integer(7),
                    ElmValue.Integer(13)]),
                new ElmValue.ElmList([
                    ElmValue.Integer(41),
                    ElmValue.Integer(43),
                    ElmValue.Integer(47)]),
            ]),

            new ElmValue.ElmBytes(System.ReadOnlyMemory<byte>.Empty),
            new ElmValue.ElmBytes((byte[])[0]),
            new ElmValue.ElmBytes((byte[])[11, 13, 17]),
            new ElmValue.ElmBytes((byte[])[0, 13, 17]),

            ElmValue.ElmFloat.Convert(0),
            ElmValue.ElmFloat.Convert(0.3),
            ElmValue.ElmFloat.Convert(1.7),
            ElmValue.ElmFloat.Convert(-0.5),
        ];

        foreach (var testCase in testCases)
        {
            var pineValue = ElmValueEncoding.ElmValueAsPineValue(testCase);

            var roundtrip =
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new System.Exception(err));

            roundtrip.Should().Be(testCase);
        }
    }

    [Fact]
    public void Elm_value_display_as_expression()
    {
        var testCases =
            (IReadOnlyList<(ElmValue, string)>)
            [
                (ElmValue.Integer(42), "42"),

                (ElmValue.CharInstance('a'), "'a'"),

                (ElmValue.StringInstance("Hello, world!"), "\"Hello, world!\""),

                (new ElmValue.ElmList([
                ElmValue.Integer(31),
                    ElmValue.Integer(37),
                    ElmValue.Integer(39)]),
                    "[ 31, 37, 39 ]"),

                (new ElmValue.ElmRecord([
                ("alfa", ElmValue.Integer(1)),
                    ("beta", ElmValue.Integer(2)),
                    ("gamma", ElmValue.Integer(3))]),
                "{ alfa = 1, beta = 2, gamma = 3 }"),

                (ElmValue.TagInstance("Just", [ElmValue.Integer(43)]),
                "Just 43"),

                (ElmValue.TagInstance("Nothing", []),
                "Nothing"),

                (ElmValue.TagInstance("Just", [ElmValue.TagInstance("Nothing", [])]),
                "Just Nothing"),

                (ElmValue.TagInstance("Just", [ElmValue.TagInstance("Just", [ElmValue.Integer(47)])]),
                "Just (Just 47)"),

                (new ElmValue.ElmBytes(System.ReadOnlyMemory<byte>.Empty),
                "<0 bytes>"),

                (new ElmValue.ElmBytes((byte[])[0]),
                "<1 bytes>"),

                (new ElmValue.ElmBytes((byte[])[11, 13, 17]),
                "<3 bytes>"),

                (new ElmValue.ElmBytes((byte[])[0, 13, 17]),
                "<3 bytes>"),

                (ElmValue.ElmFloat.Convert(0),
                "0"),

                (ElmValue.ElmFloat.Convert(0.3),
                "0.3"),

                (ElmValue.ElmFloat.Convert(1.7),
                "1.7"),

                (ElmValue.ElmFloat.Convert(-0.5),
                "-0.5"),

                (ElmValue.ListInstance(
                    [
                    ElmValue.Integer(1),
                    ElmValue.StringInstance("Hello"),
                    ]),
                "(1, \"Hello\")"),
            ];

        foreach (var (elmValue, expectedExpression) in testCases)
        {
            var (expressionString, _) =
                ElmValue.RenderAsElmExpression(elmValue);

            expressionString.Should().Be(expectedExpression);
        }
    }

    [Fact]
    public void Shallow_parsing_as_record()
    {
        var elmRecord = new ElmValue.ElmRecord(
            [("alfa", ElmValue.Integer(11)),
             ("beta", ElmValue.Integer(13)),
             ("gamma", ElmValue.Integer(17))
             ]);

        var asPineValue = ElmValueEncoding.ElmValueAsPineValue(elmRecord);

        var parseResult = ElmValueEncoding.ParsePineValueAsRecordTagged(asPineValue);

        if (parseResult is Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>>.Err parseAsRecordErr)
        {
            "Failed parsing as record".Should().BeNull(parseAsRecordErr.Value);
        }

        if (parseResult is not Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>>.Ok parseAsRecordOk)
        {
            throw new System.Exception(
                "Unexpected parse result type: " + parseResult.GetType().FullName);
        }

        var parsedFields = parseAsRecordOk.Value;

        parsedFields.Should().HaveCount(3);

        parsedFields[0].fieldName.Should().Be("alfa");
        parsedFields[0].fieldValue.Should().Be(IntegerEncoding.EncodeSignedInteger(11));

        parsedFields[1].fieldName.Should().Be("beta");
        parsedFields[1].fieldValue.Should().Be(IntegerEncoding.EncodeSignedInteger(13));

        parsedFields[2].fieldName.Should().Be("gamma");
        parsedFields[2].fieldValue.Should().Be(IntegerEncoding.EncodeSignedInteger(17));
    }

    [Fact]
    public void Elm_value_encoding_orders_record_fields()
    {
        /*
         * When emitting equality checks, the implementation of the Elm compiler assumes record fields are always ordered.
         * */

        var elmRecordFormAlfa =
            new ElmValue.ElmRecord(
                [
                ("beta", ElmValue.Integer(11)),
                ("alfa", ElmValue.Integer(13)),
                ("gamma", ElmValue.Integer(17))
                ]);

        var elmRecordFormBeta =
            new ElmValue.ElmRecord(
                [
                ("alfa", ElmValue.Integer(13)),
                ("beta", ElmValue.Integer(11)),
                ("gamma", ElmValue.Integer(17))
                ]);

        var asPineValueFormAlfa = ElmValueEncoding.ElmValueAsPineValue(elmRecordFormAlfa);

        var asPineValueFormBeta = ElmValueEncoding.ElmValueAsPineValue(elmRecordFormBeta);

        asPineValueFormAlfa.Should().Be(asPineValueFormBeta);
    }

    /// <summary>
    /// Verifies that the new (post-2025) encoder produces the flat record layout
    /// <c>[tag, name0, value0, name1, value1, ...]</c> with field names sorted
    /// alphabetically (ordinal) and the new <c>&lt;Record_Type&gt;</c> tag.
    /// </summary>
    [Fact]
    public void Elm_record_encoder_emits_flat_layout_with_new_tag()
    {
        var elmRecord =
            new ElmValue.ElmRecord(
                [
                ("beta", ElmValue.Integer(13)),
                ("alfa", ElmValue.Integer(11)),
                ("gamma", ElmValue.Integer(17))
                ]);

        var asPineValue = ElmValueEncoding.ElmValueAsPineValue(elmRecord);

        var asListValue = (PineValue.ListValue)asPineValue;

        // 1 tag + 2 items per field × 3 fields = 7 items.
        asListValue.Items.Length.Should().Be(7);

        // First item must be the new tag, not the legacy one.
        asListValue.Items.Span[0].Should().Be(ElmValue.ElmRecordTypeTagNameAsValue);
        asListValue.Items.Span[0].Should().NotBe(ElmValue.ElmRecordTypeTagNameAsValue_2025);

        // Field names alternate with values starting at index 1, sorted alphabetically.
        asListValue.Items.Span[1].Should().Be(StringEncoding.ValueFromString("alfa"));
        asListValue.Items.Span[2].Should().Be(IntegerEncoding.EncodeSignedInteger(11));
        asListValue.Items.Span[3].Should().Be(StringEncoding.ValueFromString("beta"));
        asListValue.Items.Span[4].Should().Be(IntegerEncoding.EncodeSignedInteger(13));
        asListValue.Items.Span[5].Should().Be(StringEncoding.ValueFromString("gamma"));
        asListValue.Items.Span[6].Should().Be(IntegerEncoding.EncodeSignedInteger(17));
    }

    /// <summary>
    /// Verifies that values still persisted in the legacy nested record layout
    /// <c>[Elm_Record, [[ [name, value], ... ]]]</c> remain decodable through
    /// both the type-specific <c>ParsePineValueAsRecordTagged_2025</c> entry point
    /// and the general <c>PineValueAsElmValue</c> fast path.
    /// </summary>
    [Fact]
    public void Decoder_still_accepts_legacy_2025_record_layout()
    {
        var legacyEncoded =
            ElmValueEncoding.ElmRecordAsPineValue_2025(
                [
                ("alfa", ElmValue.Integer(11)),
                ("beta", ElmValue.Integer(13)),
                ("gamma", ElmValue.Integer(17))
                ],
                additionalReusableEncodings: null,
                reportNewEncoding: null);

        // Spot-check the legacy shape: [Elm_Record, [[ [name, value], ... ]]].
        var legacyAsListValue = (PineValue.ListValue)legacyEncoded;
        legacyAsListValue.Items.Length.Should().Be(2);
        legacyAsListValue.Items.Span[0].Should().Be(ElmValue.ElmRecordTypeTagNameAsValue_2025);

        // 1) Type-specific legacy parser.
        var taggedParseResult =
            ElmValueEncoding.ParsePineValueAsRecordTagged_2025(legacyEncoded);

        if (taggedParseResult is not Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>>.Ok taggedOk)
        {
            throw new System.Exception(
                "Legacy parser failed: " + taggedParseResult.GetType().FullName);
        }

        taggedOk.Value.Should().HaveCount(3);
        taggedOk.Value[0].fieldName.Should().Be("alfa");
        taggedOk.Value[1].fieldName.Should().Be("beta");
        taggedOk.Value[2].fieldName.Should().Be("gamma");

        // 2) General fast path: PineValueAsElmValue must classify the value as ElmRecord.
        var generalParseResult = ElmValueEncoding.PineValueAsElmValue(legacyEncoded, null, null);

        if (generalParseResult is not Result<string, ElmValue>.Ok generalOk)
        {
            throw new System.Exception(
                "General decoder failed: " + generalParseResult.GetType().FullName);
        }

        var asRecord = (ElmValue.ElmRecord)generalOk.Value;
        asRecord.Fields.Should().HaveCount(3);
        asRecord.Fields[0].FieldName.Should().Be("alfa");
        asRecord.Fields[0].Value.Should().Be(ElmValue.Integer(11));
        asRecord.Fields[1].FieldName.Should().Be("beta");
        asRecord.Fields[1].Value.Should().Be(ElmValue.Integer(13));
        asRecord.Fields[2].FieldName.Should().Be("gamma");
        asRecord.Fields[2].Value.Should().Be(ElmValue.Integer(17));
    }
}
