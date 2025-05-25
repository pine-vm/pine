using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.PopularEncodings;
using System.Collections.Generic;

namespace TestElmTime;

[TestClass]
public class ElmValueTests
{
    [TestMethod]
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

    [TestMethod]
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
                    "[31,37,39]"),

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
            ];

        foreach (var (elmValue, expectedExpression) in testCases)
        {
            var (expressionString, _) =
                ElmValue.RenderAsElmExpression(elmValue);

            expressionString.Should().Be(expectedExpression);
        }
    }

    [TestMethod]
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

    [TestMethod]
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
}
