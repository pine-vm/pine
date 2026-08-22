using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System.Collections.Generic;
using Xunit;

namespace Pine.IntegrationTests;

public class ElmValueInteropTests
{
    [Fact]
    public void Pine_value_encoded_as_in_elm_compiler_roundtrips()
    {
        var testCases =
            (IReadOnlyList<PineValue>)[
            PineValue.EmptyList,
            PineValue.EmptyBlob,

            StringEncoding.ValueFromString("Hello, world!"),
            ];

        foreach (var testCase in testCases)
        {
            var encodedInCompiler =
                ElmValueInterop.PineValueEncodedAsInElmCompiler(testCase);

            var roundtrip =
                ElmValueInterop.ElmValueDecodedAsInElmCompiler(encodedInCompiler, null, null)
                .Extract(err => throw new System.Exception(err));

            roundtrip.Should().Be(testCase);
        }
    }

    [Fact]
    public void Parse_Elm_Maybe_accepts_flat_and_2025_encodings()
    {
        var payload = IntegerEncoding.EncodeSignedInteger(17);

        foreach (var encodeTag in ChoiceTagEncoders())
        {
            ElmValueInterop.ParseElmMaybeValue(
                encodeTag("Nothing", []),
                nothing: () => true,
                just: _ => false,
                invalid: error => throw new System.Exception(error))
            .Should().BeTrue();

            ElmValueInterop.ParseElmMaybeValue(
                encodeTag("Just", [payload]),
                nothing: () => null,
                just: value => value,
                invalid: error => throw new System.Exception(error))
            .Should().Be(payload);
        }
    }

    [Fact]
    public void Parse_Elm_Result_accepts_flat_and_2025_encodings()
    {
        var payload = IntegerEncoding.EncodeSignedInteger(17);

        foreach (var encodeTag in ChoiceTagEncoders())
        {
            ElmValueInterop.ParseElmResultValue(
                encodeTag("Err", [payload]),
                err: value => ("Err", value),
                ok: value => ("Ok", value),
                invalid: error => throw new System.Exception(error))
            .Should().Be(("Err", payload));

            ElmValueInterop.ParseElmResultValue(
                encodeTag("Ok", [payload]),
                err: value => ("Err", value),
                ok: value => ("Ok", value),
                invalid: error => throw new System.Exception(error))
            .Should().Be(("Ok", payload));
        }
    }

    private static IEnumerable<System.Func<string, IReadOnlyList<PineValue>, PineValue>> ChoiceTagEncoders()
    {
        yield return ElmValueEncoding.TagAsPineValue;
        yield return ElmValueEncoding.TagAsPineValue_2025;
    }
}
