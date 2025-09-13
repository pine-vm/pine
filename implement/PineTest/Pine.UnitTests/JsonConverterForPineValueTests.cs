using AwesomeAssertions;
using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.Json;
using System.Text.Json;
using Xunit;

namespace Pine.UnitTests;

public class JsonConverterForPineValueTests
{
    [Fact]
    public void JSON_serialize_pine_value_roundtrips()
    {
        var jsonSerializerOptions = new JsonSerializerOptions { };

        jsonSerializerOptions.Converters.Add(new JsonConverterForPineValue());

        var testCases = new[]
        {
            IntegerEncoding.EncodeSignedInteger(0),
            IntegerEncoding.EncodeSignedInteger(1234),
            IntegerEncoding.EncodeSignedInteger(-45678),

            PineValue.EmptyList,

            PineValue.List(
                [IntegerEncoding.EncodeSignedInteger(56), IntegerEncoding.EncodeSignedInteger(57)]),

            PineValue.Blob([]),
            PineValue.Blob([32]),
            PineValue.Blob([10,11,13]),

            PineValue.Blob([0]),

            PineValue.Blob([0, 0]),

            PineValue.Blob([0, 0, 0]),

            PineValue.List(
                [PineValue.List([]), PineValue.List([])]),

            PineValue.List(
                [PineValue.Blob([32])]),

            PineValue.List(
                [PineValue.Blob([0])]),

            PineValue.List(
                [PineValue.Blob([0, 0])]),

            PineValue.List(
                [PineValue.Blob([0, 41])]),

            StringEncoding.ValueFromString("Hello world!"),

            PineValue.List(
                [StringEncoding.ValueFromString(" ")]),

            PineValue.List(
                [StringEncoding.ValueFromString("Hello world!")]),

            PineValue.List(
                [StringEncoding.ValueFromString("+")]),

            PineValue.List(
                [StringEncoding.ValueFromString("\"")]),

            PineValue.List(
                [
                StringEncoding.ValueFromString("String"),
                PineValue.List([StringEncoding.ValueFromString("Hello world!")]),
                ]),

        };

        foreach (var testCase in testCases)
        {
            var asJson =
                JsonSerializer.Serialize(
                    testCase,
                    options: jsonSerializerOptions);

            try
            {
                var fromJson =
                    JsonSerializer.Deserialize<PineValue>(
                        asJson,
                        options: jsonSerializerOptions);

                fromJson.Should().Be(testCase);
            }
            catch (System.Exception ex)
            {
                throw new System.Exception(
                    $"Failed to deserialize: {asJson}",
                    ex);
            }
        }
    }

    [Fact]
    public void JSON_serialize_pine_value_to_native_integer()
    {
        var jsonSerializerOptions = new JsonSerializerOptions { };

        jsonSerializerOptions.Converters.Add(new JsonConverterForPineValue());

        string serializePineValue(PineValue pineValue) =>
            JsonSerializer.Serialize(
                pineValue,
                options: jsonSerializerOptions);

        serializePineValue(
            IntegerEncoding.EncodeSignedInteger(1234))
            .Should().Be("1234");

        serializePineValue(
            IntegerEncoding.EncodeSignedInteger(-34567))
            .Should().Be("-34567");
    }

    [Fact]
    public void JSON_serialize_pine_value_to_native_string()
    {
        var jsonSerializerOptions = new JsonSerializerOptions { };

        jsonSerializerOptions.Converters.Add(new JsonConverterForPineValue());

        string serializePineValue(PineValue pineValue) =>
            JsonSerializer.Serialize(
                pineValue,
                options: jsonSerializerOptions);

        serializePineValue(
            StringEncoding.BlobValueFromString("stringValue 789"))
            .Should().Be($$"""{"BlobAsString":"stringValue 789"}""");

        serializePineValue(PineValue.EmptyList)
            .Should().Be("[]");
    }
}
