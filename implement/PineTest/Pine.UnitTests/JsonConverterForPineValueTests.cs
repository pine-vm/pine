using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Json;
using System.Text.Json;

namespace Pine.UnitTests;

[TestClass]
public class JsonConverterForPineValueTests
{
    [TestMethod]
    public void JSON_serialize_pine_value_roundtrips()
    {
        var jsonSerializerOptions = new JsonSerializerOptions { };

        jsonSerializerOptions.Converters.Add(new JsonConverterForPineValue());

        var testCases = new[]
        {
            PineValueAsInteger.ValueFromSignedInteger(0),
            PineValueAsInteger.ValueFromSignedInteger(1234),
            PineValueAsInteger.ValueFromSignedInteger(-45678),

            PineValue.EmptyList,

            PineValue.List(
                [PineValueAsInteger.ValueFromSignedInteger(56), PineValueAsInteger.ValueFromSignedInteger(57)]),

            PineValue.Blob([]),
            PineValue.Blob([32]),
            PineValue.Blob([10,11,13]),

            PineValue.List(
                [PineValue.List([]), PineValue.List([])]),

            PineValue.List(
                [PineValue.Blob([32])]),

            PineValueAsString.ValueFromString("Hello world!"),

            PineValue.List(
                [PineValueAsString.ValueFromString(" ")]),

            PineValue.List(
                [PineValueAsString.ValueFromString("Hello world!")]),

            PineValue.List(
                [PineValueAsString.ValueFromString("+")]),

            PineValue.List(
                [PineValueAsString.ValueFromString("\"")]),

            PineValue.List(
                [
                PineValueAsString.ValueFromString("String"),
                PineValue.List([PineValueAsString.ValueFromString("Hello world!")]),
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

                Assert.AreEqual(testCase, fromJson);
            }
            catch (System.Exception ex)
            {
                throw new System.Exception(
                    $"Failed to deserialize: {asJson}",
                    ex);
            }
        }
    }

    [TestMethod]
    public void JSON_serialize_pine_value_to_native_integer()
    {
        var jsonSerializerOptions = new JsonSerializerOptions { };

        jsonSerializerOptions.Converters.Add(new JsonConverterForPineValue());

        string serializePineValue(PineValue pineValue) =>
            JsonSerializer.Serialize(
                pineValue,
                options: jsonSerializerOptions);

        Assert.AreEqual(
            "1234",
            serializePineValue(
                PineValueAsInteger.ValueFromSignedInteger(1234)));

        Assert.AreEqual(
            "-34567",
            serializePineValue(
                PineValueAsInteger.ValueFromSignedInteger(-34567)));
    }

    [TestMethod]
    public void JSON_serialize_pine_value_to_native_string()
    {
        var jsonSerializerOptions = new JsonSerializerOptions { };

        jsonSerializerOptions.Converters.Add(new JsonConverterForPineValue());

        string serializePineValue(PineValue pineValue) =>
            JsonSerializer.Serialize(
                pineValue,
                options: jsonSerializerOptions);

        Assert.AreEqual(
            $$"""{"BlobAsString":"stringValue 789"}""",
            serializePineValue(
                PineValueAsString.BlobValueFromString("stringValue 789")));

        Assert.AreEqual(
            "[]",
            serializePineValue(PineValue.EmptyList));
    }

    [TestMethod]
    public void JSON_serialize_pine_value_to_native_string_2024()
    {
        var jsonSerializerOptions = new JsonSerializerOptions { };

        jsonSerializerOptions.Converters.Add(new JsonConverterForPineValue());

        string serializePineValue(PineValue pineValue) =>
            JsonSerializer.Serialize(
                pineValue,
                options: jsonSerializerOptions);

        Assert.AreEqual(
            $$"""{"ListAsString_2024":"stringValue 789"}""",
            serializePineValue(
                PineValueAsString.ValueFromString_2024("stringValue 789")));

        Assert.AreEqual(
            "[]",
            serializePineValue(PineValue.EmptyList));
    }
}
