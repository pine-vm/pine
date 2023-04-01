using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Json;
using System.Collections.Immutable;
using System.Text.Json;

namespace TestElmTime;

[TestClass]
public class TestJsonConverterForPineValue
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
                ImmutableArray.Create(
                    PineValueAsInteger.ValueFromSignedInteger(56),
                    PineValueAsInteger.ValueFromSignedInteger(57))),

            PineValue.Blob(new byte[]{ }),
            PineValue.Blob(new byte[]{10,11,13}),
        };

        foreach (var testCase in testCases)
        {
            var asJson = JsonSerializer.Serialize(
                testCase,
                options: jsonSerializerOptions);

            var fromJson = JsonSerializer.Deserialize<PineValue>(
                asJson,
                options: jsonSerializerOptions);

            Assert.AreEqual(testCase, fromJson);
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
            $$"""{"ListAsString":"stringValue 789"}""",
            serializePineValue(
                PineValueAsString.ValueFromString("stringValue 789")));

        Assert.AreEqual(
            "[]",
            serializePineValue(PineValue.EmptyList));
    }
}
