using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System.Text.Json;

namespace TestElmTime;

[TestClass]
public class TestPineResult
{
    [TestMethod]
    public void Result_JSON_coding_roundtrip_string_int()
    {
        var testCases = new[]
        {
            Result<string, int>.err("Error string"),
            Result<string, int>.ok(4567),
        };

        foreach (var testCase in testCases)
        {
            var serialized = JsonSerializer.Serialize(testCase);

            var deserialized = JsonSerializer.Deserialize<Result<string, int>>(serialized);

            Assert.AreEqual(testCase, deserialized);
        }
    }

    [TestMethod]
    public void Result_JSON_coding_diverse()
    {
        /*
         * We reuse the common generic representation of choice types for `Result`:
         * Each tag of a choice type can have a list of parameters, and we encode this list as a JSON array.
         * */

        Assert.AreEqual(
            Result<string, int>.err("some text"),
            JsonSerializer.Deserialize<Result<string, int>>($$"""{"Err":["some text"]}"""));

        Assert.AreEqual(
            Result<string, int>.ok(34),
            JsonSerializer.Deserialize<Result<string, int>>($$"""{"Ok":[34]}"""));

        Assert.AreEqual(
            Result<string, int?>.ok(67),
            JsonSerializer.Deserialize<Result<string, int?>>($$"""{"Ok":[67]}"""));

        Assert.AreEqual(
            Result<string?, int>.err(null),
            JsonSerializer.Deserialize<Result<string?, int>>($$"""{"Err":[null]}"""));

        Assert.AreEqual(
            Result<string, int?>.ok(null),
            JsonSerializer.Deserialize<Result<string, int?>>($$"""{"Ok":[null]}"""));

        Assert.AreEqual(
            $$"""{"Err":["error message"]}""",
            JsonSerializer.Serialize(Result<string, int>.err("error message")));

        Assert.AreEqual(
            $$"""{"Ok":[87]}""",
            JsonSerializer.Serialize(Result<string, int>.ok(87)));

        Assert.AreEqual(
            $$"""{"Err":[null]}""",
            JsonSerializer.Serialize(Result<string?, int>.err(null)));

        Assert.AreEqual(
            $$"""{"Ok":[null]}""",
            JsonSerializer.Serialize(Result<string, int?>.ok(null)));
    }
}
