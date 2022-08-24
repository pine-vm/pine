using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System.Text.Json;

namespace test_elm_fullstack;

[TestClass]
public class TestPineResult
{
    [TestMethod]
    public void Result_JSON_coding_roundtrip_string_int()
    {
        var testCases = new[]
        {
            Result<string, int?>.err("Error string"),
            Result<string, int?>.ok(4567),
        };

        foreach (var testCase in testCases)
        {
            var serialized = JsonSerializer.Serialize(testCase);

            var deserialized = JsonSerializer.Deserialize<Result<string, int?>>(serialized);

            Assert.AreEqual(testCase, deserialized);
        }
    }

    [TestMethod]
    public void Result_JSON_coding_diverse()
    {
        /*
         * We reuse the common generic representation of DU types for `Result`:
         * Each tag of a DU type can have a list of parameters, and we encode this list as a JSON array.
         * */

        Assert.AreEqual(
            Result<string, int?>.err("some text"),
            JsonSerializer.Deserialize<Result<string, int?>>($$"""{"Err":["some text"]}"""));

        Assert.AreEqual(
            Result<string, int?>.ok(34),
            JsonSerializer.Deserialize<Result<string, int?>>($$"""{"Ok":[34]}"""));

        Assert.AreEqual(
            $$"""{"Err":["the error"]}""",
            JsonSerializer.Serialize(Result<string, int?>.err("the error")));

        Assert.AreEqual(
            $$"""{"Ok":[87]}""",
            JsonSerializer.Serialize(Result<string, int?>.ok(87)));
    }
}
