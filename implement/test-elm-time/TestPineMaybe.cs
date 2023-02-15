using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System.Text.Json;

namespace TestElmTime;

[TestClass]
public class TestPineMaybe
{
    [TestMethod]
    public void Maybe_JSON_coding_roundtrip_string()
    {
        var testCases = new[]
        {
            Maybe<string>.nothing(),
            Maybe<string>.just("some text"),
        };

        foreach (var testCase in testCases)
        {
            var serialized = JsonSerializer.Serialize(testCase);

            var deserialized = JsonSerializer.Deserialize<Maybe<string>>(serialized);

            Assert.AreEqual(testCase, deserialized);
        }
    }

    [TestMethod]
    public void Maybe_JSON_coding_diverse()
    {
        /*
         * We reuse the common generic representation of choice types for `Maybe`:
         * Each tag of a choice type can have a list of parameters, and we encode this list as a JSON array.
         * */

        Assert.AreEqual(
            Maybe<string>.nothing(),
            JsonSerializer.Deserialize<Maybe<string>>($$"""{"Nothing":[]}"""));

        Assert.AreEqual(
            Maybe<int>.just(34),
            JsonSerializer.Deserialize<Maybe<int>>($$"""{"Just":[34]}"""));

        Assert.AreEqual(
            Maybe<int?>.just(67),
            JsonSerializer.Deserialize<Maybe<int?>>($$"""{"Just":[67]}"""));

        Assert.AreEqual(
            Maybe<int?>.just(null),
            JsonSerializer.Deserialize<Maybe<int?>>($$"""{"Just":[null]}"""));
    }

    [TestMethod]
    public void Maybe_Nothing_from_null()
    {
        Assert.AreEqual(
            Maybe<int>.just(123),
            Maybe.NothingFromNull<int>(123));

        Assert.AreEqual(
            Maybe<int>.nothing(),
            Maybe.NothingFromNull<int>(null));

        Assert.AreEqual(
            Maybe<string>.just("hello"),
            Maybe.NothingFromNull("hello"));

        Assert.AreEqual(
            Maybe<string>.nothing(),
            Maybe.NothingFromNull<string>(null));
    }
}
