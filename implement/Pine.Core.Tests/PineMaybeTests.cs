using AwesomeAssertions;
using System.Text.Json;
using Xunit;

namespace Pine.Core.Tests;

public class PineMaybeTests
{
    [Fact]
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

            deserialized.Should().Be(testCase);
        }
    }

    [Fact]
    public void Maybe_JSON_coding_diverse()
    {
        /*
         * We reuse the common generic representation of choice types for `Maybe`:
         * Each tag of a choice type can have a list of parameters, and we encode this list as a JSON array.
         * */

        JsonSerializer.Deserialize<Maybe<string>>($$"""{"Nothing":[]}""")
            .Should().Be(Maybe<string>.nothing());

        JsonSerializer.Deserialize<Maybe<int>>($$"""{"Just":[34]}""")
            .Should().Be(Maybe<int>.just(34));

        JsonSerializer.Deserialize<Maybe<int?>>($$"""{"Just":[67]}""")
            .Should().Be(Maybe<int?>.just(67));

        JsonSerializer.Deserialize<Maybe<int?>>($$"""{"Just":[null]}""")
            .Should().Be(Maybe<int?>.just(null));
    }

    [Fact]
    public void Maybe_Nothing_from_null()
    {
        Maybe.NothingFromNull<int>(123)
            .Should().Be(Maybe<int>.just(123));

        Maybe.NothingFromNull<int>(null)
            .Should().Be(Maybe<int>.nothing());

        Maybe.NothingFromNull("hello")
            .Should().Be(Maybe<string>.just("hello"));

        Maybe.NothingFromNull<string>(null)
            .Should().Be(Maybe<string>.nothing());
    }
}
