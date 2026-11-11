using AwesomeAssertions;
using Pine.Core.Functional;
using System;
using System.Text.Json;
using Xunit;

namespace Pine.Core.Tests.Functional;

public class ResultTests
{
    [Fact]
    public void ValueResult_distinguishes_null_payloads_and_rejects_wrong_variant_access()
    {
        var success = ValueResult<string?, string?>.ok(null);
        var failure = ValueResult<string?, string?>.err(null);

        success.IsOk.Should().BeTrue();
        success.Value.Should().BeNull();
        Action readSuccessError = () => _ = success.Error;
        readSuccessError.Should().Throw<InvalidOperationException>();

        failure.IsErr.Should().BeTrue();
        failure.Error.Should().BeNull();
        Action readFailureValue = () => _ = failure.Value;
        readFailureValue.Should().Throw<InvalidOperationException>();

        ValueResult<string, int> zero = 0;
        ValueResult<string, int> error = "rejected";
        zero.IsOk.Should().BeTrue();
        zero.Value.Should().Be(0);
        error.IsErr.Should().BeTrue();
        error.Error.Should().Be("rejected");
    }

    [Fact]
    public void ValueResult_rejects_uninitialized_results()
    {
        var result = default(ValueResult<string, int>);

        Action[] reads =
            [
                () => _ = result.IsOk,
                () => _ = result.IsErr,
                () => _ = result.Value,
                () => _ = result.Error
            ];

        foreach (var read in reads)
            read.Should().Throw<InvalidOperationException>();
    }

    [Fact]
    public void ValueResult_success_and_error_do_not_allocate_wrappers()
    {
        static int ConsumeResults()
        {
            var sum = 0;

            for (var index = 0; index < 10_000; index++)
            {
                var result =
                    index % 2 is 0
                    ?
                    ValueResult<string, int>.ok(index)
                    :
                    ValueResult<string, int>.err("rejected");

                sum += result.IsOk ? result.Value : result.Error.Length;
            }

            return sum;
        }

        ConsumeResults();
        var before = GC.GetAllocatedBytesForCurrentThread();
        var sum = ConsumeResults();
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;

        sum.Should().Be(25_035_000);
        allocated.Should().Be(0);
    }

    [Fact]
    public void Result_JSON_coding_roundtrip_string_int()
    {
        var testCases =
            new[]
            {
                Result<string, int>.err("Error string"),
                Result<string, int>.ok(4567),
            };

        foreach (var testCase in testCases)
        {
            var serialized = JsonSerializer.Serialize(testCase);

            var deserialized = JsonSerializer.Deserialize<Result<string, int>>(serialized);

            deserialized.Should().Be(testCase);
        }
    }

    [Fact]
    public void Result_JSON_coding_diverse()
    {
        /*
         * We reuse the common generic representation of choice types for `Result`:
         * Each tag of a choice type can have a list of parameters, and we encode this list as a JSON array.
         * */

        JsonSerializer.Deserialize<Result<string, int>>($$"""{"Err":["some text"]}""")
            .Should().Be(Result<string, int>.err("some text"));

        JsonSerializer.Deserialize<Result<string, int>>($$"""{"Ok":[34]}""")
            .Should().Be(Result<string, int>.ok(34));

        JsonSerializer.Deserialize<Result<string, int?>>($$"""{"Ok":[67]}""")
            .Should().Be(Result<string, int?>.ok(67));

        JsonSerializer.Deserialize<Result<string?, int>>($$"""{"Err":[null]}""")
            .Should().Be(Result<string?, int>.err(null));

        JsonSerializer.Deserialize<Result<string, int?>>($$"""{"Ok":[null]}""")
            .Should().Be(Result<string, int?>.ok(null));

        JsonSerializer.Serialize(Result<string, int>.err("error message"))
            .Should().Be($$"""{"Err":["error message"]}""");

        JsonSerializer.Serialize(Result<string, int>.ok(87))
            .Should().Be($$"""{"Ok":[87]}""");

        JsonSerializer.Serialize(Result<string?, int>.err(null))
            .Should().Be($$"""{"Err":[null]}""");

        JsonSerializer.Serialize(Result<string, int?>.ok(null))
            .Should().Be($$"""{"Ok":[null]}""");
    }
}
