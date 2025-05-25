using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class ProcessWithLogTests
{
    [TestMethod]
    public void Process_with_log_build_and_unpack()
    {
        var testCases = new[]
        {
                new
                {
                    process = AsProcessWithStringLog(5678),
                    expectedResult = 5678,
                    expectedLog = ImmutableList<string>.Empty,
                },
                new
                {
                    process =
                    AsProcessWithStringLog(123)
                    .WithLogEntryAdded("alpha"),

                    expectedResult = 123,
                    expectedLog = ImmutableList.Create("alpha"),
                },
                new
                {
                    process =
                    AsProcessWithStringLog("3")
                    .MapResult(r => int.Parse(r) + 1)
                    .WithLogEntryFromResultAdded(r => (r + 1).ToString()),

                    expectedResult = 4,
                    expectedLog = ImmutableList.Create("5"),
                }
            };

        foreach (var testCase in testCases)
        {
            var result = testCase.process.LogToList();

            result.result.Should().Be(testCase.expectedResult);
            result.log.ToList().Should().Equal(testCase.expectedLog);
        }
    }

    private static ProcessWithLog<string, T> AsProcessWithStringLog<T>(T result) => new ProcessWithLog<string, T>.Result(result);
}
