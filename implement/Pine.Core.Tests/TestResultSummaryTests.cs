using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests;

public class TestResultSummaryTests
{
    [Fact]
    public void RenderSummary_all_passed()
    {
        var results =
            new List<TestResultSummary.TestCaseResult>
            {
                new("case-alpha", Passed: true),
                new("case-beta", Passed: true),
                new("case-gamma", Passed: true),
            };

        var summary = TestResultSummary.RenderSummary(results);

        summary.Should().Be(
            """
            Test run summary: Passed!
              total: 3
              failed: 0
              succeeded: 3
            """);
    }

    [Fact]
    public void RenderSummary_some_failed_with_messages()
    {
        var results =
            new List<TestResultSummary.TestCaseResult>
            {
                new("case-alpha", Passed: true),
                new("case-beta", Passed: false, FailureMessage: "Expected 42 but got 0"),
                new("case-gamma", Passed: true),
                new("case-delta", Passed: false, FailureMessage: "Strings differ at line 3"),
            };

        var summary = TestResultSummary.RenderSummary(results);

        summary.Should().Be(
            """
            Test run summary: Failed!
              total: 4
              failed: 2
              succeeded: 2

            Failed: case-beta
            Expected 42 but got 0

            Failed: case-delta
            Strings differ at line 3
            """);
    }

    [Fact]
    public void RenderSummary_single_failure_without_message()
    {
        var results =
            new List<TestResultSummary.TestCaseResult>
            {
                new("lonely-case", Passed: false),
            };

        var summary = TestResultSummary.RenderSummary(results);

        summary.Should().Be(
            """
            Test run summary: Failed!
              total: 1
              failed: 1
              succeeded: 0

            Failed: lonely-case
            """);
    }

    [Fact]
    public void RenderSummary_empty_results()
    {
        var results = new List<TestResultSummary.TestCaseResult>();

        var summary = TestResultSummary.RenderSummary(results);

        summary.Should().Be(
            """
            Test run summary: Passed!
              total: 0
              failed: 0
              succeeded: 0
            """);
    }

    [Fact]
    public void DescribeStringDifference_returns_null_for_equal_strings()
    {
        var result = TestResultSummary.DescribeStringDifference("hello", "hello");

        result.Should().BeNull();
    }

    [Fact]
    public void DescribeStringDifference_shows_context_around_first_differing_line()
    {
        var expected =
            """
            line 1
            line 2
            line 3 expected
            line 4
            line 5
            """;

        var actual =
            """
            line 1
            line 2
            line 3 actual
            line 4
            line 5
            """;

        var result = TestResultSummary.DescribeStringDifference(expected, actual);

        result.Should().Be(
            """
            Strings differ at line 3:
                       line 1: line 1
                       line 2: line 2
              expected line 3: line 3 expected
              actual   line 3: line 3 actual
                       line 4: line 4
                       line 5: line 5
            """);
    }

    [Fact]
    public void DescribeStringDifference_actual_shorter_shows_missing_line()
    {
        var expected = "line 1\nline 2\nline 3";
        var actual = "line 1\nline 2";

        var result = TestResultSummary.DescribeStringDifference(expected, actual);

        result.Should().Be(
            """
            Strings differ at line 3:
                       line 1: line 1
                       line 2: line 2
              expected line 3: line 3
              actual   line 3: (missing)
            """);
    }

    [Fact]
    public void DescribeStringDifference_actual_longer_shows_extra_line()
    {
        var expected = "line 1\nline 2";
        var actual = "line 1\nline 2\nline 3 extra";

        var result = TestResultSummary.DescribeStringDifference(expected, actual);

        result.Should().Be(
            """
            Strings differ at line 3:
                       line 1: line 1
                       line 2: line 2
              expected line 3: (missing)
              actual   line 3: line 3 extra
            """);
    }

    [Fact]
    public void DescribeStringDifference_first_line_different()
    {
        var expected = "expected\nsame";
        var actual = "actual\nsame";

        var result = TestResultSummary.DescribeStringDifference(expected, actual);

        result.Should().Be(
            """
            Strings differ at line 1:
              expected line 1: expected
              actual   line 1: actual
                       line 2: same
            """);
    }
}
