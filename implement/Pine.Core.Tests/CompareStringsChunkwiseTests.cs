using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests;

public class CompareStringsChunkwiseTests
{
    [Fact]
    public void Returns_null_when_strings_are_equal()
    {
        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["hello world"],
                actual: "hello world");

        result.Should().BeNull();
    }

    [Fact]
    public void Returns_null_when_multiple_chunks_match()
    {
        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["hello ", "world"],
                actual: "hello world");

        result.Should().BeNull();
    }

    [Fact]
    public void Reports_difference_at_start_of_single_chunk()
    {
        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["expected text"],
                actual: "actual text here");

        result.Should().Be(
            """
            Strings differ at char index 0:
             ↓ (actual)
            "actual text here"
            <vs>
            "expected text"
             ↑ (expected)
            """);
    }

    [Fact]
    public void Reports_difference_in_middle_of_chunk()
    {
        /*
         * Reference to emulate:
         * "the quick green fox".Should().Be("the quick brown fox");
         */

        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["the quick brown fox"],
                actual: "the quick green fox");

        result.Should().Be(
            """
            Strings differ at char index 10:
                       ↓ (actual)
            "the quick green fox"
            <vs>
            "the quick brown fox"
                       ↑ (expected)
            """);
    }

    [Fact]
    public void Reports_difference_when_actual_is_shorter()
    {
        /*
         * Reference to emulate:
         * "hello".Should().Be("hello world");
         */

        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["hello world"],
                actual: "hello");

        result.Should().Be(
            """
            Strings differ at char index 5:
                  ↓ (actual)
            "hello"
            <vs>
            "hello world"
                  ↑ (expected)
            """);
    }

    [Fact]
    public void Reports_difference_in_second_chunk()
    {
        /*
         * Reference to emulate:
         * "SECOND".Should().Be("second");
         */

        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["first-", "second"],
                actual: "first-SECOND");

        result.Should().Be(
            """
            Failed in chunk 1 of 2
            Chunks differ at char index 0:
             ↓ (actual)
            "SECOND"
            <vs>
            "second"
             ↑ (expected)
            Text following previous checked chunk is:
            SECOND
            """);
    }

    [Fact]
    public void Reports_difference_when_actual_has_extra_text_after_chunks()
    {
        /*
         * Reference to emulate:
         * "hello extra".Should().Be("hello");
         */

        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["hello"],
                actual: "hello extra");

        result.Should().Be(
            """
            Strings differ at char index 5:
                  ↓ (actual)
            "hello extra"
            "hello"
                  ↑ (expected).
            """);
    }
}
