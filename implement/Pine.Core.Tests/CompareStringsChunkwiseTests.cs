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
            Failed in chunk 0 of 1
            Chunks differ at char index 0:
            ↓ (actual)
            actual text h
            <vs>
            expected text
            ↑ (expected)
            Text following previous checked chunk is:
            actual text here
            """);
    }

    [Fact]
    public void Reports_difference_in_middle_of_chunk()
    {
        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["the quick brown fox"],
                actual: "the quick green fox");

        result.Should().Be(
            """
            Failed in chunk 0 of 1
            Chunks differ at char index 10:
            ↓ (actual)
            the quick green fox
            <vs>
            the quick brown fox
            ↑ (expected)
            Text following previous checked chunk is:
            the quick green fox
            """);
    }

    [Fact]
    public void Reports_difference_when_actual_is_shorter()
    {
        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["hello world"],
                actual: "hello");

        result.Should().Be(
            """
            Failed in chunk 0 of 1
            Chunks differ at char index 5:
            ↓ (actual)
            hello      
            <vs>
            hello world
            ↑ (expected)
            Text following previous checked chunk is:
            hello
            """);
    }

    [Fact]
    public void Reports_difference_in_second_chunk()
    {
        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["first-", "second"],
                actual: "first-SECOND");

        result.Should().Be(
            """
            Failed in chunk 1 of 2
            Chunks differ at char index 0:
            ↓ (actual)
            SECOND
            <vs>
            second
            ↑ (expected)
            Text following previous checked chunk is:
            SECOND
            """);
    }

    [Fact]
    public void Reports_difference_when_actual_has_extra_text_after_chunks()
    {
        var result =
            Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedChunks: ["hello"],
                actual: "hello extra");

        result.Should().Be("Strings differ");
    }
}
