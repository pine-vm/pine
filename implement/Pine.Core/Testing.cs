using System;
using System.Collections.Generic;

namespace Pine.Core;

/// <summary>
/// Helper functions to compare strings and render human-readable descriptions of the
/// first difference found, for use in test assertions and snapshot comparisons.
/// </summary>
public static class Testing
{
    /// <summary>
    /// Compares <paramref name="actual"/> against the concatenation of <paramref name="expectedChunks"/>,
    /// checking one chunk at a time so that the reported difference is localized to the first
    /// chunk that does not match.
    /// </summary>
    /// <param name="expectedChunks">
    /// The expected text, split into consecutive chunks. Their concatenation forms the complete
    /// expected string.
    /// </param>
    /// <param name="actual">The actual string to compare against the expected chunks.</param>
    /// <returns>
    /// A human-readable description of the first difference found, or <c>null</c> when
    /// <paramref name="actual"/> equals the concatenation of <paramref name="expectedChunks"/>.
    /// </returns>
    public static string? CompareStringsChunkwiseAndReportFirstDifference(
        IReadOnlyList<string> expectedChunks,
        string actual)
    {
        var previousChunkEnd = 0;

        for (var chunkIndex = 0; chunkIndex < expectedChunks.Count; chunkIndex++)
        {
            var expectedChunk = expectedChunks[chunkIndex];

            var responseChunk =
                actual.Substring(
                    previousChunkEnd,
                    length: Math.Min(expectedChunk.Length, actual.Length - previousChunkEnd));

            int? firstDifferentCharIndex = null;

            for (var i = 0; i < expectedChunk.Length; i++)
            {
                if (responseChunk.Length <= i)
                {
                    firstDifferentCharIndex ??= i;
                    break;
                }

                var expectedChar = expectedChunk[i];
                var responseChar = responseChunk[i];

                if (responseChar != expectedChar)
                {
                    firstDifferentCharIndex ??= i;
                    break;
                }
            }

            if (firstDifferentCharIndex.HasValue)
            {
                var remainingText =
                    actual[previousChunkEnd..];

                var coreDifference =
                    RenderCoreStringDifference(
                        actualText: remainingText,
                        expectedText: expectedChunk,
                        diffIndex: firstDifferentCharIndex.Value);

                if (expectedChunks.Count > 1)
                {
                    return
                        string.Join(
                            "\n",
                            [
                                "Failed in chunk " + chunkIndex + " of " + expectedChunks.Count,
                                "Chunks differ at char index " + firstDifferentCharIndex + ":",
                                coreDifference,
                                "Text following previous checked chunk is:",
                                remainingText,
                            ]);
                }

                return
                    string.Join(
                        "\n",
                        [
                            "Strings differ at char index " + firstDifferentCharIndex + ":",
                            coreDifference
                        ]);
            }

            previousChunkEnd += expectedChunk.Length;
        }

        var concatenatedExpected = string.Concat(expectedChunks);

        if (actual != concatenatedExpected)
        {
            var diffIndex = concatenatedExpected.Length;

            var (markerSpaces, actualQuoted, expectedQuoted) =
                FormatSlices(
                    actualText: actual,
                    expectedText: concatenatedExpected,
                    diffIndex: diffIndex);

            return
                string.Join(
                    "\n",
                    [
                        "Strings differ at char index " + diffIndex + ":",
                        markerSpaces + "↓ (actual)",
                        actualQuoted,
                        expectedQuoted,
                        markerSpaces + "↑ (expected).",
                    ]);
        }

        return null;
    }

    /// <summary>
    /// Extracts aligned slices of <paramref name="actualText"/> and <paramref name="expectedText"/>
    /// centered around <paramref name="diffIndex"/>, together with the run of leading spaces needed to
    /// align a marker under the differing character.
    /// </summary>
    /// <param name="actualText">The actual text to slice.</param>
    /// <param name="expectedText">The expected text to slice.</param>
    /// <param name="diffIndex">The character index at which the two texts first differ.</param>
    /// <returns>
    /// A tuple with the marker indentation, the quoted slice of <paramref name="actualText"/>, and the
    /// quoted slice of <paramref name="expectedText"/>.
    /// </returns>
    private static (string markerSpaces, string actualQuoted, string expectedQuoted) FormatSlices(
        string actualText,
        string expectedText,
        int diffIndex)
    {
        var sliceLengthMax = 160;

        var sliceStartIndex =
            Math.Max(0, diffIndex - sliceLengthMax / 2);

        string Slice(string text)
        {
            var startIndex = Math.Min(sliceStartIndex, text.Length);

            var endIndex =
                Math.Max(startIndex, Math.Min(text.Length, diffIndex + sliceLengthMax / 2));

            return text[startIndex..endIndex];
        }

        var quote = "\"";

        // When slices are wrapped in quotes, the marker must account for the leading quote character.
        var markerSpaces = new string(' ', diffIndex - sliceStartIndex + quote.Length);

        return (markerSpaces, quote + Slice(actualText) + quote, quote + Slice(expectedText) + quote);
    }

    /// <summary>
    /// Returns the index of the first character at which <paramref name="actual"/> and
    /// <paramref name="expected"/> differ. If one string is a prefix of the other, returns the
    /// length of the shorter string. Returns the common length if the strings are equal.
    /// </summary>
    public static int GetFirstStringDifferenceIndex(string actual, string expected)
    {
        var minLength = Math.Min(actual.Length, expected.Length);

        var index = 0;

        while (index < minLength && actual[index] == expected[index])
        {
            index++;
        }

        return index;
    }

    /// <summary>
    /// Renders the core visual difference between two strings at <paramref name="diffIndex"/>, in the
    /// form of a block with a marker pointing at the first differing character:
    /// <code>
    ///    ↓ (actual)
    /// {slice of actual}
    /// &lt;vs&gt;
    /// {slice of expected}
    ///    ↑ (expected)
    /// </code>
    /// When <paramref name="quoteSlices"/> is true, the slices are wrapped in double quotes.
    /// </summary>
    public static string RenderCoreStringDifference(
        string actualText,
        string expectedText,
        int diffIndex)
    {
        var (markerSpaces, actualSlice, expectedSlice) =
            FormatSlices(
                actualText: actualText,
                expectedText: expectedText,
                diffIndex: diffIndex);

        return
            string.Join(
                "\n",
                [
                    markerSpaces + "↓ (actual)",
                    actualSlice,
                    "<vs>",
                    expectedSlice,
                    markerSpaces + "↑ (expected)"
                ]);
    }
}
