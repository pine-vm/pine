using System;
using System.Collections.Generic;

namespace Pine.Core;

public static class Testing
{
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

                var (markerSpaces, actualQuoted, expectedQuoted) =
                    FormatSlices(
                        actualText: remainingText,
                        expectedText: expectedChunk,
                        diffIndex: firstDifferentCharIndex.Value);

                var coreDifference =
                    string.Join(
                        "\n",
                        [
                        markerSpaces + "↓ (actual)",
                        actualQuoted,
                        "<vs>",
                        expectedQuoted,
                        markerSpaces + "↑ (expected)"
                        ]);

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

        var markerSpaces = new string(' ', diffIndex - sliceStartIndex + 1);

        return (markerSpaces, "\"" + Slice(actualText) + "\"", "\"" + Slice(expectedText) + "\"");
    }
}
