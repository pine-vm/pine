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
                var sliceLengthMax = 160;

                var sliceStartIndex =
                    Math.Max(0, firstDifferentCharIndex.Value - sliceLengthMax / 2);

                var sliceEndIndex =
                    Math.Min(expectedChunk.Length, firstDifferentCharIndex.Value + sliceLengthMax / 2);

                var sliceLength = sliceEndIndex - sliceStartIndex;

                var expectedSlice =
                    expectedChunk
                    .Substring(startIndex: sliceStartIndex, length: sliceLength);

                var responseSlice =
                    responseChunk
                    .PadRight(totalWidth: expectedChunk.Length, ' ')
                    .Substring(startIndex: sliceStartIndex, length: sliceLength);

                var remainingText =
                    actual[previousChunkEnd..];

                return
                    string.Join(
                        "\n",
                        [
                            "Failed in chunk " + chunkIndex + " of " + expectedChunks.Count,
                                "Chunks differ at char index " + firstDifferentCharIndex + ":",
                                expectedSlice,
                                "<vs>",
                                responseSlice,
                                "Text following previous checked chunk is:",
                                remainingText,
                        ]);
            }

            previousChunkEnd += expectedChunk.Length;
        }

        if (actual != string.Concat(expectedChunks))
        {
            return "Strings differ";
        }

        return null;
    }
}
