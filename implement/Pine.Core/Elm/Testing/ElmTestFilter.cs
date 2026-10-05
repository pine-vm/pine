using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.Testing;

/// <summary>
/// Matches a case-insensitive expression against a project's file and description path.
/// Plain terms retain substring matching; path segments and wildcard terms match whole segments.
/// </summary>
public sealed class ElmTestFilter
{
    private readonly string[] _segments;

    /// <summary>
    /// Parses a filter. Both slash styles separate segments, '*' matches characters within
    /// a segment, and a standalone '**' matches zero or more segments.
    /// </summary>
    public ElmTestFilter(string expression)
    {
        ArgumentNullException.ThrowIfNull(expression);

        _segments =
            expression.Split(['/', '\\'], StringSplitOptions.RemoveEmptyEntries);

        if (!expression.Contains('/') && !expression.Contains('\\') && !expression.Contains('*'))
            _segments = ["*" + expression + "*"];
    }

    /// <summary>
    /// Checks whether a consecutive portion of the test's path matches the expression.
    /// The final extension of the filename may be omitted.
    /// </summary>
    public bool Matches(ListedTest test)
    {
        var (path, filenameIndex) = GetPath(test);
        var previous = Enumerable.Repeat(true, path.Length + 1).ToArray();

        foreach (var segment in _segments)
        {
            var current = new bool[path.Length + 1];
            current[0] = segment is "**" && previous[0];

            for (var index = 1; index <= path.Length; index++)
            {
                current[index] =
                    segment is "**"
                    ?
                    previous[index] || current[index - 1]
                    :
                    previous[index - 1] &&
                    SegmentMatches(segment, path[index - 1], index - 1 == filenameIndex);
            }

            previous = current;
        }

        return previous.Any(matches => matches);
    }

    /// <summary>
    /// Ranks existing tests by their distance from the filter.
    /// Ties are ordered by the full path, independently of discovery order.
    /// </summary>
    public static IReadOnlyList<ListedTest> FindClosestTests(
        IEnumerable<ListedTest> tests,
        ElmTestFilter filter,
        int count = 5)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(count);

        return
            [
            .. tests
            .Distinct()
            .Select(test => (test, distance: filter.Distance(test)))
            .OrderBy(item => item.distance)
            .ThenBy(item => item.test.FullPath, StringComparer.Ordinal)
            .Take(count)
            .Select(item => item.test)
            ];
    }

    private double Distance(ListedTest test)
    {
        var (path, filenameIndex) = GetPath(test);

        // Starting and ending anywhere is free; skipping an internal segment requires '**'.
        var previous = new double[path.Length + 1];

        foreach (var segment in _segments)
        {
            var current = new double[path.Length + 1];
            current[0] = previous[0] + (segment is "**" ? 0 : 1);

            for (var index = 1; index <= path.Length; index++)
            {
                current[index] =
                    segment is "**"
                    ?
                    Math.Min(previous[index], current[index - 1])
                    :
                    Math.Min(
                        previous[index - 1] +
                        SegmentDistance(segment, path[index - 1], index - 1 == filenameIndex),
                        Math.Min(previous[index] + 1, current[index - 1] + 1));
            }

            previous = current;
        }

        return previous.Min();
    }

    private static (string[] path, int filenameIndex) GetPath(ListedTest test)
    {
        var fileSegments =
            test.FilePath.Split(['/', '\\'], StringSplitOptions.RemoveEmptyEntries);

        return
            ([.. fileSegments, .. test.DescriptionPath, test.Name], fileSegments.Length - 1);
    }

    private static bool SegmentMatches(string pattern, string value, bool isFilename) =>
        GlobMatches(pattern, value) ||
        (isFilename &&
        value.LastIndexOf('.') is var dotIndex &&
        dotIndex > 0 &&
        GlobMatches(pattern, value[..dotIndex]));

    private static double SegmentDistance(string pattern, string value, bool isFilename)
    {
        if (SegmentMatches(pattern, value, isFilename))
            return 0;

        var distance = GlobDistance(pattern, value);

        if (isFilename && value.LastIndexOf('.') is var dotIndex && dotIndex > 0)
            distance = Math.Min(distance, GlobDistance(pattern, value[..dotIndex]));

        return
            Math.Min(
                1,
                (double)distance / Math.Max(1, pattern.Count(character => character is not '*')));
    }

    private static bool GlobMatches(string pattern, string value)
    {
        var patternIndex = 0;
        var valueIndex = 0;
        var starIndex = -1;
        var starValueIndex = 0;

        while (valueIndex < value.Length)
        {
            if (patternIndex < pattern.Length && pattern[patternIndex] is '*')
            {
                starIndex = patternIndex++;
                starValueIndex = valueIndex;
            }
            else if (patternIndex < pattern.Length &&
                CharactersEqual(pattern[patternIndex], value[valueIndex]))
            {
                patternIndex++;
                valueIndex++;
            }
            else if (starIndex >= 0)
            {
                patternIndex = starIndex + 1;
                valueIndex = ++starValueIndex;
            }
            else
            {
                return false;
            }
        }

        while (patternIndex < pattern.Length && pattern[patternIndex] is '*')
            patternIndex++;

        return patternIndex == pattern.Length;
    }

    private static int GlobDistance(string pattern, string value)
    {
        var previous = Enumerable.Range(0, value.Length + 1).ToArray();

        foreach (var character in pattern)
        {
            var current = new int[value.Length + 1];
            current[0] = previous[0] + (character is '*' ? 0 : 1);

            for (var index = 1; index <= value.Length; index++)
            {
                current[index] =
                    character is '*'
                    ?
                    Math.Min(previous[index], current[index - 1])
                    :
                    Math.Min(
                        previous[index - 1] + (CharactersEqual(character, value[index - 1]) ? 0 : 1),
                        Math.Min(previous[index] + 1, current[index - 1] + 1));
            }

            previous = current;
        }

        return previous[^1];
    }

    private static bool CharactersEqual(char first, char second) =>
        char.ToUpperInvariant(first) == char.ToUpperInvariant(second);
}
