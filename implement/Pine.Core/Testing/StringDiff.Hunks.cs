using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.Core.Testing;

public static partial class StringDiff
{
    /// <summary>
    /// Derives bounded hunks without rendering a message. Lines are paired by their ordinal
    /// index, not realigned by content. Insertions/deletions therefore compare subsequent
    /// lines at the same index. LF, CRLF, CR, and final-newline differences remain significant.
    /// </summary>
    public static IReadOnlyList<StringDiffHunk> FindHunks(
        string actual,
        string expected,
        StringDiffOptions? options = null)
    {
        ArgumentNullException.ThrowIfNull(actual);
        ArgumentNullException.ThrowIfNull(expected);

        options ??= new StringDiffOptions();

        ArgumentOutOfRangeException.ThrowIfLessThan(options.MaxLineLength, 1);
        ArgumentOutOfRangeException.ThrowIfLessThan(options.MaxLines, 1);
        ArgumentOutOfRangeException.ThrowIfNegative(options.MaxEqualLines);
        ArgumentOutOfRangeException.ThrowIfNegative(options.MaxHunks);

        if (actual == expected || options.MaxHunks is 0)
            return [];

        var actualLines = SplitLines(actual);
        var expectedLines = SplitLines(expected);
        var lineCount = Math.Max(actualLines.Count, expectedLines.Count);
        var hunks = new List<StringDiffHunk>();

        StringDiffLine Pair(int index) =>
            new(
                index,
                index < actualLines.Count ? actualLines[index] : null,
                index < expectedLines.Count ? expectedLines[index] : null);

        for (var index = 0; index < lineCount && hunks.Count < options.MaxHunks;)
        {
            var first = Pair(index++);

            if (first.Actual == first.Expected)
                continue;

            var lines = new List<StringDiffLine> { first };
            var equalLines = 0;
            var lastDifferentLine = 0;

            if (!IsOversized(first, options.MaxLineLength))
            {
                while (index < lineCount && lines.Count < options.MaxLines)
                {
                    var line = Pair(index);
                    var equal = line.Actual == line.Expected;

                    if (IsOversized(line, options.MaxLineLength) ||
                        (equal && equalLines >= options.MaxEqualLines))
                    {
                        break;
                    }

                    lines.Add(line);
                    index++;

                    if (equal)
                        equalLines++;

                    else
                        lastDifferentLine = lines.Count - 1;
                }
            }

            lines.RemoveRange(lastDifferentLine + 1, lines.Count - lastDifferentLine - 1);

            var ranges =
                lines
                .Where(line => line.Actual != line.Expected)
                .Select(DifferenceRange)
                .ToArray();

            hunks.Add(
                new StringDiffHunk(
                    lines.AsReadOnly(),
                    new StringDiffColumnRange(
                        ranges.Min(range => range.Start),
                        ranges.Max(range => range.End)),
                    options.MaxLineLength));
        }

        return hunks.AsReadOnly();
    }

    /// <summary>
    /// Renders a hunk independently of comparison, using quoted actual/expected blocks and
    /// arrows only at columns differing in at least one line. A one-line hunk has the same compact layout
    /// as <see cref="RenderCoreStringDifference"/>. Long lines share the same horizontal slice.
    /// Tabs, quotes, backslashes, and control characters are escaped to keep markers aligned.
    /// </summary>
    public static string RenderHunk(StringDiffHunk hunk)
        => RenderHunk(hunk, null);

    /// <summary>
    /// Renders the full differing column range with independently configurable left/right
    /// context. Context counts escaped display columns and does not affect hunk recognition.
    /// </summary>
    public static string RenderHunk(StringDiffHunk hunk, StringDiffRenderingOptions? options)
    {
        ArgumentNullException.ThrowIfNull(hunk);
        ArgumentOutOfRangeException.ThrowIfLessThan(hunk.MaxLineLength, 1);

        options ??= new StringDiffRenderingOptions();
        ValidateRenderingOptions(options);

        if (hunk.Lines.Count is 0)
            return "";

        var range = hunk.DifferenceColumns;

        if (range.Start < 0 || range.End <= range.Start)
        {
            throw new ArgumentException(
                "A nonempty hunk requires a nonnegative, nonempty difference column range.",
                nameof(hunk));
        }

        var sliceStart = Math.Max(0, range.Start - options.ContextColumnsLeft);
        var sliceEnd = (int)Math.Min(int.MaxValue, (long)range.End + options.ContextColumnsRight);

        var displayedLines = hunk.Lines.Select(DisplayPair).ToArray();
        var prefix = sliceStart > 0 ? "…" : "";
        var markerIndent = new string(' ', prefix.Length + 1 + range.Start - sliceStart);
        var markerLength = range.End - range.Start;

        var markers =
            new string(
                Enumerable.Range(range.Start, markerLength)
                .Select(
                    column => displayedLines.Any(pair => HasDifferenceAtColumn(pair.actual, pair.expected, column))
                    ?
                    '↓'
                    :
                    ' ')
                .ToArray());

        string Quote(string? text)
        {
            if (text is null)
                return "<missing line>";

            var start = Math.Min(sliceStart, text.Length);
            var length = Math.Min(sliceEnd, text.Length) - start;
            var suffix = text.Length - start > length ? "…" : "";

            return "\"" + prefix + text.Substring(start, length) + suffix + "\"";
        }

        return
            string.Join(
                "\n",
                new[] { markerIndent + markers + " (actual)" }
                .Concat(displayedLines.Select(pair => Quote(pair.actual)))
                .Concat(["<vs>"])
                .Concat(displayedLines.Select(pair => Quote(pair.expected)))
                .Concat([markerIndent + markers.Replace('↓', '↑') + " (expected)"]));
    }

    /// <summary>
    /// Returns a message containing the configured hunks, or null for ordinally equal strings.
    /// A zero hunk limit still reports inequality, without excerpts.
    /// </summary>
    public static string? ReportDifference(
        string actual,
        string expected,
        StringDiffOptions? options = null,
        StringDiffRenderingOptions? renderingOptions = null)
    {
        renderingOptions ??= new StringDiffRenderingOptions();
        ValidateRenderingOptions(renderingOptions);

        var hunks = FindHunks(actual, expected, options);

        if (actual == expected)
            return null;

        if (hunks.Count is 0)
            return "Strings differ.";

        return
            string.Join(
                "\n\n",
                hunks.Select(
                    hunk =>
                    "Strings differ at lines " +
                    (hunk.Lines[0].LineIndex + 1) + "-" +
                    (hunk.Lines[^1].LineIndex + 1) +
                    ", columns " + (hunk.DifferenceColumns.Start + 1) + "-" +
                    hunk.DifferenceColumns.End + ":\n" +
                    RenderHunk(hunk, renderingOptions)));
    }

    private static void ValidateRenderingOptions(StringDiffRenderingOptions options)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(options.ContextColumnsLeft);
        ArgumentOutOfRangeException.ThrowIfNegative(options.ContextColumnsRight);
    }

    private static List<string> SplitLines(string text)
    {
        var lines = new List<string>();
        var start = 0;

        for (var index = 0; index < text.Length; index++)
        {
            if (text[index] is not ('\r' or '\n'))
                continue;

            if (text[index] is '\r' && index + 1 < text.Length && text[index + 1] is '\n')
                index++;

            lines.Add(text[start..(index + 1)]);
            start = index + 1;
        }

        lines.Add(text[start..]);
        return lines;
    }

    private static (string text, string ending) SeparateEnding(string text) =>
        text.EndsWith("\r\n", StringComparison.Ordinal)
        ?
        (text[..^2], "\r\n")
        :
        text.EndsWith('\n')
        ?
        (text[..^1], "\n")
        :
        text.EndsWith('\r')
        ?
        (text[..^1], "\r")
        :
        (text, "");

    private static (string? actual, string? expected) DisplayPair(StringDiffLine line)
    {
        var actual = SeparateEnding(line.Actual ?? "");
        var expected = SeparateEnding(line.Expected ?? "");
        var showEndings = actual.ending != expected.ending;

        return
            (line.Actual is null ? null : Escape(actual.text + (showEndings ? actual.ending : "")),
            line.Expected is null ? null : Escape(expected.text + (showEndings ? expected.ending : "")));
    }

    private static string Escape(string text)
    {
        var builder = new StringBuilder();

        foreach (var character in text)
        {
            builder.Append(
                character switch
                {
                    '\t' => "\\t",
                    '\r' => "\\r",
                    '\n' => "\\n",
                    '"' => "\\\"",
                    '\\' => "\\\\",

                    _ when char.IsControl(character) =>
                    "\\u" + ((int)character).ToString("x4"),

                    _ =>
                    character.ToString()
                });
        }

        return builder.ToString();
    }

    private static bool IsOversized(StringDiffLine line, int maxLineLength)
    {
        var (actual, expected) = DisplayPair(line);

        return (actual?.Length ?? 0) > maxLineLength || (expected?.Length ?? 0) > maxLineLength;
    }

    private static StringDiffColumnRange DifferenceRange(StringDiffLine line)
    {
        var (actual, expected) = DisplayPair(line);
        var length = Math.Max(actual?.Length ?? 0, expected?.Length ?? 0);
        var first = length;
        var end = 0;

        for (var column = 0; column < length; column++)
        {
            if (!HasDifferenceAtColumn(actual, expected, column))
            {
                continue;
            }

            first = Math.Min(first, column);
            end = column + 1;
        }

        return end is 0 ? new(0, 1) : new(first, end);
    }

    private static bool HasDifferenceAtColumn(string? actual, string? expected, int column)
    {
        var actualLength = actual?.Length ?? 0;
        var expectedLength = expected?.Length ?? 0;

        if (column >= actualLength && column >= expectedLength)
            return column is 0 && (actual is null) != (expected is null);

        return column >= actualLength || column >= expectedLength || actual![column] != expected![column];
    }
}
