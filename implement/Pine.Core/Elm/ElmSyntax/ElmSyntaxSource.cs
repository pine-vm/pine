using System;
using System.Text;

using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>Source operations shared by recovery and presentation, without grammar or prose.</summary>
internal static class ElmSyntaxSource
{
    internal static string[] Lines(string source) =>
        source.Replace("\r\n", "\n", StringComparison.Ordinal).Replace('\r', '\n').Split('\n');

    internal static string Slice(string source, Range range)
    {
        var lines = Lines(source);
        var builder = new StringBuilder();

        for (var row = Math.Max(1, range.Start.Row); row <= range.End.Row && row <= lines.Length; row++)
        {
            var line = lines[row - 1];
            var start = row == range.Start.Row ? Math.Clamp(range.Start.Column - 1, 0, line.Length) : 0;
            var end = row == range.End.Row ? Math.Clamp(range.End.Column - 1, start, line.Length) : line.Length;

            if (row > range.Start.Row)
                builder.Append('\n');

            builder.Append(line.AsSpan(start, end - start));
        }

        return builder.ToString();
    }
}
