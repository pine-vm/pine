using System.Collections.Generic;

namespace Pine.Core.Testing;

/// <summary>
/// A pair of original line segments, including their line endings. Null denotes a missing
/// line, distinct from an empty line. Line indices are zero-based.
/// </summary>
public sealed record StringDiffLine(int LineIndex, string? Actual, string? Expected);

/// <summary>
/// An inclusive/exclusive range of zero-based display columns. Control characters are escaped
/// before measuring columns; missing characters occupy at least one marker column.
/// </summary>
public sealed record StringDiffColumnRange(int Start, int End);

/// <summary>
/// A bounded run of paired lines. Original text is retained for alternative renderers (for
/// example, terminal colors). The column range bounds rendering context; arrows appear only
/// at columns differing in at least one of the paired lines.
/// </summary>
public sealed record StringDiffHunk(
    IReadOnlyList<StringDiffLine> Lines,
    StringDiffColumnRange DifferenceColumns,
    int MaxLineLength);
