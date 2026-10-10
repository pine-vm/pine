namespace Pine.Core.Testing;

/// <summary>
/// Bounds for ordinal, line-by-line string comparisons. An oversized differing line forms
/// a hunk on its own. Rendering context is configured separately.
/// Equal lines are included only between differing lines, never at a hunk's edges.
/// </summary>
/// <param name="MaxLineLength">Maximum escaped content length before a line ends hunk recognition.
/// Quotes, omission indicators, and labels do not count toward this limit.</param>
/// <param name="MaxLines">Maximum number of paired lines, including equal lines, in a hunk.</param>
/// <param name="MaxEqualLines">Maximum total number of equal lines within a hunk.</param>
/// <param name="MaxHunks">Maximum number of hunks to return. Zero suppresses all hunks.</param>
public sealed record StringDiffOptions(
    int MaxLineLength = 160,
    int MaxLines = 10,
    int MaxEqualLines = 2,
    int MaxHunks = 1);
