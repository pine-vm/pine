namespace Pine.Core.Testing;

/// <summary>
/// Horizontal context for plain-text diff rendering, independent of hunk recognition limits.
/// The full range of differing columns is always displayed. Defaults include 80 columns on
/// either side, keeping performance-counter snapshots visible without ellipses.
/// </summary>
/// <param name="ContextColumnsLeft">Maximum escaped columns before the differing range. Zero hides left context.</param>
/// <param name="ContextColumnsRight">Maximum escaped columns after the differing range. Zero hides right context.</param>
public sealed record StringDiffRenderingOptions(
    int ContextColumnsLeft = 80,
    int ContextColumnsRight = 80);
