using AwesomeAssertions;
using AwesomeAssertions.Execution;
using Pine.Core.Testing;

namespace Pine.Core.Tests;

/// <summary>
/// String equality assertions that show several changed lines in statistics and snapshots.
/// </summary>
public static class StringAssertions
{
    private static readonly StringDiffOptions s_snapshotOptions =
        new(MaxLines: 20, MaxHunks: 3);

    [CustomAssertion]
    public static void ShouldBeWithDiff(
        this string? actual,
        string? expected,
        StringDiffOptions? options = null,
        StringDiffRenderingOptions? renderingOptions = null)
    {
        if (actual == expected)
            return;

        AssertionScope.Current.AddPreFormattedFailure(
            actual is null || expected is null
            ?
            "Strings differ: actual is " + (actual is null ? "null" : "non-null") +
            ", expected is " + (expected is null ? "null" : "non-null") + "."
            :
            StringDiff.ReportDifference(actual, expected, options ?? s_snapshotOptions, renderingOptions)!);
    }
}
