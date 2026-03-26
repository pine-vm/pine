using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace Pine.Core;

/// <summary>
/// Renders an overview of test results with pass/fail counts,
/// analogous to the summary produced by <c>dotnet test</c>
/// when each test scenario has its own entry point method.
/// </summary>
public static class TestResultSummary
{
    /// <summary>
    /// Represents the result of a single test case, capturing the case name,
    /// whether it passed, and an optional failure message.
    /// </summary>
    /// <param name="CaseName">Name identifying the test case (typically the directory name).</param>
    /// <param name="Passed">Whether the test case passed.</param>
    /// <param name="FailureMessage">
    /// A human-readable description of the failure, or <c>null</c> when the test passed.
    /// </param>
    public record TestCaseResult(
        string CaseName,
        bool Passed,
        string? FailureMessage = null);

    /// <summary>
    /// Renders a test-run summary with counts and, for failed cases,
    /// detailed failure messages.
    /// </summary>
    /// <param name="results">The collection of individual test case results to summarize.</param>
    /// <returns>A multi-line string with overall counts and per-failure details.</returns>
    public static string RenderSummary(IReadOnlyList<TestCaseResult> results)
    {
        var passed = results.Count(r => r.Passed);
        var failed = results.Count(r => !r.Passed);

        var sb = new StringBuilder();

        AppendLine(sb, failed is 0 ? "Test run summary: Passed!" : "Test run summary: Failed!");
        AppendLine(sb, "  total: " + results.Count);
        AppendLine(sb, "  failed: " + failed);
        AppendLine(sb, "  succeeded: " + passed);

        foreach (var failedResult in results.Where(r => !r.Passed))
        {
            sb.Append('\n');
            AppendLine(sb, "Failed: " + failedResult.CaseName);

            if (failedResult.FailureMessage is not null)
            {
                AppendLine(sb, failedResult.FailureMessage);
            }
        }

        return sb.ToString().TrimEnd();
    }

    /// <summary>
    /// Runs file-based test cases from subdirectories of a test data directory.
    /// Each subdirectory is passed to <paramref name="runCase"/>, which returns the
    /// expected and actual strings. Results for all cases are collected and a summary
    /// is rendered. If any test case fails, the returned list will contain entries
    /// with <see cref="TestCaseResult.Passed"/> set to <c>false</c>.
    /// </summary>
    /// <param name="testDataSubdirectory">
    /// Relative path under <c>TestData</c> (e.g. <c>"ReduceExpressionBottomUp"</c>).
    /// </param>
    /// <param name="runCase">
    /// A function that receives the full path of a case directory and returns
    /// a tuple of (<c>expected</c>, <c>actual</c>) strings to compare.
    /// </param>
    /// <param name="trimWhitespace">
    /// When <c>true</c>, leading and trailing whitespace is removed from both
    /// the expected and actual strings before comparison.
    /// </param>
    /// <returns>The list of results for all discovered test cases.</returns>
    public static IReadOnlyList<TestCaseResult> RunFileBasedTestCases(
        string testDataSubdirectory,
        Func<string, (string expected, string actual)> runCase,
        bool trimWhitespace = false)
    {
        var testDataDir = FindTestDataDirectory(testDataSubdirectory);

        var caseDirs =
            Directory.GetDirectories(testDataDir)
            .Order()
            .ToArray();

        if (caseDirs.Length is 0)
        {
            throw new InvalidOperationException(
                "Expected test case directories in " + testDataDir);
        }

        var results = new List<TestCaseResult>();

        for (var i = 0; i < caseDirs.Length; ++i)
        {
            var caseDir = caseDirs[i];
            var caseName = Path.GetFileName(caseDir);

            try
            {
                var (expected, actual) = runCase(caseDir);

                if (trimWhitespace)
                {
                    expected = expected.Trim();
                    actual = actual.Trim();
                }

                var diff = DescribeStringDifference(expected, actual);

                results.Add(
                    new TestCaseResult(
                        caseName,
                        Passed: diff is null,
                        FailureMessage: diff));
            }
            catch (Exception e)
            {
                results.Add(
                    new TestCaseResult(
                        caseName,
                        Passed: false,
                        FailureMessage: e.ToString()));
            }
        }

        return results;
    }

    /// <summary>
    /// Locates a <c>TestData</c> subdirectory by walking up from <see cref="AppContext.BaseDirectory"/>
    /// until a directory named <c>TestData/{subdirectory}</c> is found.
    /// </summary>
    /// <param name="subdirectory">The subdirectory name under <c>TestData</c>.</param>
    /// <returns>The full path to the test data directory.</returns>
    /// <exception cref="DirectoryNotFoundException">
    /// Thrown when the directory cannot be found in any ancestor of the base directory.
    /// </exception>
    public static string FindTestDataDirectory(string subdirectory)
    {
        var dir = new DirectoryInfo(AppContext.BaseDirectory);

        while (dir is not null)
        {
            var candidate = Path.Combine(dir.FullName, "TestData", subdirectory);

            if (Directory.Exists(candidate))
                return candidate;

            dir = dir.Parent;
        }

        throw new DirectoryNotFoundException(
            "Could not find TestData/" + subdirectory + " directory " +
            "in any parent of " + AppContext.BaseDirectory);
    }

    /// <summary>
    /// Compares two strings line-by-line and returns a human-readable
    /// description of the first difference found, including context lines.
    /// Returns <c>null</c> when the strings are equal.
    /// </summary>
    /// <param name="expected">The expected string.</param>
    /// <param name="actual">The actual string to compare against <paramref name="expected"/>.</param>
    /// <returns>
    /// A description of the first difference, or <c>null</c> when the strings are equal.
    /// </returns>
    public static string? DescribeStringDifference(string expected, string actual)
    {
        if (expected == actual)
            return null;

        var expectedLines = SplitLines(expected);
        var actualLines = SplitLines(actual);

        var maxLines = Math.Max(expectedLines.Length, actualLines.Length);

        for (var i = 0; i < maxLines; i++)
        {
            var expectedLine = i < expectedLines.Length ? expectedLines[i] : null;
            var actualLine = i < actualLines.Length ? actualLines[i] : null;

            if (expectedLine == actualLine)
                continue;

            var sb = new StringBuilder();

            AppendLine(sb, "Strings differ at line " + (i + 1) + ":");

            var contextStart = Math.Max(0, i - 2);
            var contextEnd = Math.Min(maxLines - 1, i + 2);

            for (var c = contextStart; c <= contextEnd; c++)
            {
                var expLine = c < expectedLines.Length ? expectedLines[c] : null;
                var actLine = c < actualLines.Length ? actualLines[c] : null;

                if (c == i)
                {
                    AppendLine(sb, "  expected line " + (c + 1) + ": " + (expLine ?? "(missing)"));
                    AppendLine(sb, "  actual   line " + (c + 1) + ": " + (actLine ?? "(missing)"));
                }
                else
                {
                    var line = actLine ?? expLine;
                    AppendLine(sb, "           line " + (c + 1) + ": " + line);
                }
            }

            return sb.ToString().TrimEnd();
        }

        return "Strings differ (trailing whitespace or line endings)";
    }

    /// <summary>
    /// Appends text followed by a <c>\n</c> character to the <see cref="StringBuilder"/>.
    /// <para/>
    /// This replaces direct use of <see cref="StringBuilder.AppendLine(string)"/>,
    /// which uses <see cref="Environment.NewLine"/> (<c>\r\n</c> on Windows, <c>\n</c> on Unix),
    /// causing test failures when comparing output across platforms.
    /// </summary>
    private static void AppendLine(StringBuilder sb, string text) =>
        sb.Append(text).Append('\n');

    /// <summary>
    /// Splits text into lines using <see cref="ElmModule.ModuleLines"/>,
    /// which handles all common line ending variants (LF, CR, CRLF, LFCR)
    /// without allocating an intermediate normalized string.
    /// </summary>
    private static string[] SplitLines(string text) =>
        [.. ElmModule.ModuleLines(text)];
}
