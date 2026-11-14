using Pine.Core.Elm;
using Pine.Core.Files;
using Pine.Core.Testing;
using Pine.Elm019.Testing;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm019;

public enum ElmMakeOutputType
{
    Html, JavaScript
}

public enum ElmMakeFlavor
{
    Development, Debug, Optimize
}

public enum ElmMakeExpectationKind
{
    Output, ErrorsOmittingDetails
}

public sealed record ElmMakeExpectation(
    string ScenarioName,
    string SourcePath,
    ElmPackageVersion CompilerVersion,
    ElmMakeOutputType OutputType,
    ElmMakeFlavor Flavor,
    bool Skipped,
    ElmMakeExpectationKind Kind = ElmMakeExpectationKind.Output)
{
    public string RelativePath =>
        string.Join(
            "/",
            new[] { ScenarioName, Skipped ? "skip/expect" : "expect", CompilerVersion.ToString().Replace('.', '-'), SourcePath }
            ) +
        (Flavor switch
        {
            ElmMakeFlavor.Development => "",
            ElmMakeFlavor.Debug => ".debug",
            ElmMakeFlavor.Optimize => ".optimize",

            _ =>
            throw new ArgumentOutOfRangeException(nameof(Flavor)),
        }) +
        (OutputType switch
        {
            ElmMakeOutputType.Html => ".html",
            ElmMakeOutputType.JavaScript => ".js",

            _ =>
            throw new ArgumentOutOfRangeException(nameof(OutputType)),
        }) +
        (Kind switch
        {
            ElmMakeExpectationKind.Output => "",
            ElmMakeExpectationKind.ErrorsOmittingDetails => ".errors-omitting-details.txt",

            _ =>
            throw new ArgumentOutOfRangeException(nameof(Kind)),
        });
}

public sealed record ElmMakeScenario(
    string Name,
    FileTree AppFiles,
    IReadOnlyList<ElmMakeExpectation> Expectations);

public sealed record ElmMakeSnapshotResult(ElmMakeExpectation Expectation, bool Passed, string? Error);

/// <summary>Shared corpus discovery, execution and reporting for the CLI and integration tests.</summary>
public static partial class ElmMakeEcmascriptScenarios
{
    public static ElmMakeExpectation ParseExpectation(string relativePath)
    {
        var segments = relativePath.Replace('\\', '/').Split('/');
        ValidatePath(segments);

        var skipped = segments.Length > 2 && segments[1] is "skip" && segments[2] is "expect";
        var prefixLength = skipped ? 4 : 3;

        if (segments.Length <= prefixLength || !skipped && segments[1] is not "expect")
        {
            throw new ArgumentException(
                "Expected <scenario>/[skip/]expect/<major-minor-patch>/<source>.elm[.debug|.optimize].(html|js)[.errors-omitting-details.txt]: " +
                relativePath);
        }

        var versionSegment = segments[prefixLength - 1];
        ElmPackageVersion compilerVersion;

        try
        {
            compilerVersion = ElmPackageVersion.Parse(versionSegment.Replace('-', '.'));
        }
        catch (FormatException exception)
        {
            throw new ArgumentException(
                "Invalid compiler version qualifier in expectation path: " + relativePath,
                exception);
        }

        if (compilerVersion.ToString().Replace('.', '-') != versionSegment)
        {
            throw new ArgumentException(
                "Expected a major-minor-patch compiler version qualifier in expectation path: " + relativePath);
        }

        var match = ExpectationPathRegex().Match(string.Join("/", segments.Skip(prefixLength)));

        if (!match.Success)
            throw new ArgumentException("Invalid expectation path: " + relativePath);

        return
            new(
                segments[0],
                match.Groups["source"].Value,
                compilerVersion,
                match.Groups["output"].Value is "html" ? ElmMakeOutputType.Html : ElmMakeOutputType.JavaScript,
                match.Groups["flavor"].Value switch
                {
                    "" => ElmMakeFlavor.Development,
                    "debug" => ElmMakeFlavor.Debug,
                    "optimize" => ElmMakeFlavor.Optimize,

                    _ =>
                    throw new InvalidOperationException("Unexpected expectation flavor."),
                },
                skipped,
                match.Groups["errors"].Success
                ?
                ElmMakeExpectationKind.ErrorsOmittingDetails
                :
                ElmMakeExpectationKind.Output);
    }

    public static IReadOnlyList<ElmMakeScenario> Discover(string directory, bool allowEmptyExpectations = false)
    {
        var scenariosDirectory = Path.Combine(directory, "scenarios");

        if (!Directory.Exists(scenariosDirectory))
            throw new DirectoryNotFoundException("Elm make ECMAScript scenarios directory not found: " + scenariosDirectory);

        RejectSymbolicLink(scenariosDirectory);
        var scenarios = new List<ElmMakeScenario>();

        foreach (var path in Directory.GetDirectories(scenariosDirectory).Order(StringComparer.Ordinal))
        {
            var name = Path.GetFileName(path);

            ValidatePath([name]);
            RejectSymbolicLink(path);
            var input = Path.Combine(path, "input-app");

            if (!Directory.Exists(input))
                throw new InvalidDataException("Scenario '" + name + "' is missing input-app.");

            var expectations = new List<ElmMakeExpectation>();

            foreach (var prefix in new[] { "expect", Path.Combine("skip", "expect") })
            {
                var expect = Path.Combine(path, prefix);

                if (Directory.Exists(expect))
                {
                    expectations.AddRange(
                        EnumerateSourceFiles(expect).Select(
                            file => ParseExpectation(Path.GetRelativePath(scenariosDirectory, file))));
                }
            }

            if (expectations.Count is 0 && !allowEmptyExpectations)
                throw new InvalidDataException("Scenario '" + name + "' has zero expectation files.");

            ValidateExpectations(expectations);

            foreach (var expectation in expectations.Where(item => item.Kind is ElmMakeExpectationKind.ErrorsOmittingDetails))
            {
                ElmMakeErrorSnapshots.Parse(
                    new UTF8Encoding(false, true).GetString(File.ReadAllBytes(ExpectationPath(directory, expectation))));
            }

            var app =
                FileTree.FromSetOfFilesWithStringPath(
                    EnumerateSourceFiles(input).Select(
                        file =>
                        ((IReadOnlyList<string>)Path.GetRelativePath(input, file).Split(Path.DirectorySeparatorChar),
                        (ReadOnlyMemory<byte>)File.ReadAllBytes(file))));

            scenarios.Add(
                new(
                    name,
                    app,
                    [.. expectations.OrderBy(item => item.RelativePath, StringComparer.Ordinal)]));
        }

        if (scenarios.Count is 0)
            throw new InvalidDataException("No Elm make ECMAScript scenarios found in " + scenariosDirectory);

        return scenarios;
    }

    public static ElmMakeScenario SelectScenario(IReadOnlyList<ElmMakeScenario> scenarios, string name) =>
        scenarios.FirstOrDefault(scenario => scenario.Name == name)
        ??
        throw new ArgumentException(
            "Unknown scenario '" + name + "'. Existing scenarios: " +
            string.Join(", ", scenarios.Select(scenario => scenario.Name)) +
            ". Closest name: " +
            scenarios.MinBy(scenario => EditDistance(name, scenario.Name))!.Name);

    public static async Task<IReadOnlyList<ElmMakeSnapshotResult>> RunAsync(
        string directory,
        IReadOnlyList<ElmMakeScenario> scenarios,
        Func<ElmMakeScenario, ElmMakeExpectation, CancellationToken, Task<ElmMakeCompilation>> compile,
        int maxDiffHunks = 3,
        bool skipDebug = false,
        bool skipOptimize = false,
        CancellationToken cancellationToken = default)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(maxDiffHunks);

        if (scenarios.Count is 0)
            throw new InvalidDataException("No Elm make ECMAScript scenarios selected.");

        var results = new List<ElmMakeSnapshotResult>();

        foreach (var scenario in scenarios)
        {
            if (scenario.Expectations.Count is 0)
                throw new InvalidDataException("Scenario '" + scenario.Name + "' has zero expectation files.");

            ValidateExpectations(scenario.Expectations);

            foreach (var expectation in scenario.Expectations.Where(
                item => !item.Skipped &&
                    !(skipDebug && item.Flavor is ElmMakeFlavor.Debug) &&
                    !(skipOptimize && item.Flavor is ElmMakeFlavor.Optimize)))
            {
                cancellationToken.ThrowIfCancellationRequested();

                try
                {
                    var actual =
                        NormalizeLineEndings(
                            ElmMakeErrorSnapshots.SnapshotText(
                                expectation,
                                await compile(scenario, expectation, cancellationToken)));

                    var expected =
                        NormalizeLineEndings(
                            new UTF8Encoding(false, true).GetString(
                                await File.ReadAllBytesAsync(ExpectationPath(directory, expectation), cancellationToken)));

                    if (expectation.Kind is ElmMakeExpectationKind.ErrorsOmittingDetails)
                        ElmMakeErrorSnapshots.Parse(expected);

                    var hunks = StringDiff.FindHunks(actual, expected, new(MaxHunks: int.MaxValue));

                    var error =
                        actual == expected
                        ?
                        null
                        :
                        $"{hunks.Count} differing hunk(s); showing {Math.Min(hunks.Count, maxDiffHunks)}.\n" +
                        string.Join("\n\n", hunks.Take(maxDiffHunks).Select(hunk => StringDiff.RenderHunk(hunk)));

                    results.Add(new(expectation, error is null, error));
                }
                catch (Exception exception) when (exception is not OperationCanceledException)
                {
                    results.Add(new(expectation, false, exception.Message));
                }
            }
        }

        return results;
    }

    internal static string NormalizeLineEndings(string text) =>
        text.Replace("\r\n", "\n", StringComparison.Ordinal).Replace('\r', '\n');

    public static void ValidateExpectations(IEnumerable<ElmMakeExpectation> expectations)
    {
        foreach (var request in expectations.GroupBy(
            item => (item.ScenarioName, item.SourcePath, item.CompilerVersion, item.OutputType, item.Flavor)))
        {
            if (request.Select(item => item.Kind).Distinct().Count() > 1)
            {
                throw new InvalidDataException(
                    "Contradictory output and compiler-error expectations for: " + request.First().RelativePath);
            }
        }
    }

    public static string RenderResults(IReadOnlyList<ElmMakeSnapshotResult> results)
    {
        var text =
            new StringBuilder(
                TestResultSummary.RenderSummary(
                    [
                        .. results.Select(
                            result =>
                            new TestResultSummary.TestCaseResult(
                                result.Expectation.RelativePath,
                                result.Passed,
                                result.Error))
                    ]));

        foreach (var version in results.Select(result => result.Expectation.CompilerVersion).Distinct().Order())
        {
            AppendGroup(
                "Compiler " + version,
                [.. results.Where(result => result.Expectation.CompilerVersion == version)]);
        }

        foreach (var flavor in Enum.GetValues<ElmMakeFlavor>())
            AppendGroup(flavor.ToString(), [.. results.Where(result => result.Expectation.Flavor == flavor)]);

        foreach (var output in Enum.GetValues<ElmMakeOutputType>())
            AppendGroup(output.ToString(), [.. results.Where(result => result.Expectation.OutputType == output)]);

        foreach (var kind in Enum.GetValues<ElmMakeExpectationKind>())
            AppendGroup(kind.ToString(), [.. results.Where(result => result.Expectation.Kind == kind)]);

        return text.ToString();

        void AppendGroup(string name, ElmMakeSnapshotResult[] group) =>
            text.Append(
                $"\n{name}: total {group.Length}, succeeded {group.Count(result => result.Passed)}, failed {group.Count(result => !result.Passed)}");
    }

    public static string ExpectationPath(string directory, ElmMakeExpectation expectation) =>
        Path.Combine(directory, "scenarios", Path.Combine(expectation.RelativePath.Split('/')));

    internal static IEnumerable<string> EnumerateSourceFiles(string directory)
    {
        RejectSymbolicLink(directory);

        foreach (var path in Directory.EnumerateFileSystemEntries(directory).Order(StringComparer.Ordinal))
        {
            var attributes = File.GetAttributes(path);

            if ((attributes & FileAttributes.ReparsePoint) != 0)
                throw new InvalidDataException("Symbolic links are not supported in snapshot inputs: " + path);

            if ((attributes & FileAttributes.Directory) != 0)
            {
                if (Path.GetFileName(path) is not ("elm-stuff" or ".git"))
                {
                    foreach (var file in EnumerateSourceFiles(path))
                        yield return file;
                }
            }
            else if (Path.GetFileName(path) is not "artifacts.dat")
            {
                yield return path;
            }
        }
    }

    private static void RejectSymbolicLink(string path)
    {
        if ((File.GetAttributes(path) & FileAttributes.ReparsePoint) != 0)
            throw new InvalidDataException("Symbolic links are not supported in snapshot inputs: " + path);
    }

    internal static void ValidatePath(IEnumerable<string> segments)
    {
        foreach (var segment in segments)
        {
            if (string.IsNullOrWhiteSpace(segment) || segment is "." or ".." ||
                segment.IndexOfAny(['/', '\\', ':', '\0']) >= 0 ||
                segment.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0)
            {
                throw new ArgumentException("Expected valid relative path segments, found: '" + segment + "'.");
            }
        }
    }

    private static int EditDistance(string left, string right)
    {
        var row = Enumerable.Range(0, right.Length + 1).ToArray();

        for (var i = 1; i <= left.Length; i++)
        {
            var diagonal = row[0];
            row[0] = i;

            for (var j = 1; j <= right.Length; j++)
            {
                var previous = row[j];

                row[j] =
                    Math.Min(
                        Math.Min(row[j] + 1, row[j - 1] + 1),
                        diagonal + (left[i - 1] == right[j - 1] ? 0 : 1));

                diagonal = previous;
            }
        }

        return row[^1];
    }

    [GeneratedRegex(@"^(?<source>.+\.elm)(?:\.(?<flavor>debug|optimize))?\.(?<output>html|js)(?<errors>\.errors-omitting-details\.txt)?$", RegexOptions.CultureInvariant)]
    private static partial Regex ExpectationPathRegex();
}
