using AwesomeAssertions;
using Pine.CLI;
using Pine.CLI.Elm;
using Pine.Core.Elm;
using Pine.Core.Elm.Testing;
using Pine.Core.Files;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.CLI.Elm;

public class RemoteTestCommandTests
{
    private const string PinnedProjectUrl =
        "https://github.com/pine-vm/pine/tree/cc2c94d4c96a2794806b43c416fc7716bf86ab36/implement/Pine.Core.Tests/TestData/Elm/CommandElmTest/multiple-top-level-tests/input-app";

    private const string ExpectedList =
        """
        Available tests (2)
        └── tests/Tests.elm
            ├── first test
            └── second group
                └── second test
        """;

    [Fact]
    public void RemoteTestCommand_lists_actual_pinned_GitCore_project()
    {
        var (console, output) = CreateConsole();
        var materializedDirectory = CaptureMaterializedDirectory(output);

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true).Should().Be(0);

        AssertList(output.ToString());
        output.ToString().Should().Contain("Loading remote Elm project: " + PinnedProjectUrl);
        output.ToString().Should().Contain("Resolving dependencies and compiling Elm tests.");
        AssertCleaned(materializedDirectory());
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r\n")]
    public void RemoteTestCommand_flushes_loading_progress_before_transport_and_lists_exact_tests(string newLine)
    {
        var (console, output) = CreateConsole();
        output.NewLine = newLine;
        var materializedDirectory = CaptureMaterializedDirectory(output);
        var files = FixtureFiles();
        var called = false;

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true,
            remoteSourceLoader: source =>
            {
                called = true;
                source.Should().Be(PinnedProjectUrl);

                output.ToString().ReplaceLineEndings("\n").Should().Be(
                    "Loading remote Elm project: " + PinnedProjectUrl + "\n");

                output.FlushCount.Should().BeGreaterThan(0);
                return files;
            }).Should().Be(0);

        called.Should().BeTrue();
        AssertList(output.ToString());
        AssertCleaned(materializedDirectory());
    }

    [Theory]
    [InlineData(false, null, 0)]
    [InlineData(false, "first test", 0)]
    [InlineData(true, "first test", 0)]
    [InlineData(false, "not a discovered test", 1)]
    [InlineData(true, "not a discovered test", 1)]
    public void RemoteTestCommand_shares_source_loading_for_running_listing_and_filters(
        bool listTests, string? filter, int expectedExit)
    {
        var (console, output) = CreateConsole();
        var materializedDirectory = CaptureMaterializedDirectory(output);

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            filter: filter,
            listTests: listTests,
            remoteSourceLoader: _ => FixtureFiles()).Should().Be(expectedExit);

        if (expectedExit == 1)
            output.ToString().Should().Contain("No tests matched the filter expression:");

        else if (listTests)
            output.ToString().Should().Contain("Tests remaining after filtering (1)");

        else
            output.ToString().Should().Contain("TEST RUN PASSED");

        AssertCleaned(materializedDirectory());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RemoteTestCommand_handles_transport_failure_without_stacktrace_or_sensitive_details(
        bool plainException)
    {
        var (console, output) = CreateConsole();

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true,
            remoteSourceLoader: _ => throw (
                plainException
            ?
            new Exception("secret-token server message")
            :
            new HttpRequestException("secret-token server message")))
            .Should().Be(1);

        output.ToString().Should().Contain("Error:").And.Contain("Cannot load remote Elm project: " + PinnedProjectUrl)
            .And.NotContain("secret-token").And.NotContain("Exception").And.NotContain("   at ");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RemoteTestCommand_does_not_translate_unexpected_system_exceptions(bool memoryFailure)
    {
        var (console, output) = CreateConsole();

        Exception exception =
            memoryFailure ? new OutOfMemoryException("simulated") : new NullReferenceException("simulated");

        Action execute =
            () =>
            TestCommand.Execute(
                PinnedProjectUrl,
                colorMode: FormatCommandColorMode.Never,
                console: console,
                errorConsole: console,
                remoteSourceLoader: _ => throw exception);

        execute.Should().Throw<Exception>().Which.Should().BeSameAs(exception);
        output.ToString().Should().NotContain("Error:");
    }

    [Fact]
    public void RemoteTestCommand_missing_remote_subdirectory_is_a_clean_loading_error()
    {
        var source = PinnedProjectUrl + "/__directory_that_does_not_exist__";
        var (console, output) = CreateConsole();

        TestCommand.Execute(
            source,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true).Should().Be(1);

        output.ToString().Should().Contain("Cannot load remote Elm project: " + source)
            .And.NotContain("Exception").And.NotContain("   at ");
    }

    [Fact]
    public void RemoteTestCommand_budget_stop_before_selector_preserves_URL_in_profile_followup()
    {
        var (console, output) = CreateConsole();
        var materializedDirectory = CaptureMaterializedDirectory(output);

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            evaluationOptions: new() { InvocationBudget = 1 },
            resolutionConfiguration: ElmTestRunner.DefaultResolutionConfiguration.Value with { Substitutions = [] },
            packageProvider: new CancelledPackageProvider(),
            remoteSourceLoader: _ => FixtureFiles()).Should().Be(2);

        output.ToString().Should().Contain("Execution stopped. Time budget exhausted.")
            .And.Contain("pine elm test profile '" + PinnedProjectUrl + "' --invocation-budget 1")
            .And.NotContain(" --filter ").And.NotContain("pine-elm-test-source-");

        AssertCleaned(materializedDirectory());
    }

    [Fact]
    public void RemoteTestCommand_offline_never_calls_transport()
    {
        var (console, output) = CreateConsole();

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true,
            offline: true,
            remoteSourceLoader: _ => throw new Exception("Transport must not be called"))
            .Should().Be(1);

        output.ToString().Should().Contain("Cannot load a remote Elm project with --offline.")
            .And.NotContain("Loading remote Elm project:");
    }

    [Theory]
    [InlineData("https://example.com/project")]
    [InlineData("https://github.com/not-a-tree")]
    [InlineData("https://gitlab.com/not-a-tree")]
    [InlineData("https://github.com")]
    [InlineData("https://github.com/owner/repo/tree/main?token=secret")]
    [InlineData("https://github.com:1234/owner/repo/tree/main")]
    [InlineData("ftp://github.com/owner/repo/tree/main")]
    [InlineData("https://github.com/\nowner/repo/tree/main")]
    public void RemoteTestCommand_rejects_unsupported_URLs_before_filesystem_or_transport(string source)
    {
        var (console, output) = CreateConsole();

        TestCommand.Execute(
            source,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            remoteSourceLoader: _ => throw new Exception("Transport must not be called"))
            .Should().Be(1);

        output.ToString().Should().Contain("Error:").And.NotContain("secret")
            .And.NotContain("Loading remote Elm project:").And.NotContain("directory not found");
    }

    [Fact]
    public void RemoteTestCommand_rejects_credentials_without_logging_them()
    {
        var source = new UriBuilder(PinnedProjectUrl) { UserName = "example-user" }.Uri.AbsoluteUri;
        var (console, output) = CreateConsole();

        TestCommand.Execute(
            source,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            remoteSourceLoader: _ => throw new Exception("Transport must not be called"))
            .Should().Be(1);

        output.ToString().Should().Contain("Error:").And.NotContain("example-user")
            .And.NotContain("Loading remote Elm project:");
    }

    [Theory]
    [InlineData("../escaped.elm")]
    [InlineData(@"..\escaped.elm")]
    [InlineData("/escaped.elm")]
    [InlineData(@"C:\escaped.elm")]
    [InlineData("escaped:stream")]
    [InlineData("")]
    [InlineData(".")]
    [InlineData("..")]
    [InlineData("bad\0name")]
    [InlineData(".. ")]
    [InlineData("...")]
    [InlineData("CON")]
    [InlineData("nul.txt")]
    [InlineData("COM1.elm")]
    [InlineData("lpt9")]
    [InlineData("COM¹")]
    [InlineData("CON .txt")]
    [InlineData("name*")]
    [InlineData("name?")]
    public void RemoteTestCommand_rejects_unsafe_remote_path_segments(string unsafeSegment)
    {
        var (console, output) = CreateConsole();
        var files = FixtureFiles();
        files.Add(["tests", unsafeSegment], Encoding.UTF8.GetBytes("must not be written"));

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true,
            remoteSourceLoader: _ => files).Should().Be(1);

        output.ToString().Should().Contain("Error:").And.Contain("Remote Elm project contains an unsafe file path.");
    }

    [Theory]
    [InlineData("tests", "tests.elm")]
    [InlineData("TESTS", "Another.elm")]
    public void RemoteTestCommand_rejects_case_colliding_file_and_directory_names(string directory, string fileName)
    {
        var (console, output) = CreateConsole();
        var files = FixtureFiles();
        files.Add([directory, fileName], Encoding.UTF8.GetBytes("case collision"));

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true,
            remoteSourceLoader: _ => files).Should().Be(1);

        output.ToString().Should().Contain(
            "Remote Elm project contains file paths that collide on case-insensitive filesystems.")
            .And.NotContain("Loaded ");
    }

    [Fact]
    public void RemoteTestCommand_rejects_case_colliding_manifests_before_materialization()
    {
        var (console, output) = CreateConsole();

        var existing =
            Directory.GetDirectories(Path.GetTempPath(), "pine-elm-test-source-*").ToHashSet(StringComparer.Ordinal);

        var files = FixtureFiles();
        var manifestPath = files.Keys.Single(path => path.SequenceEqual(new[] { "elm.json" }));

        var escapedManifest =
            Encoding.UTF8.GetString(files[manifestPath].Span)
            .Replace("\"src\"", "\"../outside-project\"", StringComparison.Ordinal);

        files.Add(["ELM.JSON"], Encoding.UTF8.GetBytes(escapedManifest));

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true,
            remoteSourceLoader: _ => files).Should().Be(1);

        output.ToString().Should().Contain(
            "Remote Elm project contains file paths that collide on case-insensitive filesystems.")
            .And.NotContain("Loaded ");

        Directory.GetDirectories(Path.GetTempPath(), "pine-elm-test-source-*").Should().BeSubsetOf(existing);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RemoteTestCommand_missing_manifest_reports_useful_source_error(bool nestedManifest)
    {
        var (console, output) = CreateConsole();
        var files = FixtureFiles();
        var manifestPath = files.Keys.Single(path => path.SequenceEqual(new[] { "elm.json" }));
        var manifest = files[manifestPath];
        files.Remove(manifestPath);

        if (nestedManifest)
            files.Add(["nested-project", "elm.json"], manifest);

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true,
            remoteSourceLoader: _ => files).Should().Be(1);

        output.ToString()
            .Should().Contain("Remote Elm project does not contain elm.json at the selected URL: " + PinnedProjectUrl)
            .And.Contain("Select the Elm project directory.").And.NotContain("Exception").And.NotContain("Loaded ");
    }

    [Theory]
    [InlineData("../src")]
    [InlineData("src/../../outside")]
    [InlineData(@"..\src")]
    [InlineData("/outside")]
    [InlineData(@"C:\outside")]
    public void RemoteTestCommand_rejects_manifest_directories_outside_remote_project(string sourceDirectory)
    {
        var (console, output) = CreateConsole();
        var files = FixtureFiles();
        var manifestPath = files.Keys.Single(path => path.SequenceEqual(new[] { "elm.json" }));
        var manifest = Encoding.UTF8.GetString(files[manifestPath].Span);

        files[manifestPath] =
            Encoding.UTF8.GetBytes(
                manifest.Replace(
                    "\"src\"",
                    System.Text.Json.JsonSerializer.Serialize(sourceDirectory),
                    StringComparison.Ordinal));

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true,
            remoteSourceLoader: _ => files).Should().Be(1);

        output.ToString().Should().Contain(
            "Remote Elm project source-directories must stay within the selected project directory.");

        output.ToString().Should().NotContain("Loaded ");
    }

    [Fact]
    public void RemoteTestCommand_fuzz_reproduction_retains_URL_and_cleans_project()
    {
        var (console, output) = CreateConsole();
        var materializedDirectory = CaptureMaterializedDirectory(output);
        var files = FixtureFiles();
        var testsPath = files.Keys.Single(path => path.SequenceEqual(new[] { "tests", "Tests.elm" }));

        files[testsPath] =
            Encoding.UTF8.GetBytes(
                """
                module Tests exposing (suite)
                import Test
                import Fuzz
                import Expect
                suite = Test.fuzz (Fuzz.intRange 1 100) "passing property" (\_ -> Expect.pass)
                """);

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            seed: 123,
            fuzz: 1,
            remoteSourceLoader: _ => files).Should().Be(0);

        output.ToString().Should().Contain(
            "To reproduce these results, run pine elm test \"" + PinnedProjectUrl + "\" --seed 123 --fuzz 1")
            .And.NotContain("pine-elm-test-source-");

        AssertCleaned(materializedDirectory());
    }

    [Fact]
    public void RemoteTestCommand_cleans_source_on_compilation_error()
    {
        var (console, output) = CreateConsole();
        var materializedDirectory = CaptureMaterializedDirectory(output);
        var files = FixtureFiles();
        var testsPath = files.Keys.Single(path => path.SequenceEqual(new[] { "tests", "Tests.elm" }));

        files[testsPath] =
            Encoding.UTF8.GetBytes(
                """
                module Tests exposing (suite)
                import Test exposing (Test)
                suite : Test
                suite = missingValue
                """);

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            remoteSourceLoader: _ => files).Should().Be(1);

        output.ToString().Should().Contain("Error:");
        AssertCleaned(materializedDirectory());
    }

    [Fact]
    public void RemoteTestCommand_rejects_conflicting_file_directory_paths_before_materialization()
    {
        var (console, output) = CreateConsole();

        var existing =
            Directory.GetDirectories(Path.GetTempPath(), "pine-elm-test-source-*").ToHashSet(StringComparer.Ordinal);

        var files = FixtureFiles();
        files.Add(["elm.json", "cannot-be-a-directory"], Encoding.UTF8.GetBytes("file-directory conflict"));

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            remoteSourceLoader: _ => files).Should().Be(1);

        output.ToString().Should().Contain("Remote Elm project contains conflicting file paths.")
            .And.NotContain("pine-elm-test-source-");

        Directory.GetDirectories(Path.GetTempPath(), "pine-elm-test-source-*").Should().BeSubsetOf(existing);
    }

    [Fact]
    public void RemoteTestCommand_cleans_partial_materialization_on_write_failure()
    {
        var (console, output) = CreateConsole();

        var existing =
            Directory.GetDirectories(Path.GetTempPath(), "pine-elm-test-source-*").ToHashSet(StringComparer.Ordinal);

        var files = FixtureFiles();
        files.Add(["tests", new string('x', 1024)], Encoding.UTF8.GetBytes("overlong file name"));

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            remoteSourceLoader: _ => files).Should().Be(1);

        output.ToString().Should().Contain("Cannot prepare remote Elm project: " + PinnedProjectUrl)
            .And.NotContain("pine-elm-test-source-");

        Directory.GetDirectories(Path.GetTempPath(), "pine-elm-test-source-*").Should().BeSubsetOf(existing);
    }

    [Fact]
    public void RemoteTestCommand_no_modules_reports_original_URL_not_deleted_directory()
    {
        var (console, output) = CreateConsole();
        var materializedDirectory = CaptureMaterializedDirectory(output);
        var files = FixtureFiles();
        files.Remove(files.Keys.Single(path => path.SequenceEqual(new[] { "tests", "Tests.elm" })));

        TestCommand.Execute(
            PinnedProjectUrl,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            remoteSourceLoader: _ => files).Should().Be(1);

        output.ToString().Should().Contain("Did not find Elm test modules in " + PinnedProjectUrl)
            .And.NotContain("pine-elm-test-source-");

        AssertCleaned(materializedDirectory());
    }

    [Fact]
    public void RemoteTestCommand_local_source_is_not_loaded_or_deleted()
    {
        var (console, output) = CreateConsole();
        var directory = FixtureDirectory();

        TestCommand.Execute(
            directory,
            colorMode: FormatCommandColorMode.Never,
            console: console,
            errorConsole: console,
            listTests: true,
            remoteSourceLoader: _ => throw new Exception("Transport must not be called")).Should().Be(0);

        AssertList(output.ToString());
        Directory.Exists(directory).Should().BeTrue();
        output.ToString().Should().NotContain("Loading remote Elm project:");
    }

    [Fact]
    public void RemoteTestCommand_reproduction_profile_command_retains_URL()
    {
        TestCommand.ProfileCommandLine(PinnedProjectUrl, "=tests/Tests.elm/first test").Should().Be(
            "pine elm test profile '" + PinnedProjectUrl + "' --filter '=tests/Tests.elm/first test'");
    }

    [Fact]
    public void RemoteTestCommand_profile_defaults_to_current_directory_and_records_original_source_URL()
    {
        var profileDirectory = Path.GetFullPath(Path.Combine("elm-stuff", "pine", "test-profiles"));

        var existingReports =
            Directory.Exists(profileDirectory)
            ?
            Directory.GetFiles(profileDirectory).ToHashSet(StringComparer.Ordinal)
            :
            [];

        var directoriesToClean =
            new[] { "elm-stuff", Path.Combine("elm-stuff", "pine"), profileDirectory }
            .Where(directory => !Directory.Exists(directory)).Reverse().ToArray();

        var (console, output) = CreateConsole();
        console.Profile.Width = 2000;
        var materializedDirectory = CaptureMaterializedDirectory(output);
        string? reportPath = null;

        try
        {
            var exitCode =
                TestProfileCommand.Execute(
                    PinnedProjectUrl,
                    "first test",
                    new() { Instrumentation = new() { SnapshotInterval = TimeSpan.Zero } },
                    instrumentation => TestCommand.Execute(
                        PinnedProjectUrl,
                        colorMode: FormatCommandColorMode.Never,
                        console: console,
                        errorConsole: console,
                        filter: "first test",
                        instrumentation: instrumentation,
                        remoteSourceLoader: _ => FixtureFiles()),
                    console,
                    FormatCommandColorMode.Never);

            reportPath = Directory.GetFiles(profileDirectory).Single(path => !existingReports.Contains(path));
            exitCode.Should().Be(0, output.ToString());
            output.ToString().Should().Contain("Saved JSON report: " + reportPath);
            using var report = JsonDocument.Parse(File.ReadAllText(reportPath));

            report.RootElement.GetProperty("Metadata").GetProperty("ProjectSource").GetString().Should().Be(
                PinnedProjectUrl);

            AssertCleaned(materializedDirectory());
        }
        finally
        {
            if (reportPath is not null)
                File.Delete(reportPath);

            foreach (var directory in directoriesToClean)
            {
                if (Directory.Exists(directory) && !Directory.EnumerateFileSystemEntries(directory).Any())
                    Directory.Delete(directory);
            }
        }
    }

    [Fact]
    public void RemoteTestCommand_profile_local_default_remains_in_project_directory()
    {
        var directory = Path.GetFullPath(".pine-profile-output-test-" + Guid.NewGuid().ToString("N"));
        var (console, output) = CreateConsole();
        console.Profile.Width = 2000;

        try
        {
            TestProfileCommand.Execute(
                directory,
                null,
                new(),
                _ => 0,
                console,
                FormatCommandColorMode.Never).Should().Be(0);

            var reportPath = Directory.GetFiles(Path.Combine(directory, "elm-stuff", "pine", "test-profiles")).Single();
            output.ToString().Should().Contain("Saved JSON report: " + reportPath);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RemoteTestCommand_profile_honors_explicit_output_for_local_and_remote_sources(bool remote)
    {
        var directory = Path.GetFullPath(".pine-profile-output-test-" + Guid.NewGuid().ToString("N"));
        var reportPath = Path.Combine(directory, "profile.json");
        var (console, output) = CreateConsole();
        console.Profile.Width = 2000;

        try
        {
            TestProfileCommand.Execute(
                remote ? PinnedProjectUrl : directory,
                null,
                new() { OutputPath = reportPath },
                _ => 0,
                console,
                FormatCommandColorMode.Never).Should().Be(0);

            File.Exists(reportPath).Should().BeTrue();
            output.ToString().Should().Contain("Saved JSON report: " + reportPath);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    private static void AssertList(string output)
    {
        var list = output[output.IndexOf("Available tests", StringComparison.Ordinal)..].Trim();

        list.ReplaceLineEndings("\n").Should().Be(
            ExpectedList.ReplaceLineEndings("\n"));

        output.Should().NotContain("helperValue").And.NotContain("private test")
            .And.NotContain("Running").And.NotContain("TEST RUN PASSED");
    }

    private static Dictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> FixtureFiles() =>
        Directory.GetFiles(FixtureDirectory(), "*", SearchOption.AllDirectories)
        .ToDictionary(
            path => (IReadOnlyList<string>)Path.GetRelativePath(FixtureDirectory(), path)
            .Split(Path.DirectorySeparatorChar),
            path => (ReadOnlyMemory<byte>)File.ReadAllBytes(path));

    private static string FixtureDirectory()
    {
        var directory = new DirectoryInfo(Environment.CurrentDirectory);

        while (directory is not null)
        {
            var candidate =
                Path.Combine(
                    directory.FullName,
                    "implement",
                    "Pine.Core.Tests",
                    "TestData",
                    "Elm",
                    "CommandElmTest",
                    "multiple-top-level-tests",
                    "input-app");

            if (Directory.Exists(candidate))
                return candidate;

            directory = directory.Parent;
        }

        throw new DirectoryNotFoundException("Could not locate multiple-top-level-tests fixture.");
    }

    private static Func<string?> CaptureMaterializedDirectory(RecordingWriter output)
    {
        var existing =
            Directory.GetDirectories(Path.GetTempPath(), "pine-elm-test-source-*").ToHashSet(StringComparer.Ordinal);

        string? path = null;

        output.OnFlush =
            () =>
            {
                if (path is null && output.ToString().Contains("Loaded ", StringComparison.Ordinal))
                {
                    path =
                        Directory.GetDirectories(Path.GetTempPath(), "pine-elm-test-source-*")
                        .Single(directory => !existing.Contains(directory));

                    Directory.Exists(Path.Combine(path, "project")).Should().BeTrue();
                }
            };

        return () => path;
    }

    private static void AssertCleaned(string? path)
    {
        path.Should().NotBeNull("the remote project was materialized before compilation");
        Directory.Exists(path).Should().BeFalse("remote project files are removed on every exit path");
    }

    private static (IAnsiConsole console, RecordingWriter output) CreateConsole()
    {
        var output = new RecordingWriter();

        return
            (AnsiConsole.Create(
                new AnsiConsoleSettings
                {
                    Ansi = AnsiSupport.No,
                    ColorSystem = ColorSystemSupport.Standard,
                    Interactive = InteractionSupport.No,
                    Out = new AnsiConsoleOutput(output),
                }),
            output);
    }

    private sealed class CancelledPackageProvider : IElmPackageProvider
    {
        public Task<ElmPackageVersionListing> GetVersionsAsync(string packageName, CancellationToken cancellationToken) =>
            throw new OperationCanceledException();

        public Task<ElmPackageMetadata> GetMetadataAsync(
            ElmPackageIdentity identity,
            CancellationToken cancellationToken) =>
            throw new OperationCanceledException();

        public Task<FileTree> GetSourcesAsync(ElmPackageIdentity identity, CancellationToken cancellationToken) =>
            throw new OperationCanceledException();
    }

    private sealed class RecordingWriter : StringWriter
    {
        public int FlushCount { get; private set; }

        public Action? OnFlush { get; set; }

        public override void Flush()
        {
            ++FlushCount;
            OnFlush?.Invoke();
            base.Flush();
        }
    }
}
