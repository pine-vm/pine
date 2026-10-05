using AwesomeAssertions;
using Pine.CLI;
using Pine.CLI.Elm;
using Spectre.Console;
using System;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests.CLI;

public class ElmTestCommandTests
{
    [Fact]
    public void Success_output_uses_elm_test_rs_colors()
    {
        var projectDirectory = CreateTestProject(PassingTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Always,
                    console: console);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("\u001b[4;32mTEST RUN PASSED");
            output.ToString().Should().Contain("\u001b[2mPassed:   ");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Workers_option_runs_tests_in_parallel()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    workers: 2,
                    reportDurations: true);

            var rendered = output.ToString();
            var runningMessageIndex = rendered.IndexOf("Running 3 tests", StringComparison.Ordinal);

            exitCode.Should().Be(1);
            runningMessageIndex.Should().BeGreaterThanOrEqualTo(0);
            rendered.LastIndexOf("Running 3 tests", StringComparison.Ordinal).Should().Be(runningMessageIndex);
            runningMessageIndex.Should().BeLessThan(rendered.IndexOf("TEST RUN", StringComparison.Ordinal));
            rendered.Should().Contain("Duration:");
            rendered.Should().Contain("Compilation:");
            rendered.Should().Contain("Test execution:");
            rendered.Should().Contain("Passed:   2");
            rendered.Should().Contain("Failed:   1");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Workers_option_rejects_values_smaller_than_one()
    {
        var projectDirectory = CreateTestProject(PassingTestsModule);
        var (errorConsole, errorOutput) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    errorConsole: errorConsole,
                    workers: 0);

            exitCode.Should().Be(1);
            errorOutput.ToString().Should().Contain("The --workers value must be at least 1.");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Failure_output_uses_elm_test_rs_colors()
    {
        var projectDirectory = CreateTestProject(FailingTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Always,
                    console: console);

            var rendered = output.ToString();

            exitCode.Should().Be(1);
            rendered.Should().Contain("\u001b[2m↓ Group Title");
            rendered.Should().Contain("\u001b[91m✗ Another Test Title");
            rendered.Should().Contain("\u001b[7m1\u001b[0m");
            rendered.Should().Contain("\u001b[7m3\u001b[0m");
            rendered.Should().Contain("\u001b[4;91mTEST RUN FAILED");
            rendered.Should().Contain("\u001b[2mPassed:   ");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Color_never_emits_plain_text()
    {
        var projectDirectory = CreateTestProject(FailingTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console);

            exitCode.Should().Be(1);
            output.ToString().Contains('\u001b').Should().BeFalse();
            output.ToString().Should().Contain("TEST RUN FAILED");
            output.ToString().Should().Contain("Passed:   2");
            output.ToString().Should().Contain("Failed:   1");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData("SELECTED GROUP", 0, 2, 2, 0)]
    [InlineData("UNIQUE TEST", 1, 1, 0, 1)]
    public void Filter_is_case_insensitive_and_matches_test_or_group_name(
        string filter,
        int expectedExitCode,
        int expectedTestCount,
        int expectedPassedCount,
        int expectedFailedCount)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: filter);

            exitCode.Should().Be(expectedExitCode);
            output.ToString().Should().Contain($"Running {expectedTestCount} test");
            output.ToString().Should().Contain($"Passed:   {expectedPassedCount}");
            output.ToString().Should().Contain($"Failed:   {expectedFailedCount}");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData(null, 3, "First", "Unique Test")]
    [InlineData("SELECTED GROUP", 2, "First", null)]
    [InlineData("unique test", 1, null, "Unique Test")]
    [InlineData("tests/tests.elm", 3, "First", "Unique Test")]
    [InlineData(@"tests\Tests\Root\Selected Group\First", 1, "First", null)]
    [InlineData("Tests/**/First", 1, "First", null)]
    [InlineData("Tests/*/Selected*/First", 1, "First", null)]
    [InlineData("Root/Selected Group", 2, "First", null)]
    public void List_tests_includes_metadata_and_applies_filter(
        string? filter,
        int expectedTestCount,
        string? expectedFirstName,
        string? expectedUniqueName)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: filter,
                    listTests: true);

            var rendered = output.ToString();

            exitCode.Should().Be(0);
            rendered.Should().Contain(
                filter is null
                ?
                $"Available tests ({expectedTestCount})"
                :
                $"Tests remaining after filtering ({expectedTestCount})");
            rendered.Should().Contain("tests/Tests.elm");
            rendered.Should().Contain("Root");
            rendered.Should().Contain("└──");
            rendered.Contains('\u001b').Should().BeFalse();

            if (expectedFirstName is null)
                rendered.Should().NotContain("First");

            else
                rendered.Should().Contain(expectedFirstName);

            if (expectedUniqueName is null)
                rendered.Should().NotContain("Unique Test");

            else
                rendered.Should().Contain(expectedUniqueName);
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Path_filter_selects_individual_test_for_running_and_listing(bool listTests)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "tests/Tests/Root/Selected Group/*irst",
                    listTests: listTests);

            var rendered = output.ToString();

            exitCode.Should().Be(0);
            rendered.Should().NotContain("Second").And.NotContain("Unique Test");

            if (listTests)
            {
                rendered.Should().Contain("Tests remaining after filtering (1)").And.Contain("First");
                rendered.Should().NotContain("Running").And.NotContain("TEST RUN");
            }
            else
            {
                rendered.Should().Contain("Running 1 test.").And.Contain("Passed:   1");
            }
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void No_matches_prints_closest_paths_and_guidance_without_running(bool listTests)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "tests/Tests/**/Selected Group/Firstt",
                    listTests: listTests);

            var rendered = output.ToString();

            exitCode.Should().Be(1);
            rendered.Should().Contain("No tests matched the filter expression:");
            rendered.Should().Contain("tests/Tests/**/Selected Group/Firstt");
            rendered.Should().Contain("Closest existing test paths (not selected or run):");
            rendered.Should().Contain("tests/Tests.elm/Root/Selected Group/First");
            rendered.Should().Contain("broaden").And.Contain("**").And.Contain("--list-tests").And.Contain("--help");
            rendered.Should().NotContain("Running").And.NotContain("TEST RUN PASSED");
            rendered.Contains('\u001b').Should().BeFalse();

            if (listTests)
            {
                var treeOutput = rendered[..rendered.IndexOf("No tests matched", StringComparison.Ordinal)];
                treeOutput.Should().Contain("Tests remaining after filtering (0)");
                treeOutput.Should().Contain("tests/Tests.elm").And.Contain("Selected Group").And.Contain("Other Group");
                treeOutput.Should().Contain("3 tests filtered out").And.Contain("2 tests filtered out").And.Contain("1 test filtered out");
                treeOutput.Should().NotContain("First").And.NotContain("Second").And.NotContain("Unique Test");
            }
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void No_match_suggestions_preserve_full_long_paths_in_redirected_output()
    {
        var testName = new string('a', 160);
        var projectDirectory = CreateTestProject(PassingTestsModule.Replace("Test Title", testName));
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "missing test");

            exitCode.Should().Be(1);
            output.ToString().Should().Contain("tests/Tests.elm/Group Title/" + testName);
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Command_accepts_one_filter_expression()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var arguments =
                new[] { projectDirectory, "--color", "never", "--list-tests", "--filter", "Tests/**/First" };

            var command = TestCommand.Create();
            var option = command.Options.OfType<Option<string>>().Single();
            var parseResult = command.Parse(arguments);

            parseResult.Errors.Should().BeEmpty();
            parseResult.GetValue(option).Should().Be("Tests/**/First");

            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: parseResult.GetValue(option),
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("Tests remaining after filtering (1)").And.Contain("First");
            output.ToString().Should().NotContain("Second").And.NotContain("Unique Test");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filter_arity_requires_one_value()
    {
        var command = TestCommand.Create();
        var option = command.Options.OfType<Option<string>>().Single();
        var parseResult = command.Parse(["--filter"]);

        option.Arity.Should().Be(ArgumentArity.ExactlyOne);
        parseResult.Errors.Should().NotBeEmpty();
    }


    [Theory]
    [InlineData("--filter", "First", "--filter", "Second")]
    [InlineData("--filter", "First", "--filter", "First")]
    [InlineData("--filter=First", "--filter=Second")]
    [InlineData("--filter", "First", "--filter")]
    public void Command_rejects_repeated_filter_options(params string[] arguments)
    {
        var parseResult = TestCommand.Create().Parse(arguments);

        parseResult.Errors.Should().NotBeEmpty();
    }


    [Fact]
    public void Command_accepts_no_filter_option()
    {
        var command = TestCommand.Create();
        var option = command.Options.OfType<Option<string>>().Single();
        var parseResult = command.Parse([]);

        parseResult.Errors.Should().BeEmpty();
        parseResult.GetValue(option).Should().BeNull();
    }


    [Fact]
    public void Filter_arity_rejects_multiple_values_per_occurrence()
    {
        TestCommand.Create().Parse(["project", "--filter", "First", "Second"])
            .Errors.Should().NotBeEmpty();
    }


    [Fact]
    public void Help_explains_filter_paths_and_wildcards()
    {
        var output = new StringWriter();
        var root = new RootCommand();
        root.Add(TestCommand.Create());
        var exitCode =
            root.Parse(["test", "--help"])
            .Invoke(new InvocationConfiguration { Output = output, Error = output });

        exitCode.Should().Be(0);
        var rendered = output.ToString();
        rendered.Should().Contain("--filter <EXPRESSION>");
        rendered.Should().Contain("Run tests matching the expression.").And.Contain("**").And.Contain("Examples:");
        rendered.Should().Contain("case-insensitive").And.Contain("extension may be omitted");
    }


    [Fact]
    public void Filter_matches_nested_file_paths_relative_to_discovery_root()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var nestedDirectory = Path.Combine(projectDirectory, "tests", "nested");
        Directory.CreateDirectory(nestedDirectory);
        File.WriteAllText(
            Path.Combine(nestedDirectory, "NestedTests.elm"),
            """
            module Nested.NestedTests exposing (suite)
            import Expect
            import Test exposing (Test)
            suite : Test
            suite = Test.test "Nested test" <| \_ -> Expect.pass
            """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: @"tests\nested\NestedTests\Nested test",
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("Tests remaining after filtering (1)");
            output.ToString().Should().Contain("tests/nested/NestedTests.elm").And.Contain("Nested test");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filtered_empty_groups_report_no_individual_tests()
    {
        var projectDirectory = CreateTestProject(EmptyGroupTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "Empty group");

            exitCode.Should().Be(1);
            output.ToString().Should().Contain("No individual tests were discovered.");
            output.ToString().Should().NotContain("Closest existing").And.NotContain("Running");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filter_includes_matches_from_both_file_paths_and_test_descriptions()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var testsDirectory = Path.Combine(projectDirectory, "tests");

        File.WriteAllText(
            Path.Combine(testsDirectory, "SelectedFile.elm"),
            """
            module SelectedFile exposing (suite)

            import Expect
            import Test exposing (Test)

            suite : Test
            suite =
                Test.test "File path match" <|
                    \_ ->
                        Expect.pass
            """,
            new UTF8Encoding(encoderShouldEmitUTF8Identifier: false));

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "selected",
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("Tests remaining after filtering (3)");
            output.ToString().Should().Contain("tests/SelectedFile.elm");
            output.ToString().Should().Contain("Selected Group");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filtered_listing_counts_exclusions_in_every_file_and_group()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        File.WriteAllText(
            Path.Combine(projectDirectory, "tests", "ExcludedFile.elm"),
            """
            module ExcludedFile exposing (suite)

            import Expect
            import Test exposing (Test)

            suite : Test
            suite =
                Test.describe "Excluded File Group"
                    [ Test.test "Hidden file test" <| \_ -> Expect.pass ]
            """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "Tests/**/First",
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Replace("\r\n", "\n").Trim().Should().Be(
                """
                Tests remaining after filtering (1)
                ├── tests/ExcludedFile.elm
                │   ├── Excluded File Group
                │   │   └── 1 test filtered out
                │   └── 1 test filtered out
                └── tests/Tests.elm
                    ├── Root
                    │   ├── Other Group
                    │   │   └── 1 test filtered out
                    │   ├── Selected Group
                    │   │   ├── First
                    │   │   └── 1 test filtered out
                    │   └── 2 tests filtered out
                    └── 2 tests filtered out
                """);
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData(null, "Available tests (3)")]
    [InlineData("*", "Tests remaining after filtering (3)")]
    public void Listing_does_not_report_exclusions_when_all_tests_are_selected(
        string? filter,
        string expectedHeading)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: filter,
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain(expectedHeading).And.NotContain("filtered out");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filtered_listing_styles_exclusion_counts_as_dim_text()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Always,
                    console: console,
                    filter: "Tests/**/First",
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("\u001b[1mTests remaining after filtering (1)");
            output.ToString().Should().Contain("\u001b[2m1 test filtered out");
            output.ToString().Should().Contain("\u001b[2m2 tests filtered out");
            output.ToString().Should().NotContain("Second").And.NotContain("Unique Test");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void List_tests_uses_colors_when_enabled()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Always,
                    console: console,
                    listTests: true);

            var rendered = output.ToString();

            exitCode.Should().Be(0);
            rendered.Should().Contain("\u001b[1mAvailable tests (3)");
            rendered.Should().Contain("\u001b[93mRoot");
            rendered.Should().Contain("\u001b[32mFirst");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void List_tests_does_not_count_empty_descriptions_as_tests()
    {
        var projectDirectory = CreateTestProject(EmptyGroupTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("Available tests (0)");
            output.ToString().Should().NotContain("Empty group");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void No_Elm_test_modules_reports_error()
    {
        var projectDirectory =
            Path.Combine(
                Path.GetTempPath(),
                "pine-elm-test-command-tests",
                Guid.NewGuid().ToString("N") + new string('a', 80));

        Directory.CreateDirectory(projectDirectory);

        var (errorConsole, errorOutput) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    errorConsole: errorConsole);

            exitCode.Should().Be(1);
            errorOutput.ToString().Should().Contain("Error:");
            errorOutput.ToString().Should().Contain("Did not find Elm test modules");
            errorOutput.ToString().Should().Contain(projectDirectory);
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Unexpected_exception_from_Elm_test_runner_propagates()
    {
        var projectDirectory = CreateTestProject("This is not a valid Elm module.");

        try
        {
            Action execute =
                () =>
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never);

            execute.Should().Throw<InvalidOperationException>()
                .WithMessage("Failed parsing Elm test module:*");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    private static string CreateTestProject(string testsModule)
    {
        var projectDirectory =
            Path.Combine(
                Path.GetTempPath(),
                "pine-elm-test-command-tests",
                Guid.NewGuid().ToString("N"));

        Directory.CreateDirectory(Path.Combine(projectDirectory, "tests"));

        File.WriteAllText(
            Path.Combine(projectDirectory, "elm.json"),
            ElmJson,
            new UTF8Encoding(encoderShouldEmitUTF8Identifier: false));

        File.WriteAllText(
            Path.Combine(projectDirectory, "tests", "Tests.elm"),
            testsModule,
            new UTF8Encoding(encoderShouldEmitUTF8Identifier: false));

        return projectDirectory;
    }


    private static (IAnsiConsole console, StringWriter output) CreateConsole(
        AnsiSupport ansi)
    {
        var output = new StringWriter();

        var console =
            AnsiConsole.Create(
                new AnsiConsoleSettings
                {
                    Ansi = ansi,
                    ColorSystem = ColorSystemSupport.Standard,
                    Interactive = InteractionSupport.No,
                    Out = new AnsiConsoleOutput(output),
                });

        return (console, output);
    }


    private const string ElmJson =
        """
        {
            "type": "application",
            "source-directories": ["src"],
            "elm-version": "0.19.1",
            "dependencies": {
                "direct": {
                    "elm/core": "1.0.5"
                },
                "indirect": {}
            },
            "test-dependencies": {
                "direct": {
                    "elm-explorations/test": "2.2.0"
                },
                "indirect": {
                    "elm/bytes": "1.0.8",
                    "elm/json": "1.1.4",
                    "elm/random": "1.0.0"
                }
            }
        }
        """;


    private const string FailingTestsModule =
        """
        module Tests exposing (..)

        import Expect
        import Test exposing (Test)


        suite : Test
        suite =
            Test.describe
                "Group Title"
                [ Test.test "Test Title" <|
                    \_ ->
                        71 |> Expect.equal 71
                , Test.test "Another Test Title" <|
                    \_ ->
                        41 |> Expect.equal 43
                , Test.test "Yet Another Test Title" <|
                    \_ ->
                        21 |> Expect.equal 21
                ]
        """;


    private const string PassingTestsModule =
        """
        module Tests exposing (..)

        import Expect
        import Test exposing (Test)


        suite : Test
        suite =
            Test.describe "Group Title"
                [ Test.test "Test Title" <|
                    \_ ->
                        71 |> Expect.equal 71
                ]
        """;


    private const string EmptyGroupTestsModule =
        """
        module Tests exposing (..)

        import Test exposing (Test)


        suite : Test
        suite =
            Test.describe "Empty group" []
        """;


    private const string FilterTestsModule =
        """
        module Tests exposing (..)

        import Expect
        import Test exposing (Test)


        suite : Test
        suite =
            Test.describe "Root"
                [ Test.describe "Selected Group"
                    [ Test.test "First" <|
                        \_ ->
                            1 |> Expect.equal 1
                    , Test.test "Second" <|
                        \_ ->
                            2 |> Expect.equal 2
                    ]
                , Test.describe "Other Group"
                    [ Test.test "Unique Test" <|
                        \_ ->
                            3 |> Expect.equal 4
                    ]
                ]
        """;
}
