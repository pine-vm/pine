using AwesomeAssertions;
using Pine.CLI;
using Pine.CLI.Elm;
using Spectre.Console;
using System;
using System.IO;
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
}
