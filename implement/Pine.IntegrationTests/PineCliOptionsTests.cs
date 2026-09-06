using AwesomeAssertions;
using System;
using System.Diagnostics;
using System.IO;
using Xunit;

namespace Pine.IntegrationTests;

public class PineCliOptionsTests
{
    [Fact]
    public void Uppercase_short_version_option_prints_pine_version()
    {
        var result = RunPine("-V");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Trim().Should().Be("pine " + global::Pine.CLI.PineCliCommand.AppVersionId);
        result.StandardError.Should().BeEmpty();
    }

    [Fact]
    public void Lowercase_short_verbose_option_is_available_to_subcommands()
    {
        var result = RunPine("help", "-v");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("Usage: pine [command] [options]");
        result.StandardError.Should().BeEmpty();
    }


    [Fact]
    public void Help_lists_elm_command()
    {
        var result = RunPine("help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("elm                            Elm development tools.");
        result.StandardOutput.Should().NotContain("elm-format");
        result.StandardError.Should().BeEmpty();
    }


    [Fact]
    public void Elm_command_exposes_format_and_test_subcommands()
    {
        var result = RunPine("elm", "--help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("format");
        result.StandardOutput.Should().Contain("test");
        result.StandardError.Should().BeEmpty();
    }


    [Fact]
    public void Root_help_hides_backward_compatible_elm_format_command()
    {
        var result = RunPine("--help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().NotContain("elm-format");
        result.StandardError.Should().BeEmpty();
    }


    [Theory]
    [InlineData("elm", "format")]
    [InlineData("elm-format")]
    public void Elm_format_commands_are_available(params string[] command)
    {
        var sourcePath =
            Path.Combine(
                Path.GetTempPath(),
                "pine-cli-options-tests-" + Guid.NewGuid().ToString("N") + ".elm");

        try
        {
            File.WriteAllText(
                sourcePath,
                """
                module Main exposing (main)

                main=0
                """);

            var result = RunPine([.. command, sourcePath, "--yes", "--color", "never"]);

            result.ExitCode.Should().Be(0);
            result.StandardError.Should().BeEmpty();
        }
        finally
        {
            File.Delete(sourcePath);
        }
    }

    [Fact]
    public void Elm_test_command_exposes_filter_list_and_workers_options()
    {
        var result = RunPine("elm", "test", "--help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("--filter");
        result.StandardOutput.Should().Contain("--list-tests");
        result.StandardOutput.Should().Contain("--workers");
        result.StandardError.Should().BeEmpty();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Elm_format_reports_fatal_structured_errors(bool verifyNoChanges)
    {
        var directory = CreateFormatTestDirectory();
        var sourcePath = Path.Combine(directory, "Fatal.elm");
        const string Source = "\n{- unclosed\n";

        try
        {
            File.WriteAllText(sourcePath, Source);

            var result = RunElmFormat(directory, verifyNoChanges);

            result.ExitCode.Should().Be(200);
            result.StandardError.Should().BeEmpty();
            result.StandardOutput.Should().Contain(sourcePath);
            result.StandardOutput.Should().Contain(
                "2:1: I cannot find the end of this multi-line comment:\n\n" +
                "2| {- unclosed\n   ^^\nAdd a -} somewhere after this to end the comment.");
            File.ReadAllText(sourcePath).Should().Be(Source);
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Elm_format_reports_recovered_structured_errors_and_keeps_partial_formatting(bool verifyNoChanges)
    {
        var directory = CreateFormatTestDirectory();
        var sourcePath = Path.Combine(directory, "Recovered.elm");
        const string Source = "module Main exposing (..)\n\nfirst = 903.\n\nvalid=1\n\n{-| documentation -}\n";

        try
        {
            File.WriteAllText(sourcePath, Source);

            var result = RunElmFormat(directory, verifyNoChanges);

            result.ExitCode.Should().Be(verifyNoChanges ? 200 : 0);
            result.StandardError.Should().BeEmpty();
            result.StandardOutput.Should().Contain(sourcePath);
            result.StandardOutput.Should().Contain("SYNTAX ERRORS (2)");
            result.StandardOutput.Should().Contain(
                "3:12: Numbers cannot end with a dot like this:\n\n" +
                "3| first = 903.\n              ^\nSwitching to 903 or 903.0 will work though!");
            result.StandardOutput.Should().Contain(
                "8:1: I am trying to parse a declaration, but I am getting stuck here:\n\n8| \n   ^");
            result.StandardOutput.IndexOf("3:12:", StringComparison.Ordinal)
                .Should().BeLessThan(result.StandardOutput.IndexOf("8:1:", StringComparison.Ordinal));

            if (verifyNoChanges)
            {
                File.ReadAllText(sourcePath).Should().Be(Source);
            }
            else
            {
                var formatted = File.ReadAllText(sourcePath);
                formatted.Should().Contain("valid =\n    1");
                formatted.Should().Contain("first = 903.");
                formatted.Should().Contain("{-| documentation -}");

                var stableResult = RunElmFormat(directory, verifyNoChanges: false);
                stableResult.ExitCode.Should().Be(200);
                stableResult.StandardError.Should().BeEmpty();
                stableResult.StandardOutput.Should().Contain("SYNTAX ERRORS (2)");
                stableResult.StandardOutput.Should().Contain("Switching to 903 or 903.0 will work though!");
                File.ReadAllText(sourcePath).Should().Be(formatted);
            }
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Elm_format_reports_every_file_when_fatal_and_recovered_errors_coexist(bool verifyNoChanges)
    {
        var directory = CreateFormatTestDirectory();
        var fatalPath = Path.Combine(directory, "Fatal.elm");
        var recoveredPath = Path.Combine(directory, "Recovered.elm");
        const string FatalSource = "\n{- unclosed\n";
        const string RecoveredSource = "module Main exposing (..)\n\namount = 903.\n\nvalid=1\n";

        try
        {
            File.WriteAllText(fatalPath, FatalSource);
            File.WriteAllText(recoveredPath, RecoveredSource);

            var result = RunElmFormat(directory, verifyNoChanges);

            result.ExitCode.Should().Be(200);
            result.StandardError.Should().BeEmpty();
            result.StandardOutput.Should().Contain(fatalPath).And.Contain(recoveredPath);
            result.StandardOutput.Should().Contain("2:1: I cannot find the end of this multi-line comment:");
            result.StandardOutput.Should().Contain("3:13: Numbers cannot end with a dot like this:");
            result.StandardOutput.Should().Contain("Switching to 903 or 903.0 will work though!");
            File.ReadAllText(fatalPath).Should().Be(FatalSource);
            File.ReadAllText(recoveredPath).Should().Be(RecoveredSource);
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    private static string CreateFormatTestDirectory()
    {
        var directory =
            Path.GetFullPath(
                Path.Combine("artifacts", "test-files", "elm-format-" + Guid.NewGuid().ToString("N")));

        Directory.CreateDirectory(directory);
        return directory;
    }

    private static ProcessResult RunElmFormat(string path, bool verifyNoChanges) =>
        RunPine("elm", "format", path, verifyNoChanges ? "--verify-no-changes" : "--yes", "--color", "never");

    private static ProcessResult RunPine(params string[] arguments)
    {
        var executableName = OperatingSystem.IsWindows() ? "pine.exe" : "pine";

        var startInfo =
            new ProcessStartInfo(Path.Combine(AppContext.BaseDirectory, executableName))
            {
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                CreateNoWindow = true
            };

        foreach (var argument in arguments)
            startInfo.ArgumentList.Add(argument);

        using var process = Process.Start(startInfo) ?? throw new InvalidOperationException("Failed to start Pine.");

        var standardOutput = process.StandardOutput.ReadToEnd();
        var standardError = process.StandardError.ReadToEnd();

        process.WaitForExit();

        return new ProcessResult(process.ExitCode, standardOutput, standardError);
    }

    private record ProcessResult(int ExitCode, string StandardOutput, string StandardError);
}
