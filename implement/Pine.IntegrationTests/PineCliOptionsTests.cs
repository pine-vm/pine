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
        result.StandardError.Should().BeEmpty();
    }


    [Fact]
    public void Elm_command_exposes_test_subcommand()
    {
        var result = RunPine("elm", "--help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("test");
        result.StandardError.Should().BeEmpty();
    }


    [Fact]
    public void Elm_test_command_exposes_filter_option()
    {
        var result = RunPine("elm", "test", "--help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("--filter");
        result.StandardError.Should().BeEmpty();
    }


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
