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

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Install_on_unix_uses_user_bin_and_explains_path_setup(bool binOnPath)
    {
        if (OperatingSystem.IsWindows())
            return;

        var homeDirectory = CreateFormatTestDirectory();
        var sourceExecutable = Path.Combine(AppContext.BaseDirectory, "pine");
        var installDirectory = Path.Combine(homeDirectory, ".local", "bin");
        var installedExecutable = Path.Combine(installDirectory, "pine");

        var path =
            binOnPath
            ?
            installDirectory + Path.PathSeparator + Environment.GetEnvironmentVariable("PATH")
            :
            Environment.GetEnvironmentVariable("PATH") ?? "";

        try
        {
            var result = RunPineProcess(sourceExecutable, homeDirectory, path, "install");

            result.ExitCode.Should().Be(0);
            result.StandardError.Should().BeEmpty();
            result.StandardOutput.Should().Contain(installedExecutable);
            File.ReadAllBytes(installedExecutable).Should().Equal(File.ReadAllBytes(sourceExecutable));
            File.GetUnixFileMode(installedExecutable).Should().HaveFlag(UnixFileMode.UserExecute);

            if (binOnPath)
                result.StandardOutput.Should().Contain("new terminal instances");

            else
                result.StandardOutput.Should().Contain("export PATH=\"$HOME/.local/bin:$PATH\"");

            // The test project's apphost needs its companion assemblies; the published Pine executable is single-file.
            var installedResult = RunPineProcess(sourceExecutable, homeDirectory, path, "install");

            installedResult.ExitCode.Should().Be(0);
            installedResult.StandardOutput.Should().Contain("already installed");
            installedResult.StandardError.Should().BeEmpty();

            var helpResult = RunPineProcess(sourceExecutable, homeDirectory, path, "help");

            helpResult.ExitCode.Should().Be(0);

            if (binOnPath)
                helpResult.StandardOutput.Should().NotContain("Set up your development environment:");

            else
                helpResult.StandardOutput.Should().Contain("Set up your development environment:");
        }
        finally
        {
            Directory.Delete(homeDirectory, recursive: true);
        }
    }

    [Fact]
    public void Install_on_unix_reports_unwritable_destination_without_a_stack_trace()
    {
        if (OperatingSystem.IsWindows())
            return;

        var homeDirectory = CreateFormatTestDirectory();

        try
        {
            File.WriteAllText(Path.Combine(homeDirectory, ".local"), "not a directory");

            var result =
                RunPineProcess(
                    Path.Combine(AppContext.BaseDirectory, "pine"),
                    homeDirectory,
                    Environment.GetEnvironmentVariable("PATH"),
                    "install");

            result.ExitCode.Should().Be(1);
            result.StandardError.Should().Contain("Installation failed:");
            result.StandardError.Should().NotContain("Unhandled exception");
        }
        finally
        {
            Directory.Delete(homeDirectory, recursive: true);
        }
    }

    [Fact]
    public void Missing_home_does_not_break_help_and_reports_an_install_error()
    {
        if (OperatingSystem.IsWindows())
            return;

        var executable = Path.Combine(AppContext.BaseDirectory, "pine");
        var helpResult = RunPineProcess(executable, "", null, "help");
        var installResult = RunPineProcess(executable, "", null, "install");

        helpResult.ExitCode.Should().Be(0);
        helpResult.StandardOutput.Should().Contain("install");
        installResult.ExitCode.Should().Be(1);
        installResult.StandardError.Should().Contain("HOME must be set to an absolute path");
    }

    [Fact]
    public void Lowercase_short_verbose_option_is_available_to_subcommands()
    {
        var result = RunPine("help", "-v");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("Usage: pine [command] [options]");
        result.StandardError.Should().BeEmpty();
    }


    [Theory]
    [InlineData()]
    [InlineData("help")]
    public void Default_help_lists_elm_first_and_hides_phased_out_commands(params string[] arguments)
    {
        var result = RunPine(arguments);

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("elm                            Elm development tools.");
        result.StandardOutput.Should().MatchRegex(@"Develop and learn:\s+elm\s+Elm development tools\.");
        result.StandardOutput.Should().Contain("interactive                    Alias for 'elm interactive'.");
        result.StandardOutput.Should().NotContain("elm-test-rs");
        result.StandardOutput.Should().NotContain("make");
        result.StandardOutput.Should().NotContain("elm-format");
        result.StandardError.Should().BeEmpty();
    }


    [Fact]
    public void Elm_command_exposes_interactive_make_format_and_test_subcommands()
    {
        var result = RunPine("elm", "--help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("interactive");
        result.StandardOutput.Should().Contain("make");
        result.StandardOutput.Should().Contain("format");
        result.StandardOutput.Should().Contain("test");
        result.StandardError.Should().BeEmpty();
    }


    [Fact]
    public void Root_help_hides_phased_out_and_backward_compatible_commands()
    {
        var result = RunPine("--help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("elm");
        result.StandardOutput.Should().Contain("Alias for 'elm interactive'.");
        result.StandardOutput.Should().NotContain("elm-test-rs");
        result.StandardOutput.Should().NotContain("make");
        result.StandardOutput.Should().NotContain("elm-format");
        result.StandardError.Should().BeEmpty();
    }

    [Theory]
    [InlineData("-a")]
    [InlineData("--all")]
    public void All_commands_help_lists_hidden_legacy_commands(string allOption)
    {
        var result = RunPine("help", allOption);

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("elm-test-rs");
        result.StandardOutput.Should().Contain("make");
        result.StandardOutput.Should().Contain("elm-format");
        result.StandardError.Should().BeEmpty();
    }

    [Theory]
    [InlineData("elm", "interactive")]
    [InlineData("interactive")]
    [InlineData("elm", "repl")]
    [InlineData("repl")]
    public void Elm_interactive_and_aliases_expose_the_same_options_and_subcommands(params string[] command)
    {
        var result = RunPine([.. command, "--help"]);

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("--context-app");
        result.StandardOutput.Should().Contain("--init-steps");
        result.StandardOutput.Should().Contain("--submit");
        result.StandardOutput.Should().Contain("--elm-engine");
        result.StandardOutput.Should().Contain("--save-to-file");
        result.StandardOutput.Should().Contain("test");
        result.StandardError.Should().BeEmpty();

        var testHelp = RunPine([.. command, "test", "--help"]);

        testHelp.ExitCode.Should().Be(0);
        testHelp.StandardOutput.Should().Contain("--scenario");
        testHelp.StandardOutput.Should().Contain("--scenarios");
        testHelp.StandardError.Should().BeEmpty();
    }

    [Fact]
    public void Elm_make_exposes_arguments_and_options()
    {
        var result = RunPine("elm", "make", "--help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("path-to-elm-file");
        result.StandardOutput.Should().Contain("--output");
        result.StandardOutput.Should().Contain("--input-directory");
        result.StandardOutput.Should().Contain("--debug");
        result.StandardOutput.Should().Contain("--optimize");
        result.StandardError.Should().BeEmpty();
    }

    [Theory]
    [InlineData("elm", "make")]
    [InlineData("make")]
    public void Elm_make_and_legacy_command_accept_options_and_require_an_entry_point(params string[] command)
    {
        var result =
            RunPine(
                [
                .. command,
                "--output", "unused",
                "--input-directory", "unused",
                "--debug", "--optimize", "--help"
                ]);

        result.ExitCode.Should().Be(0);
        result.StandardError.Should().BeEmpty();

        var missingArgumentResult = RunPine(command);

        missingArgumentResult.ExitCode.Should().Be(1);
        missingArgumentResult.StandardError.Should().Contain("Required argument missing for command: 'make'");
    }

    [Theory]
    [InlineData("elm", "make")]
    [InlineData("make")]
    public void Elm_make_and_legacy_command_execute_the_same_handler(params string[] command)
    {
        var directory = CreateFormatTestDirectory();

        try
        {
            var result = RunPine([.. command, "src/Main.elm", "--input-directory", directory]);

            result.ExitCode.Should().Be(10);
            result.StandardOutput.Should().Contain("Did not find elm.json file in that directory.");
            result.StandardError.Should().BeEmpty();
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void Hidden_elm_test_rs_command_remains_available()
    {
        var result = RunPine("elm-test-rs", "--elm-test-rs-output", "unused", "--help");

        result.ExitCode.Should().Be(0);
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
    public void Elm_test_command_exposes_filter_list_workers_and_duration_options()
    {
        var result = RunPine("elm", "test", "--help");

        result.ExitCode.Should().Be(0);
        result.StandardOutput.Should().Contain("--filter");
        result.StandardOutput.Should().Contain("--list-tests");
        result.StandardOutput.Should().Contain("--workers");
        result.StandardOutput.Should().Contain("--report-durations");
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

    private static ProcessResult RunPine(params string[] arguments) =>
        RunPineProcess(
            Path.Combine(AppContext.BaseDirectory, OperatingSystem.IsWindows() ? "pine.exe" : "pine"),
            homeDirectory: null,
            pathEnvironment: null,
            arguments);

    private static ProcessResult RunPineProcess(
        string executablePath,
        string? homeDirectory,
        string? pathEnvironment,
        params string[] arguments)
    {
        var startInfo =
            new ProcessStartInfo(executablePath)
            {
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                CreateNoWindow = true
            };

        if (homeDirectory is not null)
            startInfo.Environment["HOME"] = homeDirectory;

        if (pathEnvironment is not null)
            startInfo.Environment["PATH"] = pathEnvironment;

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
