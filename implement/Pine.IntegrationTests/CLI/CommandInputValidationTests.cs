using AwesomeAssertions;
using Pine.CLI;
using System.Collections.Generic;
using System.CommandLine;
using System.CommandLine.Parsing;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.CLI;

public class CommandInputValidationTests
{
    [Fact]
    public void Required_positional_arguments_are_validated_by_the_parser()
    {
        foreach (var testCase in RequiredArgumentCases())
        {
            var parseResult = testCase.Command.Parse([]);

            parseResult.Errors
                .Select(error => (error.SymbolResult as ArgumentResult)?.Argument.Name)
                .Should()
                .BeEquivalentTo(testCase.RequiredArgumentNames);

            testCase.Command.Parse(testCase.ValidArguments).Errors.Should().BeEmpty();
        }
    }

    [Fact]
    public void Run_cache_server_requires_file_cache_directory()
    {
        var command = RunCacheServerCommand.Create();

        command.Parse([]).Errors
            .Select(error => error.Message)
            .Should()
            .ContainSingle(message => message.Contains("'--file-cache-directory'"));

        command.Parse(["--file-cache-directory", "cache"]).Errors.Should().BeEmpty();
    }

    [Fact]
    public void Compile_interactive_env_requires_at_least_one_environment_source()
    {
        var command = CompileInteractiveEnvCommand.Create();

        command.Parse([]).Errors
            .Select(error => error.Message)
            .Should()
            .ContainSingle(message => message.Contains("'--env-source'"));

        command.Parse(["--env-source", "source.zip"]).Errors.Should().BeEmpty();
    }

    [Theory]
    [InlineData("8_080", 8080)]
    [InlineData("1__234", 1234)]
    public void File_server_port_accepts_integer_digit_separators(string input, int expected)
    {
        var command = RunFileServerCommand.Create();
        var parsed = command.Parse(["--port", input]);

        parsed.Errors.Should().BeEmpty();

        parsed.GetValue(command.Options.OfType<Option<int?>>().Single(option => option.Name is "--port"))
            .Should().Be(expected);
    }

    [Theory]
    [InlineData("_8080")]
    [InlineData("8080_")]
    [InlineData("8k")]
    [InlineData("8 080")]
    [InlineData("2147483648")]
    public void File_server_port_rejects_invalid_integer_inputs(string input)
    {
        RunFileServerCommand.Create().Parse(["--port", input]).Errors.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("--quality", "1__0", 10)]
    [InlineData("--viewport-width", "1_280", 1280)]
    [InlineData("--viewport-height", "7_20", 720)]
    [InlineData("--screen-width", "1_920", 1920)]
    [InlineData("--screen-height", "1_080", 1080)]
    [InlineData("--viewport-width", "2k", 2000)]
    [InlineData("--screen-width", "2 k", 2000)]
    public void Screenshot_integer_options_accept_digit_separators(string optionName, string input, int expected)
    {
        var command = ScreenshotCommand.Create();
        var parsed = command.Parse(["Main.html", optionName, input]);

        parsed.Errors.Should().BeEmpty();

        parsed.GetValue(command.Options.OfType<Option<int?>>().Single(option => option.Name == optionName))
            .Should().Be(expected);
    }

    [Theory]
    [InlineData("_100")]
    [InlineData("100_")]
    [InlineData("1.5")]
    [InlineData("1K")]
    [InlineData("2147483648")]
    public void Screenshot_integer_options_reject_invalid_inputs(string input)
    {
        foreach (var optionName in new[] { "--quality", "--viewport-width", "--viewport-height", "--screen-width", "--screen-height" })
            ScreenshotCommand.Create().Parse(["Main.html", optionName, input]).Errors.Should().NotBeEmpty();
    }

    [Fact]
    public void Screenshot_quality_does_not_accept_count_units()
    {
        ScreenshotCommand.Create().Parse(["Main.html", "--quality", "1k"]).Errors.Should().NotBeEmpty();
    }

    [Fact]
    public void Numeric_option_parsers_preserve_absent_nullable_values_and_fractional_scale()
    {
        var fileServer = RunFileServerCommand.Create();
        var port = fileServer.Options.OfType<Option<int?>>().Single(option => option.Name is "--port");
        fileServer.Parse([]).GetValue(port).Should().BeNull();

        var screenshot = ScreenshotCommand.Create();
        var parsed = screenshot.Parse(["Main.html", "--device-scale-factor", "1.5"]);

        parsed.Errors.Should().BeEmpty();

        foreach (var option in screenshot.Options.OfType<Option<int?>>())
            parsed.GetValue(option).Should().BeNull();

        parsed.GetValue(screenshot.Options.OfType<Option<float?>>().Single())
            .Should().Be(1.5f);
    }

    private static IEnumerable<RequiredArgumentCase> RequiredArgumentCases()
    {
        var userSecretsStoreCommand =
            UserSecretsCommand.Create().Subcommands.Single(command => command.Name is "store");

        yield return new(userSecretsStoreCommand, ["site", "password"], ["site", "password"]);
        yield return new(TruncateProcessHistoryCommand.Create(), ["process-site"], ["site"]);
        yield return new(ApplyFunctionCommand.Create(), ["process-site", "function-name"], ["site", "function"]);
        yield return new(RunCommand.Create(), ["entry-point-module"], ["Main"]);
        yield return new(MakeCommand.Create(), ["path-to-elm-file"], ["src/Main.elm"]);
        yield return new(ListFunctionsCommand.Create(), ["process-site"], ["site"]);
        yield return new(DeployCommand.Create(), ["source", "process-site"], ["source", "site"]);
        yield return new(DescribeCommand.Create(), ["source-path"], ["source"]);
        yield return new(CopyProcessCommand.Create(), ["process-site"], ["site"]);
        yield return new(CopyAppStateCommand.Create(), ["source"], ["source"]);
        yield return new(CompileCommand.Create(), ["source"], ["source"]);
    }

    private sealed record RequiredArgumentCase(
        Command Command,
        IReadOnlyList<string> RequiredArgumentNames,
        IReadOnlyList<string> ValidArguments);
}
