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

    private static IEnumerable<RequiredArgumentCase> RequiredArgumentCases()
    {
        var userSecretsStoreCommand = UserSecretsCommand.Create().Subcommands.Single(command => command.Name is "store");

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
