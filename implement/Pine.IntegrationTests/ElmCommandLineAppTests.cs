using AwesomeAssertions;
using Pine.Core;
using Pine.Elm.Platform;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests;

public class ElmCommandLineAppTests
{
    private static string PathToSimpleAppDirectory => @"./../../../../example-apps/cli-demo";

    public static BlobTreeWithStringPath LoadFromLocalFilesystem(string path) =>
        PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(
            Filesystem.GetAllFilesFromDirectory(path));

    [Fact(Timeout = 1000 * 60 * 10)]
    public void Run_simple_command_line_interface_app()
    {
        var console = (IConsole)StaticConsole.Instance;

        var appFiles = LoadFromLocalFilesystem(PathToSimpleAppDirectory);

        var appConfig =
            CommandLineAppConfig.ConfigFromSourceFilesAndModuleName(
                appFiles,
                ["App"]);

        var mutatingCliApp =
            new MutatingCommandLineApp(
                appConfig,
                environment: new CommandLineAppConfig.CommandLineAppInitEnvironment(
                    CommandLine: "fake command line",
                    EnvironmentVariables: []));

        {
            var outputBatches =
                mutatingCliApp.DequeueStdOut();

            var outputTexts =
                outputBatches
                .Select(outputBatch =>
                    System.Text.Encoding.UTF8.GetString(outputBatch.Span))
                .ToImmutableArray();

            string.Concat(outputTexts).Should().Be("Started with the following command line: fake command line\n");
        }

        {
            mutatingCliApp.EventStdIn(
                System.Text.Encoding.UTF8.GetBytes("Hello"));

            var outputBatches =
                mutatingCliApp.DequeueStdOut();

            var outputTexts =
                outputBatches
                .Select(outputBatch =>
                    System.Text.Encoding.UTF8.GetString(outputBatch.Span))
                .ToImmutableArray();

            string.Concat(outputTexts).Should().Be("Hello");
        }

        {
            mutatingCliApp.EventStdIn(
                System.Text.Encoding.UTF8.GetBytes(" app!"));

            var outputBatches =
                mutatingCliApp.DequeueStdOut();

            var outputTexts =
                outputBatches
                .Select(outputBatch =>
                    System.Text.Encoding.UTF8.GetString(outputBatch.Span))
                .ToImmutableArray();

            string.Concat(outputTexts).Should().Be(" app!");
        }


        {
            mutatingCliApp.EventStdIn(
                System.Text.Encoding.UTF8.GetBytes("\n"));

            var outputBatches =
                mutatingCliApp.DequeueStdOut();

            var outputTexts =
                outputBatches
                .Select(outputBatch =>
                    System.Text.Encoding.UTF8.GetString(outputBatch.Span))
                .ToImmutableArray();

            string.Concat(outputTexts).Should().Be("\nReceived line:\nHello app!\n");
        }
    }
}
