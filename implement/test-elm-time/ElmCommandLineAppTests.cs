using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Core;
using Pine.Elm.Platform;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class ElmCommandLineAppTests
{
    private static string PathToSimpleAppDirectory => @"./../../../../example-apps/cli-demo";

    public static TreeNodeWithStringPath LoadFromLocalFilesystem(string path) =>
        PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(
            Filesystem.GetAllFilesFromDirectory(path));

    [TestMethod]
    [Timeout(1000 * 60 * 10)]
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

            Assert.AreEqual(
                expected: "Started with the following command line: fake command line\n",
                actual: string.Concat(outputTexts));
        }

        {
            mutatingCliApp.EventStdIn(
                System.Text.Encoding.UTF8.GetBytes("Hello app!"));

            var outputBatches =
                mutatingCliApp.DequeueStdOut();

            var outputTexts =
                outputBatches
                .Select(outputBatch =>
                    System.Text.Encoding.UTF8.GetString(outputBatch.Span))
                .ToImmutableArray();

            Assert.AreEqual(
                expected: "Received: Hello app!\n",
                actual: string.Concat(outputTexts));
        }
    }
}
