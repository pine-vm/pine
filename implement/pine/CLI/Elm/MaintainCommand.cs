using Pine.Elm019;
using Pine.Elm019.Testing;
using System;
using System.CommandLine;
using System.CommandLine.Help;
using System.CommandLine.Invocation;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;

namespace Pine.CLI.Elm;

public static class MaintainCommand
{
    public static Command Create()
    {
        var maintain =
            new Command("maintain", "Maintain repository test data and reference snapshots.")
            {
                Hidden = true,
            };

        var help = new HelpOption();

        var standardHelp =
            help.Action as HelpAction
            ?? throw new InvalidOperationException("The standard help option must provide a HelpAction.");

        help.Action = new MaintainerHelpAction(standardHelp);
        maintain.Add(help);
        var command = new Command("make-ecmascript", "Maintain the Elm make ECMAScript snapshot corpus.");

        var directoryOption =
            new Option<string?>("--test-data")
            {
                Description =
                "Corpus directory (defaults to implement/Pine.IntegrationTests/TestData/ElmMakeEcmascript in this checkout).",
                Recursive = true,
            };

        command.Add(directoryOption);
        maintain.Add(command);

        string DirectoryPath(System.CommandLine.ParseResult parseResult) =>
            Path.GetFullPath(parseResult.GetValue(directoryOption) ?? FindTestDataDirectory());

        var stats =
            new Command(
                "stats",
                "Display scenario, expectation, package and Elm 0.19.1 registry statistics (offline).");

        command.Add(stats);

        stats.SetAction(
            parseResult => Execute(
                async () =>
                {
                    var directory = DirectoryPath(parseResult);
                    var scenarios = ElmMakeEcmascriptScenarios.Discover(directory);
                    var snapshots = new ElmMakePackageSnapshots(directory);
                    var inventory = snapshots.Inventory();
                    var required = await snapshots.RequiredPackagesAsync(scenarios);
                    var registry = snapshots.ReadRegistry();
                    var expectations = scenarios.SelectMany(scenario => scenario.Expectations).ToArray();

                    Console.WriteLine(
                        $"Scenarios: {scenarios.Count}; expectations: {expectations.Length}; skipped: {expectations.Count(item => item.Skipped)}.");

                    foreach (var version in expectations.Select(item => item.CompilerVersion).Distinct().Order())
                    {
                        Console.WriteLine(
                            $"Compiler {version}: {expectations.Count(item => item.CompilerVersion == version)} expectations.");
                    }

                    foreach (var flavor in Enum.GetValues<ElmMakeFlavor>())
                    {
                        Console.WriteLine(
                            $"{flavor}: {expectations.Count(item => item.Flavor == flavor)} expectations.");
                    }

                    foreach (var output in Enum.GetValues<ElmMakeOutputType>())
                    {
                        Console.WriteLine(
                            $"{output}: {expectations.Count(item => item.OutputType == output)} expectations.");
                    }

                    foreach (var kind in Enum.GetValues<ElmMakeExpectationKind>())
                        Console.WriteLine($"{kind}: {expectations.Count(item => item.Kind == kind)} expectations.");

                    Console.WriteLine(
                        $"Package archives: {inventory.Count}; required: {required.Count}; unused: {inventory.Except(required).Count()}.");

                    Console.WriteLine(
                        $"Registry: {registry.Count} versions across {registry.Select(item => item.Name).Distinct().Count()} packages; {new FileInfo(snapshots.RegistryPath).Length} bytes.");

                    foreach (var identity in required.Except(registry))
                    {
                        throw new InvalidDataException(
                            "Registry is missing required package " + identity + ". Run update-packages.");
                    }

                    foreach (var identity in inventory.Except(required))
                        Console.WriteLine("Unused package snapshot (retained): " + identity);

                    return 0;
                }));

        var update =
            new Command(
                "update-packages",
                "Acquire only missing package archives from GitHub and extend the checked-in registry; retain unused snapshots.");

        command.Add(update);

        update.SetAction(
            parseResult => Execute(
                async () =>
                {
                    var directory = DirectoryPath(parseResult);

                    await new ElmMakePackageSnapshots(directory).UpdateAsync(
                        ElmMakeEcmascriptScenarios.Discover(directory),
                        Console.Out);

                    return 0;
                }));

        var generate =
            new Command(
                "generate",
                "Create one missing expectation using isolated, offline Elm 0.19.1 reference compilation.");

        var expectationArgument = PineCliCommand.RequiredArgument("expectation");

        expectationArgument.Description =
            "Path relative to scenarios: <scenario>/[skip/]expect/<major-minor-patch>/<source>.elm[.debug|.optimize].(js|html); append .errors-omitting-details.txt for compiler diagnostics.";

        generate.Add(expectationArgument);
        command.Add(generate);

        generate.SetAction(
            parseResult => Execute(
                async () =>
                {
                    var directory = DirectoryPath(parseResult);

                    var expectation =
                        ElmMakeEcmascriptScenarios.ParseExpectation(parseResult.GetValue(expectationArgument)!);

                    var scenarios = ElmMakeEcmascriptScenarios.Discover(directory, allowEmptyExpectations: true);
                    var scenario = ElmMakeEcmascriptScenarios.SelectScenario(scenarios, expectation.ScenarioName);
                    await new ElmMakeReferenceCompiler(directory).GenerateAsync(scenario, expectation, Console.Out);
                    return 0;
                }));

        var run =
            new Command(
                "run",
                "Run selected scenarios using the Elm-based ECMAScript compiler and report exact snapshot differences.");

        var scenarioArgument = new Argument<string[]>("scenarios") { Arity = ArgumentArity.ZeroOrMore };

        var referenceOption =
            new Option<bool>("--reference") { Description = "Check reference reproducibility using classic Elm 0.19.1 instead of the implementation under test." };

        var executionPathOption =
            new Option<ElmMakeExecutionPath>("--execution-path")
            {
                DefaultValueFactory = _ => ElmMakeExecutionPath.PineVM,
                Description = "Execute the Elm implementation using PineVM or SyntaxInterpreter.",
            };

        var skipDebugOption = new Option<bool>("--skip-expect-debug");
        var skipOptimizeOption = new Option<bool>("--skip-expect-optimize");

        var hunksOption =
            new Option<int>("--max-diff-hunks") { DefaultValueFactory = _ => 3, Description = "Maximum differing hunks rendered per expectation (nonnegative)." };

        run.Add(scenarioArgument);
        run.Add(referenceOption);
        run.Add(executionPathOption);
        run.Add(skipDebugOption);
        run.Add(skipOptimizeOption);
        run.Add(hunksOption);
        command.Add(run);

        run.SetAction(
            parseResult => Execute(
                async () =>
                {
                    var maxHunks = parseResult.GetValue(hunksOption);
                    ArgumentOutOfRangeException.ThrowIfNegative(maxHunks);
                    var directory = DirectoryPath(parseResult);
                    var scenarios = ElmMakeEcmascriptScenarios.Discover(directory);
                    var names = parseResult.GetValue(scenarioArgument) ?? [];

                    var selected =
                        names.Length is 0
                        ?
                        scenarios
                        :
                        [
                        .. names.Distinct().Select(name => ElmMakeEcmascriptScenarios.SelectScenario(scenarios, name))
                        ];

                    var reference = parseResult.GetValue(referenceOption);

                    Console.WriteLine(
                        reference
                        ?
                        "Backend: Elm 0.19.1 reference compiler (reproducibility check only)."
                        :
                        "Backend: Elm-based ECMAScript compiler (" + parseResult.GetValue(executionPathOption) + ").");

                    Console.WriteLine(
                        "Explicitly skipped expectations: " +
                        selected.Sum(scenario => scenario.Expectations.Count(expectation => expectation.Skipped)));

                    Console.WriteLine(
                        "Filtered expectations: " +
                        selected.Sum(
                            scenario => scenario.Expectations.Count(
                                expectation => !expectation.Skipped &&
                                    (parseResult.GetValue(skipDebugOption) && expectation.Flavor is ElmMakeFlavor.Debug ||
                                    parseResult.GetValue(skipOptimizeOption) &&
                                    expectation.Flavor is ElmMakeFlavor.Optimize))));

                    var results =
                        await ElmMakeEcmascriptScenarios.RunAsync(
                            directory,
                            selected,
                            reference
                            ?
                            new ElmMakeReferenceCompiler(directory).CompileOutcomeAsync
                            :
                            (scenario, expectation, token) => ElmMakeScenarioCompiler.CompileOutcomeAsync(
                                directory,
                                scenario,
                                expectation,
                                token,
                                parseResult.GetValue(executionPathOption)),
                            maxHunks,
                            parseResult.GetValue(skipDebugOption),
                            parseResult.GetValue(skipOptimizeOption));

                    Console.WriteLine(ElmMakeEcmascriptScenarios.RenderResults(results));
                    return results.All(result => result.Passed) ? 0 : 1;
                }));

        return maintain;
    }

    private sealed class MaintainerHelpAction(HelpAction standardHelp) : SynchronousCommandLineAction
    {
        public override bool ClearsParseErrors => true;

        public override int Invoke(ParseResult result)
        {
            var command = result.CommandResult.Command;
            var hidden = command.Hidden;

            // System.CommandLine suppresses even explicitly requested help for a hidden command.
            command.Hidden = false;

            try
            {
                return standardHelp.Invoke(result);
            }
            finally
            {
                command.Hidden = hidden;
            }
        }
    }

    private static int Execute(Func<Task<int>> action)
    {
        try
        {
            return action().GetAwaiter().GetResult();
        }
        catch (Exception exception) when (exception is IOException or InvalidDataException or ArgumentException or FormatException or JsonException or InvalidOperationException or NotSupportedException or TimeoutException or System.Net.Http.HttpRequestException or UnauthorizedAccessException)
        {
            Console.Error.WriteLine("Elm make ECMAScript maintenance failed: " + exception.Message);
            return 1;
        }
    }

    private static string FindTestDataDirectory()
    {
        foreach (var start in new[] { Environment.CurrentDirectory, AppContext.BaseDirectory })
        {
            for (var directory = new DirectoryInfo(start); directory is not null; directory = directory.Parent)
            {
                foreach (var relative in new[]
                {
                    Path.Combine("implement", "Pine.IntegrationTests", "TestData", "ElmMakeEcmascript"),
                    Path.Combine("Pine.IntegrationTests", "TestData", "ElmMakeEcmascript"),
                    Path.Combine("TestData", "ElmMakeEcmascript"),
                })
                {
                    var candidate = Path.Combine(directory.FullName, relative);

                    if (System.IO.Directory.Exists(candidate))
                        return candidate;
                }
            }
        }

        throw new DirectoryNotFoundException(
            "Cannot locate ElmMakeEcmascript test data in this checkout. Supply --test-data <directory>.");
    }
}
