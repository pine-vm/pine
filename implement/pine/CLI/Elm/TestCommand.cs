using Pine.Core.CLI;
using Pine.Core.Elm;
using Pine.Core.Elm.Testing;
using Pine.Core.Interpreter.IntermediateVM;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.CommandLine.Help;
using System.CommandLine.Invocation;
using System.Globalization;
using System.IO;
using System.Linq;

using IntermediatePineVM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.CLI.Elm;

public static class TestCommand
{
    // Keep these defaults at the Elm test entry point so future CLI options can override them.
    private static readonly IntermediatePineVM.EvaluationConfig s_testEvaluationConfigDefault =
        ElmTestRunner.DefaultEvaluationConfig;

    public static Command Create()
    {
        var command =
            new Command(
                "test",
                """
                Compile and run Elm tests.

                Investigating a hang or expensive computation? Use:
                  pine elm test profile <project> --filter "<single test path>" --budget 10000
                """);

        var budgets = TestBudgetOptions.AddTo(command);
        ConfigureCommonCommand(command, instrumented: false, budgets);
        budgets.MoveAfterCommonOptions(command);

        var profile =
            new Command(
                "profile",
                "Profile one Elm test and see where computation time is spent. All test declarations are prepared before --filter is applied.");

        ConfigureCommonCommand(profile, instrumented: true, budgets);
        command.Add(profile);
        ConfigureFormattedHelp(command, profile);
        return command;
    }

    private static void ConfigureFormattedHelp(Command command, Command profile)
    {
        var helpOption = new HelpOption { Recursive = true };

        var standardHelp =
            helpOption.Action as HelpAction
            ?? throw new InvalidOperationException("The standard help option must provide a HelpAction.");

        helpOption.Action =
            new FormattedHelpAction(
                standardHelp,
                [
                .. command.Options.Concat(profile.Options).Distinct()
                .Where(
                    option => option is Option<int> or Option<int?> or Option<uint> or Option<double> or Option<double?>)
                ]);

        command.Add(helpOption);
    }

    private sealed class FormattedHelpAction(HelpAction standardHelp, Option[] numericOptions) : SynchronousCommandLineAction
    {
        public override bool ClearsParseErrors => true;

        public override int Invoke(ParseResult result)
        {
            var writer = result.InvocationConfiguration.Output;
            using var buffer = new StringWriter();
            int exitCode;
            result.InvocationConfiguration.Output = buffer;

            try
            {
                exitCode = standardHelp.Invoke(result);
            }
            finally
            {
                result.InvocationConfiguration.Output = writer;
            }

            // System.CommandLine's default-value formatter is internal; keep its layout and customize numeric annotations.
            var text = buffer.ToString();

            foreach (var option in numericOptions.Where(option => option.HasDefaultValue))
            {
                var raw = option.GetDefaultValue()?.ToString();

                if (long.TryParse(raw, NumberStyles.Integer, CultureInfo.InvariantCulture, out var number))
                {
                    text =
                        text.Replace(
                            "[default: " + raw + "]",
                            "[default: " + CommandLineInterface.FormatIntegerForDisplay(number) + "]",
                            StringComparison.Ordinal);
                }
            }

            writer.Write(text);
            return exitCode;
        }
    }

    private static void ConfigureCommonCommand(Command command, bool instrumented, TestBudgetOptions budgets)
    {
        var sourceArgument =
            new Argument<string?>("source")
            {
                Arity = ArgumentArity.ZeroOrOne,
                Description = "Elm project directory or GitHub/GitLab tree URL. Defaults to the current directory.",
            };

        var colorOption = FormatCommandShared.CreateColorOption();

        var filterOption =
            new Option<string?>("--filter")
            {
                Arity = ArgumentArity.ExactlyOne,
                AllowMultipleArgumentsPerToken = false,
                HelpName = "EXPRESSION",
                Description =
                """
                Run tests matching the expression.
                Matching is case-insensitive. A plain term matches a substring of a file, group, or test name.
                Paths use / or \ on any OS: directories/filename/group descriptions/test name.
                Path segments match consecutively anywhere in the test path; the filename extension may be omitted.
                * matches zero or more characters within one segment; ** matches zero or more whole segments.
                Quote expressions containing spaces or wildcards. Conjunctive filters are not supported.

                Examples:
                  --filter "convert concrete"
                  --filter "tests/ConvertConcreteToAbstractTests/convert*/converts every*"
                  --filter "ConvertConcreteToAbstractTests/**/converts every*"
                  --filter "tests/*Tests.elm/**/converts*drops documentation"
                No matches: show the closest existing test paths. Use --list-tests to explore tests without running them.
                Prefix a full path with '=' for exact, case-sensitive matching, including literal wildcard characters.
                Use '#N' for a discovery ordinal when duplicate test paths cannot distinguish a test.
                """
            };

        filterOption.Validators.Add(
            result =>
            {
                if (result.IdentifierTokenCount > 1)
                    result.AddError("The --filter option can only be specified once.");

            });

        var listTestsOption =
            new Option<bool>("--list-tests")
            {
                Description = "List tests without running them."
            };

        var workersOption =
            new Option<int?>("--workers")
            {
                Description =
                "Number of worker threads. Defaults to " +
                CommandLineInterface.FormatIntegerForDisplay(DefaultWorkerCount()) +
                "."
            };

        NumericOptionParsing.SetIntegerParser(
            workersOption,
            value => value is <= 0 ? "The --workers value must be at least 1." : null);

        var reportDurationsOption =
            new Option<bool>("--report-durations")
            {
                Description = "Show detailed durations, including compilation and test execution."
            };

        var seedOption =
            new Option<uint?>("--seed")
            {
                Description =
                "Initial unsigned 32-bit random seed for fuzz tests. Defaults to a new random seed. Integers accept internal underscores."
            };

        var fuzzOption =
            new Option<uint>("--fuzz")
            {
                Description =
                "Number of iterations per fuzz test. Must be positive. Defaults to 100. Counts accept underscores and SI units k/M/G.",
                DefaultValueFactory = _ => 100,
            };

        NumericOptionParsing.SetIntegerParser(seedOption);

        NumericOptionParsing.SetCountParser(
            fuzzOption,
            value => value is 0 ? "The --fuzz value must be a positive unsigned 32-bit integer." : null);

        fuzzOption.Validators.Add(
            result =>
            {
                if (result.IdentifierTokenCount > 1)
                    result.AddError("The --fuzz option can only be specified once.");
            });

        seedOption.Validators.Add(
            result =>
            {
                if (result.IdentifierTokenCount > 1)
                    result.AddError("The --seed option can only be specified once.");
            });

        var offlineOption =
            new Option<bool>("--offline")
            {
                Description =
                "Resolve packages using only local registry metadata and sources; make no network requests.",
            };

        var dependencyReportOption =
            new Option<string?>("--dependency-report")
            {
                Description =
                "Write the full dependency resolution report as JSON, including rejected branches and conflicts.",
            };

        command.Add(sourceArgument);
        command.Add(colorOption);
        command.Add(filterOption);
        command.Add(listTestsOption);
        command.Add(workersOption);
        command.Add(reportDurationsOption);
        command.Add(seedOption);
        command.Add(fuzzOption);
        command.Add(offlineOption);
        command.Add(dependencyReportOption);

        int Run(ParseResult parseResult, ElmTestInstrumentation? instrumentation) =>
            Execute(
                source: parseResult.GetValue(sourceArgument) ?? Environment.CurrentDirectory,
                colorMode: parseResult.GetValue(colorOption),
                filter: parseResult.GetValue(filterOption),
                listTests: parseResult.GetValue(listTestsOption),
                workers: parseResult.GetValue(workersOption),
                reportDurations: parseResult.GetValue(reportDurationsOption),
                offline: parseResult.GetValue(offlineOption),
                dependencyReportPath: parseResult.GetValue(dependencyReportOption),
                seed: parseResult.GetValue(seedOption),
                fuzz: parseResult.GetValue(fuzzOption),
                instrumentation: instrumentation,
                evaluationOptions: budgets.Read(parseResult),
                showEffectiveLimits: budgets.HasExplicitOptions(parseResult));

        if (instrumented)
        {
            var options = TestProfileCommand.AddOptions(command, budgets);

            command.SetAction(
                parseResult => TestProfileCommand.Execute(
                    parseResult.GetValue(sourceArgument) ?? Environment.CurrentDirectory,
                    parseResult.GetValue(filterOption),
                    options.Read(parseResult),
                    instrumentation => Run(parseResult, instrumentation),
                    colorMode: parseResult.GetValue(colorOption)));
        }
        else
            command.SetAction(parseResult => Run(parseResult, null));

    }


    public static int Execute(
        string source,
        FormatCommandColorMode? colorMode = null,
        IAnsiConsole? console = null,
        IAnsiConsole? errorConsole = null,
        string? filter = null,
        bool listTests = false,
        int? workers = null,
        bool reportDurations = false,
        bool offline = false,
        string? dependencyReportPath = null,
        ElmDependencyResolutionConfiguration? resolutionConfiguration = null,
        IElmPackageProvider? packageProvider = null,
        uint? seed = null,
        uint fuzz = 100,
        ElmTestInstrumentation? instrumentation = null,
        ElmTestEvaluationOptions? evaluationOptions = null,
        bool showEffectiveLimits = false,
        Func<string, IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>? remoteSourceLoader = null)
    {
        FormatCommandColorMode resolvedColorMode;

        try
        {
            resolvedColorMode =
                FormatCommandShared.ResolveColorMode(
                    colorMode,
                    Environment.GetEnvironmentVariable(FormatCommandShared.ColorEnvironmentVariable));
        }
        catch (ArgumentException exception)
        {
            errorConsole ??=
                CreateSystemConsole(
                    Console.Error,
                    FormatCommandColorMode.Auto);

            errorConsole.Write(new Text("Error: ", TestCommandTheme.Failure));
            errorConsole.WriteLine(exception.Message);

            return 1;
        }

        console ??= CreateSystemConsole(Console.Out, resolvedColorMode);

        var effectiveLimits = instrumentation?.Options ?? evaluationOptions ?? new ElmTestEvaluationOptions();

        try
        {
            effectiveLimits.Validate();
        }
        catch (ArgumentException exception)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);
            errorConsole.Profile.Out.Writer.WriteLine("Error: " + exception.Message);
            return 1;
        }

        if (instrumentation is null && (effectiveLimits.HasCustomLimits || showEffectiveLimits))
            TestBudgetOptions.PrintEffective(console.Profile.Out.Writer, effectiveLimits);

        using var budgetTracker =
            instrumentation is null && (effectiveLimits.HasCustomLimits || showEffectiveLimits)
            ?
            new ElmTestInstrumentation(
                effectiveLimits.ToInstrumentationOptions(),
                precompiledLeavesProvider: () => IntermediateVM.SetupVM.DefaultPrecompiledLeaves,
                recordDiagnostics: false)
            {
                OnStopped = summary => WriteStoppedTest(console, source, summary, effectiveLimits),
            }
            :
            null;

        using var budgetCancellation =
            budgetTracker is not null ? TestBudgetOptions.RegisterCancellation(budgetTracker) : null;

        instrumentation ??= budgetTracker;

        var resolvedWorkers =
            workers ?? DefaultWorkerCount();

        if (resolvedWorkers < 1)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);
            errorConsole.Write(new Text("Error: ", TestCommandTheme.Failure));
            errorConsole.WriteLine("The --workers value must be at least 1.");

            return 1;
        }

        if (fuzz is 0)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);
            errorConsole.WriteLine("Error: The --fuzz value must be a positive unsigned 32-bit integer.");
            return 1;
        }

        ElmTestRun testRun;

        try
        {
            using var projectSource =
                ElmTestProjectSource.Resolve(source, offline, console, remoteSourceLoader);

            if (instrumentation is not null && ElmTestProjectSource.IsRemote(source))
                instrumentation.Metadata["ProjectSource"] = source;

            resolutionConfiguration ??= ElmTestRunner.DefaultResolutionConfiguration.Value;

            if (offline)
                resolutionConfiguration = resolutionConfiguration with { Offline = true };

            testRun =
                ElmTestRunner.CompileAndRunTests(
                    projectSource.DirectoryPath,
                    workers: resolvedWorkers,
                    resolutionConfiguration: resolutionConfiguration,
                    packageProvider: packageProvider,
                    fuzzOptions: new() { Seed = seed, Runs = fuzz },
                    onDependenciesResolved: report =>
                    {
                        if (dependencyReportPath is not null)
                            File.WriteAllText(dependencyReportPath, report.ToJson());
                    },
                    pineVmFactory:
                    (invocationCache, sharedCaches) =>
                    IntermediateVM.SetupVM.Create(
                        evaluationConfigDefault: s_testEvaluationConfigDefault,
                        invocationCache: invocationCache,
                        parseCache: sharedCaches.ParsedExpressions,
                        tryGetExpressionCompilation: sharedCaches.ExpressionCompilations.TryGet,
                        getOrAddExpressionCompilation: sharedCaches.ExpressionCompilations.GetOrAdd,
                        expressionEncodingCache: sharedCaches.EncodedExpressions,
                        reducedExpressionCache: sharedCaches.ReducedExpressions),
                    filter: filter,
                    listTests: listTests,
                    instrumentation: instrumentation,
                    onTestsDiscovered:
                    testCount =>
                    {
                        console.Write(
                            new Text(
                                "Running " + testCount + " test" +
                                (testCount is 1 ? "." : "s.") + "\n\n"));
                    });
        }
        catch (ElmTestInstrumentationStoppedException)
        {
            return instrumentation?.CancelledByUser is true ? 130 : 2;
        }
        catch (ElmTestInstrumentationSelectionException exception)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);

            WriteProfileSelection(
                errorConsole,
                source,
                exception,
                resolvedColorMode is not FormatCommandColorMode.Never);

            return 1;
        }
        catch (OperationCanceledException) when (instrumentation is not null)
        {
            instrumentation.NotifyCancellation();
            return instrumentation.CancelledByUser ? 130 : 2;
        }
        catch (Core.Elm.ElmCompilerInDotnet.ElmCompilationException exception)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);
            errorConsole.Write(new Text("Error: ", TestCommandTheme.Failure));
            errorConsole.Profile.Out.Writer.WriteLine(exception.Message);
            return 1;
        }
        catch (ElmDependencyResolutionException exception)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);
            errorConsole.Write(new Text("Error: ", TestCommandTheme.Failure));
            errorConsole.Profile.Out.Writer.WriteLine(exception.Message);

            if (dependencyReportPath is not null)
            {
                try
                {
                    File.WriteAllText(dependencyReportPath, exception.Report.ToJson());
                }
                catch (Exception reportException) when (reportException is IOException or UnauthorizedAccessException)
                {
                    errorConsole.Profile.Out.Writer.WriteLine(
                        $"Cannot write dependency report '{dependencyReportPath}': {reportException.Message}");
                }
            }

            return 1;
        }
        catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);
            errorConsole.Write(new Text("Error: ", TestCommandTheme.Failure));
            errorConsole.Profile.Out.Writer.WriteLine(exception.Message);
            return 1;
        }

        if (testRun is ElmTestRun.NoTestModules noTestModules)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);

            errorConsole.Write(new Text("Error: ", TestCommandTheme.Failure));

            var message =
                "Did not find Elm test modules in " +
                (ElmTestProjectSource.IsRemote(source) ? source : noTestModules.AppDirectory);

            if (errorConsole.Profile.Out.IsTerminal)
                errorConsole.WriteLine(message);

            else
                errorConsole.Profile.Out.Writer.WriteLine(message);

            return 1;
        }

        if (testRun is ElmTestRun.NoMatchingTests noMatchingTests)
        {
            if (listTests)
            {
                WriteTestList(
                    console,
                    tests: [],
                    noMatchingTests.FilteredOutTests,
                    filterApplied: true,
                    useColor: resolvedColorMode is not FormatCommandColorMode.Never);
            }

            console.WriteLine("No tests matched the filter expression:");
            console.WriteLine("  " + noMatchingTests.Filter);

            if (noMatchingTests.ClosestTests.Count > 0)
            {
                console.WriteLine();
                console.WriteLine("Closest existing test paths (not selected or run):");

                foreach (var test in noMatchingTests.ClosestTests)
                {
                    if (console.Profile.Out.IsTerminal)
                        console.WriteLine("  " + test.FullPath);

                    else
                        console.Profile.Out.Writer.WriteLine("  " + test.FullPath);
                }
            }
            else
            {
                console.WriteLine("No individual tests were discovered.");
            }

            console.WriteLine();
            console.WriteLine("Shorten the filter path or use wildcards to broaden the selection.");
            console.WriteLine("Use / or \\ between consecutive segments, * within a segment, or ** between levels.");
            console.WriteLine("Use --list-tests without --filter to list all tests, or --help for filter examples.");

            return 1;
        }

        if (testRun is ElmTestRun.Listed listed)
        {
            WriteTestList(
                console,
                listed.Tests,
                listed.FilteredOutTests,
                filterApplied: filter is not null,
                useColor: resolvedColorMode is not FormatCommandColorMode.Never);

            return 0;
        }

        if (testRun is not ElmTestRun.Completed completed)
            throw new InvalidOperationException("Unexpected Elm test run type: " + testRun.GetType());

        var output =
            ElmTestRunner.RenderTestResults(
                completed.Tests,
                includeTestDetails: true,
                duration: completed.Duration,
                compilationDuration:
                reportDurations
                ?
                completed.CompilationDuration
                :
                null,
                includeRunningMessage: false,
                incompleteReason: completed.IncompleteReason);

        if (resolvedColorMode is FormatCommandColorMode.Never)
        {
            console.Write(new Text(output.PlainText));
        }
        else
        {
            foreach (var fragment in output.Fragments)
                console.Write(new Text(fragment.Text, StyleFor(fragment.Style)));
        }

        console.WriteLine();

        if (completed.Tests.Any(test => test.Fuzz is not null) && completed.ExecutionSettings is { } settings)
        {
            var reproductionCommand =
                $"To reproduce these results, run pine elm test \"{ElmTestProjectSource.CommandSource(source)}\" --seed {settings.Seed} --fuzz {settings.FuzzRuns}";

            if (console.Profile.Out.IsTerminal)
                console.WriteLine(reproductionCommand);

            else
                console.Profile.Out.Writer.WriteLine(reproductionCommand);
        }

        return
            completed.IncompleteReason is null && completed.Tests.Count > 0 &&
            completed.Tests.All(test => test.Kind is CompletedTestKind.Passed)
            ?
            0
            :
            1;
    }


    internal static string QuoteCommandArgument(string value) =>
        OperatingSystem.IsWindows()
        ?
        "'" + value.Replace("'", "''", StringComparison.Ordinal) + "'"
        :
        "'" + value.Replace("'", "'\\''", StringComparison.Ordinal) + "'";

    internal static string ProfileCommandLine(string source, string selector) =>
        "pine elm test profile " + QuoteCommandArgument(ElmTestProjectSource.CommandSource(source)) +
        " --filter " + QuoteCommandArgument(selector);

    private static void WriteStoppedTest(
        IAnsiConsole console,
        string source,
        ElmTestProfileSummary summary,
        ElmTestEvaluationOptions limits)
    {
        var writer = console.Profile.Out.Writer;
        writer.WriteLine("Execution stopped. " + summary.StopReason);

        if (summary.Phase is "execution")
            writer.WriteLine("Elm test: " + summary.Context);

        else if (summary.Phase is "preparation")
        {
            writer.WriteLine("Elm test preparation: " + summary.Context);

            writer.WriteLine(
                "The individual test has not been constructed yet; this identifies its source declaration.");
        }
        else
        {
            writer.WriteLine(
                "Elm test phase: " + summary.Phase + "; context: " +
                (summary.Phase is "compilation" && ElmTestProjectSource.IsRemote(source) ? source : summary.Context));
        }

        writer.WriteLine(
            "Invocations: " + CommandLineInterface.FormatIntegerForDisplay(summary.Counters.InvocationCount) +
            "; loops: " + CommandLineInterface.FormatIntegerForDisplay(summary.Counters.LoopIterationCount) +
            "; instructions: " +
            CommandLineInterface.FormatIntegerForDisplay(summary.Counters.InstructionCount) +
            ".");

        writer.WriteLine(PerformanceCountersFormatting.FormatCounts(summary.Counters));
        writer.WriteLine(PerformanceCountersFormatting.FormatCountsByPhase(summary.CountersByPhase));

        writer.WriteLine("Investigate with the profile command:");

        var command =
            summary.ProfileFilter is { } selector
            ?
            ProfileCommandLine(source, selector)
            :
            "pine elm test profile " + QuoteCommandArgument(ElmTestProjectSource.CommandSource(source));

        if (limits.InvocationBudget is { } inv)
            command += " --invocation-budget " + inv.ToString(CultureInfo.InvariantCulture);

        if (limits.LoopBudget is { } loops)
            command += " --loop-budget " + loops.ToString(CultureInfo.InvariantCulture);

        if (limits.Timeout is { } timeout)
            command += " --timeout " + timeout.TotalSeconds.ToString(CultureInfo.InvariantCulture);

        if (limits.StackDepthLimit != 100_000)
            command += " --max-stack-depth " + limits.StackDepthLimit.ToString(CultureInfo.InvariantCulture);

        writer.WriteLine("  " + command);

        if (summary.Phase is "preparation")
        {
            writer.WriteLine(
                "All test declarations are prepared before filtering; --filter cannot guarantee bypassing preparation.");

            writer.WriteLine("Once preparation completes, profile will suggest an exact single-test filter if needed.");
        }

        writer.Flush();
    }

    private static void WriteProfileSelection(
        IAnsiConsole console,
        string source,
        ElmTestInstrumentationSelectionException selection,
        bool useColor)
    {
        console.Write(
            new Text(
                "Cannot profile: exactly one runnable Elm test must remain.\n",
                useColor ? TestCommandTheme.TodoHeadline : Style.Plain));

        var writer = console.Profile.Out.Writer;
        writer.WriteLine("Tests found: " + CommandLineInterface.FormatIntegerForDisplay(selection.FoundCount));

        writer.WriteLine(
            "Tests remaining after filter: " + CommandLineInterface.FormatIntegerForDisplay(selection.RemainingCount));

        writer.WriteLine("Current filter: " + (selection.Filter is null ? "(none)" : selection.Filter));

        if (selection.FoundCount is 0)
            writer.WriteLine("No runnable tests were found. Add a runnable Elm test to this project.");

        else if (!selection.SuggestionsFromRemaining)
        {
            writer.WriteLine(
                "No tests remain. Change or remove the current filter; these commands select discovered tests instead:");
        }
        else if (selection.Suggestions.Count > 0)
            writer.WriteLine("Select one of the remaining tests with an exact filter:");

        else
            writer.WriteLine("No runnable test remains (selected entries may be TODOs). Select a runnable test.");

        foreach (var suggestion in selection.Suggestions)
        {
            writer.WriteLine("  " + suggestion.Test.FullPath);
            writer.WriteLine("    " + ProfileCommandLine(source, suggestion.Filter));
        }

        writer.Flush();
    }

    private static void WriteTestList(
        IAnsiConsole console,
        IReadOnlyList<ListedTest> tests,
        IReadOnlyList<ListedTest> filteredOutTests,
        bool filterApplied,
        bool useColor)
    {
        var tree =
            new Tree(
                new Text(
                    filterApplied
                    ?
                    $"Tests remaining after filtering ({tests.Count})"
                    :
                    $"Available tests ({tests.Count})",
                    ListStyle(TestCommandTheme.ListHeading)))
            {
                Guide = TreeGuide.Line,
                Style = ListStyle(TestCommandTheme.Default),
            };

        foreach (var testsInFile in
            tests.Select(test => (test, filteredOut: false))
            .Concat(filteredOutTests.Select(test => (test, filteredOut: true)))
            .GroupBy(entry => entry.test.FilePath, StringComparer.Ordinal)
            .OrderBy(group => group.Key, StringComparer.Ordinal))
        {
            var fileNode =
                tree.AddNode(
                    new Text(
                        testsInFile.Key,
                        ListStyle(TestCommandTheme.ListFile)));

            AddDescriptionNodes(
                fileNode,
                [.. testsInFile],
                descriptionDepth: 0,
                useColor);
        }

        console.Write(tree);
        console.WriteLine();

        Style ListStyle(Style colorStyle) =>
            useColor
            ?
            colorStyle
            :
            Style.Plain;
    }


    private static void AddDescriptionNodes(
        TreeNode parent,
        IReadOnlyList<(ListedTest test, bool filteredOut)> tests,
        int descriptionDepth,
        bool useColor)
    {
        foreach (var test in
            tests
            .Where(entry => !entry.filteredOut && entry.test.DescriptionPath.Count == descriptionDepth)
            .Select(entry => entry.test)
            .OrderBy(test => test.Name, StringComparer.Ordinal))
        {
            parent.AddNode(
                new Text(
                    test.Name,
                    ListStyle(TestCommandTheme.ListTest)));
        }

        foreach (var testsInDescription in
            tests
            .Where(entry => entry.test.DescriptionPath.Count > descriptionDepth)
            .GroupBy(
                entry => entry.test.DescriptionPath[descriptionDepth],
                StringComparer.Ordinal)
            .OrderBy(group => group.Key, StringComparer.Ordinal))
        {
            var descriptionNode =
                parent.AddNode(
                    new Text(
                        testsInDescription.Key,
                        ListStyle(TestCommandTheme.ListDescription)));

            AddDescriptionNodes(
                descriptionNode,
                [.. testsInDescription],
                descriptionDepth + 1,
                useColor);
        }

        var filteredOutCount = tests.Count(entry => entry.filteredOut);

        if (filteredOutCount > 0)
        {
            parent.AddNode(
                new Text(
                    $"{filteredOutCount} test{(filteredOutCount is 1 ? "" : "s")} filtered out",
                    ListStyle(TestCommandTheme.Dark)));
        }

        Style ListStyle(Style colorStyle) =>
            useColor
            ?
            colorStyle
            :
            Style.Plain;
    }


    private static Style StyleFor(TestOutputStyle style) =>
        style switch
        {
            TestOutputStyle.Default => TestCommandTheme.Default,
            TestOutputStyle.Dark => TestCommandTheme.Dark,
            TestOutputStyle.Success => TestCommandTheme.Success,
            TestOutputStyle.SuccessHeadline => TestCommandTheme.SuccessHeadline,
            TestOutputStyle.Failure => TestCommandTheme.Failure,
            TestOutputStyle.FailureHeadline => TestCommandTheme.FailureHeadline,
            TestOutputStyle.Todo => TestCommandTheme.Todo,
            TestOutputStyle.TodoHeadline => TestCommandTheme.TodoHeadline,
            TestOutputStyle.Highlighted => TestCommandTheme.Highlighted,

            _ =>
            throw new ArgumentOutOfRangeException(nameof(style)),
        };


    internal static IAnsiConsole CreateSystemConsole(
        TextWriter writer,
        FormatCommandColorMode colorMode) =>
        AnsiConsole.Create(
            new AnsiConsoleSettings
            {
                Ansi = FormatCommandShared.AnsiSupportForColorMode(colorMode),
                ColorSystem = FormatCommandShared.ColorSystemSupportForColorMode(colorMode),
                Out = new AnsiConsoleOutput(writer),
            });

    private static int DefaultWorkerCount() =>
        ElmTestRunner.DefaultWorkerCount(Environment.ProcessorCount);

    private static class TestCommandTheme
    {
        public static Style Default { get; } =
            new(foreground: Color.Default);

        public static Style Dark { get; } =
            new(foreground: Color.Default, decoration: Decoration.Dim);

        public static Style Success { get; } =
            new(foreground: Color.Green);

        public static Style SuccessHeadline { get; } =
            new(foreground: Color.Green, decoration: Decoration.Underline);

        public static Style Failure { get; } =
            new(foreground: Color.Red);

        public static Style FailureHeadline { get; } =
            new(foreground: Color.Red, decoration: Decoration.Underline);

        public static Style Todo { get; } =
            new(foreground: Color.Yellow);

        public static Style TodoHeadline { get; } =
            new(foreground: Color.Yellow, decoration: Decoration.Underline);

        public static Style Highlighted { get; } =
            new(foreground: Color.Default, decoration: Decoration.Invert);

        public static Style ListHeading { get; } =
            new(foreground: Color.Default, decoration: Decoration.Bold);

        public static Style ListFile { get; } =
            new(foreground: Color.Default, decoration: Decoration.Bold);

        public static Style ListDescription { get; } =
            new(foreground: Color.Yellow);

        public static Style ListTest { get; } =
            new(foreground: Color.Green);
    }
}
