using Pine.Core.Elm;
using Pine.Core.Elm.Testing;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.IO;
using System.Linq;

using IntermediatePineVM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.CLI.Elm;

public static class TestCommand
{
    // Keep these defaults at the Elm test entry point so future CLI options can override them.
    private static readonly IntermediatePineVM.EvaluationConfig s_testEvaluationConfigDefault =
        new(
            InvocationCountLimit: 10_000_000,
            LoopIterationCountLimit: 10_000_000,
            StackDepthLimit: 100_000);

    public static Command Create()
    {
        var command =
            new Command(
                "test",
                "Compile and run Elm tests.");

        var sourceArgument =
            new Argument<string?>("source")
            {
                Arity = ArgumentArity.ZeroOrOne,
                Description = "Path to the Elm project. Defaults to the current directory.",
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
                "Number of worker threads. Defaults to " + DefaultWorkerCount() + "."
            };

        var reportDurationsOption =
            new Option<bool>("--report-durations")
            {
                Description = "Show detailed durations, including compilation and test execution."
            };

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
        command.Add(offlineOption);
        command.Add(dependencyReportOption);

        command.SetAction(
            parseResult =>
            Execute(
                source: parseResult.GetValue(sourceArgument) ?? Environment.CurrentDirectory,
                colorMode: parseResult.GetValue(colorOption),
                filter: parseResult.GetValue(filterOption),
                listTests: parseResult.GetValue(listTestsOption),
                workers: parseResult.GetValue(workersOption),
                reportDurations: parseResult.GetValue(reportDurationsOption),
                offline: parseResult.GetValue(offlineOption),
                dependencyReportPath: parseResult.GetValue(dependencyReportOption)));

        return command;
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
        IElmPackageProvider? packageProvider = null)
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

        var resolvedWorkers =
            workers ?? DefaultWorkerCount();

        if (resolvedWorkers < 1)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);
            errorConsole.Write(new Text("Error: ", TestCommandTheme.Failure));
            errorConsole.WriteLine("The --workers value must be at least 1.");

            return 1;
        }

        ElmTestRun testRun;

        try
        {
            resolutionConfiguration ??= ElmTestRunner.DefaultResolutionConfiguration.Value;

            if (offline)
                resolutionConfiguration = resolutionConfiguration with { Offline = true };

            testRun =
                ElmTestRunner.CompileAndRunTests(
                    source,
                    workers: resolvedWorkers,
                    resolutionConfiguration: resolutionConfiguration,
                    packageProvider: packageProvider,
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
                    onTestsDiscovered:
                    testCount =>
                    {
                        console.Write(
                            new Text(
                                "Running " + testCount + " test" +
                                (testCount is 1 ? "." : "s.") + "\n\n"));
                    });
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

            var message = "Did not find Elm test modules in " + noTestModules.AppDirectory;

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
                includeRunningMessage: false);

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

        return
            completed.Tests.All(test => test.Kind is CompletedTestKind.Passed)
            ?
            0
            :
            1;
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


    private static IAnsiConsole CreateSystemConsole(
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
