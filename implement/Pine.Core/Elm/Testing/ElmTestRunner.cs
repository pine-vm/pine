using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.Elm019;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading;
using System.Threading.Tasks;

using IPineVM = Pine.Core.PineVM.IPineVM;
using IntermediatePineVM = Pine.Core.Interpreter.IntermediateVM.PineVM;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Elm.Testing;

/// <summary>
/// Compiles, runs, and renders Elm tests.
/// </summary>
public static class ElmTestRunner
{
    /// <summary>
    /// Default configuration for test preparation and execution. Work is not limited by
    /// invocation or loop counts; callers can cancel long-running commands.
    /// </summary>
    public static IntermediatePineVM.EvaluationConfig DefaultEvaluationConfig { get; } =
        new(
            InvocationCountLimit: null,
            LoopIterationCountLimit: null,
            StackDepthLimit: 100_000);

    /// <summary>Test-scoped terminal substitutions for the pinned Elm test/fuzz engine and pure PCG generator.</summary>
    public static readonly Lazy<ElmDependencyResolutionConfiguration> DefaultResolutionConfiguration =
        new(
            () => ElmPackageSubstitutions.DefaultBuild.Value with
            {
                IncludeTests = true,
                Substitutions =
                ElmPackageSubstitutions.DefaultBuild.Value.Substitutions
                .Add(ElmFuzzPackageSources.Test()).Add(ElmFuzzPackageSources.Random()),
            });

    /// <summary>
    /// Computes the default worker count for the available logical processor count.
    /// </summary>
    public static int DefaultWorkerCount(int processorCount) =>
        Math.Max(
            1,
            Math.Min(
                processorCount / 2,
                processorCount - 2));


    /// <summary>
    /// Compiles and runs the tests in an Elm project.
    /// </summary>
    public static ElmTestRun CompileAndRunTests(
        string appDirectory,
        IPineVM? pineVm = null,
        string? filter = null,
        bool listTests = false,
        Action<int>? onTestsDiscovered = null,
        ElmDependencyResolutionConfiguration? resolutionConfiguration = null,
        IElmPackageProvider? packageProvider = null,
        Action<ElmDependencyResolutionReport>? onDependenciesResolved = null,
        ElmFuzzOptions? fuzzOptions = null,
        ElmTestInstrumentation? instrumentation = null) =>
        CompileAndRunTests(
            appDirectory,
            pineVm,
            filter,
            listTests,
            workers: 1,
            pineVmFactory: null,
            onTestsDiscovered,
            resolutionConfiguration,
            packageProvider,
            onDependenciesResolved,
            fuzzOptions,
            instrumentation);


    /// <summary>
    /// Compiles and runs the tests in an Elm project using a separate VM for each test and shared compilation caches.
    /// </summary>
    public static ElmTestRun CompileAndRunTests(
        string appDirectory,
        int workers,
        Func<IInvocationCacheAccess, PineVMSharedCaches, IPineVM> pineVmFactory,
        string? filter = null,
        bool listTests = false,
        Action<int>? onTestsDiscovered = null,
        ElmDependencyResolutionConfiguration? resolutionConfiguration = null,
        IElmPackageProvider? packageProvider = null,
        Action<ElmDependencyResolutionReport>? onDependenciesResolved = null,
        ElmFuzzOptions? fuzzOptions = null,
        ElmTestInstrumentation? instrumentation = null)
    {
        ArgumentNullException.ThrowIfNull(pineVmFactory);

        return
            CompileAndRunTests(
                appDirectory,
                pineVm: null,
                filter,
                listTests,
                workers,
                pineVmFactory,
                onTestsDiscovered,
                resolutionConfiguration,
                packageProvider,
                onDependenciesResolved,
                fuzzOptions,
                instrumentation);
    }


    private static ElmTestRun CompileAndRunTests(
        string appDirectory,
        IPineVM? pineVm,
        string? filter,
        bool listTests,
        int workers,
        Func<IInvocationCacheAccess, PineVMSharedCaches, IPineVM>? pineVmFactory,
        Action<int>? onTestsDiscovered,
        ElmDependencyResolutionConfiguration? resolutionConfiguration,
        IElmPackageProvider? packageProvider,
        Action<ElmDependencyResolutionReport>? onDependenciesResolved,
        ElmFuzzOptions? fuzzOptions,
        ElmTestInstrumentation? instrumentation)
    {
        if (workers < 1)
            throw new ArgumentOutOfRangeException(nameof(workers), "Worker count must be at least one.");

        if (instrumentation is not null && pineVm is not null)
            throw new ArgumentException("Budgeted or profiled execution uses its own VMs.", nameof(pineVm));

        using var compilationScope = instrumentation?.EnterScope("compilation", appDirectory);

        var executionSettings = (fuzzOptions ?? new()).Resolve();

        if (instrumentation is { RecordDiagnostics: true })
        {
            instrumentation.Metadata["ProjectDirectory"] = Path.GetFullPath(appDirectory);
            instrumentation.Metadata["Filter"] = filter ?? "";

            instrumentation.Metadata["Seed"] =
                executionSettings.Seed.ToString(System.Globalization.CultureInfo.InvariantCulture);

            instrumentation.Metadata["FuzzRuns"] =
                executionSettings.FuzzRuns.ToString(System.Globalization.CultureInfo.InvariantCulture);
        }

        appDirectory = Path.GetFullPath(appDirectory);

        if (!Directory.Exists(appDirectory))
            throw new DirectoryNotFoundException("Elm project directory not found: " + appDirectory);

        var compilationStopwatch = Stopwatch.StartNew();

        var appFiles =
            Filesystem.GetFilesFromDirectory(
                appDirectory,
                filterByRelativeName:
                path =>
                !path.Any(
                    segment =>
                    segment is ".git" or "elm-stuff"))
            .Select(file => (file.path, file.content))
            .ToList();

        var appCodeTreeWithoutPackages = FileTree.FromSetOfFilesWithStringPath(appFiles);

        var nestedProjects =
            appFiles.Where(file => file.path.Count > 1 && file.path[^1] is "elm.json")
            .Select(file => file.path.Take(file.path.Count - 1).ToArray()).ToArray();

        var testModules =
            appFiles
            .Where(
                file =>
                file.path.Count > 1 &&
                file.path[0] is "tests" &&
                !nestedProjects.Any(project => file.path.Take(project.Length).SequenceEqual(project)) &&
                file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(
                file =>
                {
                    var parsedModule =
                        ElmSyntaxParser.ParseModuleText(
                            Encoding.UTF8.GetString(file.content.Span))
                        .Extract(
                            error =>
                            throw new InvalidOperationException(
                                "Failed parsing Elm test module: " + error));

                    var moduleName =
                        SyntaxTypes.Module.GetModuleName(parsedModule.ModuleDefinition.Value).Value;

                    var exposedZeroParameterDeclarations =
                        parsedModule.Declarations
                        .Select(declaration => declaration.Value)
                        .OfType<SyntaxTypes.Declaration.FunctionDeclaration>()
                        .Where(
                            declaration =>
                            declaration.Function.Declaration.Value.Arguments.Count is 0)
                        .Select(
                            declaration =>
                            declaration.Function.Declaration.Value.Name.Value)
                        .Where(
                            declarationName =>
                            IsDeclarationExposed(
                                parsedModule.ModuleDefinition.Value,
                                declarationName))
                        .ToImmutableArray();

                    return
                        (file.path,
                        filePathText: string.Join('/', file.path),
                        moduleName,
                        moduleNameText: string.Join('.', moduleName),
                        exposedZeroParameterDeclarations);
                })
            .OrderBy(testModule => testModule.moduleNameText, StringComparer.Ordinal)
            .ToImmutableArray();

        if (testModules.Length is 0)
        {
            if (instrumentation is { RecordDiagnostics: true })
                throw new ElmTestInstrumentationSelectionException(0, 0, filter, []);

            return new ElmTestRun.NoTestModules(appDirectory);
        }

        resolutionConfiguration ??= DefaultResolutionConfiguration.Value;

        if (!resolutionConfiguration.IncludeTests)
        {
            throw new ArgumentException(
                "Elm test compilation requires a configuration with IncludeTests = true.",
                nameof(resolutionConfiguration));
        }

        var build =
            ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                appCodeTreeWithoutPackages,
                ["elm.json"],
                [.. testModules.Select(testModule => testModule.path)],
                resolutionConfiguration,
                packageProvider,
                cancellationToken: instrumentation?.CancellationToken ?? default,
                projectDirectory: appDirectory).GetAwaiter().GetResult();

        onDependenciesResolved?.Invoke(build.Resolution);

        if (instrumentation is not null && build.Resolution.Fingerprint is { } fingerprint)
            instrumentation.Metadata["ResolutionFingerprint"] = fingerprint;

        var testDeclarationNames =
            testModules
            .SelectMany(
                testModule =>
                testModule.exposedZeroParameterDeclarations.Select(
                    declarationName =>
                    DeclQualifiedName.Create(testModule.moduleName, declarationName)))
            .ToImmutableArray();

        string[] bridgePath = ["elm-packages", "elm-explorations", "test", "src", "PineTestBridge.elm"];

        if (build.Sources.GetNodeAtPath(bridgePath) is null)
        {
            throw new InvalidOperationException(
                "The configured test substitution does not implement Pine's fuzz execution bridge.");
        }

        var bridgeName = build.CompilerModuleNames[string.Join("/", bridgePath)];

        var compilationRoots =
            testDeclarationNames.Add(DeclQualifiedName.Create(bridgeName.Split('.'), "prepare"));

        var preparationCaches = new PineVMSharedCaches();

        var preparationVm =
            instrumentation is not null
            ?
            instrumentation.CreateVm(new ConcurrentInvocationCache(), preparationCaches)
            :
            CreatePineVm(
                new ConcurrentInvocationCache(),
                preparationCaches);

        var (compiledEnvironment, _) =
            ElmCompiler.CompileResolvedEnvironment(
                build,
                rootDeclarations: compilationRoots)
            .Extract(error => throw new ElmCompilationException("Failed compiling Elm tests: " + error));

        var parsedEnvironment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnvironment)
            .Extract(error => throw new InvalidOperationException("Failed parsing compiled Elm tests: " + error));

        instrumentation?.RegisterDeclarations(parsedEnvironment);

        var discoveredTests = new List<DiscoveredTest>();

        var prepareFunction =
            FunctionRecord.ParseFunctionRecordTagged(
                parsedEnvironment.Modules.Single(module => module.moduleName == bridgeName).moduleContent.FunctionDeclarations["prepare"],
                preparationCaches.ParsedExpressions).Extract(error => throw new InvalidOperationException(error));

        var hasOnly = false;
        var hasSkipped = false;

        foreach (var testModule in testModules)
        {
            var compiledTestModule =
                parsedEnvironment.Modules
                .FirstOrDefault(module => module.moduleName == testModule.moduleNameText);

            if (compiledTestModule.moduleContent is null)
            {
                throw new InvalidOperationException(
                    "Did not find compiled Elm module '" + testModule.moduleNameText + "'");
            }

            foreach (var declarationName in testModule.exposedZeroParameterDeclarations)
            {
                using var preparationScope =
                    instrumentation?.EnterScope("preparation",
                        testModule.filePathText + " (" + testModule.moduleNameText + "." + declarationName + ")",
                        testModule.filePathText + "/**");

                if (!compiledTestModule.moduleContent.FunctionDeclarations.TryGetValue(
                    declarationName,
                    out var declarationWrapper))
                {
                    throw new InvalidOperationException(
                        "Did not find declaration '" +
                        testModule.moduleNameText + "." + declarationName + "'");
                }

                var declarationValue =
                    ElmSourceCompilation.EvaluateZeroParameterRoot(
                        declarationWrapper,
                        preparationVm,
                        preparationCaches.ParsedExpressions)
                    .Extract(
                        error => throw new InvalidOperationException("Failed constructing Elm test value: " + error));

                if (!IsTestValue(declarationValue))
                    continue;

                var prepared =
                    ElmInteractiveEnvironment.ApplyFunction(
                        preparationVm,
                        prepareFunction,
                        [
                        IntegerEncoding.EncodeSignedInteger(executionSettings.FuzzRuns),
                        IntegerEncoding.EncodeSignedInteger(executionSettings.Seed), declarationValue
                        ])
                    .Extract(error => throw new InvalidOperationException("Failed preparing Elm tests: " + error));

                var (preparedTag, preparedArguments) = ParseTaggedValue(prepared);

                if (preparedTag is "Invalid")
                {
                    discoveredTests.Add(
                        new(testModule.filePathText, [declarationName], DiscoveredTestKind.Invalid, null)
                        {
                            PreparationError = ParseElmString(preparedArguments.Span[0])
                        });

                    continue;
                }

                if (preparedTag is not ("Plain" or "Only" or "Skipping") ||
                    preparedArguments.Span[0] is not PineValue.ListValue runners)
                    throw new InvalidOperationException("Invalid seeded test runners: " + preparedTag);

                hasOnly |= preparedTag is "Only";
                hasSkipped |= preparedTag is "Skipping";

                foreach (var runner in runners.Items.Span)
                {
                    var labels = ParseList(Field(runner, "labels")).Select(ParseElmString).Reverse().ToArray();
                    var kind = ParseElmString(Field(runner, "kind"));
                    var path = labels.Length is 0 ? [declarationName] : labels;

                    discoveredTests.Add(
                        new(
                            testModule.filePathText,
                            path,
                            kind switch
                            {
                                "todo" => DiscoveredTestKind.Todo,
                                "empty" => DiscoveredTestKind.EmptyGroup,
                                "unit" or "fuzz" => DiscoveredTestKind.Runnable,

                                _ =>
                                throw new InvalidOperationException("Unknown runner kind: " + kind),
                            },
                            Field(runner, "run"))
                        {
                            Only = preparedTag is "Only",
                            Fuzz =
                            kind is "fuzz"
                            ?
                            new(
                                executionSettings,
                                ParseElmString(Field(runner, "seedState")),
                                (uint)ParseInteger(Field(runner, "runs")))
                            :
                            null,
                        });
                }
            }
        }

        var allFound = discoveredTests.Where(test => test.Kind is not DiscoveredTestKind.EmptyGroup).ToArray();

        for (var index = 0; index < allFound.Length; index++)
            allFound[index].DiscoveryIndex = index + 1;

        var allFoundListed = allFound.Select(ToListedTest).ToArray();

        string ExactFilter(DiscoveredTest test) =>
            ElmTestFilter.ExactSelector(ToListedTest(test), test.DiscoveryIndex, allFoundListed);

        if (hasOnly)
            discoveredTests.RemoveAll(test => !test.Only && test.Kind != DiscoveredTestKind.Invalid);

        var selectableTests = discoveredTests.Where(test => test.Kind is DiscoveredTestKind.Runnable).ToArray();
        var filteredOutTests = new List<ListedTest>();

        if (filter is { } filterExpression)
        {
            var parsedFilter = new ElmTestFilter(filterExpression);

            var availableTests =
                discoveredTests
                .Where(test => test.Kind is not DiscoveredTestKind.EmptyGroup)
                .Select(ToListedTest)
                .ToArray();

            discoveredTests.RemoveAll(
                test =>
                {
                    var listedTest = ToListedTest(test);

                    if (parsedFilter.Matches(listedTest, test.DiscoveryIndex))
                        return false;

                    if (test.Kind is not DiscoveredTestKind.EmptyGroup)
                        filteredOutTests.Add(listedTest);

                    return true;
                });

            if (!discoveredTests.Any(test => test.Kind is not DiscoveredTestKind.EmptyGroup))
            {
                if (instrumentation is { RecordDiagnostics: true })
                {
                    throw new ElmTestInstrumentationSelectionException(
                        allFound.Length,
                        0,
                        filter,
                        [
                        .. selectableTests.Take(5)
                        .Select(test => new ElmTestSelectionSuggestion(ToListedTest(test), ExactFilter(test)))
                        ],
                        suggestionsFromRemaining: false);
                }

                return
                    new ElmTestRun.NoMatchingTests(
                        filterExpression,
                        ElmTestFilter.FindClosestTests(availableTests, parsedFilter))
                    {
                        FilteredOutTests = availableTests
                    };
            }
        }

        if (instrumentation is { RecordDiagnostics: true })
        {
            var remaining = discoveredTests.Where(test => test.Kind is not DiscoveredTestKind.EmptyGroup).ToArray();

            if (remaining.Length != 1 || remaining[0].Kind is not DiscoveredTestKind.Runnable)
            {
                throw new ElmTestInstrumentationSelectionException(
                    allFound.Length,
                    remaining.Length,
                    filter,
                    [
                    .. remaining.Where(test => test.Kind is DiscoveredTestKind.Runnable).Take(5)
                    .Select(test => new ElmTestSelectionSuggestion(ToListedTest(test), ExactFilter(test)))
                    ]);
            }

            discoveredTests = [remaining[0]];
            instrumentation.SelectTest(ToListedTest(discoveredTests[0]).FullPath);
        }

        if (listTests)
        {
            return
                new ElmTestRun.Listed(
                    [
                    ..                    discoveredTests
                    .Where(test => test.Kind is not DiscoveredTestKind.EmptyGroup)
                    .Select(ToListedTest)
                    ],
                    filteredOutTests);
        }

        compilationStopwatch.Stop();

        onTestsDiscovered?.Invoke(discoveredTests.Count);

        var stopwatch = Stopwatch.StartNew();

        IReadOnlyList<CompletedTest> completedTests;

        if (pineVm is not null)
        {
            var parseCache = new PineVMParseCache();

            completedTests =
                [.. discoveredTests.Select(test => RunTest(test, pineVm, parseCache))];
        }
        else
        {
            var sharedInvocationCache = new ConcurrentInvocationCache();
            var sharedPineVMCaches = new PineVMSharedCaches();
            var completedTestsByIndex = new CompletedTest[discoveredTests.Count];
            var nextTestIndex = -1;

            var workerCount =
                Math.Min(instrumentation is { RecordDiagnostics: true } ? 1 : workers, discoveredTests.Count);

            pineVmFactory ??= CreatePineVm;

            var workerTasks =
                Enumerable.Range(0, workerCount)
                .Select(
                    _ =>
                    Task.Run(
                        () =>
                        {
                            var invocationCache =
                                new BufferedInvocationCacheAccess(sharedInvocationCache);

                            try
                            {
                                while (Interlocked.Increment(ref nextTestIndex) is var testIndex &&
                                    testIndex < discoveredTests.Count)
                                {
                                    using var executionScope =
                                        instrumentation?.EnterScope("execution", ToListedTest(discoveredTests[testIndex]).FullPath,
                                            ExactFilter(discoveredTests[testIndex]));

                                    var testPineVm =
                                        instrumentation?.CreateVm(invocationCache, sharedPineVMCaches)
                                        ?? pineVmFactory(invocationCache, sharedPineVMCaches);

                                    completedTestsByIndex[testIndex] =
                                        RunTest(
                                            discoveredTests[testIndex],
                                            testPineVm,
                                            sharedPineVMCaches.ParsedExpressions);

                                    invocationCache.MergeIntoShared();
                                }
                            }
                            finally
                            {
                                invocationCache.MergeIntoShared();
                            }
                        }))
                .ToArray();

            Task.WhenAll(workerTasks).GetAwaiter().GetResult();

            completedTests = completedTestsByIndex;
        }

        stopwatch.Stop();

        return
            new ElmTestRun.Completed(
                completedTests,
                compilationStopwatch.Elapsed + stopwatch.Elapsed)
            {
                CompilationDuration = compilationStopwatch.Elapsed,
                ExecutionSettings = executionSettings,
                IncompleteReason =
                hasOnly
                ?
                "Test.only was used; the complete suite was not run."
                :
                hasSkipped ? "Test.skip was used; the complete suite was not run." : null,
                ResolutionFingerprint = build.Resolution.Fingerprint,
                Resolution = build.Resolution,
            };
    }


    private static ListedTest ToListedTest(DiscoveredTest test) =>
        new(
            test.FilePath,
            [.. test.Path.SkipLast(1)],
            test.Path[^1]);


    internal static FileTree AddPackageSources(
        FileTree appCodeTree,
        IEnumerable<(string packageName, FileTree files)> packages)
    {
        foreach (var (packageName, packageFiles) in packages)
            appCodeTree = ElmResolvedBuildPreparation.AddPackageSources(appCodeTree, packageName, packageFiles);

        return appCodeTree;
    }

    internal static IReadOnlyDictionary<string, (FileTree files, ElmJsonStructure elmJson)>
        LoadPackagesForTestCompilation(
        FileTree appCodeTree,
        Func<string, string, IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>? loadPackage = null) =>
        ElmAppDependencyResolution.LoadPackagesForElmApp(
            FileTreeExtensions.ToFlatDictionaryWithPathComparer(appCodeTree),
            loadPackage: loadPackage,
            configuration: DefaultResolutionConfiguration.Value);


    private static CompletedTest RunTest(
        DiscoveredTest discoveredTest,
        IPineVM pineVm,
        PineVMParseCache parseCache)
    {
        if (discoveredTest.Kind is DiscoveredTestKind.Invalid)
        {
            return
                new(
                    discoveredTest.Path,
                    CompletedTestKind.Failed,
                    new MessageFailure(discoveredTest.PreparationError!));
        }

        if (discoveredTest.Kind is DiscoveredTestKind.Todo)
        {
            return
                new CompletedTest(
                    discoveredTest.Path,
                    CompletedTestKind.Todo,
                    failure: null);
        }

        if (discoveredTest.Kind is DiscoveredTestKind.EmptyGroup)
        {
            return
                new CompletedTest(
                    discoveredTest.Path,
                    CompletedTestKind.FailedEmptyGroup,
                    failure: null);
        }

        if (discoveredTest.Thunk is null)
            throw new InvalidOperationException("Runnable test has no thunk");

        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(discoveredTest.Thunk, parseCache)
            .Extract(error => throw new InvalidOperationException("Failed parsing test thunk: " + error));

        var expectationResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVm,
                functionRecord,
                [PineValue.EmptyList]);

        if (expectationResult.IsErrOrNull() is { } evaluationError)
        {
            return
                new CompletedTest(
                    discoveredTest.Path,
                    CompletedTestKind.Failed,
                    new MessageFailure(
                        message: "Failed evaluating test: " + evaluationError))
                {
                    Fuzz = discoveredTest.Fuzz is { } fuzz ? fuzz with { EvaluationError = evaluationError } : null,
                };
        }

        if (expectationResult.IsOkOrNull() is not { } expectationValue)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + expectationResult.GetType().FullName);
        }

        var expectations = ParseList(expectationValue);

        if (expectations.Count is 0)
            throw new InvalidOperationException("A runnable test returned no expectations.");

        var expectation =
            expectations.FirstOrDefault(value => ParseTaggedValue(value).tag is "Fail") ?? expectations[0];

        var (expectationTag, expectationArguments) = ParseTaggedValue(expectation);
        var record = expectationArguments.Span[0];
        var fuzzResult = discoveredTest.Fuzz;

        if (fuzzResult is not null)
        {
            fuzzResult = fuzzResult with { DistributionReport = RenderElm(Field(record, "distributionReport")) };
            var (detailsTag, detailsArguments) = ParseTaggedValue(Field(record, "fuzzDetails"));

            if (detailsTag is "Just")
            {
                var details = detailsArguments.Span[0];
                var iteration = (uint)ParseInteger(Field(details, "failingIteration"));

                fuzzResult =
                    fuzzResult with
                    {
                        RunsRequested = (uint)ParseInteger(Field(details, "runsRequested")),
                        RunsElapsed = (uint)ParseInteger(Field(details, "runsElapsed")),
                        FailingIteration = iteration is 0 ? null : iteration,
                        OriginalInput = iteration is 0 ? null : ParseElmString(Field(details, "originalInput")),
                        ShrunkInput = iteration is 0 ? null : ParseElmString(Field(details, "shrunkInput")),
                        OriginalChoices = [.. ParseList(Field(details, "originalChoices")).Select(ParseInteger)],
                        ShrunkChoices = [.. ParseList(Field(details, "shrunkChoices")).Select(ParseInteger)],
                        ShrinkingCompleted = iteration is not 0,
                    };
            }
        }

        if (expectationTag is "Pass")
            return new(discoveredTest.Path, CompletedTestKind.Passed, null) { Fuzz = fuzzResult };

        if (expectationTag is not "Fail")
            throw new InvalidOperationException("Unsupported expectation tag: " + expectationTag);

        var description = ParseElmString(Field(record, "description"));
        var (reason, reasonArguments) = ParseTaggedValue(Field(record, "reason"));

        if (fuzzResult is not null)
            fuzzResult = fuzzResult with { FailureReason = RenderElm(Field(record, "reason")) };

        TestFailure failure =
            reason switch
            {
                "Equality" or "Comparison" =>
                new EqualityFailure(
                    description,
                    ParseElmString(reasonArguments.Span[1]),
                    ParseElmString(reasonArguments.Span[0])),

                "ListDiff" =>
                new EqualityFailure(
                    description,
                    "[ " + string.Join(", ", ParseList(reasonArguments.Span[1]).Select(ParseElmString)) + " ]",
                    "[ " + string.Join(", ", ParseList(reasonArguments.Span[0]).Select(ParseElmString)) + " ]"),

                "CollectionDiff" =>
                new EqualityFailure(
                    description,
                    ParseElmString(Field(reasonArguments.Span[0], "actual")),
                    ParseElmString(Field(reasonArguments.Span[0], "expected"))),

                "Custom" or "TODO" or "Invalid" => new MessageFailure(description),

                _ =>
                throw new InvalidOperationException("Unsupported test failure reason: " + reason),
            };

        return
            new(discoveredTest.Path, reason is "TODO" ? CompletedTestKind.Todo : CompletedTestKind.Failed, failure)
            {
                Fuzz = fuzzResult
            };
    }

    private static bool IsDeclarationExposed(
        SyntaxTypes.Module module,
        string declarationName)
    {
        var exposing =
            module switch
            {
                SyntaxTypes.Module.NormalModule normalModule =>
                normalModule.ModuleData.ExposingList.Value,

                SyntaxTypes.Module.PortModule portModule =>
                portModule.ModuleData.ExposingList.Value,

                SyntaxTypes.Module.EffectModule effectModule =>
                effectModule.ModuleData.ExposingList.Value,

                _ =>
                throw new NotImplementedException(
                    nameof(IsDeclarationExposed) +
                    " does not handle module variant: " + module.GetType().Name)
            };

        return
            exposing switch
            {
                SyntaxTypes.Exposing.All =>
                true,

                SyntaxTypes.Exposing.Explicit explicitExposing =>
                explicitExposing.Nodes.Nodes.Any(
                    exposed =>
                    exposed.Value is SyntaxTypes.TopLevelExpose.FunctionExpose functionExpose &&
                    functionExpose.Name == declarationName),

                _ =>
                throw new NotImplementedException(
                    nameof(IsDeclarationExposed) +
                    " does not handle exposing variant: " + exposing.GetType().Name)
            };
    }


    private static bool IsTestValue(PineValue value)
    {
        var parseResult = ElmValueEncoding.ParseAsTag(value);

        if (parseResult.IsOkOrNullable() is not { } tagged)
            return false;

        return
            tagged.tagName.StartsWith("ElmTestVariant__", StringComparison.Ordinal) ||
            tagged.tagName is "PineTodo" or "PineEmptyGroup";
    }


    /// <summary>
    /// Renders completed Elm tests as styled output fragments.
    /// </summary>
    public static StructuredTestOutput RenderTestResults(
        IReadOnlyList<CompletedTest> tests,
        bool includeTestDetails,
        TimeSpan? duration = null,
        TimeSpan? compilationDuration = null,
        bool includeRunningMessage = true,
        string? incompleteReason = null)
    {
        var fragments = new List<TestOutputFragment>();
        var passedCount = tests.Count(test => test.Kind is CompletedTestKind.Passed);

        var failedCount =
            tests.Count(
                test =>
                test.Kind is CompletedTestKind.Failed or
                CompletedTestKind.FailedEmptyGroup);

        var todoCount = tests.Count(test => test.Kind is CompletedTestKind.Todo);

        if (includeRunningMessage)
        {
            Append(
                "Running " + tests.Count + " test" +
                (tests.Count is 1 ? "." : "s.") + "\n\n",
                TestOutputStyle.Default);
        }

        if (includeTestDetails)
        {
            foreach (var failedTest in tests.Where(test => test.Kind is not CompletedTestKind.Passed))
            {
                if (failedTest.Kind is CompletedTestKind.Todo)
                {
                    Append(
                        "◦ TODO: " + failedTest.Path[^1] + "\n",
                        TestOutputStyle.Default);

                    continue;
                }

                if (failedTest.Kind is CompletedTestKind.FailedEmptyGroup)
                {
                    Append(
                        "\n    This `describe " + failedTest.Path[^1] +
                        "` has no tests in it. Let's give it some!\n",
                        TestOutputStyle.Failure);

                    continue;
                }

                foreach (var groupName in failedTest.Path.SkipLast(1))
                    Append("↓ " + groupName + "\n", TestOutputStyle.Dark);

                Append("✗ " + failedTest.Path[^1] + "\n", TestOutputStyle.Failure);

                if (failedTest.Fuzz is { ShrunkInput: { } given })
                    Append("\n    Given: " + given + "\n", TestOutputStyle.Default);

                if (failedTest.Failure is { } failure)
                {
                    switch (failure)
                    {
                        case EqualityFailure equalityFailure:
                            Append("\n    ", TestOutputStyle.Default);
                            AppendEqualityValue(equalityFailure.Actual, equalityFailure.Expected);

                            Append(
                                "\n    ╷" +
                                "\n    │ " + equalityFailure.Description +
                                "\n    ╵" +
                                "\n    ",
                                TestOutputStyle.Default);

                            AppendEqualityValue(equalityFailure.Expected, equalityFailure.Actual);
                            Append("\n", TestOutputStyle.Default);
                            break;

                        case MessageFailure messageFailure:
                            Append("\n    " + messageFailure.Message + "\n", TestOutputStyle.Default);
                            break;

                        default:
                            throw new NotImplementedException(
                                "RenderTestResults does not handle test failure variant: " +
                                failure.GetType().Name);
                    }
                }
            }
        }
        else if (todoCount > 0)
        {
            foreach (var todo in tests.Where(test => test.Kind is CompletedTestKind.Todo))
                Append("◦ TODO: " + todo.Path[^1] + "\n", TestOutputStyle.Default);
        }
        else
        {
            foreach (var emptyGroup in tests.Where(test => test.Kind is CompletedTestKind.FailedEmptyGroup))
            {
                Append(
                    "\n    This `describe " + emptyGroup.Path[^1] +
                    "` has no tests in it. Let's give it some!\n",
                    TestOutputStyle.Failure);
            }
        }

        if (failedCount > 0)
        {
            Append("\n\nTEST RUN FAILED", TestOutputStyle.FailureHeadline);
            Append("\n\n", TestOutputStyle.Failure);
        }
        else if (todoCount > 0)
        {
            Append("\nTEST RUN INCOMPLETE", TestOutputStyle.TodoHeadline);

            Append(
                " because there " + (todoCount is 1 ? "is " : "are ") + todoCount +
                " TODO" + (todoCount is 1 ? "" : "s") + " remaining\n\n",
                TestOutputStyle.Todo);
        }
        else if (incompleteReason is not null)
        {
            Append("\nTEST RUN INCOMPLETE\n" + incompleteReason + "\n\n", TestOutputStyle.TodoHeadline);
        }
        else
        {
            Append("\nTEST RUN PASSED", TestOutputStyle.SuccessHeadline);
            Append("\n\n", TestOutputStyle.Success);
        }

        if (duration is { } elapsed)
        {
            Append("Duration: ", TestOutputStyle.Dark);

            Append(
                Math.Round(elapsed.TotalMilliseconds).ToString(System.Globalization.CultureInfo.InvariantCulture) +
                " ms\n",
                TestOutputStyle.Default);

            if (compilationDuration is { } compilationElapsed)
            {
                Append("  Compilation:    ", TestOutputStyle.Dark);
                Append(FormatDuration(compilationElapsed) + "\n", TestOutputStyle.Default);
                Append("  Test execution: ", TestOutputStyle.Dark);
                Append(FormatDuration(elapsed - compilationElapsed) + "\n", TestOutputStyle.Default);
            }
        }

        Append("Passed:   ", TestOutputStyle.Dark);
        Append(passedCount + "\n", TestOutputStyle.Default);
        Append("Failed:   ", TestOutputStyle.Dark);
        Append(failedCount.ToString(), TestOutputStyle.Default);

        if (todoCount > 0)
        {
            Append("\nTodo:     ", TestOutputStyle.Dark);
            Append(todoCount.ToString(), TestOutputStyle.Default);
        }

        return new StructuredTestOutput(fragments);

        void Append(string text, TestOutputStyle style) =>
            fragments.Add(new TestOutputFragment(text, style));

        static string FormatDuration(TimeSpan elapsed) =>
            Math.Round(elapsed.TotalMilliseconds).ToString(System.Globalization.CultureInfo.InvariantCulture) +
            " ms";

        void AppendEqualityValue(string value, string other)
        {
            var commonPrefixLength = 0;

            while (commonPrefixLength < value.Length &&
                commonPrefixLength < other.Length &&
                value[commonPrefixLength] == other[commonPrefixLength])
            {
                commonPrefixLength++;
            }

            var commonSuffixLength = 0;

            while (commonSuffixLength < value.Length - commonPrefixLength &&
                commonSuffixLength < other.Length - commonPrefixLength &&
                value[value.Length - commonSuffixLength - 1] ==
                other[other.Length - commonSuffixLength - 1])
            {
                commonSuffixLength++;
            }

            Append(value[..commonPrefixLength], TestOutputStyle.Default);

            Append(
                value[commonPrefixLength..(value.Length - commonSuffixLength)],
                TestOutputStyle.Highlighted);

            if (commonSuffixLength > 0)
                Append(value[^commonSuffixLength..], TestOutputStyle.Default);
        }
    }


    private static IntermediatePineVM CreatePineVm(
        IInvocationCacheAccess invocationCache,
        PineVMSharedCaches sharedCaches) =>
        IntermediatePineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: DefaultEvaluationConfig,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: false,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: true,
            parseCache: sharedCaches.ParsedExpressions,
            precompiledLeaves: SetupVM.DefaultPrecompiledLeaves,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null,
            invocationCache: invocationCache,
            tryGetExpressionCompilation: sharedCaches.ExpressionCompilations.TryGet,
            getOrAddExpressionCompilation: sharedCaches.ExpressionCompilations.GetOrAdd,
            expressionEncodingCache: sharedCaches.EncodedExpressions,
            reducedExpressionCache: sharedCaches.ReducedExpressions);


    private static (string tag, ReadOnlyMemory<PineValue> arguments) ParseTaggedValue(PineValue value)
    {
        var tagged =
            ElmValueEncoding.ParseAsTag(value)
            .Extract(error => throw new InvalidOperationException("Failed parsing tagged Elm value: " + error));

        return (tagged.tagName, tagged.tagArguments);
    }


    private static string ParseElmString(PineValue value) =>
        ElmValueEncoding.PineValueAsElmValue(value, null, null)
        .Map(
            elmValue =>
            elmValue is ElmValue.ElmString elmString
            ?
            elmString.Value
            :
            throw new InvalidOperationException(
                "Expected Elm string, got " + elmValue.GetType().Name))
        .Extract(error => throw new InvalidOperationException("Failed parsing Elm string: " + error));

    private static PineValue Field(PineValue value, string name) =>
        ElmValueEncoding.ParsePineValueAsRecordTagged(value)
        .Extract(error => throw new InvalidOperationException("Invalid test runner record: " + error))
        .Single(field => field.fieldName == name).fieldValue;

    private static IReadOnlyList<PineValue> ParseList(PineValue value) =>
        value is PineValue.ListValue list
        ?
        list.Items.ToArray()
        :
        throw new InvalidOperationException("Expected an Elm list in test runner protocol.");

    private static long ParseInteger(PineValue value) =>
        (long)IntegerEncoding.ParseSignedIntegerStrict(value).Extract(error => throw new InvalidOperationException(error));

    private static string RenderElm(PineValue value) =>
        ElmValueEncoding.PineValueAsElmValue(value, null, null)
        .Extract(error => throw new InvalidOperationException(error)).ToString();

    private enum DiscoveredTestKind
    {
        Runnable,
        Todo,
        EmptyGroup,
        Invalid,
    }


    private sealed record DiscoveredTest(
        string FilePath,
        IReadOnlyList<string> Path,
        DiscoveredTestKind Kind,
        PineValue? Thunk)
    {
        public int DiscoveryIndex { get; set; }

        public bool Only { get; init; }

        public ElmFuzzResult? Fuzz { get; init; }

        public string? PreparationError { get; init; }
    }


}


/// <summary>
/// Identifies the outcome of a completed Elm test.
/// </summary>
public enum CompletedTestKind
{
    /// <summary>
    /// The test passed.
    /// </summary>
    Passed,

    /// <summary>
    /// The test failed.
    /// </summary>
    Failed,

    /// <summary>
    /// The test remains to be implemented.
    /// </summary>
    Todo,

    /// <summary>
    /// The test group failed because it contained no tests.
    /// </summary>
    FailedEmptyGroup,
}


/// <summary>
/// Identifies the presentation style of a test output fragment.
/// </summary>
public enum TestOutputStyle
{
    /// <summary>
    /// Uses the default presentation.
    /// </summary>
    Default,

    /// <summary>
    /// Uses subdued presentation.
    /// </summary>
    Dark,

    /// <summary>
    /// Presents a successful result.
    /// </summary>
    Success,

    /// <summary>
    /// Presents a successful result headline.
    /// </summary>
    SuccessHeadline,

    /// <summary>
    /// Presents a failed result.
    /// </summary>
    Failure,

    /// <summary>
    /// Presents a failed result headline.
    /// </summary>
    FailureHeadline,

    /// <summary>
    /// Presents an incomplete test.
    /// </summary>
    Todo,

    /// <summary>
    /// Presents an incomplete test headline.
    /// </summary>
    TodoHeadline,

    /// <summary>
    /// Highlights the differing part of a value.
    /// </summary>
    Highlighted,
}


/// <summary>
/// Describes the reason an Elm test failed.
/// </summary>
public abstract record TestFailure;


/// <summary>
/// Describes a failed equality expectation.
/// </summary>
public sealed record EqualityFailure : TestFailure
{
    /// <summary>
    /// Creates a failed equality expectation.
    /// </summary>
    public EqualityFailure(string actual, string expected)
        : this("Expect.equal", actual, expected)
    {
    }

    /// <summary>
    /// Creates a failed comparison expectation.
    /// </summary>
    public EqualityFailure(string description, string actual, string expected)
    {
        Description = description;
        Actual = actual;
        Expected = expected;
    }

    /// <summary>
    /// Gets the expectation function description.
    /// </summary>
    public string Description { get; init; }

    /// <summary>
    /// Gets the actual value.
    /// </summary>
    public string Actual { get; init; }

    /// <summary>
    /// Gets the expected value.
    /// </summary>
    public string Expected { get; init; }

    /// <summary>
    /// Deconstructs the failed equality expectation.
    /// </summary>
    public void Deconstruct(out string actual, out string expected)
    {
        actual = Actual;
        expected = Expected;
    }
}


/// <summary>
/// Describes a failed expectation with a message.
/// </summary>
public sealed record MessageFailure : TestFailure
{
    /// <summary>
    /// Creates a failed expectation with a message.
    /// </summary>
    public MessageFailure(string message)
    {
        Message = message;
    }

    /// <summary>
    /// Gets the failure message.
    /// </summary>
    public string Message { get; init; }

    /// <summary>
    /// Deconstructs the failed expectation.
    /// </summary>
    public void Deconstruct(out string message)
    {
        message = Message;
    }
}


/// <summary>
/// Describes a completed Elm test.
/// </summary>
public sealed record CompletedTest
{
    /// <summary>
    /// Creates a completed Elm test.
    /// </summary>
    public CompletedTest(
        IReadOnlyList<string> path,
        CompletedTestKind kind,
        TestFailure? failure)
    {
        Path = path;
        Kind = kind;
        Failure = failure;
    }

    /// <summary>
    /// Gets the nested path to the test.
    /// </summary>
    public IReadOnlyList<string> Path { get; init; }

    /// <summary>
    /// Gets the test outcome.
    /// </summary>
    public CompletedTestKind Kind { get; init; }

    /// <summary>
    /// Gets the failure details when the test failed.
    /// </summary>
    public TestFailure? Failure { get; init; }

    /// <summary>Generation, replay and shrinking metadata, present only for a fuzz property.</summary>
    public ElmFuzzResult? Fuzz { get; init; }

    /// <summary>
    /// Deconstructs the completed Elm test.
    /// </summary>
    public void Deconstruct(
        out IReadOnlyList<string> path,
        out CompletedTestKind kind,
        out TestFailure? failure)
    {
        path = Path;
        kind = Kind;
        failure = Failure;
    }
}


/// <summary>
/// Describes a discovered Elm test.
/// </summary>
public sealed record ListedTest
{
    /// <summary>
    /// Creates a description of a discovered Elm test.
    /// </summary>
    public ListedTest(
        string filePath,
        IReadOnlyList<string> descriptionPath,
        string name)
    {
        FilePath = filePath;
        DescriptionPath = descriptionPath;
        Name = name;
    }

    /// <summary>
    /// Gets the test module's path relative to the Elm project.
    /// </summary>
    public string FilePath { get; init; }

    /// <summary>
    /// Gets the path of nested descriptions containing the test.
    /// </summary>
    public IReadOnlyList<string> DescriptionPath { get; init; }

    /// <summary>
    /// Gets the test name.
    /// </summary>
    public string Name { get; init; }

    /// <summary>
    /// Gets the project-relative file, description, and test path with portable separators.
    /// </summary>
    public string FullPath =>
        string.Join('/', new[] { FilePath.Replace('\\', '/') }.Concat(DescriptionPath).Append(Name));

    /// <inheritdoc/>
    public bool Equals(ListedTest? other) =>
        ReferenceEquals(this, other) ||
        (other is not null &&
        FilePath == other.FilePath &&
        DescriptionPath.SequenceEqual(other.DescriptionPath, StringComparer.Ordinal) &&
        Name == other.Name);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new HashCode();

        hashCode.Add(FilePath, StringComparer.Ordinal);

        foreach (var description in DescriptionPath)
            hashCode.Add(description, StringComparer.Ordinal);

        hashCode.Add(Name, StringComparer.Ordinal);

        return hashCode.ToHashCode();
    }
}


/// <summary>
/// Contains a styled fragment of rendered test output.
/// </summary>
public sealed record TestOutputFragment
{
    /// <summary>
    /// Creates a styled test output fragment.
    /// </summary>
    public TestOutputFragment(string text, TestOutputStyle style)
    {
        Text = text;
        Style = style;
    }

    /// <summary>
    /// Gets the fragment text.
    /// </summary>
    public string Text { get; init; }

    /// <summary>
    /// Gets the fragment style.
    /// </summary>
    public TestOutputStyle Style { get; init; }

    /// <summary>
    /// Deconstructs the styled test output fragment.
    /// </summary>
    public void Deconstruct(out string text, out TestOutputStyle style)
    {
        text = Text;
        style = Style;
    }
}


/// <summary>
/// Contains rendered test output.
/// </summary>
public sealed record StructuredTestOutput
{
    /// <summary>
    /// Creates structured test output.
    /// </summary>
    public StructuredTestOutput(IReadOnlyList<TestOutputFragment> fragments)
    {
        Fragments = fragments;
    }

    /// <summary>
    /// Gets the styled output fragments.
    /// </summary>
    public IReadOnlyList<TestOutputFragment> Fragments { get; init; }

    /// <summary>
    /// Gets the output without style information.
    /// </summary>
    public string PlainText =>
        string.Concat(Fragments.Select(fragment => fragment.Text));

    /// <summary>
    /// Deconstructs the structured test output.
    /// </summary>
    public void Deconstruct(out IReadOnlyList<TestOutputFragment> fragments)
    {
        fragments = Fragments;
    }
}


/// <summary>
/// Contains the results of an Elm test run.
/// </summary>
public abstract record ElmTestRun
{
    /// <summary>
    /// Contains the results of a completed Elm test run.
    /// </summary>
    public sealed record Completed(
        IReadOnlyList<CompletedTest> Tests,
        TimeSpan Duration)
        : ElmTestRun
    {
        /// <summary>
        /// Gets the portion of <see cref="Duration"/> spent compiling and discovering tests.
        /// </summary>
        public TimeSpan CompilationDuration { get; init; }

        /// <summary>The seed and default run count selected once for this invocation.</summary>
        public ElmTestExecutionSettings? ExecutionSettings { get; init; }

        /// <summary>Reason the suite is incomplete despite any passing selected tests, such as only/skip.</summary>
        public string? IncompleteReason { get; init; }

        /// <summary>Identifies the exact compiler, substituted sources and resolved packages used.</summary>
        public string? ResolutionFingerprint { get; init; }

        /// <summary>Full dependency report, including pinned replacement source identities.</summary>
        public ElmDependencyResolutionReport? Resolution { get; init; }

        /// <summary>Exports replay settings, all property diagnostics and the dependency report without compiled functions.</summary>
        public string ToDebugJson() =>
            JsonSerializer.Serialize(
                new
                {
                    ExecutionSettings,
                    ResolutionFingerprint,
                    Resolution,
                    IncompleteReason,
                    Tests =
                    Tests.Select(
                        test => new
                        {
                            test.Path,
                            test.Kind,
                            test.Fuzz,
                            FailureKind = test.Failure?.GetType().Name,
                            Failure =
                            test.Failure is null
                            ?
                            (JsonElement?)null
                            :
                            JsonSerializer.SerializeToElement(test.Failure, test.Failure.GetType()),
                        }),
                },
                new JsonSerializerOptions { WriteIndented = true, Converters = { new JsonStringEnumConverter() } });
    }

    /// <summary>
    /// Contains tests discovered without running them.
    /// </summary>
    public sealed record Listed : ElmTestRun
    {
        /// <summary>
        /// Creates a result containing tests discovered without running them.
        /// </summary>
        public Listed(
            IReadOnlyList<ListedTest> tests,
            IReadOnlyList<ListedTest>? filteredOutTests = null)
        {
            Tests = tests;
            FilteredOutTests = filteredOutTests ?? [];
        }

        /// <summary>
        /// Gets the discovered tests remaining after filtering.
        /// </summary>
        public IReadOnlyList<ListedTest> Tests { get; init; }

        /// <summary>
        /// Gets individual tests excluded by the filter, preserving their file and group paths.
        /// </summary>
        public IReadOnlyList<ListedTest> FilteredOutTests { get; init; }

        /// <inheritdoc/>
        public bool Equals(Listed? other) =>
            ReferenceEquals(this, other) ||
            (other is not null &&
            Tests.SequenceEqual(other.Tests) &&
            FilteredOutTests.SequenceEqual(other.FilteredOutTests));

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new HashCode();

            hashCode.Add(Tests.Count);

            foreach (var test in Tests)
                hashCode.Add(test);

            hashCode.Add(FilteredOutTests.Count);

            foreach (var test in FilteredOutTests)
                hashCode.Add(test);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>
    /// Represents a test run for a project without Elm test modules.
    /// </summary>
    public sealed record NoTestModules(string AppDirectory) : ElmTestRun;

    /// <summary>
    /// Represents a filter that selected no tests, with existing tests ranked by similarity.
    /// </summary>
    public sealed record NoMatchingTests(
        string Filter,
        IReadOnlyList<ListedTest> ClosestTests) : ElmTestRun
    {
        /// <summary>
        /// Gets all individual tests excluded by the filter.
        /// </summary>
        public IReadOnlyList<ListedTest> FilteredOutTests { get; init; } = [];
    }
}
