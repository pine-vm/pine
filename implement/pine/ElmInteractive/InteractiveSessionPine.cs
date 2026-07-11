using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.IO;
using Pine.Core.PineVM;
using Pine.Elm;
using Pine.IntermediateVM;
using Pine.PineVM;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using static ElmTime.ElmInteractive.IInteractiveSession;

namespace ElmTime.ElmInteractive;

public class InteractiveSessionPine : IInteractiveSession
{
    private readonly System.Threading.Lock _submissionLock = new();

    private System.Threading.Tasks.Task<Result<string, PineValue>> _buildPineEvalContextTask;

    private readonly Result<string, ElmCompilerInElm> _buildCompilerResult;

    private readonly IPineVM _pineVM;

    private static readonly IPineVM s_compileEnvPineVM =
        new LockingPineVM(
            new PineVMWithPersistentCache(
                new FileStoreFromSystemIOFile(
                    System.IO.Path.Combine(Filesystem.CacheDirectory, "elm-compiler-vm", Program.AppVersionId))));

    /*
     * TODO: Move these caches to a dedicated scope to enable for better control over reuse/disposal.
     * */

    static readonly ElmCompilerCache elmCompilerCache = new();

    static readonly ConcurrentDictionary<string, Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>>> TryParseModuleTextCache = new();

    public InteractiveSessionPine(
        FileTree compilerSourceFiles,
        FileTree? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        bool caching,
        DynamicPGOShare? autoPGO)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree:
            appCodeTree is null ? null : AppCompilationUnits.WithoutPackages(appCodeTree),
            overrideSkipLowering: overrideSkipLowering,
            entryPointsFilePaths: entryPointsFilePaths,
            BuildPineVM(
                caching: caching,
                autoPGO: autoPGO,
                new PineVM.EvaluationConfig(InvocationCountLimit: 10_000_000, LoopIterationCountLimit: 10_000_000, StackDepthLimit: 100_000)))
    {
    }

    public InteractiveSessionPine(
        FileTree compilerSourceFiles,
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        bool caching,
        DynamicPGOShare? autoPGO)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree: appCodeTree,
            overrideSkipLowering: overrideSkipLowering,
            entryPointsFilePaths: entryPointsFilePaths,
            BuildPineVM(
                caching: caching,
                autoPGO: autoPGO,
                new PineVM.EvaluationConfig(InvocationCountLimit: 10_000_000, LoopIterationCountLimit: 10_000_000, StackDepthLimit: 100_000)))
    {
    }

    public InteractiveSessionPine(
        FileTree compilerSourceFiles,
        FileTree? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        IPineVM pineVM)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree:
            appCodeTree is null ? null : AppCompilationUnits.WithoutPackages(appCodeTree),
            overrideSkipLowering: overrideSkipLowering,
            entryPointsFilePaths: entryPointsFilePaths,
            pineVM)
    {
    }

    public InteractiveSessionPine(
        FileTree compilerSourceFiles,
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        IPineVM pineVM)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree: appCodeTree,
            overrideSkipLowering: overrideSkipLowering,
            entryPointsFilePaths: entryPointsFilePaths,
            (pineVM, pineVMCache: null))
    {
    }

    public InteractiveSessionPine(
        FileTree compilerSourceFiles,
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        (IPineVM pineVM, InvocationCache? pineVMCache) pineVMAndCache)
    {
        _pineVM = pineVMAndCache.pineVM;

        _buildCompilerResult =
            ElmCompilerInElm.GetElmCompilerAsync(compilerSourceFiles).Result;

        _buildPineEvalContextTask =
            System.Threading.Tasks.Task.Run(() =>
            CompileInteractiveEnvironment(
                appCodeTree: appCodeTree,
                overrideSkipLowering: overrideSkipLowering,
                entryPointsFilePaths: entryPointsFilePaths,
                skipFilteringForSourceDirs: false));
    }

    public static (IPineVM, InvocationCache?) BuildPineVM(
        bool caching,
        DynamicPGOShare? autoPGO,
        PineVM.EvaluationConfig? evaluationConfig = null)
    {
        var cache = caching ? new InvocationCache() : null;

        return (BuildPineVM(cache, autoPGO, evaluationConfig), cache);
    }

    public static IPineVM BuildPineVM(
        InvocationCache? cache,
        DynamicPGOShare? autoPGO,
        PineVM.EvaluationConfig? evaluationConfig)
    {
        if (autoPGO is not null)
        {
            return
                autoPGO.GetVMAutoUpdating(evalCache: cache);
        }

        return
            SetupVM.Create(
                evalCache: cache,
                evaluationConfigDefault: evaluationConfig,
                reportFunctionApplication: null);
    }

    public static Result<string, PineValue>
        CompileInteractiveEnvironment(
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        bool skipFilteringForSourceDirs)
    {
        var skipLowering =
            overrideSkipLowering ??
            !ElmCompilerInElm.CheckIfAppUsesLowering(appCodeTree?.AppFiles ?? FileTree.EmptyTree);

        var appSourceFiles =
            appCodeTree ?? AppCompilationUnits.WithoutPackages(FileTree.EmptyTree);

        var defaultKernelModulesTree =
            ElmCompilerInElm.ElmCoreAndKernelModuleFilesDefault.Value;

        var appCodeModules =
            appSourceFiles.AppFiles
            .EnumerateFilesTransitive()
            .Where(blob => blob.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blob =>
            {
                var moduleText = System.Text.Encoding.UTF8.GetString(blob.fileContent.Span);

                var moduleName =
                ElmModule.ParseModuleName(moduleText)
                .Extract(err => throw new Exception("Failed parsing module name: " + err));

                return new KeyValuePair<IReadOnlyList<string>, ReadOnlyMemory<byte>>(moduleName, blob.fileContent);
            })
            .ToImmutableDictionary(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        ReadOnlyMemory<byte>? replaceKernelModule(
            IImmutableList<string> path,
            ReadOnlyMemory<byte> blobContent)
        {
            if (!path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            {
                return null;
            }

            if (ModuleNameFromFileContent(blobContent.Span) is not { } moduleName)
            {
                return null;
            }

            if (appCodeModules.TryGetValue(moduleName, out var appCodeModule) && 0 < appCodeModule.Length)
            {
                return appCodeModule;
            }

            return null;
        }

        var mergedKernelModulesTree =
            FileTree.FromSetOfFilesWithStringPath(
                defaultKernelModulesTree.EnumerateFilesTransitive()
                .Select(blob =>
                (blob.path,
                replaceKernelModule(blob.path, blob.fileContent) is { } overrideContent
                ?
                overrideContent
                :
                blob.fileContent)));

        var mergedKernelModulesTreeBlobs =
            mergedKernelModulesTree
            .EnumerateFilesTransitive()
            .ToImmutableArray();

        var appFilesAfterKernelModules =
            FileTree.FromSetOfFilesWithStringPath(
                appSourceFiles.AppFiles.EnumerateFilesTransitive()
                .Where(blob =>
                !mergedKernelModulesTreeBlobs.Any(
                    kernelBlob =>
                    kernelBlob.fileContent.Span.SequenceEqual(blob.fileContent.Span))));

        var orderedModules =
            AppSourceFileTreesForIncrementalCompilation(
                appFilesAfterKernelModules,
                skipLowering: skipLowering,
                entryPointsFilePaths:
                entryPointsFilePaths?.ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>()),
                skipFilteringForSourceDirs: skipFilteringForSourceDirs)
            .ToImmutableArray();

        var appSourceFilesTree =
            appCodeTree is null
            ?
            FileTree.EmptyTree
            :
            appCodeTree.Packages.Aggregate(
                seed: appCodeTree.AppFiles,
                func: (files, pkg) => FileTree.MergeFiles(files, pkg.files));

        entryPointsFilePaths ??=
            appSourceFilesTree.EnumerateFilesTransitive()
            .Select(blob => blob.path)
            .ToImmutableArray();

        var compileResult =
            ElmCompiler.CompileInteractiveEnvironment(
                appSourceFilesTree,
                rootFilePaths: entryPointsFilePaths,
                syntaxOptimization: null);

        {
            if (compileResult.IsErrOrNull() is { } compileErr)
            {
                return "Failed to compile interactive environment: " + compileErr;
            }
        }

        if (compileResult.IsOkOrNullable() is not { } compileOk)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + compileResult.GetType());
        }

        return compileOk.compiledEnvValue;
    }

    public static FileTree MergeDefaultElmCoreAndKernelModules(
        FileTree appCodeTree) =>
        MergeDefaultElmCoreAndKernelModules(
            appCodeTree,
            ElmCompilerInElm.ElmCoreAndKernelModuleFilesDefault.Value);

    public static FileTree MergeDefaultElmCoreAndKernelModules(
        FileTree appCodeTree,
        FileTree elmCoreAndKernelModuleFilesDefault)
    {
        var appCodeTreeModuleNames =
            appCodeTree.EnumerateFilesTransitive()
            .SelectMany((blob) =>
            {
                var fileName = blob.path.Last();

                if (!fileName.EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
                    return (IEnumerable<IReadOnlyList<string>>)[];

                if (ModuleNameFromFileContent(blob.fileContent.Span) is not { } moduleName)
                {
                    return [];
                }

                return [moduleName];
            })
            .ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        return
            elmCoreAndKernelModuleFilesDefault
            .EnumerateFilesTransitive()
            .Aggregate(
                seed:
                appCodeTree,

                func:
                (aggregate, nextBlob) =>
                {
                    if (aggregate.GetNodeAtPath(nextBlob.path) is not null)
                        return aggregate;

                    if (ModuleNameFromFileContent(nextBlob.fileContent.Span) is { } moduleName)
                    {
                        if (appCodeTreeModuleNames.Contains(moduleName))
                            return aggregate;
                    }

                    return
                    aggregate.SetNodeAtPathSorted(
                        nextBlob.path,
                        FileTree.File(nextBlob.fileContent));
                });
    }

    static IReadOnlyList<string>? ModuleNameFromFileContent(ReadOnlySpan<byte> fileContent)
    {
        var blobChars = new char[fileContent.Length];

        if (!System.Text.Encoding.UTF8.TryGetChars(fileContent, blobChars, out var charsWritten))
            return null;

        if (ElmModule.ParseModuleName(
            new string(blobChars.AsSpan()[..charsWritten])).IsOkOrNull() is not { } moduleName)
        {
            return null;
        }

        return moduleName;
    }

    public record ParsedModule(
        IReadOnlyList<string> FilePath,
        IReadOnlyList<string> ModuleName,
        string ModuleText);

    public static IEnumerable<(FileTree tree, ParsedModule sourceModule)>
        AppSourceFileTreesForIncrementalCompilation(
        FileTree appSourceFiles,
        bool skipLowering,
        IReadOnlySet<IReadOnlyList<string>>? entryPointsFilePaths,
        bool skipFilteringForSourceDirs)
    {
        var compileableSourceModules =
            ElmInteractive.ModulesFilePathsAndTextsFromAppCodeTree(
                appSourceFiles,
                skipLowering: skipLowering,
                entryPointsFilePaths: entryPointsFilePaths,
                skipFilteringForSourceDirs: skipFilteringForSourceDirs,
                mergeKernelModules: false) ?? [];

        var baseTree =
            compileableSourceModules
            .Aggregate(
                seed: appSourceFiles,
                func: (tree, compileableModule) => tree.RemoveNodeAtPath(compileableModule.filePath) ?? throw new Exception());

        var compileableSourceModulesTexts =
            compileableSourceModules
            .Where(sm => !ElmInteractive.ShouldIgnoreSourceFile(sm.filePath, sm.fileContent))
            .Select(sm => sm.moduleText)
            .ToImmutableArray();

        var modulesTextsOrdered =
            ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: compileableSourceModulesTexts,
                availableModulesTexts: []);

        var compileableSourceModulesOrdered =
            modulesTextsOrdered
            .Select(mt => compileableSourceModules.First(c => c.moduleText == mt))
            .ToImmutableArray();

        var mutatedTree = baseTree;

        foreach (var sourceModule in compileableSourceModulesOrdered)
        {
            var moduleName =
                ElmModule.ParseModuleName(sourceModule.moduleText)
                .Extract(err => throw new Exception("Failed parsing module name: " + err));

            mutatedTree =
                mutatedTree.SetNodeAtPathSorted(sourceModule.filePath, FileTree.File(sourceModule.fileContent));

            yield return
                (mutatedTree,
                new ParsedModule(
                    FilePath: sourceModule.filePath,
                    ModuleName: moduleName,
                    ModuleText: sourceModule.moduleText));
        }
    }

    static Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>> CachedTryParseModuleText(
        string moduleText,
        ElmCompilerInElm elmCompiler,
        IPineVM pineVM) =>
        TryParseModuleTextCache
        .GetOrAdd(
            moduleText,
            valueFactory:
            moduleText => TryParseModuleText(moduleText, elmCompiler, pineVM));

    static Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>> TryParseModuleText(
        string moduleText,
        ElmCompilerInElm elmCompiler,
        IPineVM pineVM)
    {
        var parseNameResult =
            ElmModule.ParseModuleName(moduleText);

        {
            if (parseNameResult.IsErrOrNull() is { } parseNameErr)
            {
                return
                    "Failed parsing name for module " +
                    moduleText.Split('\n', '\r').FirstOrDefault() + ": " + parseNameErr;
            }
        }

        if (parseNameResult.IsOkOrNull() is not { } moduleName)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseNameResult.GetType());
        }

        {
            var fromDotnetResult =
                ElmSyntaxParser.ParseModuleTextAsElmSyntaxElmValue(moduleText);

            // .NET based implementation of ElmSyntaxParser does not yet cover all syntax.

            if (fromDotnetResult.IsOkOrNull() is { } parsedOk)
            {
                // The bundled Elm-in-Elm compiler still emits and destructures records using
                // the legacy nested layout, so the AST handed to it must use the same layout.
                // Once the Elm-in-Elm compiler is migrated, switch back to ElmValueAsPineValue.
                var asPineValue =
                    ElmValueEncoding.ElmValueAsPineValue(parsedOk);

                return
                    new KeyValuePair<IReadOnlyList<string>, PineValue>(
                        moduleName,
                        asPineValue);
            }
        }

        var parseModuleResult =
            elmCompiler.ParseElmModuleText(moduleText, pineVM);

        {
            if (parseModuleResult.IsErrOrNull() is { } parseModuleErr)
            {
                return
                    "Failed parsing module " +
                    string.Join(".", moduleName) + ": " + parseModuleErr;
            }
        }

        if (parseModuleResult.IsOkOrNull() is not { } parsedModule)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseModuleResult.GetType());
        }

        return new KeyValuePair<IReadOnlyList<string>, PineValue>(moduleName, parsedModule);
    }

    public Result<string, SubmissionResponse> Submit(string submission)
    {
        var inspectionLog = new List<string>();

        return
            Submit(submission, inspectionLog.Add)
            .Map(r => new SubmissionResponse(r, inspectionLog));
    }

    public static Result<string, PineValue> ParseInteractiveSubmission(
        ElmCompilerInElm elmCompiler,
        IPineVM pineVM,
        string submission,
        Action<string>? addInspectionLogEntry)
    {
        var clock = System.Diagnostics.Stopwatch.StartNew();

        void logDuration(string label) =>
            addInspectionLogEntry?.Invoke(
                label + " duration: " + CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms");

        var response = elmCompiler.ParseElmInteractiveSubmissionText(submission, pineVM);

        logDuration("parse via Elm compiler");

        return response;
    }

    public Result<string, ElmInteractive.EvaluatedStruct> Submit(
        string submission,
        Action<string>? addInspectionLogEntry)
    {
        lock (_submissionLock)
        {
            var clock = System.Diagnostics.Stopwatch.StartNew();

            void LogDuration(string label) =>
                addInspectionLogEntry?.Invoke(
                    label + " duration: " + CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms");

            if (_buildCompilerResult.IsErrOrNull() is { } buildCompilerErr)
            {
                return "Failed to build Elm compiler: " + buildCompilerErr;
            }

            if (_buildCompilerResult.IsOkOrNull() is not { } elmCompiler)
            {
                throw new NotImplementedException(
                    "Unexpected build compiler result type: " + _buildCompilerResult.GetType());
            }

            if (_buildPineEvalContextTask.Result.IsErrOrNull() is { } buildPineEvalContextErr)
            {
                return "Failed to build initial Pine eval context: " + buildPineEvalContextErr;
            }

            if (_buildPineEvalContextTask.Result.IsOkOrNull() is not { } buildPineEvalContextOk)
            {
                throw new NotImplementedException(
                    "Unexpected build Pine eval context result type: " + _buildPineEvalContextTask.Result.GetType());
            }

            clock.Restart();

            var parseSubmissionResult =
                ParseInteractiveSubmission(
                    elmCompiler,
                    _pineVM,
                    submission: submission,
                    addInspectionLogEntry: compileEntry => addInspectionLogEntry?.Invoke("Parse: " + compileEntry));

            if (parseSubmissionResult.IsErrOrNull() is { } parseSubmissionErr)
            {
                return "Failed to parse submission: " + parseSubmissionErr;
            }

            if (parseSubmissionResult.IsOkOrNull() is not { } parsedSubmissionOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse submission result type: " + parseSubmissionResult.GetType());
            }

            var parsedSubmissionAsElmValueLazy =
                new Lazy<ElmValue>(
                    () =>
                    elmCompilerCache.PineValueDecodedAsElmValue(parsedSubmissionOk)
                    .Extract(decodeErr => throw new Exception("Failed decoding submission as Elm value: " + decodeErr)));

            LogDuration("parse");

            clock.Restart();

            var environmentEncodedForCompiler =
                elmCompilerCache.EncodeValueForCompiler(buildPineEvalContextOk);

            LogDuration("compile - encode environment");

            clock.Restart();

            var compileParsedResult =
                Pine.Core.CodeAnalysis.ElmInteractiveEnvironment.ApplyFunction(
                    _pineVM,
                    elmCompiler.CompileParsedInteractiveSubmission,
                    arguments:
                    [
                        environmentEncodedForCompiler,
                        parsedSubmissionOk
                    ]);

            if (compileParsedResult.IsErrOrNull() is { } compileParsedErr)
            {
                return
                    "Failed compiling parsed submission (" +
                    ElmValue.RenderAsElmExpression(parsedSubmissionAsElmValueLazy.Value) +
                    "): " + compileParsedErr;
            }

            if (compileParsedResult.IsOkOrNull() is not { } compileParsedOk)
            {
                return
                    "Failed compiling parsed submission (" +
                    ElmValue.RenderAsElmExpression(parsedSubmissionAsElmValueLazy.Value) +
                    "): " +
                    compileParsedResult.Unpack(err => err, ok => "Not an err");
            }

            LogDuration("compile - apply");

            clock.Restart();

            var compileParsedOkAsElmValueResult =
                elmCompilerCache.PineValueDecodedAsElmValue(compileParsedOk);

            if (compileParsedOkAsElmValueResult.IsErrOrNull() is { } compileParsedOkAsElmValueErr)
            {
                return "Failed decoding compiled expression: " + compileParsedOkAsElmValueErr;
            }

            if (compileParsedOkAsElmValueResult.IsOkOrNull() is not { } compileParsedOkAsElmValue)
            {
                throw new NotImplementedException(
                    "Unexpected compile parsed ok as Elm value result type: " + compileParsedOkAsElmValueResult.GetType());
            }

            LogDuration("compile - decode result");

            clock.Restart();

            if (compileParsedOkAsElmValue is not ElmValue.ElmTag compiledResultTag)
            {
                return "Unexpected return value: No tag: " + compileParsedOkAsElmValue;
            }

            if (compiledResultTag.TagName is not "Ok" || compiledResultTag.Arguments.Count is not 1)
            {
                return "Failed compile: " + compiledResultTag;
            }

            var decodeExpressionResult =
                elmCompilerCache.DecodeExpressionFromElmValue(compiledResultTag.Arguments[0]);

            if (decodeExpressionResult.IsErrOrNull() is { } decodeExpressionErr)
            {
                return "Failed decoding compiled expression: " + decodeExpressionErr;
            }

            if (decodeExpressionResult.IsOkOrNull() is not { } resultingExpr)
            {
                throw new NotImplementedException(
                    "Unexpected decode expression result type: " + decodeExpressionResult.GetType());
            }

            LogDuration("compile - decode expression");

            clock.Restart();

            var evalResult = _pineVM.EvaluateExpression(resultingExpr, buildPineEvalContextOk);

            LogDuration("eval");

            if (evalResult.IsErrOrNull() is { } evalErr)
            {
                return "Failed to evaluate compiled expression in PineVM: " + evalErr;
            }

            if (evalResult.IsOkOrNull() is not { } evalOk)
            {
                throw new NotImplementedException(
                    "Unexpected eval result type: " + evalResult.GetType());
            }

            if (evalOk is not PineValue.ListValue evalResultListComponent)
            {
                return "Type mismatch: Pine expression not evaluated to a list: " + evalOk;
            }

            if (evalResultListComponent.Items.Length is not 2)
            {
                return
                    "Type mismatch: Pine expression evaluated to a list with unexpected number of elements: " +
                    evalResultListComponent.Items.Length +
                    " instead of 2";
            }

            _buildPineEvalContextTask = System.Threading.Tasks.Task.FromResult(
                Result<string, PineValue>.ok(evalResultListComponent.Items.Span[0]));

            clock.Restart();

            var parseSubmissionResponseResult =
                ElmInteractive.SubmissionResponseFromResponsePineValue(
                    response: evalResultListComponent.Items.Span[1]);

            LogDuration("parse-result");

            if (parseSubmissionResponseResult.IsErrOrNull() is { } parseSubmissionResponseErr)
            {
                return "Failed to parse submission response: " + parseSubmissionResponseErr;
            }

            if (parseSubmissionResponseResult.IsOkOrNull() is not { } parseSubmissionResponseOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse submission response result type: " + parseSubmissionResponseResult.GetType());
            }

            return parseSubmissionResponseOk;
        }
    }

    public PineValue CurrentEnvironmentValue()
    {
        lock (_submissionLock)
        {
            return _buildPineEvalContextTask.Result.Extract(err => throw new Exception(err));
        }
    }

    void IDisposable.Dispose()
    {
    }

    public static Result<string, Pine.CompilePineToDotNet.CompileCSharpClassResult> CompileForProfiledScenarios(
        FileTree compileElmProgramCodeFiles,
        IReadOnlyList<TestElmInteractive.Scenario> scenarios,
        Pine.CompilePineToDotNet.SyntaxContainerConfig syntaxContainerConfig,
        int limitNumber,
        bool enableEvalExprCache)
    {
        var expressionsProfiles =
            scenarios
            .Select(scenario =>
            CollectExpressionsToOptimizeFromScenario(
                compileElmProgramCodeFiles: compileElmProgramCodeFiles,
                scenario: scenario,
                enableEvalExprCache: enableEvalExprCache))
            .ToImmutableArray();

        var aggregateExpressionsProfiles =
            ProfilingPineVM.AggregateExpressionUsageProfiles(expressionsProfiles);

        var expressionsToCompile =
            DynamicPGOShare.FilterAndRankExpressionProfilesForCompilation(aggregateExpressionsProfiles)
            .Take(limitNumber)
            .Select(expressionAndProfile => expressionAndProfile.Key)
            .ToImmutableList();

        return
            Pine.CompilePineToDotNet.CompileToCSharp.CompileExpressionsToCSharpClass(
                expressionsToCompile,
                syntaxContainerConfig);
    }

    public static IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile> CollectExpressionsToOptimizeFromScenario(
        FileTree compileElmProgramCodeFiles,
        TestElmInteractive.Scenario scenario,
        bool enableEvalExprCache)
    {
        var cache =
            enableEvalExprCache ? new InvocationCache() : null;

        var profilingVM =
            new ProfilingPineVM(
                evalCache: cache);

        var profilingSession = new InteractiveSessionPine(
            compilerSourceFiles: compileElmProgramCodeFiles,
            appCodeTree: (AppCompilationUnits?)null,
            overrideSkipLowering: false,
            entryPointsFilePaths: null,
            profilingVM.PineVM);

        foreach (var step in scenario.Steps)
            profilingSession.Submit(step.step.Submission);

        var exprUsageSamples = profilingVM.ExprEnvUsagesFlat;

        var expressionUsages =
            exprUsageSamples
            .Select(sample => sample.Value.Analysis.Value.ToMaybe())
            .WhereNotNothing()
            .SelectMany(analysis => analysis)
            .ToImmutableList();

        var pineExpressionsToOptimize =
            ProfilingPineVM.UsageProfileDictionaryFromListOfUsages(expressionUsages);

        return pineExpressionsToOptimize;
    }


    public class PineVMWithPersistentCache : IPineVM
    {
        private readonly Dictionary<(Expression, PineValue), PineValue> _evalCache = [];

        private readonly Dictionary<EvalCacheEntryKey, PineValue> _vmEvalCache = [];

        private readonly IPineVM _pineVM;

        private readonly IFileStore _fileStore;

        public PineVMWithPersistentCache(IFileStore fileStore)
        {
            _fileStore = fileStore;

            _pineVM = SetupVM.Create(evalCache: _vmEvalCache);
        }

        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
        {
            if (_evalCache.TryGetValue((expression, environment), out var cachedResult))
            {
                return cachedResult;
            }

            var fileName = FileNameFromKey(expression, environment);

            try
            {
                if (_fileStore.GetFileContent([fileName]) is { } fileContent)
                {
                    var loadedValue = ValueBinaryEncodingClassic.DecodeRoot(fileContent);

                    _evalCache[(expression, environment)] = loadedValue;

                    return loadedValue;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Failed to read or parse cache file: " + e);
            }

            var evalResult = _pineVM.EvaluateExpression(expression, environment);

            _vmEvalCache.Clear();

            if (evalResult.IsOkOrNull() is { } evalOk)
            {
                _evalCache[(expression, environment)] = evalOk;

                System.Threading.Tasks.Task.Run(() =>
                {
                    try
                    {
                        using var stream = new System.IO.MemoryStream();

                        ValueBinaryEncodingClassic.Encode(stream, evalOk);

                        stream.Seek(0, System.IO.SeekOrigin.Begin);

                        var fileContent = stream.ToArray();

                        _fileStore.SetFileContent([fileName], fileContent);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("Failed to write cache file: " + e);
                    }
                });
            }

            return evalResult;
        }

        public string FileNameFromKey(Expression expression, PineValue environment)
        {
            var exprHash =
                _encodedExprCache.GetOrAdd(
                    expression,
                    valueFactory:
                    expr => ComputeHash(ExpressionEncoding.EncodeExpressionAsValue(expr)));

            var envHash = ComputeHash(environment);

            return
                Convert.ToHexStringLower(exprHash[..8].Span) + "_x_" +
                Convert.ToHexStringLower(envHash[..8].Span);
        }

        readonly ConcurrentDictionary<Expression, ReadOnlyMemory<byte>> _encodedExprCache = new();

        readonly ConcurrentPineValueHashCache _valueHashCache = new();

        public ReadOnlyMemory<byte> ComputeHash(PineValue pineValue)
        {
            return _valueHashCache.GetHash(pineValue);
        }
    }
}
