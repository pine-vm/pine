using Pine;
using Pine.Core;
using Pine.Core.PineVM;
using Pine.Elm;
using Pine.ElmInteractive;
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
    private readonly object submissionLock = new();

    private System.Threading.Tasks.Task<Result<string, PineValue>> buildPineEvalContextTask;

    private readonly Result<string, ElmCompiler> buildCompilerResult;

    private readonly IPineVM pineVM;

    private static readonly IPineVM compileEnvPineVM =
        new LockingPineVM(
            new PineVMWithPersistentCache(
                new FileStoreFromSystemIOFile(
                    System.IO.Path.Combine(Filesystem.CacheDirectory, "elm-compiler-vm", Program.AppVersionId))));

    /*
     * TODO: Move these caches to a dedicated scope to enable for better control over reuse/disposal.
     * */

    static readonly ElmCompilerCache elmCompilerCache = new();

    static readonly ConcurrentDictionary<string, Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>>> TryParseModuleTextCache = new();

    private static JavaScript.IJavaScriptEngine ParseSubmissionOrCompileDefaultJavaScriptEngine { get; } =
        BuildParseSubmissionOrCompileDefaultJavaScriptEngine();

    private static JavaScript.IJavaScriptEngine BuildParseSubmissionOrCompileDefaultJavaScriptEngine()
    {
        return ElmCompiler.JavaScriptEngineFromElmCompilerSourceFiles(
            ElmCompiler.CompilerSourceContainerFilesDefault.Value);
    }

    public InteractiveSessionPine(
        TreeNodeWithStringPath compilerSourceFiles,
        TreeNodeWithStringPath? appCodeTree,
        bool? overrideSkipLowering,
        bool caching,
        DynamicPGOShare? autoPGO)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree: appCodeTree,
            overrideSkipLowering: overrideSkipLowering,
            BuildPineVM(caching: caching, autoPGO: autoPGO))
    {
    }

    public InteractiveSessionPine(
        TreeNodeWithStringPath compilerSourceFiles,
        TreeNodeWithStringPath? appCodeTree,
        bool? overrideSkipLowering,
        IPineVM pineVM)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree: appCodeTree,
            overrideSkipLowering: overrideSkipLowering,
            (pineVM, pineVMCache: null))
    {
    }

    private InteractiveSessionPine(
        TreeNodeWithStringPath compilerSourceFiles,
        TreeNodeWithStringPath? appCodeTree,
        bool? overrideSkipLowering,
        (IPineVM pineVM, PineVMCache? pineVMCache) pineVMAndCache)
    {
        pineVM = pineVMAndCache.pineVM;

        buildCompilerResult =
            ElmCompiler.GetElmCompilerAsync(compilerSourceFiles).Result;

        buildPineEvalContextTask =
            System.Threading.Tasks.Task.Run(() =>
            CompileInteractiveEnvironment(
                appCodeTree: appCodeTree,
                overrideSkipLowering: overrideSkipLowering));
    }

    public static (IPineVM, PineVMCache?) BuildPineVM(
        bool caching,
        DynamicPGOShare? autoPGO)
    {
        var cache = caching ? new PineVMCache() : null;

        return (BuildPineVM(cache, autoPGO), cache);
    }

    public static IPineVM BuildPineVM(
        PineVMCache? cache,
        DynamicPGOShare? autoPGO)
    {
        if (autoPGO is not null)
        {
            return
                autoPGO.GetVMAutoUpdating(evalCache: cache?.EvalCache);
        }

        return
            new PineVM(evalCache: cache?.EvalCache);
    }


    private Result<string, PineValue> CompileInteractiveEnvironment(
        TreeNodeWithStringPath? appCodeTree,
        bool? overrideSkipLowering)
    {
        if (buildCompilerResult.IsOkOrNull() is not { } elmCompiler)
        {
            if (buildCompilerResult.IsErrOrNull() is { } err)
            {
                return "Failed to build Elm compiler: " + err;
            }

            throw new NotImplementedException(
                "Unexpected compiler result type: " + buildCompilerResult.GetType());
        }

        return
            CompileInteractiveEnvironment(
                appCodeTree,
                overrideSkipLowering: overrideSkipLowering,
                elmCompiler,
                compileEnvPineVM);
    }

    public static Result<string, PineValue>
        CompileInteractiveEnvironment(
        TreeNodeWithStringPath? appCodeTree,
        bool? overrideSkipLowering,
        ElmCompiler elmCompiler) =>
        CompileInteractiveEnvironment(
            appCodeTree,
            overrideSkipLowering: overrideSkipLowering,
            elmCompiler,
            compileEnvPineVM);

    public static Result<string, PineValue>
        CompileInteractiveEnvironment(
        TreeNodeWithStringPath? appCodeTree,
        bool? overrideSkipLowering,
        ElmCompiler elmCompiler,
        IPineVM pineVM)
    {
        var skipLowering =
            overrideSkipLowering ??
            !ElmCompiler.CheckIfAppUsesLowering(appCodeTree ?? TreeNodeWithStringPath.EmptyTree);

        var appCodeTreeWithCoreModules =
            /*
            ElmCompiler.MergeElmCoreModules(appCodeTree ?? TreeNodeWithStringPath.EmptyTree);
            */
            appCodeTree ?? TreeNodeWithStringPath.EmptyTree;

        var orderedModules =
            AppSourceFileTreesForIncrementalCompilation(
                appCodeTreeWithCoreModules,
                skipLowering: skipLowering)
            .ToImmutableArray();

        var initialStateElmValue =
            ElmValueInterop.PineValueEncodedAsInElmCompiler(PineValue.EmptyList);

        var initialStateElmValueInCompiler =
            ElmValueEncoding.ElmValueAsPineValue(initialStateElmValue);

        PineValue compiledNewEnvInCompiler = initialStateElmValueInCompiler;

        foreach (var compilationIncrement in orderedModules)
        {
            var parseResult =
                CachedTryParseModuleText(compilationIncrement.sourceModule.ModuleText);

            if (parseResult.IsErrOrNull() is { } parseErr)
            {
                return "Failed parsing module " + string.Join(".", compilationIncrement.sourceModule.ModuleName) + ": " + parseErr;
            }

            if (parseResult is not Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>>.Ok parseOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType());
            }

            var parsedModule = parseOk.Value;

            var parsedModuleNameFlat = string.Join(".", parsedModule.Key);

            var compileModuleResult =
                CompileOneElmModule(
                    compiledNewEnvInCompiler,
                    compilationIncrement.sourceModule.ModuleText,
                    parsedModule.Value,
                    elmCompiler,
                    pineVM: pineVM);

            if (compileModuleResult.IsOkOrNull() is not { } compileModuleOk)
            {
                return
                    "Compiling module " + parsedModuleNameFlat + " failed: " +
                    compileModuleResult.Unpack(fromErr: err => err, fromOk: _ => "no err");
            }

            compiledNewEnvInCompiler = compileModuleOk;
        }

        var asElmValueResult =
            elmCompilerCache.PineValueDecodedAsElmValue(compiledNewEnvInCompiler);

        if (asElmValueResult.IsErrOrNull() is { } asElmValueErr)
        {
            return "Failed to decode environment from compiler as Elm value: " + asElmValueErr;
        }

        if (asElmValueResult.IsOkOrNull() is not { } compiledNewEnvInCompilerElm)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + asElmValueResult.GetType());
        }

        var compiledNewEnvValueResult =
            elmCompilerCache.DecodeElmValueFromCompiler(compiledNewEnvInCompilerElm);

        if (compiledNewEnvValueResult.IsErrOrNull() is { } compiledNewEnvValueErr)
        {
            return "Failed to decode environment from Elm value: " + compiledNewEnvValueErr;
        }

        if (compiledNewEnvValueResult.IsOkOrNull() is not { } compiledNewEnvValue)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + compiledNewEnvValueResult.GetType());
        }

        return compiledNewEnvValue;
    }

    public static TreeNodeWithStringPath MergeDefaultElmCoreAndKernelModules(
        TreeNodeWithStringPath appCodeTree) =>
        MergeDefaultElmCoreAndKernelModules(
            appCodeTree,
            ElmCompiler.ElmCoreAndKernelModuleFilesDefault.Value);

    public static TreeNodeWithStringPath MergeDefaultElmCoreAndKernelModules(
        TreeNodeWithStringPath appCodeTree,
        TreeNodeWithStringPath elmCoreAndKernelModuleFilesDefault)
    {
        return
            elmCoreAndKernelModuleFilesDefault
            .EnumerateBlobsTransitive()
            .Aggregate(
                seed:
                appCodeTree,

                func:
                (aggregate, nextBlob) =>
                aggregate.GetNodeAtPath(nextBlob.path) is { } existingNode
                ?
                aggregate
                :
                aggregate.SetNodeAtPathSorted(nextBlob.path, TreeNodeWithStringPath.Blob(nextBlob.blobContent)));
    }

    public record ParsedModule(
        IReadOnlyList<string> FilePath,
        IReadOnlyList<string> ModuleName,
        string ModuleText);

    public static IEnumerable<(TreeNodeWithStringPath tree, ParsedModule sourceModule)> AppSourceFileTreesForIncrementalCompilation(
        TreeNodeWithStringPath appSourceFiles,
        bool skipLowering)
    {
        var compileableSourceModules =
            ElmInteractive.ModulesFilePathsAndTextsFromAppCodeTree(
                appSourceFiles,
                skipLowering: skipLowering) ?? [];

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
            ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
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
                ElmSyntax.ElmModule.ParseModuleName(sourceModule.moduleText)
                .Extract(err => throw new Exception("Failed parsing module name: " + err));

            mutatedTree =
                mutatedTree.SetNodeAtPathSorted(sourceModule.filePath, TreeNodeWithStringPath.Blob(sourceModule.fileContent));

            yield return
                (mutatedTree,
                new ParsedModule(
                    FilePath: sourceModule.filePath,
                    ModuleName: moduleName,
                    ModuleText: sourceModule.moduleText));
        }
    }

    static Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>> CachedTryParseModuleText(string moduleText) =>
        TryParseModuleTextCache
        .GetOrAdd(
            moduleText,
            valueFactory: TryParseModuleText);

    static Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>> TryParseModuleText(string moduleText)
    {
        return
            ElmSyntax.ElmModule.ParseModuleName(moduleText)
            .MapError(err => "Failed parsing name for module " + moduleText.Split('\n', '\r').FirstOrDefault())
            .AndThen(moduleName =>
            ElmInteractive.ParseElmModuleTextToPineValue(moduleText, ParseSubmissionOrCompileDefaultJavaScriptEngine)
            .MapError(err => "Failed parsing module " + moduleName + ": " + err)
            .Map(parsedModule => new KeyValuePair<IReadOnlyList<string>, PineValue>(moduleName, parsedModule)));
    }

    public static Result<string, PineValue> CompileOneElmModule(
        PineValue prevEnvValue,
        string moduleText,
        PineValue parsedModuleValue,
        ElmCompiler elmCompiler,
        IPineVM pineVM)
    {
        var applyFunctionResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                elmCompiler.ExpandElmInteractiveEnvironmentWithModules,
                arguments:
                [
                    prevEnvValue,
                    PineValue.List([ParsedElmFileRecordValue(moduleText, parsedModuleValue)])
                ]
                );

        if (applyFunctionResult.IsErrOrNull() is { } applyErr)
            return "Failed to apply function: " + applyErr;

        if (applyFunctionResult.IsOkOrNull() is not { } applyFunctionOk)
            throw new Exception("Unexpected result type: " + applyFunctionResult.GetType().FullName);

        var parseAsTagResult = ElmValueEncoding.ParseAsTag(applyFunctionOk);

        if (parseAsTagResult.IsErrOrNull() is { } parseAsTagErr)
            return "Failed to parse result as tag: " + parseAsTagErr;

        if (parseAsTagResult is not Result<string, (string tagName, IReadOnlyList<PineValue> tagArgs)>.Ok parseAsTagOk)
            throw new Exception("Unexpected result type: " + parseAsTagResult.GetType().FullName);

        if (parseAsTagOk.Value.tagName is not "Ok")
        {
            return
                "Failed to extract environment: Tag not 'Ok': " +
                elmCompilerCache.PineValueDecodedAsElmValue(applyFunctionOk)
                .Unpack(
                    fromErr: err => "Failed to parse as Elm value: " + err,
                    fromOk: elmValue => ElmValue.RenderAsElmExpression(elmValue).expressionString);
        }

        if (parseAsTagOk.Value.tagArgs.Count is not 1)
            return "Failed to extract environment: Expected one element in the list, got " + parseAsTagOk.Value.tagArgs.Count;

        var parseAsRecordResult = ElmValueEncoding.ParsePineValueAsRecordTagged(parseAsTagOk.Value.tagArgs[0]);

        if (parseAsRecordResult.IsErrOrNull() is { } parseAsRecordError)
            return "Failed to parse as record: " + parseAsRecordError;

        if (parseAsRecordResult.IsOkOrNull() is not { } recordFields)
            throw new Exception("Unexpected result type: " + parseAsRecordResult.GetType().FullName);

        var environmentValueField =
            recordFields
            .SingleOrDefault(f => f.fieldName is "environment")
            .fieldValue;

        if (environmentValueField is not PineValue environmentValue)
            return "Failed to extract environment: not a Pine value: " + environmentValueField;

        return environmentValue;
    }

    static PineValue ParsedElmFileRecordValue(
        string fileText,
        PineValue parsedModuleValue) =>
        ElmValueEncoding.ElmRecordAsPineValue(
            [
            ("fileText", ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmString(fileText)))
            ,("parsedModule", parsedModuleValue)
            ]);

    public Result<string, SubmissionResponse> Submit(string submission)
    {
        var inspectionLog = new List<string>();

        return
            Submit(submission, inspectionLog.Add)
            .Map(r => new SubmissionResponse(r, inspectionLog));
    }

    public Result<string, ElmInteractive.EvaluatedStruct> Submit(
        string submission,
        Action<string>? addInspectionLogEntry)
    {
        lock (submissionLock)
        {
            var clock = System.Diagnostics.Stopwatch.StartNew();

            void logDuration(string label) =>
                addInspectionLogEntry?.Invoke(
                    label + " duration: " + CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms");

            return
                buildCompilerResult
                .MapError(err => "Failed to build compiler: " + err)
                .AndThen(elmCompiler =>
                buildPineEvalContextTask.Result
                .MapError(error => "Failed to build initial Pine eval context: " + error)
                .AndThen(buildPineEvalContextOk =>
                {
                    clock.Restart();

                    var parseSubmissionResult =
                        ElmInteractive.ParseInteractiveSubmission(
                            ParseSubmissionOrCompileDefaultJavaScriptEngine,
                            submission: submission,
                            addInspectionLogEntry: compileEntry => addInspectionLogEntry?.Invoke("Parse: " + compileEntry));

                    return
                    parseSubmissionResult
                    .MapError(error => "Failed to parse submission: " + error)
                    .AndThen(parsedSubmissionOk =>
                    {
                        return
                        elmCompilerCache.PineValueDecodedAsElmValue(parsedSubmissionOk)
                        .MapError(error => "Failed parsing result as Elm value: " + error)
                        .AndThen(parsedSubmissionAsElmValue =>
                        {
                            logDuration("parse");

                            clock.Restart();

                            var environmentEncodedForCompiler =
                                elmCompilerCache.EncodeValueForCompiler(buildPineEvalContextOk);

                            logDuration("compile - encode environment");

                            clock.Restart();

                            var compileParsedResult =
                                ElmInteractiveEnvironment.ApplyFunction(
                                    pineVM,
                                    elmCompiler.CompileParsedInteractiveSubmission,
                                    arguments:
                                    [
                                        environmentEncodedForCompiler,
                                        parsedSubmissionOk
                                    ]);

                            if (compileParsedResult.IsOkOrNull() is not { } compileParsedOk)
                            {
                                return
                                "Failed compiling parsed submission (" +
                                parsedSubmissionAsElmValue + "): " +
                                compileParsedResult.Unpack(err => err, ok => "Not an err");
                            }

                            logDuration("compile - apply");

                            clock.Restart();

                            var compileParsedOkAsElmValue =
                            elmCompilerCache.PineValueDecodedAsElmValue(compileParsedOk)
                            .Extract(err => throw new Exception("Failed decoding as Elm value: " + err));

                            logDuration("compile - decode result");

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

                            return
                            decodeExpressionResult
                            .MapError(error => "Failed decoding compiled expression: " + error)
                            .AndThen(resultingExpr =>
                            {
                                logDuration("compile - decode expression");

                                clock.Restart();

                                var evalResult = pineVM.EvaluateExpression(resultingExpr, buildPineEvalContextOk);

                                logDuration("eval");

                                return
                                evalResult
                                .MapError(error => "Failed to evaluate expression in PineVM: " + error)
                                .AndThen(evalOk =>
                                {
                                    if (evalOk is not PineValue.ListValue evalResultListComponent)
                                    {
                                        return
                                        "Type mismatch: Pine expression evaluated to a blob";
                                    }

                                    if (evalResultListComponent.Elements.Count is not 2)
                                    {
                                        return
                                        "Type mismatch: Pine expression evaluated to a list with unexpected number of elements: " +
                                        evalResultListComponent.Elements.Count +
                                        " instead of 2";
                                    }

                                    buildPineEvalContextTask = System.Threading.Tasks.Task.FromResult(
                                        Result<string, PineValue>.ok(evalResultListComponent.Elements[0]));

                                    clock.Restart();

                                    var parseSubmissionResponseResult =
                                        ElmInteractive.SubmissionResponseFromResponsePineValue(
                                            response: evalResultListComponent.Elements[1]);

                                    logDuration("parse-result");

                                    return
                                    parseSubmissionResponseResult
                                    .MapError(error => "Failed to parse submission response: " + error);
                                });
                            });
                        });
                    });
                }));
        }
    }

    public PineValue CurrentEnvironmentValue()
    {
        lock (submissionLock)
        {
            return buildPineEvalContextTask.Result.Extract(err => throw new Exception(err));
        }
    }

    void IDisposable.Dispose()
    {
    }

    public static Result<string, Pine.CompilePineToDotNet.CompileCSharpClassResult> CompileForProfiledScenarios(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
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
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        TestElmInteractive.Scenario scenario,
        bool enableEvalExprCache)
    {
        var cache =
            enableEvalExprCache ? new PineVMCache() : null;

        var profilingVM =
            new ProfilingPineVM(
                evalCache: cache?.EvalCache);

        var profilingSession = new InteractiveSessionPine(
            compilerSourceFiles: compileElmProgramCodeFiles,
            appCodeTree: null,
            overrideSkipLowering: false,
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
        private readonly Dictionary<(Expression, PineValue), PineValue> evalCache = [];

        private readonly Dictionary<EvalCacheEntryKey, PineValue> vmEvalCache = [];

        private readonly IPineVM pineVM;

        private readonly IFileStore fileStore;

        public PineVMWithPersistentCache(IFileStore fileStore)
        {
            this.fileStore = fileStore;

            pineVM = new PineVM(evalCache: vmEvalCache);
        }

        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
        {
            if (evalCache.TryGetValue((expression, environment), out var cachedResult))
            {
                return cachedResult;
            }

            var fileName = FileNameFromKey(expression, environment);

            try
            {
                if (fileStore.GetFileContent([fileName]) is { } fileContent)
                {
                    var dictionaryEntries =
                        System.Text.Json.JsonSerializer.Deserialize<IReadOnlyList<PineValueCompactBuild.ListEntry>>(fileContent.Span);

                    var dictionary = PineValueCompactBuild.BuildDictionaryFromEntries(dictionaryEntries);

                    // Since we write only with a single root, the one we are looking for must be the largest composition.

                    var loadedValue = dictionary[dictionaryEntries.Last().Key];

                    evalCache[(expression, environment)] = loadedValue;

                    return loadedValue;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Failed to read or parse cache file: " + e);
            }

            var evalResult = pineVM.EvaluateExpression(expression, environment);

            if (evalResult.IsOkOrNull() is { } evalOk)
            {
                evalCache[(expression, environment)] = evalOk;

                System.Threading.Tasks.Task.Run(() =>
                {
                    try
                    {
                        var compactBuild =
                            PineValueCompactBuild.PrebuildListEntriesAllFromRoot(evalOk);

                        var fileContent =
                            System.Text.Json.JsonSerializer.SerializeToUtf8Bytes(compactBuild.listEntries);

                        fileStore.SetFileContent([fileName], fileContent);
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
            var exprValue = EncodeExpression(expression);

            var exprHash = ComputeHash(exprValue);

            var envHash = ComputeHash(environment);

            return
                CommonConversion.StringBase16(exprHash[..8]) + "_x_" +
                CommonConversion.StringBase16(envHash[..8]);
        }

        readonly ConcurrentDictionary<Expression, PineValue> encodedExprCache = new();

        public PineValue EncodeExpression(Expression expression)
        {
            return encodedExprCache.GetOrAdd(
                expression,
                valueFactory:
                ExpressionEncoding.EncodeExpressionAsValue);
        }

        readonly ConcurrentDictionary<PineValue, ReadOnlyMemory<byte>> valueHashCache = new();

        public ReadOnlyMemory<byte> ComputeHash(PineValue pineValue)
        {
            {
                if (valueHashCache.TryGetValue(pineValue, out var hash))
                {
                    return hash;
                }
            }

            {
                var hash = PineValueHashTree.ComputeHash(pineValue, other => ComputeHash(other));

                valueHashCache[pineValue] = hash;

                return hash;
            }
        }
    }
}
