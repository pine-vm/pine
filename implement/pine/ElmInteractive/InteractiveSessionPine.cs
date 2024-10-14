using Pine;
using Pine.Core;
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

    private static readonly ConcurrentDictionary<EvalCacheEntryKey, PineValue> compileEnvPineVMCache = new();

    private static readonly IPineVM compileEnvPineVM =
        /*
         * TODO: Also add persistent cache incrementally adding Elm modules to the environment.
         * */
        new PineVM(evalCache: compileEnvPineVMCache);

    private static readonly ConcurrentDictionary<ElmInteractive.CompileInteractiveEnvironmentResult, ElmInteractive.CompileInteractiveEnvironmentResult> compiledEnvironmentCache = new();

    private static readonly ConcurrentDictionary<PineValue, PineValue> encodedForCompilerCache = new();

    private static JavaScript.IJavaScriptEngine ParseSubmissionOrCompileDefaultJavaScriptEngine { get; } =
        BuildParseSubmissionOrCompileDefaultJavaScriptEngine();

    private static JavaScript.IJavaScriptEngine BuildParseSubmissionOrCompileDefaultJavaScriptEngine()
    {
        return ElmCompiler.JavaScriptEngineFromElmCompilerSourceFiles(
            ElmCompiler.CompilerSourceContainerFilesDefault.Value);
    }

    private static PineValue EncodeValueForCompiler(PineValue pineValue)
    {
        return encodedForCompilerCache.GetOrAdd(
            pineValue,
            valueFactory:
            pineValue =>
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValueInterop.PineValueEncodedAsInElmCompiler(pineValue)));
    }

    public InteractiveSessionPine(
        TreeNodeWithStringPath compilerSourceFiles,
        TreeNodeWithStringPath? appCodeTree,
        bool caching,
        DynamicPGOShare? autoPGO)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree: appCodeTree,
            BuildPineVM(caching: caching, autoPGO: autoPGO))
    {
    }

    public InteractiveSessionPine(
        TreeNodeWithStringPath compilerSourceFiles,
        TreeNodeWithStringPath? appCodeTree,
        IPineVM pineVM)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree: appCodeTree,
            (pineVM, pineVMCache: null))
    {
    }

    private InteractiveSessionPine(
        TreeNodeWithStringPath compilerSourceFiles,
        TreeNodeWithStringPath? appCodeTree,
        (IPineVM pineVM, PineVMCache? pineVMCache) pineVMAndCache)
    {
        pineVM = pineVMAndCache.pineVM;

        buildCompilerResult =
            ElmCompiler.GetElmCompilerAsync(compilerSourceFiles).Result;

        buildPineEvalContextTask =
            System.Threading.Tasks.Task.Run(() =>
            CompileInteractiveEnvironment(appCodeTree: appCodeTree));
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
        TreeNodeWithStringPath? appCodeTree)
    {
        var appCodeTreeHash =
            appCodeTree switch
            {
                null => "",
                not null => CommonConversion.StringBase16(PineValueHashTree.ComputeHashNotSorted(appCodeTree))
            };

        if (buildCompilerResult is not Result<string, ElmCompiler>.Ok elmCompiler)
        {
            if (buildCompilerResult is Result<string, ElmCompiler>.Err err)
            {
                return "Failed to build Elm compiler: " + err.Value;
            }

            throw new NotImplementedException(
                "Unexpected compiler result type: " + buildCompilerResult.GetType());
        }

        return
            CompileInteractiveEnvironment(
                appCodeTree,
                elmCompiler.Value,
                compileEnvPineVM);
    }

    public static Result<string, PineValue>
        CompileInteractiveEnvironment(
        TreeNodeWithStringPath? appCodeTree,
        ElmCompiler elmCompiler,
        IPineVM pineVM)
    {
        var appCodeTreeHash =
            appCodeTree switch
            {
                null => "",
                not null => CommonConversion.StringBase16(PineValueHashTree.ComputeHashNotSorted(appCodeTree))
            };

        var appCodeTreeWithCoreModules =
            ElmCompiler.MergeElmCoreModules(appCodeTree ?? TreeNodeWithStringPath.EmptyTree);

        Result<string, KeyValuePair<IReadOnlyList<string>, (string moduleText, PineValue parsed)>> TryParseModuleText(string moduleText)
        {
            return
                ElmSyntax.ElmModule.ParseModuleName(moduleText)
                .MapError(err => "Failed parsing name for module " + moduleText.Split('\n', '\r').FirstOrDefault())
                .AndThen(moduleName =>
                ElmInteractive.ParseElmModuleTextToPineValue(moduleText, ParseSubmissionOrCompileDefaultJavaScriptEngine)
                .MapError(err => "Failed parsing module " + moduleName + ": " + err)
                .Map(parsedModule => new KeyValuePair<IReadOnlyList<string>, (string moduleText, PineValue parsed)>(
                    moduleName, (moduleText, parsedModule))));
        }


        var allModulesTexts =
            ElmInteractive.ModulesTextsFromAppCodeTree(appCodeTreeWithCoreModules) ?? [];

        var allModulesTextsOrdered =
            ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: allModulesTexts,
                availableModulesTexts: []);

        var initialStateElmValue =
            ElmValueInterop.PineValueEncodedAsInElmCompiler(PineValue.EmptyList);

        var initialStateElmValueInCompiler =
            ElmValueEncoding.ElmValueAsPineValue(initialStateElmValue);

        PineValue compiledNewEnvInCompiler = initialStateElmValueInCompiler;

        foreach (var moduleText in allModulesTextsOrdered)
        {
            var parseResult = TryParseModuleText(moduleText);

            if (parseResult.IsErrOrNull() is { } parseErr)
            {
                return "Failed parsing module " + moduleText.Split('\n').FirstOrDefault() + ": " + parseErr;
            }

            if (parseResult is not Result<string, KeyValuePair<IReadOnlyList<string>, (string moduleText, PineValue parsed)>>.Ok parseOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType());
            }

            var parsedModule = parseOk.Value;

            var parsedModuleNameFlat = string.Join(".", parsedModule.Key);

            var compileModuleResult =
                CompileOneElmModule(
                    compiledNewEnvInCompiler,
                    parsedModule.Value.moduleText,
                    parsedModule.Value.parsed,
                    elmCompiler,
                    pineVM: pineVM);

            if (compileModuleResult is not Result<string, PineValue>.Ok compileModuleOk)
            {
                return
                    "Compiling module " + parsedModuleNameFlat + " failed: " +
                    compileModuleResult.Unpack(fromErr: err => err, fromOk: _ => "no err");
            }

            compiledNewEnvInCompiler = compileModuleOk.Value;
        }

        var asElmValueResult =
            ElmValueEncoding.PineValueAsElmValue(compiledNewEnvInCompiler);

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
            ElmValueInterop.ElmValueDecodedAsInElmCompiler(compiledNewEnvInCompilerElm);

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

        if (applyFunctionResult is Result<string, PineValue>.Err err)
            return "Failed to apply function: " + err.Value;

        if (applyFunctionResult is not Result<string, PineValue>.Ok applyFunctionOk)
            throw new Exception("Unexpected result type: " + applyFunctionResult.GetType().FullName);

        var parseAsTagResult = ElmValueEncoding.ParseAsTag(applyFunctionOk.Value);

        if (parseAsTagResult is Result<string, (string, IReadOnlyList<PineValue>)>.Err parseAsTagErr)
            return "Failed to parse result as tag: " + parseAsTagErr.Value;

        if (parseAsTagResult is not Result<string, (string, IReadOnlyList<PineValue>)>.Ok parseAsTagOk)
            throw new Exception("Unexpected result type: " + parseAsTagResult.GetType().FullName);

        if (parseAsTagOk.Value.Item1 is not "Ok")
            return
                "Failed to extract environment: Tag not 'Ok': " +
                ElmValueEncoding.PineValueAsElmValue(applyFunctionOk.Value)
                .Unpack(
                    fromErr: err => "Failed to parse as Elm value: " + err,
                    fromOk: elmValue => ElmValue.RenderAsElmExpression(elmValue).expressionString);

        if (parseAsTagOk.Value.Item2.Count is not 1)
            return "Failed to extract environment: Expected one element in the list, got " + parseAsTagOk.Value.Item2.Count;

        var parseAsRecordResult = ElmValueEncoding.ParsePineValueAsRecordTagged(parseAsTagOk.Value.Item2[0]);

        if (parseAsRecordResult is Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>>.Err parseAsRecordErr)
            return "Failed to parse as record: " + parseAsRecordErr.Value;

        if (parseAsRecordResult is not Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>>.Ok parseAsRecordOk)
            throw new Exception("Unexpected result type: " + parseAsRecordResult.GetType().FullName);

        var environmentValueField =
            parseAsRecordOk.Value
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
                        ElmValueEncoding.PineValueAsElmValue(parsedSubmissionOk)
                        .MapError(error => "Failed parsing result as Elm value: " + error)
                        .AndThen(parsedSubmissionAsElmValue =>
                        {
                            logDuration("parse");

                            clock.Restart();

                            var environmentEncodedForCompiler =
                                EncodeValueForCompiler(buildPineEvalContextOk);

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

                            if (compileParsedResult is not Result<string, PineValue>.Ok compileParsedOk)
                            {
                                return
                                "Failed compiling parsed submission (" +
                                parsedSubmissionAsElmValue + "): " +
                                compileParsedResult.Unpack(err => err, ok => "Not an err");
                            }

                            logDuration("compile - apply");

                            clock.Restart();

                            var compileParsedOkAsElmValue =
                            ElmValueEncoding.PineValueAsElmValue(compileParsedOk.Value)
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

                            logDuration("compile - decode expression");

                            clock.Restart();

                            return
                                ElmValueEncoding.PineValueAsElmValue(compileParsedOk.Value)
                                .MapError(error => "Failed decoding parse result as Elm value: " + error)
                                .AndThen(compileParsedOkAsElmValue =>
                                {
                                    if (compileParsedOkAsElmValue is not ElmValue.ElmTag compiledResultTag)
                                    {
                                        return "Unexpected return value: No tag: " + compileParsedOkAsElmValue;
                                    }

                                    if (compiledResultTag.TagName is not "Ok" || compiledResultTag.Arguments.Count is not 1)
                                    {
                                        return "Failed compile: " + compiledResultTag;
                                    }

                                    return
                                    ElmValueInterop.ElmValueFromCompilerDecodedAsExpression(compiledResultTag.Arguments[0])
                                    .MapError(error => "Failed decoding compiled expression: " + error)
                                    .AndThen(resultingExpr =>
                                    {
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
}
