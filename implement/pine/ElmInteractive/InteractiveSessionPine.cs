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

    private ElmInteractive.CompilationCache lastCompilationCache = ElmInteractive.CompilationCache.Empty;

    private readonly IPineVM pineVM;

    private static readonly ConcurrentDictionary<ElmInteractive.CompileInteractiveEnvironmentResult, ElmInteractive.CompileInteractiveEnvironmentResult> compiledEnvironmentCache = new();

    private static readonly ConcurrentDictionary<PineValue, PineValue> encodedForCompilerCache = new();

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
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        PineValue? initialState,
        TreeNodeWithStringPath? appCodeTree,
        bool caching,
        DynamicPGOShare? autoPGO)
        :
        this(
            compileElmProgramCodeFiles: compileElmProgramCodeFiles,
            initialState: initialState,
            appCodeTree: appCodeTree,
            BuildPineVM(caching: caching, autoPGO: autoPGO))
    {
    }

    public InteractiveSessionPine(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        PineValue? initialState,
        TreeNodeWithStringPath? appCodeTree,
        IPineVM pineVM)
        :
        this(
            compileElmProgramCodeFiles: compileElmProgramCodeFiles,
            initialState: initialState,
            appCodeTree: appCodeTree,
            (pineVM, pineVMCache: null))
    {
    }

    private InteractiveSessionPine(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        PineValue? initialState,
        TreeNodeWithStringPath? appCodeTree,
        (IPineVM pineVM, PineVMCache? pineVMCache) pineVMAndCache)
    {
        pineVM = pineVMAndCache.pineVM;

        buildCompilerResult =
            ElmCompiler.GetElmCompilerAsync(compileElmProgramCodeFiles).Result;

        buildPineEvalContextTask =
            System.Threading.Tasks.Task.Run(() =>
            CompileInteractiveEnvironment(
                initialState: initialState,
                appCodeTree: appCodeTree));
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

    private static readonly ConcurrentDictionary<string, Result<string, PineValue>> compileEvalContextCache = new();


    private Result<string, PineValue> CompileInteractiveEnvironment(
        PineValue? initialState,
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

        try
        {
            return
                CompileInteractiveEnvironment(
                    elmCompiler.Value.CompileElmPreparedJavaScriptEngine,
                    lastCompilationCache,
                    initialState,
                    appCodeTree)
                .Map(ok =>
                {
                    lastCompilationCache = ok.compilationCache;

                    return ok.compileResult;
                });
        }
        finally
        {
            // Build JavaScript engine and warm-up anyway.
            System.Threading.Tasks.Task.Run(() => elmCompiler.Value.CompileElmPreparedJavaScriptEngine.Evaluate("0"));
        }
    }

    public static Result<string, (PineValue compileResult, ElmInteractive.CompilationCache compilationCache)>
        CompileInteractiveEnvironment(
        JavaScript.IJavaScriptEngine compileElmPreparedJavaScriptEngine,
        ElmInteractive.CompilationCache lastCompilationCache,
        PineValue? initialState,
        TreeNodeWithStringPath? appCodeTree)
    {
        var appCodeTreeHash =
            appCodeTree switch
            {
                null => "",
                not null => CommonConversion.StringBase16(PineValueHashTree.ComputeHashNotSorted(appCodeTree))
            };

        var compileResult =
            compileEvalContextCache.GetOrAdd(
            key: appCodeTreeHash,
            valueFactory: _ =>
            {
                var compileInteractiveEnvironmentResults =
                lastCompilationCache.CompileInteractiveEnvironmentResults
                .Union(compiledEnvironmentCache.Keys);

                var resultWithCache =
                    ElmInteractive.CompileInteractiveEnvironment(
                        compileElmPreparedJavaScriptEngine,
                        initialState: initialState,
                        appCodeTree: appCodeTree,
                        lastCompilationCache with
                        {
                            CompileInteractiveEnvironmentResults = compileInteractiveEnvironmentResults
                        });

                lastCompilationCache = resultWithCache.Unpack(fromErr: _ => lastCompilationCache, fromOk: ok => ok.compilationCache);

                foreach (var compileEnvironmentResult in lastCompilationCache.CompileInteractiveEnvironmentResults)
                {
                    compiledEnvironmentCache[compileEnvironmentResult] = compileEnvironmentResult;
                }

                return resultWithCache.Map(withCache => withCache.compileResult);
            });

        return compileResult.Map(compileResult => (compileResult, lastCompilationCache));
    }

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
                            elmCompiler.CompileElmPreparedJavaScriptEngine,
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
            compileElmProgramCodeFiles: compileElmProgramCodeFiles,
            initialState: null,
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
