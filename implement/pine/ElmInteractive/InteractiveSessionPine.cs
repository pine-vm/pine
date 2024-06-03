using ElmTime.JavaScript;
using Pine;
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

    private readonly Lazy<IJavaScriptEngine> compileElmPreparedJavaScriptEngine;

    private ElmInteractive.CompilationCache lastCompilationCache = ElmInteractive.CompilationCache.Empty;

    private readonly IPineVM pineVM;

    private readonly PineVMCache? pineVMCache;

    private static readonly ConcurrentDictionary<ElmInteractive.CompileInteractiveEnvironmentResult, ElmInteractive.CompileInteractiveEnvironmentResult> compiledEnvironmentCache = new();

    public long? FunctionApplicationCacheLookupCount => pineVMCache?.FunctionApplicationCacheLookupCount;

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
        pineVMCache = pineVMAndCache.pineVMCache;

        compileElmPreparedJavaScriptEngine =
        new(() => ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
            compileElmProgramCodeFiles: compileElmProgramCodeFiles,
            InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8));

        buildPineEvalContextTask = System.Threading.Tasks.Task.Run(() =>
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
        var overrideParseExpression =
            cache is null
            ?
            null
            :
            (PineVM.OverrideParseExprDelegate?)cache.BuildParseExprDelegate;

        var overrideEvaluateExpression =
            cache is null
            ?
            null
            :
            (PineVM.OverrideEvalExprDelegate?)cache.BuildEvalExprDelegate;

        if (autoPGO is not null)
        {
            return
                autoPGO.GetVMAutoUpdating(
                    overrideParseExpression: overrideParseExpression,
                    overrideEvaluateExpression: overrideEvaluateExpression);
        }

        return
            new PineVM(
                overrideParseExpression: overrideParseExpression,
                overrideEvaluateExpression: overrideEvaluateExpression);
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

        try
        {
            return compileEvalContextCache.GetOrAdd(
                key: appCodeTreeHash,
                valueFactory: _ =>
                {
                    var compileInteractiveEnvironmentResults =
                    lastCompilationCache.CompileInteractiveEnvironmentResults
                    .Union(compiledEnvironmentCache.Keys);

                    var resultWithCache =
                        ElmInteractive.CompileInteractiveEnvironment(
                            compileElmPreparedJavaScriptEngine.Value,
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
        }
        finally
        {
            // Build JavaScript engine and warm-up anyway.
            System.Threading.Tasks.Task.Run(() => compileElmPreparedJavaScriptEngine.Value.Evaluate("0"));
        }
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
                buildPineEvalContextTask.Result
                .MapError(error => "Failed to build initial Pine eval context: " + error)
                .AndThen(buildPineEvalContextOk =>
                {
                    clock.Restart();

                    var compileSubmissionResult =
                        ElmInteractive.CompileInteractiveSubmission(
                            compileElmPreparedJavaScriptEngine.Value,
                            environment: buildPineEvalContextOk,
                            submission: submission,
                            addInspectionLogEntry: compileEntry => addInspectionLogEntry?.Invoke("Compile: " + compileEntry),
                            compilationCacheBefore: lastCompilationCache);

                    logDuration("compile");

                    return
                    compileSubmissionResult
                    .MapError(error => "Failed to parse submission: " + error)
                    .AndThen(compileSubmissionOk =>
                    {
                        lastCompilationCache = compileSubmissionOk.cache;

                        return
                        PineVM.ParseExpressionFromValueDefault(compileSubmissionOk.compiledValue)
                        .MapError(error => "Failed to decode expression: " + error)
                        .AndThen(decodeExpressionOk =>
                        {
                            clock.Restart();

                            var evalResult = pineVM.EvaluateExpression(decodeExpressionOk, buildPineEvalContextOk);

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

                                if (evalResultListComponent.Elements.Count != 2)
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
        if (compileElmPreparedJavaScriptEngine.IsValueCreated)
            compileElmPreparedJavaScriptEngine.Value?.Dispose();
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
        var cache = new PineVMCache();

        var profilingVM =
            new ProfilingPineVM(
                overrideParseExpression: cache.BuildParseExprDelegate,
                overrideEvaluateExpression: enableEvalExprCache ? cache.BuildEvalExprDelegate : null);

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
