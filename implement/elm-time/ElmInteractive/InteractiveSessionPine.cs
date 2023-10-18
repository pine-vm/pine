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
        TreeNodeWithStringPath? appCodeTree,
        bool caching,
        DynamicPGOShare? autoPGO)
        :
        this(
            compileElmProgramCodeFiles: compileElmProgramCodeFiles,
            appCodeTree: appCodeTree,
            BuildPineVM(caching: caching, autoPGO: autoPGO))
    {
    }

    public InteractiveSessionPine(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        TreeNodeWithStringPath? appCodeTree,
        IPineVM pineVM)
        :
        this(
            compileElmProgramCodeFiles: compileElmProgramCodeFiles,
            appCodeTree: appCodeTree,
            (pineVM, pineVMCache: null))
    {
    }

    private InteractiveSessionPine(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
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
            CompileInteractiveEnvironment(appCodeTree: appCodeTree));
    }

    private static (IPineVM, PineVMCache?) BuildPineVM(
        bool caching,
        DynamicPGOShare? autoPGO)
    {
        var cache = caching ? new PineVMCache() : null;

        var overrideEvaluateExpression =
            cache is null
            ?
            null
            :
            (PineVM.OverrideEvalExprDelegate?)cache.BuildEvalExprDelegate;

        if (autoPGO is not null)
            return (autoPGO.GetVMAutoUpdating(overrideEvaluateExpression: overrideEvaluateExpression), cache);

        return (new PineVM(overrideEvaluateExpression: overrideEvaluateExpression), cache);
    }

    private static readonly ConcurrentDictionary<string, Result<string, PineValue>> compileEvalContextCache = new();

    private Result<string, PineValue> CompileInteractiveEnvironment(TreeNodeWithStringPath? appCodeTree)
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
                    lastCompilationCache.compileInteractiveEnvironmentResults
                    .Union(compiledEnvironmentCache.Keys);

                    var resultWithCache =
                        ElmInteractive.CompileInteractiveEnvironment(
                            compileElmPreparedJavaScriptEngine.Value,
                            appCodeTree: appCodeTree,
                            lastCompilationCache with
                            {
                                compileInteractiveEnvironmentResults = compileInteractiveEnvironmentResults
                            });

                    lastCompilationCache = resultWithCache.Unpack(fromErr: _ => lastCompilationCache, fromOk: ok => ok.compilationCache);

                    foreach (var compileEnvironmentResult in lastCompilationCache.compileInteractiveEnvironmentResults)
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

    public Result<string, ElmInteractive.EvaluatedSctructure> Submit(
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
                        PineVM.DecodeExpressionFromValueDefault(compileSubmissionOk.compiledValue)
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
                                    return Result<string, ElmInteractive.EvaluatedSctructure>.err(
                                        "Type mismatch: Pine expression evaluated to a blob");
                                }

                                if (evalResultListComponent.Elements.Count != 2)
                                {
                                    return Result<string, ElmInteractive.EvaluatedSctructure>.err(
                                        "Type mismatch: Pine expression evaluated to a list with unexpected number of elements: " +
                                        evalResultListComponent.Elements.Count +
                                        " instead of 2");
                                }

                                buildPineEvalContextTask = System.Threading.Tasks.Task.FromResult(
                                    Result<string, PineValue>.ok(evalResultListComponent.Elements[0]));

                                clock.Restart();

                                var parseSubmissionResponseResult =
                                    ElmInteractive.SubmissionResponseFromResponsePineValue(
                                        compileElmPreparedJavaScriptEngine.Value,
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

    void IDisposable.Dispose()
    {
        if (compileElmPreparedJavaScriptEngine.IsValueCreated)
            compileElmPreparedJavaScriptEngine.Value?.Dispose();
    }

    public static Result<string, Pine.CompilePineToDotNet.CompileCSharpClassResult> CompileForProfiledScenarios(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        IReadOnlyList<TestElmInteractive.Scenario> scenarios,
        Pine.CompilePineToDotNet.SyntaxContainerConfig syntaxContainerConfig,
        int limitNumber)
    {
        var expressionsProfiles =
            scenarios
            .Select(scenario =>
            CollectExpressionsToOptimizeFromScenario(
                compileElmProgramCodeFiles: compileElmProgramCodeFiles,
                scenario: scenario))
            .ToImmutableArray();

        var aggregateExpressionsProfiles =
            ProfilingPineVM.AggregateExpressionUsageProfiles(expressionsProfiles);

        var totalTrackedExpressionUsageCount =
            aggregateExpressionsProfiles.Sum(ep => ep.Value.UsageCount);

        var expressionsToCompile =
            aggregateExpressionsProfiles
            .Where(expressionProfile => 4 < expressionProfile.Value.UsageCount)
            .OrderByDescending(expressionAndProfile => expressionAndProfile.Value.UsageCount)
            .Take(limitNumber)
            .Select(expressionAndProfile => expressionAndProfile.Key)
            .ToImmutableList();

        return
            Pine.CompilePineToDotNet.CompileToCSharp.CompileExpressionsToCSharpClass(
                expressionsToCompile,
                syntaxContainerConfig);
    }

    public static IReadOnlyDictionary<Expression, ExpressionUsageProfile> CollectExpressionsToOptimizeFromScenario(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        TestElmInteractive.Scenario scenario)
    {
        var profilingVM = ProfilingPineVM.BuildProfilingVM();

        var profilingSession = new InteractiveSessionPine(
            compileElmProgramCodeFiles: compileElmProgramCodeFiles,
            appCodeTree: null,
            profilingVM.PineVM);

        foreach (var step in scenario.Steps)
            profilingSession.Submit(step.step.Submission);

        var pineExpressionsToOptimize =
            ProfilingPineVM.UsageProfileDictionaryFromListOfUsages(profilingVM.ExpressionEvaluations);

        return pineExpressionsToOptimize;
    }
}
