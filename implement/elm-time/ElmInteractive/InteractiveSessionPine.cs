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

    private readonly Lazy<IJavaScriptEngine> compileElmPreparedJavaScriptEngine =
        new(ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8));

    private ElmInteractive.CompilationCache lastCompilationCache = ElmInteractive.CompilationCache.Empty;

    private readonly PineVM pineVM;

    private readonly PineVMCache? pineVMCache;

    private static readonly ConcurrentDictionary<ElmInteractive.CompileInteractiveEnvironmentResult, ElmInteractive.CompileInteractiveEnvironmentResult> compiledEnvironmentCache = new();

    public long? FunctionApplicationCacheLookupCount => pineVMCache?.FunctionApplicationCacheLookupCount;

    public long EvaluateExpressionCount => pineVM.EvaluateExpressionCount;

    public InteractiveSessionPine(TreeNodeWithStringPath? appCodeTree, bool caching)
        :
        this(appCodeTree: appCodeTree, BuildPineVM(caching))
    {
    }

    public InteractiveSessionPine(TreeNodeWithStringPath? appCodeTree, PineVM pineVM)
        :
        this(appCodeTree: appCodeTree, (pineVM, pineVMCache: null))
    {
    }

    private InteractiveSessionPine(TreeNodeWithStringPath? appCodeTree, (PineVM pineVM, PineVMCache? pineVMCache) pineVMAndCache)
    {
        pineVM = pineVMAndCache.pineVM;
        pineVMCache = pineVMAndCache.pineVMCache;

        buildPineEvalContextTask = System.Threading.Tasks.Task.Run(() =>
            CompileInteractiveEnvironment(appCodeTree: appCodeTree));
    }

    private static (PineVM, PineVMCache?) BuildPineVM(bool caching)
    {
        var cache = caching ? new PineVMCache() : null;

        var overrideEvaluateExpression =
            cache is null
            ?
            null
            :
            (PineVM.OverrideEvalExprDelegate?)cache.BuildEvalExprDelegate;

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

    public static Result<string, PineCompileToDotNet.CompileCSharpClassResult> CompileForProfiledScenarios(
        IReadOnlyList<TestElmInteractive.Scenario> scenarios,
        PineCompileToDotNet.SyntaxContainerConfig syntaxContainerConfig,
        int limitNumber)
    {
        var expressionsToCompile =
            scenarios
            .SelectMany(CollectExpressionsToOptimizeFromScenario)
            .Distinct()
            .Take(limitNumber)
            .ToImmutableList();

        return
            PineCompileToDotNet.CompileExpressionsToCSharpClass(
                expressionsToCompile,
                syntaxContainerConfig);
    }

    public static IReadOnlyList<Expression> CollectExpressionsToOptimizeFromScenario(TestElmInteractive.Scenario scenario)
    {
        var expressionEvaluations = new ConcurrentQueue<Expression>();

        var profilingPineVM = new PineVM(
            decodeExpressionOverrides: ImmutableDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>.Empty,
            overrideEvaluateExpression: originalHandler => (expression, environment) =>
            {
                if (expression is Expression.DecodeAndEvaluateExpression decodeAndEvaluateExpression)
                {
                    originalHandler(decodeAndEvaluateExpression.expression, environment)
                    .AndThen(PineVM.DecodeExpressionFromValueDefault)
                    .MapError(err =>
                    {
                        Console.WriteLine("Failed to decode expression: " + err);
                        return err;
                    })
                    .Map(innerExpr =>
                    {
                        expressionEvaluations.Enqueue(innerExpr);

                        return innerExpr;
                    });
                }

                return originalHandler(expression, environment);
            });

        var profilingSession = new InteractiveSessionPine(appCodeTree: null, profilingPineVM);

        foreach (var step in scenario.steps)
            profilingSession.Submit(step.step.submission);

        var pineExpressionsToOptimize =
            expressionEvaluations
            .Distinct()
            .Select(expression =>
            {
                var usageCount = expressionEvaluations.Count(c => c == expression);

                return (expression, stats: new { usageCount });
            })
            .OrderByDescending(expressionAndStats => expressionAndStats.stats.usageCount)
            .ToImmutableList();

        return
            pineExpressionsToOptimize
            .Select(expressionAndStats => expressionAndStats.expression)
            .ToImmutableList();
    }
}
