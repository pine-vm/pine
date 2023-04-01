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
    readonly object submissionLock = new();

    private System.Threading.Tasks.Task<Result<string, PineValue>> buildPineEvalContextTask;

    readonly Lazy<IJsEngine> compileElmPreparedJsEngine =
        new(ElmInteractive.PrepareJsEngineToEvaluateElm(InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8));

    ElmInteractive.CompilationCache lastCompilationCache = ElmInteractive.CompilationCache.Empty;

    readonly PineVM pineVM;

    readonly static ConcurrentDictionary<ElmInteractive.CompileInteractiveEnvironmentResult, ElmInteractive.CompileInteractiveEnvironmentResult> compiledEnvironmentCache = new();

    public InteractiveSessionPine(TreeNodeWithStringPath? appCodeTree, PineVM? pineVM = null)
    {
        this.pineVM = pineVM ?? new PineVM();

        buildPineEvalContextTask = System.Threading.Tasks.Task.Run(() =>
            CompileInteractiveEnvironment(appCodeTree: appCodeTree));
    }

    static private readonly ConcurrentDictionary<string, Result<string, PineValue>> compileEvalContextCache = new();

    private Result<string, PineValue> CompileInteractiveEnvironment(TreeNodeWithStringPath? appCodeTree)
    {
        var appCodeTreeHash =
            appCodeTree switch
            {
                null => "",
                not null => CommonConversion.StringBase16(PineValueComposition.GetHash(PineValueComposition.FromTreeWithStringPath(appCodeTree)))
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
                            compileElmPreparedJsEngine.Value,
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
            // Build JS engine and warm-up anyway.
            System.Threading.Tasks.Task.Run(() => compileElmPreparedJsEngine.Value.Evaluate("0"));
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
                            compileElmPreparedJsEngine.Value,
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
                                        compileElmPreparedJsEngine.Value,
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
        if (compileElmPreparedJsEngine.IsValueCreated)
            compileElmPreparedJsEngine.Value?.Dispose();
    }

    static public Result<string, PineCompileToDotNet.CompileCSharpClassResult> CompileForProfiledScenarios(
        IReadOnlyList<TestElmInteractive.Scenario> scenarios,
        PineCompileToDotNet.SyntaxContainerConfig syntaxContainerConfig,
        int limitNumber)
    {
        var expressionsToCompile =
            scenarios
            .SelectMany(CollectExpressionsToOptimizeFromScenario)
            .Distinct()
            .ToImmutableList();

        return
            PineCompileToDotNet.CompileExpressionsToCSharpFile(
                expressionsToCompile,
                syntaxContainerConfig,
                limitNumber: limitNumber);
    }

    static public IReadOnlyList<Expression> CollectExpressionsToOptimizeFromScenario(TestElmInteractive.Scenario scenario)
    {
        var expressionEvaluations = new ConcurrentQueue<Expression>();

        var profilingPineVM = new PineVM(
            decodeExpressionOverrides: ImmutableDictionary<PineValue, Func<PineValue, Result<string, PineValue>>>.Empty,
            overrideEvaluateExpression: originalHandler => new PineVM.EvalExprDelegate(
                (expression, environment) =>
                {
                    if
                    (expression is not Expression.EnvironmentExpression &&
                    expression is not Expression.LiteralExpression)
                    {
                        expressionEvaluations.Enqueue(expression);
                    }

                    return originalHandler(expression, environment);
                }));

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
