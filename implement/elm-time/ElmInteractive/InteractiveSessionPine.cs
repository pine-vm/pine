using Pine;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using static ElmTime.ElmInteractive.IInteractiveSession;

namespace ElmTime.ElmInteractive;

public class InteractiveSessionPine : IInteractiveSession
{
    readonly object submissionLock = new();

    private System.Threading.Tasks.Task<Result<string, PineValue>> buildPineEvalContextTask;

    readonly Lazy<IJsEngine> evalElmPreparedJsEngine = new(ElmInteractive.PrepareJsEngineToEvaluateElm);

    ElmInteractive.CompilationCache lastCompilationCache = ElmInteractive.CompilationCache.Empty;

    readonly PineVM pineVM = new();

    readonly static ConcurrentDictionary<ElmInteractive.CompileInteractiveEnvironmentResult, ElmInteractive.CompileInteractiveEnvironmentResult> compiledEnvironmentCache = new();

    public InteractiveSessionPine(TreeNodeWithStringPath? appCodeTree)
    {
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
                not null => CommonConversion.StringBase16(Composition.GetHash(Composition.FromTreeWithStringPath(appCodeTree)))
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
                            evalElmPreparedJsEngine.Value,
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
            System.Threading.Tasks.Task.Run(() => evalElmPreparedJsEngine.Value.Evaluate("0"));
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
                            evalElmPreparedJsEngine.Value,
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
                        PineVM.DecodeExpressionFromValue(compileSubmissionOk.compiledValue)
                        .MapError(error => "Failed to decode expression: " + error)
                        .AndThen(decodeExpressionOk =>
                        {
                            clock.Restart();

                            var evalResult = pineVM.EvaluateExpression(buildPineEvalContextOk, decodeExpressionOk);

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
                                        evalElmPreparedJsEngine.Value,
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
        if (evalElmPreparedJsEngine.IsValueCreated)
            evalElmPreparedJsEngine.Value?.Dispose();
    }
}
