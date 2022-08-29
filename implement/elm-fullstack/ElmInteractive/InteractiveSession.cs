using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace ElmFullstack.ElmInteractive;

public interface IInteractiveSession : IDisposable
{
    Result<string, SubmissionResponse> Submit(string submission);

    static ElmEngineType DefaultImplementation => ElmEngineType.JavaScript;

    static IInteractiveSession Create(TreeNodeWithStringPath? appCodeTree, ElmEngineType engineType) =>
        engineType switch
        {
            ElmEngineType.JavaScript => new InteractiveSessionJavaScript(appCodeTree),
            ElmEngineType.Pine => new InteractiveSessionPine(appCodeTree),
            _ => throw new ArgumentOutOfRangeException(nameof(engineType), $"Unexpected engine type value: {engineType}"),
        };
}

public enum ElmEngineType
{
    JavaScript = 1,
    Pine = 4
}

public class InteractiveSessionJavaScript : IInteractiveSession
{
    readonly TreeNodeWithStringPath? appCodeTree;

    readonly Lazy<JavaScriptEngineSwitcher.Core.IJsEngine> evalElmPreparedJsEngine =
        new(ElmInteractive.PrepareJsEngineToEvaluateElm);

    readonly IList<string> previousSubmissions = new List<string>();

    public InteractiveSessionJavaScript(TreeNodeWithStringPath? appCodeTree)
    {
        this.appCodeTree = appCodeTree;
    }

    public Result<string, SubmissionResponse> Submit(string submission)
    {
        var result =
            ElmInteractive.EvaluateSubmissionAndGetResultingValue(
                evalElmPreparedJsEngine.Value,
                appCodeTree: appCodeTree,
                submission: submission,
                previousLocalSubmissions: previousSubmissions.ToImmutableList());

        previousSubmissions.Add(submission);

        return result.Map(ir => new SubmissionResponse(ir));
    }

    void IDisposable.Dispose()
    {
        if (evalElmPreparedJsEngine.IsValueCreated)
            evalElmPreparedJsEngine.Value?.Dispose();
    }
}

public class InteractiveSessionPine : IInteractiveSession
{
    readonly object submissionLock = new();

    private System.Threading.Tasks.Task<Result<string, PineValue>> buildPineEvalContextTask;

    readonly Lazy<JavaScriptEngineSwitcher.Core.IJsEngine> evalElmPreparedJsEngine = new(ElmInteractive.PrepareJsEngineToEvaluateElm);

    ElmInteractive.CompilationCache? lastSubmissionCompilationCache;

    readonly PineVM pineVM = new();

    public InteractiveSessionPine(TreeNodeWithStringPath? appCodeTree)
    {
        buildPineEvalContextTask = System.Threading.Tasks.Task.Run(() =>
            ElmInteractive.CompileEvalContextForElmInteractive(
                evalElmPreparedJsEngine.Value,
                appCodeTree: appCodeTree));
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

            var buildPineEvalContextResult = buildPineEvalContextTask.Result;

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
                            compilationCacheBefore: lastSubmissionCompilationCache);

                    logDuration("compile");

                    return
                    compileSubmissionResult
                    .MapError(error => "Failed to parse submission: " + error)
                    .AndThen(compileSubmissionOk =>
                    {
                        lastSubmissionCompilationCache = compileSubmissionOk.cache;

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

public record SubmissionResponse(
    ElmInteractive.EvaluatedSctructure interactiveResponse,
    IReadOnlyList<string>? inspectionLog = null);
