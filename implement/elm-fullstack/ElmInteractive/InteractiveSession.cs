using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using static Pine.Composition;

namespace elm_fullstack.ElmInteractive;

public interface IInteractiveSession : IDisposable
{
    Pine.Result<string, SubmissionResponse> Submit(string submission);

    static ElmEngineType DefaultImplementation => ElmEngineType.JavaScript;

    static IInteractiveSession Create(TreeWithStringPath? appCodeTree, ElmEngineType engineType) =>
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
    readonly TreeWithStringPath? appCodeTree;

    readonly Lazy<JavaScriptEngineSwitcher.Core.IJsEngine> evalElmPreparedJsEngine =
        new(ElmInteractive.PrepareJsEngineToEvaluateElm);

    readonly IList<string> previousSubmissions = new List<string>();

    public InteractiveSessionJavaScript(TreeWithStringPath? appCodeTree)
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

        return result.map(ir => new SubmissionResponse(ir));
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

    private System.Threading.Tasks.Task<Result<string, Component>> buildPineEvalContextTask;

    readonly Lazy<JavaScriptEngineSwitcher.Core.IJsEngine> evalElmPreparedJsEngine = new(ElmInteractive.PrepareJsEngineToEvaluateElm);

    public InteractiveSessionPine(TreeWithStringPath? appCodeTree)
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
            .map(r => new SubmissionResponse(r, inspectionLog));
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

            if (buildPineEvalContextResult.Ok == null)
            {
                return Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Failed to build initial Pine eval context: " + buildPineEvalContextResult.Err);
            }

            clock.Restart();

            var compileSubmissionResult =
                ElmInteractive.CompileInteractiveSubmission(
                    evalElmPreparedJsEngine.Value,
                    environment: buildPineEvalContextResult.Ok,
                    submission: submission,
                    addInspectionLogEntry: compileEntry => addInspectionLogEntry?.Invoke("Compile: " + compileEntry));

            logDuration("compile");

            if (compileSubmissionResult.Ok == null)
            {
                return Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Failed to parse submission: " + compileSubmissionResult.Err);
            }

            var decodeExpressionResult = PineVM.DecodeExpressionFromValue(compileSubmissionResult.Ok);

            if (decodeExpressionResult.Ok == null)
            {
                return Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Failed to decode expression: " + decodeExpressionResult.Err);
            }

            clock.Restart();

            var evalResult = PineVM.EvaluateExpression(buildPineEvalContextResult.Ok, decodeExpressionResult.Ok);

            logDuration("eval");

            if (evalResult.Ok == null)
            {
                return Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Failed to evaluate expression in PineVM: " + evalResult.Err);
            }

            if (evalResult.Ok is not ListComponent evalResultListComponent)
            {
                return Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Type mismatch: Pine expression evaluated to a blob");
            }

            if (evalResultListComponent.ListContent.Count != 2)
            {
                return Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Type mismatch: Pine expression evaluated to a list with unexpected number of elements: " +
                    evalResultListComponent.ListContent.Count +
                    " instead of 2");
            }

            buildPineEvalContextTask = System.Threading.Tasks.Task.FromResult(Result<string, Component>.ok(evalResultListComponent.ListContent[0]));

            clock.Restart();

            var parseSubmissionResponseResult =
                ElmInteractive.SubmissionResponseFromResponsePineValue(
                    evalElmPreparedJsEngine.Value,
                    response: evalResultListComponent.ListContent[1]);

            logDuration("parse-result");

            if (parseSubmissionResponseResult.Ok == null)
                return Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Failed to parse submission response: " + parseSubmissionResponseResult.Err);

            return parseSubmissionResponseResult!;
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
