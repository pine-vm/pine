using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using static Pine.Composition;

namespace elm_fullstack.ElmInteractive;

public interface IInteractiveSession : IDisposable
{
    Pine.Result<string, ElmInteractive.EvaluatedSctructure> Submit(string submission);

    static ImplementationType DefaultImplementation => ImplementationType.Old;

    static IInteractiveSession Create(TreeWithStringPath? appCodeTree, ImplementationType type) =>
        type switch
        {
            ImplementationType.Old => new InteractiveSessionClassic(appCodeTree),
            ImplementationType.New => new InteractiveSessionNew(appCodeTree),
            _ => throw new ArgumentOutOfRangeException(nameof(type), $"Not expected type value: {type}"),
        };
}

public enum ImplementationType
{
    Old = 1,
    New = 4
}

public class InteractiveSessionClassic : IInteractiveSession
{
    readonly TreeWithStringPath? appCodeTree;

    readonly Lazy<JavaScriptEngineSwitcher.Core.IJsEngine> evalElmPreparedJsEngine =
        new(ElmInteractive.PrepareJsEngineToEvaluateElm);

    readonly IList<string> previousSubmissions = new List<string>();

    public InteractiveSessionClassic(TreeWithStringPath? appCodeTree)
    {
        this.appCodeTree = appCodeTree;
    }

    public Pine.Result<string, ElmInteractive.EvaluatedSctructure> Submit(string submission)
    {
        var result =
            ElmInteractive.EvaluateSubmissionAndGetResultingValue(
                evalElmPreparedJsEngine.Value,
                appCodeTree: appCodeTree,
                submission: submission,
                previousLocalSubmissions: previousSubmissions.ToImmutableList());

        previousSubmissions.Add(submission);

        return result;
    }

    void IDisposable.Dispose()
    {
        if (evalElmPreparedJsEngine.IsValueCreated)
            evalElmPreparedJsEngine.Value?.Dispose();
    }
}

public class InteractiveSessionNew : IInteractiveSession
{
    readonly object submissionLock = new();

    private System.Threading.Tasks.Task<Pine.Result<string, Component>> buildPineEvalContextTask;

    readonly Lazy<JavaScriptEngineSwitcher.Core.IJsEngine> evalElmPreparedJsEngine = new(ElmInteractive.PrepareJsEngineToEvaluateElm);

    public InteractiveSessionNew(TreeWithStringPath? appCodeTree)
    {
        buildPineEvalContextTask = System.Threading.Tasks.Task.Run(() =>
            ElmInteractive.PineEvalContextForElmInteractive(
                evalElmPreparedJsEngine.Value,
                appCodeTree: appCodeTree));
    }

    public Pine.Result<string, ElmInteractive.EvaluatedSctructure> Submit(string submission)
    {
        lock (submissionLock)
        {
            var buildPineEvalContextResult = buildPineEvalContextTask.Result;

            if (buildPineEvalContextResult.Ok == null)
            {
                return Pine.Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Failed to build initial Pine eval context: " + buildPineEvalContextResult.Err);
            }

            var parseSubmissionResult =
                ElmInteractive.CompileInteractiveSubmissionIntoPineExpression(
                    evalElmPreparedJsEngine.Value,
                    environment: buildPineEvalContextResult.Ok,
                    submission: submission);

            if (parseSubmissionResult.Ok == null)
            {
                return Pine.Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Failed to parse submission: " + parseSubmissionResult.Err);
            }

            var decodeExpressionResult = Pine.PineVM.DecodeExpressionFromValue(parseSubmissionResult.Ok);

            if (decodeExpressionResult.Ok == null)
            {
                return Pine.Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Failed to decode expression: " + decodeExpressionResult.Err);
            }

            var evalResult = Pine.PineVM.EvaluateExpression(buildPineEvalContextResult.Ok, decodeExpressionResult.Ok);

            if (evalResult.Ok == null)
            {
                return Pine.Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Failed to evaluate expression in PineVM: " + evalResult.Err);
            }

            if (evalResult.Ok.ListContent == null)
            {
                return Pine.Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Type mismatch: Pine expression evaluated to a blob");
            }

            if (evalResult.Ok.ListContent.Count != 2)
            {
                return Pine.Result<string, ElmInteractive.EvaluatedSctructure>.err(
                    "Type mismatch: Pine expression evaluated to a list with unexpected number of elements: " +
                    evalResult.Ok.ListContent.Count +
                    " instead of 2");
            }

            buildPineEvalContextTask = System.Threading.Tasks.Task.FromResult(Pine.Result<string, Component>.ok(evalResult.Ok.ListContent[0]));

            var parseSubmissionResponseResult =
                ElmInteractive.SubmissionResponseFromResponsePineValue(
                    evalElmPreparedJsEngine.Value,
                    response: evalResult.Ok.ListContent[1]);

            if (parseSubmissionResponseResult.Ok == null)
                return Pine.Result<string, ElmInteractive.EvaluatedSctructure>.err(
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
