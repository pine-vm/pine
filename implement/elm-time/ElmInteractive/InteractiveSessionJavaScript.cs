using ElmTime.JavaScript;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using static ElmTime.ElmInteractive.IInteractiveSession;

namespace ElmTime.ElmInteractive;


public class InteractiveSessionJavaScript : IInteractiveSession
{
    public enum JavaScriptEngineFlavor
    {
        Jint = 1,
        V8 = 2,
    }

    private readonly TreeNodeWithStringPath? appCodeTree;

    private readonly Lazy<IJavaScriptEngine> evalElmPreparedJavaScriptEngine;

    private readonly IList<string> previousSubmissions = new List<string>();

    public InteractiveSessionJavaScript(
        TreeNodeWithStringPath? appCodeTree,
        JavaScriptEngineFlavor javaScriptEngineFlavor)
    {
        evalElmPreparedJavaScriptEngine = new Lazy<IJavaScriptEngine>(() => ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(javaScriptEngineFlavor));
        this.appCodeTree = appCodeTree;
    }

    public Result<string, SubmissionResponse> Submit(string submission)
    {
        var result =
            ElmInteractive.EvaluateSubmissionAndGetResultingValue(
                evalElmPreparedJavaScriptEngine.Value,
                appCodeTree: appCodeTree,
                submission: submission,
                previousLocalSubmissions: previousSubmissions.ToImmutableList());

        previousSubmissions.Add(submission);

        return result.Map(ir => new SubmissionResponse(ir));
    }

    void IDisposable.Dispose()
    {
        if (evalElmPreparedJavaScriptEngine.IsValueCreated)
            evalElmPreparedJavaScriptEngine.Value?.Dispose();
    }
}
