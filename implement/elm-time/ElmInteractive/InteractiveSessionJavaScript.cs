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

    readonly TreeNodeWithStringPath? appCodeTree;

    readonly Lazy<IJsEngine> evalElmPreparedJsEngine;

    readonly IList<string> previousSubmissions = new List<string>();

    public InteractiveSessionJavaScript(
        TreeNodeWithStringPath? appCodeTree,
        JavaScriptEngineFlavor javaScriptEngineFlavor)
    {
        evalElmPreparedJsEngine = new Lazy<IJsEngine>(() => ElmInteractive.PrepareJsEngineToEvaluateElm(javaScriptEngineFlavor));
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
