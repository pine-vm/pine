using ElmTime.JavaScript;
using Pine.Core;
using System;
using System.Collections.Generic;
using static ElmTime.ElmInteractive.IInteractiveSession;

namespace ElmTime.ElmInteractive;


public class InteractiveSessionJavaScript(
    TreeNodeWithStringPath compileElmProgramCodeFiles,
    TreeNodeWithStringPath? appCodeTree,
    InteractiveSessionJavaScript.JavaScriptEngineFlavor javaScriptEngineFlavor)
    : IInteractiveSession
{
    public enum JavaScriptEngineFlavor
    {
        Jint = 1,
        V8 = 2,
    }

    private readonly Lazy<IJavaScriptEngine> evalElmPreparedJavaScriptEngine =
        new(() => ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
            compileElmProgramCodeFiles: compileElmProgramCodeFiles,
            javaScriptEngineFlavor));

    private readonly IList<string> previousSubmissions = [];

    public Result<string, SubmissionResponse> Submit(string submission)
    {
        var result =
            ElmInteractive.EvaluateSubmissionAndGetResultingValue(
                evalElmPreparedJavaScriptEngine.Value,
                appCodeTree: appCodeTree,
                submission: submission,
                previousLocalSubmissions: [.. previousSubmissions]);

        previousSubmissions.Add(submission);

        return result.Map(ir => new SubmissionResponse(ir));
    }

    void IDisposable.Dispose()
    {
        if (evalElmPreparedJavaScriptEngine.IsValueCreated)
            evalElmPreparedJavaScriptEngine.Value?.Dispose();
    }
}
