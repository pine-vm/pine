using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using static ElmTime.ElmInteractive.IInteractiveSession;

namespace ElmTime.ElmInteractive;


public class InteractiveSessionJavaScript : IInteractiveSession
{
    readonly TreeNodeWithStringPath? appCodeTree;

    readonly Lazy<IJsEngine> evalElmPreparedJsEngine = new(ElmInteractive.PrepareJsEngineToEvaluateElm);

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
