using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using static Pine.Composition;

namespace elm_fullstack.ElmInteractive;

public class InteractiveSession : IDisposable
{
    readonly TreeWithStringPath appCodeTree;

    readonly Lazy<JavaScriptEngineSwitcher.Core.IJsEngine> evalElmPreparedJsEngine =
        new(ElmInteractive.PrepareJsEngineToEvaluateElm);

    readonly IList<string> previousSubmissions = new List<string>();

    public InteractiveSession(TreeWithStringPath appCodeTree)
    {
        this.appCodeTree = appCodeTree;
    }

    public Pine.Result<string, ElmInteractive.SubmissionResponseValueStructure> SubmitAndGetResultingValue(string submission)
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
