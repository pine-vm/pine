using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using Kalmit;
using static Kalmit.Composition;

namespace elm_fullstack
{

    public class InteractiveSession : IDisposable
    {
        readonly Composition.TreeWithStringPath appCodeTree;

        readonly Lazy<JavaScriptEngineSwitcher.Core.IJsEngine> evalElmPreparedJsEngine =
            new Lazy<JavaScriptEngineSwitcher.Core.IJsEngine>(ElmEngine.EvaluateElm.PrepareJsEngineToEvaluateElm);

        readonly IList<string> previousSubmissions = new List<string>();

        public InteractiveSession(Composition.TreeWithStringPath appCodeTree)
        {
            this.appCodeTree = appCodeTree;
        }

        public Result<string, ElmEngine.EvaluateElm.SubmissionResponseValueStructure> SubmitAndGetResultingValue(string submission)
        {
            var result =
                ElmEngine.EvaluateElm.EvaluateSubmissionAndGetResultingValue(
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
}