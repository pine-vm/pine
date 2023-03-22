using Pine;
using System;
using System.Collections.Generic;

namespace ElmTime.ElmInteractive;

public interface IInteractiveSession : IDisposable
{
    Result<string, SubmissionResponse> Submit(string submission);

    static ElmEngineType DefaultImplementation => ElmEngineType.JavaScript_V8;

    static IInteractiveSession Create(TreeNodeWithStringPath? appCodeTree, ElmEngineType engineType) =>
        engineType switch
        {
            ElmEngineType.JavaScript_Jint =>
            new InteractiveSessionJavaScript(appCodeTree, InteractiveSessionJavaScript.JavaScriptEngineFlavor.Jint),

            ElmEngineType.JavaScript_V8 =>
            new InteractiveSessionJavaScript(appCodeTree, InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8),

            ElmEngineType.Pine => new InteractiveSessionPine(appCodeTree),
            _ => throw new ArgumentOutOfRangeException(nameof(engineType), $"Unexpected engine type value: {engineType}"),
        };

    public record SubmissionResponse(
        ElmInteractive.EvaluatedSctructure interactiveResponse,
        IReadOnlyList<string>? inspectionLog = null);

}

public enum ElmEngineType
{
    JavaScript_Jint = 1,
    JavaScript_V8 = 2,
    Pine = 4
}
