using Pine;
using System;
using System.Collections.Generic;

namespace ElmTime.ElmInteractive;

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

    public record SubmissionResponse(
        ElmInteractive.EvaluatedSctructure interactiveResponse,
        IReadOnlyList<string>? inspectionLog = null);

}

public enum ElmEngineType
{
    JavaScript = 1,
    Pine = 4
}
