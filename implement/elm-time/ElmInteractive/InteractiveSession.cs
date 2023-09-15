using Pine;
using System;
using System.Collections.Generic;

namespace ElmTime.ElmInteractive;

public interface IInteractiveSession : IDisposable
{
    Result<string, SubmissionResponse> Submit(string submission);

    static ElmEngineType DefaultImplementation => ElmEngineType.JavaScript_V8;

    static public readonly Lazy<TreeNodeWithStringPath> CompileElmProgramCodeFilesDefault =
        new(() => PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(
            ElmInteractive.LoadCompileElmProgramCodeFiles()
            .Extract(error => throw new NotImplementedException(nameof(ElmInteractive.LoadCompileElmProgramCodeFiles) + ": " + error))));

    static IInteractiveSession Create(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        TreeNodeWithStringPath? appCodeTree,
        ElmEngineType engineType) =>
        engineType switch
        {
            ElmEngineType.JavaScript_Jint =>
            new InteractiveSessionJavaScript(
                compileElmProgramCodeFiles: compileElmProgramCodeFiles,
                appCodeTree: appCodeTree,
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.Jint),

            ElmEngineType.JavaScript_V8 =>
            new InteractiveSessionJavaScript(
                compileElmProgramCodeFiles: compileElmProgramCodeFiles,
                appCodeTree: appCodeTree,
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8),

            ElmEngineType.Pine =>
            new InteractiveSessionPine(
                compileElmProgramCodeFiles: compileElmProgramCodeFiles,
                appCodeTree: appCodeTree,
                caching: true),

            ElmEngineType.Pine_without_cache =>
            new InteractiveSessionPine(
                compileElmProgramCodeFiles: compileElmProgramCodeFiles,
                appCodeTree: appCodeTree,
                caching: false),

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
    Pine = 4,
    Pine_without_cache = 4001
}
