using Pine;
using Pine.PineVM;
using System;
using System.Collections.Generic;

namespace ElmTime.ElmInteractive;

public interface IInteractiveSession : IDisposable
{
    Result<string, SubmissionResponse> Submit(string submission);

    static ElmEngineTypeCLI DefaultImplementation => ElmEngineTypeCLI.JavaScript_V8;

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

            ElmEngineType.Pine pineConfig =>
            new InteractiveSessionPine(
                compileElmProgramCodeFiles: compileElmProgramCodeFiles,
                appCodeTree: appCodeTree,
                caching: pineConfig.Caching,
                autoPGO: pineConfig.DynamicPGOShare),

            _ =>
            throw new ArgumentOutOfRangeException(nameof(engineType), $"Unexpected engine type value: {engineType}"),
        };

    public record SubmissionResponse(
        ElmInteractive.EvaluatedSctructure interactiveResponse,
        IReadOnlyList<string>? inspectionLog = null);
}

public enum ElmEngineTypeCLI
{
    JavaScript_Jint = 1,
    JavaScript_V8 = 2,
    Pine = 4,
    Pine_without_cache = 4001
}

public abstract record ElmEngineType
{
    public record JavaScript_Jint
        : ElmEngineType;

    public record JavaScript_V8
        : ElmEngineType;

    public record Pine(
        bool Caching,
        DynamicPGOShare? DynamicPGOShare)
        : ElmEngineType;
}