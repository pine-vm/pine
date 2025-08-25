using Pine.Core;
using Pine.PineVM;
using System;
using System.Collections.Generic;

namespace ElmTime.ElmInteractive;

public interface IInteractiveSession : IDisposable
{
    Result<string, SubmissionResponse> Submit(string submission);

    static ElmEngineTypeCLI DefaultImplementation => ElmEngineTypeCLI.Pine;

    static IInteractiveSession Create(
        BlobTreeWithStringPath compilerSourceFiles,
        BlobTreeWithStringPath? appCodeTree,
        ElmEngineType engineType) =>
        engineType switch
        {
            ElmEngineType.Pine pineConfig =>
            new InteractiveSessionPine(
                compilerSourceFiles: compilerSourceFiles,
                appCodeTree: appCodeTree,
                overrideSkipLowering: null,
                entryPointsFilePaths: null,
                caching: pineConfig.Caching,
                autoPGO: pineConfig.DynamicPGOShare),

            _ =>
            throw new ArgumentOutOfRangeException(nameof(engineType), $"Unexpected engine type value: {engineType}"),
        };

    public record SubmissionResponse(
        ElmInteractive.EvaluatedStruct InteractiveResponse,
        IReadOnlyList<string>? InspectionLog = null);
}

public enum ElmEngineTypeCLI
{
    Pine = 4,
    Pine_without_cache = 4001
}

public abstract record ElmEngineType
{
    public record Pine(
        bool Caching,
        DynamicPGOShare? DynamicPGOShare)
        : ElmEngineType;
}
