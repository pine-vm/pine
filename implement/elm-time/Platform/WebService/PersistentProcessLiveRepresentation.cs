using ElmTime.JavaScript;
using ElmTime.Platform.WebService.ProcessStoreSupportingMigrations;
using Pine;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace ElmTime.Platform.WebService;

public interface IPersistentProcess
{
    Result<string, StateShim.InterfaceToHost.FunctionApplicationResult> ProcessElmAppEvent(
        IProcessStoreWriter storeWriter, string serializedEvent);

    (ProvisionalReductionRecordInFile? reductionRecord, StoreProvisionalReductionReport report) StoreReductionRecordForCurrentState(
        IProcessStoreWriter storeWriter);
}

public record struct StoreProvisionalReductionReport(
    int lockTimeSpentMilli,
    int? getElmAppStateFromEngineTimeSpentMilli,
    int? serializeElmAppStateToStringTimeSpentMilli,
    int? serializeElmAppStateLength,
    int? storeDependenciesTimeSpentMilli);

public record struct ProcessAppConfig(
    PineValue appConfigComponent,
    ProcessFromElm019Code.BuildArtifacts buildArtifacts);

public class PersistentProcessLiveRepresentation : IPersistentProcess, IDisposable
{
    private readonly object processLock = new();

    private string lastCompositionLogRecordHashBase16;

    public readonly ProcessAppConfig lastAppConfig;

    private readonly IDisposableProcessWithStringInterface lastElmAppVolatileProcess;

    public record struct CompositionLogRecordWithResolvedDependencies(
        CompositionLogRecordInFile compositionRecord,
        string compositionRecordHashBase16,
        ReductionWithResolvedDependencies? reduction,
        CompositionEventWithResolvedDependencies? composition);

    public record struct ReductionWithResolvedDependencies(
        ReadOnlyMemory<byte> elmAppState,
        PineValue appConfig,
        TreeNodeWithStringPath appConfigAsTree);

    public record struct CompositionEventWithResolvedDependencies(
        byte[]? UpdateElmAppStateForEvent = null,
        byte[]? SetElmAppState = null,
        byte[]? ApplyFunctionOnElmAppState = null,
        TreeNodeWithStringPath? DeployAppConfigAndInitElmAppState = null,
        TreeNodeWithStringPath? DeployAppConfigAndMigrateElmAppState = null);

    public static ProcessFromElm019Code.PreparedProcess ProcessFromDeployment(
        TreeNodeWithStringPath deployment,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null,
        Func<IJavaScriptEngine>? overrideJavaScriptEngineFactory = null)
    {
        var deploymentFiles = PineValueComposition.TreeToFlatDictionaryWithPathComparer(deployment);

        var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
            sourceFiles: deploymentFiles,
            workingDirectoryRelative: ImmutableList<string>.Empty,
            ElmAppInterfaceConfig.Default)
            .Extract(error => throw new Exception(ElmAppCompilation.CompileCompilationErrorsDisplayText(error)));

        var (loweredAppFiles, _) = compilationResult;

        return
            ProcessFromElm019Code.ProcessFromElmCodeFiles(
                loweredAppFiles.compiledFiles,
                overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig,
                overrideJavaScriptEngineFactory: overrideJavaScriptEngineFactory);
    }

    private PersistentProcessLiveRepresentation(
        string lastCompositionLogRecordHashBase16,
        ProcessAppConfig lastAppConfig,
        IDisposableProcessWithStringInterface lastElmAppVolatileProcess)
    {
        this.lastCompositionLogRecordHashBase16 = lastCompositionLogRecordHashBase16;
        this.lastAppConfig = lastAppConfig;
        this.lastElmAppVolatileProcess = lastElmAppVolatileProcess;
    }

    public static (IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> files, string lastCompositionLogRecordHashBase16)
        GetFilesForRestoreProcess(
        IFileStoreReader fileStoreReader)
    {
        var filesForProcessRestore = new ConcurrentDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        var recordingReader = new DelegatingFileStoreReader
        (
            ListFilesInDirectoryDelegate: fileStoreReader.ListFilesInDirectory,
            GetFileContentDelegate: filePath =>
            {
                var fileContent = fileStoreReader.GetFileContent(filePath);

                if (fileContent is not null)
                {
                    filesForProcessRestore[filePath] = fileContent.Value;
                }

                return fileContent;
            }
        );

        var compositionLogRecords =
            EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(new ProcessStoreReaderInFileStore(recordingReader))
            .ToImmutableList();

        return (
            files: filesForProcessRestore.ToImmutableDictionary(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>()),
            lastCompositionLogRecordHashBase16: compositionLogRecords.LastOrDefault().compositionRecordHashBase16);
    }

    private static IEnumerable<CompositionLogRecordWithResolvedDependencies>
        EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(IProcessStoreReader storeReader) =>
            storeReader
            .EnumerateSerializedCompositionLogRecordsReverse()
            .Select(serializedCompositionLogRecord =>
            {
                var compositionRecordHashBase16 =
                    CompositionLogRecordInFile.HashBase16FromCompositionRecord(serializedCompositionLogRecord);

                var compositionRecord =
                JsonSerializer.Deserialize<CompositionLogRecordInFile>(
                    Encoding.UTF8.GetString(serializedCompositionLogRecord))!;

                var reductionRecord = storeReader.LoadProvisionalReduction(compositionRecordHashBase16);

                ReductionWithResolvedDependencies? reduction = null;

                if (reductionRecord?.appConfig?.HashBase16 != null && reductionRecord?.elmAppState?.HashBase16 != null)
                {
                    var appConfigComponent = storeReader.LoadComponent(reductionRecord.appConfig.HashBase16);

                    var elmAppStateComponent = storeReader.LoadComponent(reductionRecord.elmAppState.HashBase16);

                    if (appConfigComponent != null && elmAppStateComponent != null)
                    {
                        var appConfigAsTree =
                        PineValueComposition.ParseAsTreeWithStringPath(appConfigComponent)
                        .Extract(_ => throw new Exception("Unexpected content of appConfigComponent " + reductionRecord.appConfig?.HashBase16 + ": Failed to parse as tree."));

                        if (elmAppStateComponent is not PineValue.BlobValue elmAppStateComponentBlob)
                        {
                            throw new Exception("Unexpected content of elmAppStateComponent " + reductionRecord.elmAppState?.HashBase16 + ": This is not a blob.");
                        }

                        reduction = new ReductionWithResolvedDependencies
                        (
                            appConfig: appConfigComponent,
                            appConfigAsTree: appConfigAsTree,
                            elmAppState: elmAppStateComponentBlob.Bytes
                        );
                    }
                }

                return new CompositionLogRecordWithResolvedDependencies
                (
                    compositionRecord: compositionRecord,
                    compositionRecordHashBase16: compositionRecordHashBase16,
                    composition: LoadCompositionEventDependencies(compositionRecord.compositionEvent, storeReader),
                    reduction: reduction
                );
            })
            .TakeUntil(compositionAndReduction => compositionAndReduction.reduction != null)
            .Reverse();

    public static Result<string, RestoreFromCompositionEventSequenceResult>
        LoadFromStoreAndRestoreProcess(
        IProcessStoreReader storeReader,
        Action<string>? logger,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null,
        Func<IJavaScriptEngine>? overrideJavaScriptEngineFactory = null)
    {
        var restoreStopwatch = System.Diagnostics.Stopwatch.StartNew();

        logger?.Invoke("Begin to restore the process state.");

        var compositionEventsFromLatestReduction =
            EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(storeReader)
            .ToImmutableList();

        if (compositionEventsFromLatestReduction.IsEmpty)
        {
            var message = "Found no composition record, default to initial state.";

            logger?.Invoke(message);

            return
                Result<string, RestoreFromCompositionEventSequenceResult>.err(message);
        }

        logger?.Invoke("Found " + compositionEventsFromLatestReduction.Count + " composition log records to use for restore.");

        var processLiveRepresentation = RestoreFromCompositionEventSequence(
            compositionEventsFromLatestReduction,
            overrideElmAppInterfaceConfig,
            overrideJavaScriptEngineFactory);

        logger?.Invoke("Restored the process state in " + (int)restoreStopwatch.Elapsed.TotalSeconds + " seconds.");

        return processLiveRepresentation;
    }

    public record RestoreFromCompositionEventSequenceResult(
        PersistentProcessLiveRepresentation process,
        InterfaceToHost.BackendEventResponseStruct? initOrMigrateCmds);

    public static Result<string, RestoreFromCompositionEventSequenceResult>
        RestoreFromCompositionEventSequence(
        IEnumerable<CompositionLogRecordWithResolvedDependencies> compositionLogRecords,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null,
        Func<IJavaScriptEngine>? overrideJavaScriptEngineFactory = null)
    {
        var firstCompositionLogRecord =
            compositionLogRecords.FirstOrDefault();

        if (firstCompositionLogRecord.reduction == null &&
            firstCompositionLogRecord.compositionRecord.parentHashBase16 != CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16)
        {
            return
                Result<string, RestoreFromCompositionEventSequenceResult>.err(
                    "Failed to get sufficient history: Composition log record points to parent " +
                    firstCompositionLogRecord.compositionRecord.parentHashBase16);
        }

        var initialProcessRepresentation = new PersistentProcessLiveRepresentationDuringRestore(
            lastCompositionLogRecordHashBase16: null,
            lastAppConfig: null,
            lastElmAppVolatileProcess: null,
            initOrMigrateCmds: null);


        Result<string, PersistentProcessLiveRepresentationDuringRestore> integrateCompositionLogRecord(
            PersistentProcessLiveRepresentationDuringRestore process,
            CompositionLogRecordWithResolvedDependencies compositionLogRecord)
        {
            Result<string, PersistentProcessLiveRepresentationDuringRestore> continueOk(
                PersistentProcessLiveRepresentationDuringRestore process) =>
                Result<string, PersistentProcessLiveRepresentationDuringRestore>.ok(process
                with
                {
                    lastCompositionLogRecordHashBase16 = compositionLogRecord.compositionRecordHashBase16
                });

            var compositionEvent = compositionLogRecord.compositionRecord.compositionEvent;

            if (compositionLogRecord.reduction is { } reductionWithResolvedDependencies)
            {
                var prepareProcessResult =
                    ProcessFromDeployment(
                        reductionWithResolvedDependencies.appConfigAsTree,
                        overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig,
                        overrideJavaScriptEngineFactory: overrideJavaScriptEngineFactory);

                var newElmAppProcess = prepareProcessResult.startProcess();

                var elmAppState = JsonSerializer.Deserialize<JsonElement>(reductionWithResolvedDependencies.elmAppState.Span);

                return
                    StateShim.StateShim.SetAppStateOnMainBranch(process: newElmAppProcess, stateJson: elmAppState)
                    .AndThen(_ =>
                    {
                        process.lastElmAppVolatileProcess?.Dispose();

                        return
                            continueOk(
                                process
                                with
                                {
                                    lastAppConfig = new ProcessAppConfig(
                                        reductionWithResolvedDependencies.appConfig,
                                        prepareProcessResult.buildArtifacts),
                                    lastElmAppVolatileProcess = newElmAppProcess,
                                    initOrMigrateCmds = null
                                });
                    });
            }

            if (compositionEvent.RevertProcessTo != null)
            {
                if (compositionEvent.RevertProcessTo.HashBase16 != process.lastCompositionLogRecordHashBase16)
                {
                    return
                        Result<string, PersistentProcessLiveRepresentationDuringRestore>.err(
                            "Error in enumeration of process composition events: Got revert to " +
                            compositionEvent.RevertProcessTo.HashBase16 +
                            ", but previous version in the enumerated sequence was " + process.lastCompositionLogRecordHashBase16 + ".");
                }

                return continueOk(process);
            }

            return
                ApplyCompositionEvent(
                    compositionLogRecord.composition!.Value,
                    process,
                    overrideElmAppInterfaceConfig,
                    overrideJavaScriptEngineFactory: overrideJavaScriptEngineFactory)
                .AndThen(continueOk);
        }

        var aggregateLogRecordsResult =
            compositionLogRecords.Aggregate(
                seed: Result<string, PersistentProcessLiveRepresentationDuringRestore>.ok(initialProcessRepresentation),
                func: (intermediateResult, compositionLogRecord) =>
                intermediateResult.AndThen(intermediateOk =>
                integrateCompositionLogRecord(intermediateOk, compositionLogRecord)));

        return
            aggregateLogRecordsResult
            .AndThen(aggregateOk =>
            {
                if (aggregateOk.lastCompositionLogRecordHashBase16 is null ||
                    aggregateOk.lastAppConfig is null ||
                    aggregateOk.lastElmAppVolatileProcess is null)
                {
                    return Result<string, RestoreFromCompositionEventSequenceResult>.err(
                        "Failed to get sufficient history: " + nameof(compositionLogRecords) + " does not contain app init.");
                }

                return
                    Result<string, RestoreFromCompositionEventSequenceResult>.ok(
                        new RestoreFromCompositionEventSequenceResult(
                            new PersistentProcessLiveRepresentation(
                                lastCompositionLogRecordHashBase16: aggregateOk.lastCompositionLogRecordHashBase16,
                                lastAppConfig: aggregateOk.lastAppConfig.Value,
                                lastElmAppVolatileProcess: aggregateOk.lastElmAppVolatileProcess),
                            aggregateOk.initOrMigrateCmds));
            });
    }

    private record PersistentProcessLiveRepresentationDuringRestore(
        string? lastCompositionLogRecordHashBase16,
        ProcessAppConfig? lastAppConfig,
        IDisposableProcessWithStringInterface? lastElmAppVolatileProcess,
        InterfaceToHost.BackendEventResponseStruct? initOrMigrateCmds);

    private static Result<string, PersistentProcessLiveRepresentationDuringRestore> ApplyCompositionEvent(
        CompositionEventWithResolvedDependencies compositionEvent,
        PersistentProcessLiveRepresentationDuringRestore processBefore,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig,
        Func<IJavaScriptEngine>? overrideJavaScriptEngineFactory = null)
    {
        if (compositionEvent.UpdateElmAppStateForEvent is { } updateElmAppStateForEvent)
        {
            return
                TranslateUpdateElmAppEventFromStore(updateElmAppStateForEvent)
                .MapError(err => "Failed to migrate event update Elm app event string to current version: " + err)
                .AndThen(updateElmAppEvent =>
                {
                    var eventStateShimRequest =
                        new StateShim.InterfaceToHost.StateShimRequestStruct.ApplyFunctionShimRequest(
                            new StateShim.InterfaceToHost.ApplyFunctionShimRequestStruct(
                                functionName: updateElmAppEvent.functionName,
                                arguments: updateElmAppEvent.arguments.MapStateArgument(takesState =>
                                takesState ?
                                Maybe<StateShim.InterfaceToHost.StateSource>.just(
                                    new StateShim.InterfaceToHost.StateSource.BranchStateSource("main")) :
                                    Maybe<StateShim.InterfaceToHost.StateSource>.nothing()),
                                stateDestinationBranches: ["main"]));

                    var eventString = JsonSerializer.Serialize<StateShim.InterfaceToHost.StateShimRequestStruct>(eventStateShimRequest);

                    if (processBefore.lastElmAppVolatileProcess == null)
                        return Result<string, PersistentProcessLiveRepresentationDuringRestore>.ok(processBefore);

                    var processEventReturnValue =
                        processBefore.lastElmAppVolatileProcess.ProcessEvent(eventString);

                    var processEventResult =
                        JsonSerializer.Deserialize<Result<string, object>>(processEventReturnValue);

                    return processEventResult.Map(_ => processBefore);
                });
        }

        if (compositionEvent.ApplyFunctionOnElmAppState is { } applyFunctionOnElmAppStateSerial)
        {
            var applyFunctionOnElmAppState =
                JsonSerializer.Deserialize<CompositionLogRecordInFile.ApplyFunctionOnStateEvent>(applyFunctionOnElmAppStateSerial);

            if (processBefore.lastElmAppVolatileProcess is null)
                return Result<string, PersistentProcessLiveRepresentationDuringRestore>.err("Process is null, no app deployed");

            return
                StateShim.StateShim.ApplyFunctionOnMainBranch(
                    process: processBefore.lastElmAppVolatileProcess,
                    new AdminInterface.ApplyDatabaseFunctionRequest(
                        functionName: applyFunctionOnElmAppState.functionName,
                        serializedArgumentsJson: applyFunctionOnElmAppState.serializedArgumentsJson,
                        commitResultingState: true))
                .Map(_ => processBefore);
        }

        if (compositionEvent.SetElmAppState is { } setElmAppState)
        {
            if (processBefore.lastElmAppVolatileProcess == null)
            {
                return Result<string, PersistentProcessLiveRepresentationDuringRestore>.err(
                    "Failed to load the serialized state with the elm app: Looks like no app was deployed so far.");
            }

            var projectedElmAppState = JsonSerializer.Deserialize<JsonElement>(setElmAppState);

            return
                StateShim.StateShim.SetAppStateOnMainBranch(
                    process: processBefore.lastElmAppVolatileProcess,
                    stateJson: projectedElmAppState)
                .MapError(error => "Set state function in the hosted app returned an error: " + error)
                .Map(_ => processBefore);
        }

        if (compositionEvent.DeployAppConfigAndMigrateElmAppState is { } deployAppConfigAndMigrateElmAppState)
        {
            var elmAppStateBefore =
                processBefore.lastElmAppVolatileProcess is null ?
                (JsonElement?)null
                :
                StateShim.StateShim.GetAppStateFromMainBranch(processBefore.lastElmAppVolatileProcess)
                .Extract(err => throw new Exception("Failed to get serialized state: " + err));

            if (!elmAppStateBefore.HasValue)
                return Result<string, PersistentProcessLiveRepresentationDuringRestore>.err("No state from previous app");

            var prepareProcessResult =
                ProcessFromDeployment(
                    deployAppConfigAndMigrateElmAppState,
                    overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig,
                    overrideJavaScriptEngineFactory: overrideJavaScriptEngineFactory);

            var newElmAppProcess = prepareProcessResult.startProcess();

            var migrateEventResult = AttemptProcessRequest(
                newElmAppProcess,
                new StateShim.InterfaceToHost.StateShimRequestStruct.ApplyFunctionShimRequest(
                    new StateShim.InterfaceToHost.ApplyFunctionShimRequestStruct(
                        functionName: "migrate",
                        arguments: new StateShim.InterfaceToHost.ApplyFunctionArguments<Maybe<StateShim.InterfaceToHost.StateSource>>(
                            stateArgument: Maybe<StateShim.InterfaceToHost.StateSource>.nothing(),
                            serializedArgumentsJson: [elmAppStateBefore.Value]),
                        stateDestinationBranches: ["main"])));

            return
                migrateEventResult
                .MapError(error => "Failed to process the event in the hosted app: " + error)
                .AndThen(shimResponse => shimResponse switch
                {
                    StateShim.InterfaceToHost.StateShimResponseStruct.ApplyFunctionShimResponse applyFunctionResponse =>
                    applyFunctionResponse.Result,

                    _ =>
                    Result<string, StateShim.InterfaceToHost.FunctionApplicationResult>.err(
                        "Unexpected type of response: " + JsonSerializer.Serialize(shimResponse))
                })
                .Map(shimResponse => JsonSerializer.Deserialize<InterfaceToHost.BackendEventResponseStruct>(
                    shimResponse.resultLessStateJson.WithDefaultBuilder(() => throw new Exception("Missing resultLessStateJson"))))
                .Map(migrationBackendResponse =>
                {
                    processBefore.lastElmAppVolatileProcess?.Dispose();

                    return
                    processBefore
                    with
                    {
                        lastAppConfig = new ProcessAppConfig(
                            PineValueComposition.FromTreeWithStringPath(deployAppConfigAndMigrateElmAppState),
                            prepareProcessResult.buildArtifacts),
                        lastElmAppVolatileProcess = newElmAppProcess,
                        initOrMigrateCmds = migrationBackendResponse
                    };
                });
        }

        if (compositionEvent.DeployAppConfigAndInitElmAppState != null)
        {
            var appConfig = compositionEvent.DeployAppConfigAndInitElmAppState;

            var prepareProcessResult =
                ProcessFromDeployment(
                    appConfig,
                    overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig,
                    overrideJavaScriptEngineFactory: overrideJavaScriptEngineFactory);

            var newElmAppProcess = prepareProcessResult.startProcess();

            var applyInitSerialInterfaceResult = InitBranchesInElmInJsProcess(
                newElmAppProcess,
                stateDestinationBranches: ["main"]);

            return
                applyInitSerialInterfaceResult
                .MapError(error => "Failed to process the event in the hosted app: " + error)
                .AndThen(applyInitSerialInterfaceOk =>
                applyInitSerialInterfaceOk switch
                {
                    StateShim.InterfaceToHost.StateShimResponseStruct.ApplyFunctionShimResponse applyFunctionResponse =>
                    applyFunctionResponse.Result,

                    _ => Result<string, StateShim.InterfaceToHost.FunctionApplicationResult>.err("Unexpected response variant")
                })
                .Map(applyInitOk =>
                {
                    processBefore.lastElmAppVolatileProcess?.Dispose();

                    var backendResponse =
                    JsonSerializer.Deserialize<InterfaceToHost.BackendEventResponseStruct>(
                        applyInitOk.resultLessStateJson.WithDefaultBuilder(() => throw new Exception("Missing resultLessStateJson")));

                    return
                    processBefore
                    with
                    {
                        lastAppConfig = new ProcessAppConfig(
                            PineValueComposition.FromTreeWithStringPath(appConfig),
                            prepareProcessResult.buildArtifacts),
                        lastElmAppVolatileProcess = newElmAppProcess,
                        initOrMigrateCmds = backendResponse
                    };
                });
        }

        return Result<string, PersistentProcessLiveRepresentationDuringRestore>.err(
            "Unexpected shape of composition event: " + JsonSerializer.Serialize(compositionEvent));
    }

    public static Result<string, StateShim.InterfaceToHost.StateShimResponseStruct> InitBranchesInElmInJsProcess(
        IProcessWithStringInterface elmInJsProcess,
        IReadOnlyList<string> stateDestinationBranches) =>
        AttemptProcessRequest(
            elmInJsProcess,
            new StateShim.InterfaceToHost.StateShimRequestStruct.ApplyFunctionShimRequest(
                new StateShim.InterfaceToHost.ApplyFunctionShimRequestStruct(
                    functionName: "init",
                    arguments: new StateShim.InterfaceToHost.ApplyFunctionArguments<Maybe<StateShim.InterfaceToHost.StateSource>>(
                        stateArgument: Maybe<StateShim.InterfaceToHost.StateSource>.nothing(),
                        serializedArgumentsJson: ImmutableList<JsonElement>.Empty),
                    stateDestinationBranches: stateDestinationBranches)));

    private record UpdateElmAppStateForEvent(
        string functionName,
        StateShim.InterfaceToHost.ApplyFunctionArguments<bool> arguments);

    private static Result<string, UpdateElmAppStateForEvent> TranslateUpdateElmAppEventFromStore(
        Span<byte> updateElmAppEventFromStore)
    {
        var asString = Encoding.UTF8.GetString(updateElmAppEventFromStore);

        Result<string, UpdateElmAppStateForEvent> continueWithWebServiceEvent(InterfaceToHost.BackendEventStruct webServiceEvent) =>
            Result<string, UpdateElmAppStateForEvent>.ok(new UpdateElmAppStateForEvent(
                functionName: "processEvent",
                arguments: new StateShim.InterfaceToHost.ApplyFunctionArguments<bool>(
                    stateArgument: true,
                    serializedArgumentsJson: [JsonSerializer.SerializeToElement(webServiceEvent)])));
        try
        {
            var webServiceEvent = JsonSerializer.Deserialize<InterfaceToHost.BackendEventStruct>(asString);

            return continueWithWebServiceEvent(webServiceEvent);
        }
        catch (JsonException)
        {
        }

        try
        {
            var webServiceEvent_2023_02_27 = JsonSerializer.Deserialize<InterfaceToHost._2023_02_27.AppEventStructure>(asString);

            return
                webServiceEvent_2023_02_27 switch
                {
                    { } webServiceEvent
                    when webServiceEvent.ArrivedAtTimeEvent is { } arrivedAtTime =>
                    continueWithWebServiceEvent(
                        new InterfaceToHost.BackendEventStruct.PosixTimeHasArrivedEvent(
                            new InterfaceToHost.PosixTimeHasArrivedEventStruct(posixTimeMilli: arrivedAtTime.posixTimeMilli))),

                    { } webServiceEvent
                    when webServiceEvent.HttpRequestEvent is { } httpRequestEvent =>

                    continueWithWebServiceEvent(
                        new InterfaceToHost.BackendEventStruct.HttpRequestEvent(
                            new InterfaceToHost.HttpRequestEventStruct(
                                posixTimeMilli: httpRequestEvent.posixTimeMilli,
                                httpRequestId: httpRequestEvent.httpRequestId,
                                requestContext: new InterfaceToHost.HttpRequestContext(
                                    clientAddress: Maybe.NothingFromNull(httpRequestEvent.requestContext.clientAddress)),
                                request: new InterfaceToHost.HttpRequest(
                                    method: httpRequestEvent.request.method,
                                    uri: httpRequestEvent.request.uri,
                                    bodyAsBase64: Maybe.NothingFromNull(httpRequestEvent.request.bodyAsBase64),
                                    headers: httpRequestEvent.request.headers)))),

                    { } webServiceEvent
                    when webServiceEvent.TaskCompleteEvent is { } taskCompleteEvent =>

                    InterfaceToHost.TaskResult.From_2023_02_27(taskCompleteEvent.taskResult)
                    .AndThen(taskResult =>
                    continueWithWebServiceEvent(
                        new InterfaceToHost.BackendEventStruct.TaskCompleteEvent(
                            new InterfaceToHost.ResultFromTaskWithId(
                                taskId: taskCompleteEvent.taskId,
                                taskResult: taskResult)))),

                    _ =>
                    Result<string, UpdateElmAppStateForEvent>.err(
                        "Unexpected structure in webServiceEvent_2023_02_27: " +
                        JsonSerializer.Serialize(webServiceEvent_2023_02_27))
                };
        }
        catch (JsonException)
        {
        }

        return Result<string, UpdateElmAppStateForEvent>.err("Did not match any expected shape");
    }

    private static Result<string, StateShim.InterfaceToHost.StateShimResponseStruct> AttemptProcessRequest(
        IProcessWithStringInterface process,
        StateShim.InterfaceToHost.StateShimRequestStruct stateShimRequest)
    {
        var serializedInterfaceEvent = stateShimRequest.SerializeToJsonString();

        var eventResponseSerial = process.ProcessEvent(serializedInterfaceEvent);

        try
        {
            var eventResponse =
                JsonSerializer.Deserialize<Result<string, StateShim.InterfaceToHost.StateShimResponseStruct>>(eventResponseSerial)!;

            return
                eventResponse
                .MapError(decodeErr => "Hosted app failed to decode the event: " + decodeErr);
        }
        catch (Exception parseException)
        {
            return Result<string, StateShim.InterfaceToHost.StateShimResponseStruct>.err(
                "Failed to parse event response from the app. Looks like the loaded elm app is not compatible with the interface.\nI got following response from the app:\n" +
                eventResponseSerial + "\nException: " + parseException);
        }
    }

    private static CompositionEventWithResolvedDependencies? LoadCompositionEventDependencies(
        CompositionLogRecordInFile.CompositionEvent compositionEvent,
        IProcessStoreReader storeReader)
    {
        ReadOnlyMemory<byte> loadComponentFromValueInFileStructureAndAssertIsBlob(ValueInFileStructure valueInFileStructure)
        {
            if (valueInFileStructure.LiteralStringUtf8 != null)
                return Encoding.UTF8.GetBytes(valueInFileStructure.LiteralStringUtf8);

            return loadComponentFromStoreAndAssertIsBlob(valueInFileStructure.HashBase16!);
        }

        ReadOnlyMemory<byte> loadComponentFromStoreAndAssertIsBlob(string componentHash)
        {
            var component = storeReader.LoadComponent(componentHash);

            if (component is null)
                throw new Exception("Failed to load component " + componentHash + ": Not found in store.");

            if (component is not PineValue.BlobValue blobComponent)
                throw new Exception("Failed to load component " + componentHash + " as blob: This is not a blob.");

            return blobComponent.Bytes;
        }

        TreeNodeWithStringPath loadComponentFromStoreAndAssertIsTree(string componentHash)
        {
            var component = storeReader.LoadComponent(componentHash);

            if (component == null)
                throw new Exception("Failed to load component " + componentHash + ": Not found in store.");

            return
                PineValueComposition.ParseAsTreeWithStringPath(component)
                .Extract(_ => throw new Exception("Failed to load component " + componentHash + " as tree: Failed to parse as tree."));
        }

        if (compositionEvent.UpdateElmAppStateForEvent != null)
        {
            return new CompositionEventWithResolvedDependencies
            {
                UpdateElmAppStateForEvent =
                    loadComponentFromValueInFileStructureAndAssertIsBlob(compositionEvent.UpdateElmAppStateForEvent).ToArray(),
            };
        }

        if (compositionEvent.ApplyFunctionOnElmAppState is not null)
        {
            return new CompositionEventWithResolvedDependencies
            {
                ApplyFunctionOnElmAppState =
                    loadComponentFromValueInFileStructureAndAssertIsBlob(compositionEvent.ApplyFunctionOnElmAppState).ToArray(),
            };
        }

        if (compositionEvent.SetElmAppState != null)
        {
            return new CompositionEventWithResolvedDependencies
            {
                SetElmAppState = loadComponentFromStoreAndAssertIsBlob(
                    compositionEvent.SetElmAppState.HashBase16!).ToArray(),
            };
        }

        if (compositionEvent.DeployAppConfigAndMigrateElmAppState != null)
        {
            return new CompositionEventWithResolvedDependencies
            {
                DeployAppConfigAndMigrateElmAppState = loadComponentFromStoreAndAssertIsTree(
                    compositionEvent.DeployAppConfigAndMigrateElmAppState.HashBase16!),
            };
        }

        if (compositionEvent.DeployAppConfigAndInitElmAppState != null)
        {
            return new CompositionEventWithResolvedDependencies
            {
                DeployAppConfigAndInitElmAppState = loadComponentFromStoreAndAssertIsTree(
                    compositionEvent.DeployAppConfigAndInitElmAppState.HashBase16!),
            };
        }

        if (compositionEvent.RevertProcessTo != null)
            return null;

        throw new Exception("Unexpected shape of composition event: " + JsonSerializer.Serialize(compositionEvent));
    }

    public static Result<string, FileStoreReaderProjectionResult>
        TestContinueWithCompositionEvent(
            CompositionLogRecordInFile.CompositionEvent compositionLogEvent,
            IFileStoreReader fileStoreReader,
            Action<string>? logger = null,
            Func<IJavaScriptEngine>? overrideJavaScriptEngineFactory = null)
    {
        var projectionResult = IProcessStoreReader.ProjectFileStoreReaderForAppendedCompositionLogEvent(
            originalFileStore: fileStoreReader,
            compositionLogEvent: compositionLogEvent);

        try
        {
            return
                LoadFromStoreAndRestoreProcess(
                    new ProcessStoreReaderInFileStore(projectionResult.projectedReader),
                    logger: message => logger?.Invoke(message),
                    overrideJavaScriptEngineFactory: overrideJavaScriptEngineFactory)
                .Map(_ => projectionResult);
        }
        catch (Exception e)
        {
            return Result<string, FileStoreReaderProjectionResult>.err("Failed with exception: " + e);
        }
    }

    public Result<string, StateShim.InterfaceToHost.FunctionApplicationResult> ProcessElmAppEvent(
        IProcessStoreWriter storeWriter, string serializedAppEvent)
    {
        lock (processLock)
        {
            return
                StateShim.StateShim.ApplyFunctionOnMainBranch(
                    process: lastElmAppVolatileProcess,
                    new AdminInterface.ApplyDatabaseFunctionRequest(
                        functionName: "processEvent",
                        serializedArgumentsJson: [serializedAppEvent],
                        commitResultingState: true))
                .Map(applyFunctionSuccess =>
                {
                    var compositionEvent =
                        new CompositionLogRecordInFile.CompositionEvent
                        {
                            UpdateElmAppStateForEvent = new ValueInFileStructure
                            {
                                LiteralStringUtf8 = serializedAppEvent
                            }
                        };

                    var recordHash = storeWriter.AppendCompositionLogRecord(compositionEvent);

                    lastCompositionLogRecordHashBase16 = recordHash.recordHashBase16;

                    return applyFunctionSuccess.functionApplicationResult;
                });
        }
    }

    public Result<string, (CompositionLogRecordInFile.CompositionEvent compositionLogEvent, string)> SetStateOnMainBranch(
        IProcessStoreWriter storeWriter,
        JsonElement appState)
    {
        lock (processLock)
        {
            return
                StateShim.StateShim.SetAppStateOnMainBranch(lastElmAppVolatileProcess, appState)
                .Map(setStateSuccess =>
                {
                    using var stream = new System.IO.MemoryStream();

                    using var jsonWriter = new Utf8JsonWriter(stream);

                    appState.WriteTo(jsonWriter);

                    jsonWriter.Flush();

                    var elmAppStateComponent = PineValue.Blob(stream.ToArray());

                    storeWriter.StoreComponent(elmAppStateComponent);

                    var compositionEvent =
                        new CompositionLogRecordInFile.CompositionEvent
                        {
                            SetElmAppState = new ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(elmAppStateComponent))
                            }
                        };

                    var recordHash = storeWriter.AppendCompositionLogRecord(compositionEvent);

                    lastCompositionLogRecordHashBase16 = recordHash.recordHashBase16;

                    return (compositionEvent, setStateSuccess);
                });
        }
    }

    public Result<string, AdminInterface.ApplyDatabaseFunctionSuccess> ApplyFunctionOnMainBranch(
        IProcessStoreWriter storeWriter,
        AdminInterface.ApplyDatabaseFunctionRequest request)
    {
        lock (processLock)
        {
            return
                StateShim.StateShim.ApplyFunctionOnMainBranch(lastElmAppVolatileProcess, request)
                .Map(applyFunctionSuccess =>
                {
                    if (applyFunctionSuccess.committedResultingState)
                    {
                        var applyFunctionRecord = new CompositionLogRecordInFile.ApplyFunctionOnStateEvent(
                            functionName: request.functionName,
                            serializedArgumentsJson: request.serializedArgumentsJson);

                        var compositionEvent =
                            new CompositionLogRecordInFile.CompositionEvent
                            {
                                ApplyFunctionOnElmAppState = new ValueInFileStructure
                                {
                                    LiteralStringUtf8 = JsonSerializer.Serialize(applyFunctionRecord)
                                }
                            };

                        var recordHash = storeWriter.AppendCompositionLogRecord(compositionEvent);

                        lastCompositionLogRecordHashBase16 = recordHash.recordHashBase16;
                    }

                    return applyFunctionSuccess;
                });
        }
    }

    public Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>> ListDatabaseFunctions()
    {
        return StateShim.StateShim.ListExposedFunctions(lastElmAppVolatileProcess);
    }

    public Result<string, long> EstimateSerializedStateLengthOnMainBranch()
    {
        lock (processLock)
        {
            return StateShim.StateShim.EstimateSerializedStateLengthOnMainBranch(lastElmAppVolatileProcess);
        }
    }

    public Result<string, IReadOnlyList<string>> ListBranches()
    {
        lock (processLock)
        {
            return StateShim.StateShim.ListBranches(lastElmAppVolatileProcess);
        }
    }

    public Result<string, string> SetBranchesState(
        StateShim.InterfaceToHost.StateSource stateSource,
        IReadOnlyList<string> branches)
    {
        lock (processLock)
        {
            return StateShim.StateShim.SetBranchesState(lastElmAppVolatileProcess, stateSource, branches: branches);
        }
    }

    public Result<string, StateShim.InterfaceToHost.RemoveBranchesShimResponseStruct> RemoveBranches(
        IReadOnlyList<string> branches)
    {
        lock (processLock)
        {
            return StateShim.StateShim.RemoveBranches(lastElmAppVolatileProcess, branches: branches);
        }
    }

    public void Dispose() => lastElmAppVolatileProcess?.Dispose();

    public (ProvisionalReductionRecordInFile? reductionRecord, StoreProvisionalReductionReport report) StoreReductionRecordForCurrentState(
        IProcessStoreWriter storeWriter)
    {
        var report = new StoreProvisionalReductionReport();

        JsonElement? elmAppState = null;

        string? elmAppStateSerial = null;

        var lockStopwatch = System.Diagnostics.Stopwatch.StartNew();

        lock (processLock)
        {
            lockStopwatch.Stop();

            report.lockTimeSpentMilli = (int)lockStopwatch.ElapsedMilliseconds;

            if (lastCompositionLogRecordHashBase16 == CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16 ||
                lastCompositionLogRecordHashBase16 == null)
                return (null, report);

            var serializeStopwatch = System.Diagnostics.Stopwatch.StartNew();

            elmAppState =
                lastElmAppVolatileProcess is null ?
                null :
                StateShim.StateShim.GetAppStateFromMainBranch(lastElmAppVolatileProcess)
                .Extract(err => throw new Exception("Failed to get serialized state: " + err));

            report.getElmAppStateFromEngineTimeSpentMilli = (int)serializeStopwatch.ElapsedMilliseconds;

            elmAppStateSerial = elmAppState?.ToString();

            if (elmAppStateSerial == "")
                elmAppStateSerial = "\"\"";

            report.serializeElmAppStateToStringTimeSpentMilli = (int)serializeStopwatch.ElapsedMilliseconds;
            report.serializeElmAppStateLength = elmAppStateSerial?.Length;
        }

        var elmAppStateBlob =
            elmAppStateSerial is null
            ?
            null
            :
            Encoding.UTF8.GetBytes(elmAppStateSerial);

        var elmAppStateComponent =
            elmAppStateBlob is null
            ?
            null
            :
            PineValue.Blob(elmAppStateBlob);

        var reductionRecord =
            new ProvisionalReductionRecordInFile
            (
                reducedCompositionHashBase16: lastCompositionLogRecordHashBase16,
                elmAppState:
                    elmAppStateComponent == null
                    ? null
                    : new ValueInFileStructure
                    {
                        HashBase16 = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(elmAppStateComponent))
                    },
                appConfig:
                    new ValueInFileStructure
                    {
                        HashBase16 = CommonConversion.StringBase16(
                            PineValueHashTree.ComputeHash(lastAppConfig.appConfigComponent)),
                    }
            );

        var storeDependenciesStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var dependencies =
            new[] { elmAppStateComponent, lastAppConfig.appConfigComponent }
            .WhereNotNull()
            .ToImmutableList();

        foreach (var dependency in dependencies)
            storeWriter.StoreComponent(dependency);

        storeDependenciesStopwatch.Stop();

        report.storeDependenciesTimeSpentMilli = (int)storeDependenciesStopwatch.ElapsedMilliseconds;

        storeWriter.StoreProvisionalReduction(reductionRecord);

        return (reductionRecord, report);
    }
}
