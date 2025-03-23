using ElmTime.Platform.WebService.ProcessStoreSupportingMigrations;
using Pine;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.PineVM;
using Pine.Elm.Platform;
using Pine.PineVM;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace ElmTime.Platform.WebService;


public record struct StoreProvisionalReductionReport(
    int lockTimeSpentMilli,
    int? getElmAppStateFromEngineTimeSpentMilli,
    int? serializeElmAppStateToStringTimeSpentMilli,
    int? serializeElmAppStateLength,
    int? storeDependenciesTimeSpentMilli);

public record struct ProcessAppConfig(
    PineValue appConfigComponent);

public class PersistentProcessLiveRepresentation : IAsyncDisposable
{
    private readonly Pine.CompilePineToDotNet.CompilerMutableCache hashCache = new();

    private readonly System.Threading.Lock processLock = new();

    public readonly ProcessAppConfig lastAppConfig;

    private readonly Func<DateTimeOffset> getDateTimeOffset;

    private readonly WebServiceInterface.WebServiceConfig appConfigParsed;

    private readonly MutatingWebServiceApp mutatingWebServiceApp;

    private readonly VolatileProcessHost volatileProcessHost;

    private readonly IProcessStoreWriter storeWriter;

    private (PineValue state, CompositionLogRecordInFile.CompositionEvent compositionLogEvent, string hashBase16)? lastAppStatePersisted;

    private readonly System.Threading.Timer notifyTimeHasArrivedTimer;

    public record struct CompositionLogRecordWithResolvedDependencies(
        CompositionLogRecordInFile compositionRecord,
        string compositionRecordHashBase16,
        ReductionWithResolvedDependencies? reduction,
        CompositionEventWithResolvedDependencies? composition);

    public record struct ReductionWithResolvedDependencies(
        PineValue elmAppState,
        PineValue appConfig,
        TreeNodeWithStringPath appConfigAsTree);

    public record struct CompositionEventWithResolvedDependencies(
        byte[]? UpdateElmAppStateForEvent = null,
        PineValue? SetElmAppState = null,
        byte[]? ApplyFunctionOnElmAppState = null,
        TreeNodeWithStringPath? DeployAppConfigAndInitElmAppState = null,
        TreeNodeWithStringPath? DeployAppConfigAndMigrateElmAppState = null);

    private PersistentProcessLiveRepresentation(
        ProcessAppConfig lastAppConfig,
        PineValue? lastAppState,
        IProcessStoreWriter storeWriter,
        Func<DateTimeOffset> getDateTimeOffset,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null,
        System.Threading.CancellationToken cancellationToken = default)
    {
        this.lastAppConfig = lastAppConfig;
        this.storeWriter = storeWriter;
        this.getDateTimeOffset = getDateTimeOffset;

        var appSourceTree =
            PineValueComposition.ParseAsTreeWithStringPath(lastAppConfig.appConfigComponent)
            .Extract(err => throw new Exception("Failed to parse app source tree: " + err));

        var compilationRootFilePath =
            overrideElmAppInterfaceConfig?.compilationRootFilePath
            ??
            ["src", "Backend", "Main.elm"];

        appConfigParsed =
            WebServiceInterface.ConfigFromSourceFilesAndEntryFileName(
                appSourceTree,
                compilationRootFilePath);

        mutatingWebServiceApp =
            new MutatingWebServiceApp(appConfigParsed);

        volatileProcessHost =
            new VolatileProcessHost([lastAppConfig.appConfigComponent]);

        if (lastAppState is not null)
        {
            mutatingWebServiceApp.ResetAppState(lastAppState);
        }

        notifyTimeHasArrivedTimer = new System.Threading.Timer(
            callback: _ =>
            {
                if (cancellationToken.IsCancellationRequested)
                {
                    notifyTimeHasArrivedTimer?.Dispose();
                    return;
                }

                ProcessElmAppCmdsAsync().Wait();

                ProcessEventTimeHasArrived(getDateTimeOffset());
            },
            state: null,
            dueTime: TimeSpan.Zero,
            period: TimeSpan.FromMilliseconds(100));

        ProcessEventTimeHasArrived(getDateTimeOffset());
    }

    public async System.Threading.Tasks.Task<WebServiceInterface.HttpResponse> ProcessHttpRequestAsync(
        WebServiceInterface.HttpRequestEventStruct requestEvent)
    {
        var response =
            await mutatingWebServiceApp.HttpRequestSendAsync(requestEvent);

        await ProcessElmAppCmdsAsync();

        return response;
    }

    public void ProcessEventTimeHasArrived(DateTimeOffset currentTime)
    {
        mutatingWebServiceApp.UpdateForPosixTime(
            posixTimeMilli: currentTime.ToUnixTimeMilliseconds());
    }

    private async System.Threading.Tasks.Task ProcessElmAppCmdsAsync()
    {
        EnsurePersisted();

        await volatileProcessHost.ExchangeAsync(mutatingWebServiceApp);
    }

    private (CompositionLogRecordInFile.CompositionEvent compositionLogEvent, string) EnsurePersisted()
    {
        PineValue elmAppState = mutatingWebServiceApp.AppState;

        if (lastAppStatePersisted?.state == elmAppState)
        {
            return
                (lastAppStatePersisted.Value.compositionLogEvent, lastAppStatePersisted.Value.hashBase16);
        }

        storeWriter.StoreComponent(elmAppState);

        var elmAppStateHash =
            Convert.ToHexStringLower(hashCache.ComputeHash(elmAppState).Span);

        storeWriter.StoreComponent(lastAppConfig.appConfigComponent);

        var compositionEvent =
            new CompositionLogRecordInFile.CompositionEvent
            {
                SetElmAppState = new ValueInFileStructure
                {
                    HashBase16 = elmAppStateHash
                }
            };

        var recordHash =
            storeWriter.AppendCompositionLogRecord(compositionEvent);

        storeWriter.StoreProvisionalReduction(
            new ProvisionalReductionRecordInFile(
                reducedCompositionHashBase16: recordHash.recordHashBase16,
                elmAppState:
                new ValueInFileStructure(
                    HashBase16: elmAppStateHash),
                appConfig:
                new ValueInFileStructure(
                    HashBase16:
                    Convert.ToHexStringLower(
                        hashCache.ComputeHash(lastAppConfig.appConfigComponent).Span))));

        lastAppStatePersisted =
            (elmAppState, compositionEvent, recordHash.recordHashBase16);

        return (compositionEvent, recordHash.recordHashBase16);
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
            EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(
                new ProcessStoreReaderInFileStore(recordingReader))
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

                if (reductionRecord?.appConfig?.HashBase16 is { } appConfigHash && reductionRecord?.elmAppState?.HashBase16 is { } appStateHash)
                {
                    var appConfigComponent = storeReader.LoadComponent(appConfigHash);

                    var elmAppStateComponent = storeReader.LoadComponent(appStateHash);

                    if (appConfigComponent is not null && elmAppStateComponent is not null)
                    {
                        var appConfigAsTree =
                        PineValueComposition.ParseAsTreeWithStringPath(appConfigComponent)
                        .Extract(_ => throw new Exception(
                            "Unexpected content of appConfigComponent " + appConfigHash + ": Failed to parse as tree."));

                        reduction = new ReductionWithResolvedDependencies
                        (
                            appConfig: appConfigComponent,
                            appConfigAsTree: appConfigAsTree,
                            elmAppState: elmAppStateComponent
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
            .TakeUntil(compositionAndReduction => compositionAndReduction.reduction is not null)
            .Reverse();

    public static Result<string, RestoreFromCompositionEventSequenceResult>
        LoadFromStoreAndRestoreProcess(
        IProcessStoreReader storeReader,
        IProcessStoreWriter storeWriter,
        System.Threading.CancellationToken cancellationToken,
        Func<DateTimeOffset> getDateTimeOffset,
        Action<string>? logger,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
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

            return message;
        }

        logger?.Invoke("Found " + compositionEventsFromLatestReduction.Count + " composition log records to use for restore.");

        var processLiveRepresentation =
            RestoreFromCompositionEventSequence(
                compositionEventsFromLatestReduction,
                storeWriter,
                cancellationToken,
                getDateTimeOffset,
                overrideElmAppInterfaceConfig);

        logger?.Invoke("Restored the process state in " + (int)restoreStopwatch.Elapsed.TotalSeconds + " seconds.");

        return processLiveRepresentation;
    }

    public record RestoreFromCompositionEventSequenceResult(
        PersistentProcessLiveRepresentation process,
        InterfaceToHost.BackendEventResponseStruct? initOrMigrateCmds);

    public static Result<string, RestoreFromCompositionEventSequenceResult>
        RestoreFromCompositionEventSequence(
        IEnumerable<CompositionLogRecordWithResolvedDependencies> compositionLogRecords,
        IProcessStoreWriter storeWriter,
        System.Threading.CancellationToken cancellationToken,
        Func<DateTimeOffset> getDateTimeOffset,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
    {
        var firstCompositionLogRecord =
            compositionLogRecords.FirstOrDefault();

        if (firstCompositionLogRecord.reduction is null &&
            firstCompositionLogRecord.compositionRecord.parentHashBase16 != CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16)
        {
            return
                "Failed to get sufficient history: Composition log record points to parent " +
                firstCompositionLogRecord.compositionRecord.parentHashBase16;
        }

        var pineVMCache = new PineVMCache();

        var pineVM = new PineVM(pineVMCache.EvalCache);

        var initialProcessRepresentation = new PersistentProcessLiveRepresentationDuringRestore(
            lastCompositionLogRecordHashBase16: null,
            lastAppConfig: null,
            initOrMigrateCmds: null,
            lastAppState: null);


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
                return
                    continueOk(
                        process
                        with
                        {
                            lastAppConfig =
                            new ProcessAppConfig(reductionWithResolvedDependencies.appConfig),
                            lastAppState = reductionWithResolvedDependencies.elmAppState,
                            initOrMigrateCmds = null
                        });
            }

            if (compositionEvent.RevertProcessTo is { } revertProcessTo)
            {
                if (revertProcessTo.HashBase16 != process.lastCompositionLogRecordHashBase16)
                {
                    return
                        "Error in enumeration of process composition events: Got revert to " +
                        revertProcessTo.HashBase16 +
                        ", but previous version in the enumerated sequence was " + process.lastCompositionLogRecordHashBase16 + ".";
                }

                return continueOk(process);
            }

            return
                ApplyCompositionEvent(
                    compositionLogRecord.composition!.Value,
                    process,
                    pineVM,
                    overrideElmAppInterfaceConfig)
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
                    aggregateOk.lastAppConfig is not { } lastAppConfig)
                {
                    return
                    (Result<string, RestoreFromCompositionEventSequenceResult>)
                    "Failed to get sufficient history: " +
                    nameof(compositionLogRecords) +
                    " does not contain app init.";
                }

                return
                    (Result<string, RestoreFromCompositionEventSequenceResult>)
                    new RestoreFromCompositionEventSequenceResult(
                        new PersistentProcessLiveRepresentation(
                            lastAppConfig: lastAppConfig,
                            lastAppState: aggregateOk.lastAppState,
                            storeWriter: storeWriter,
                            getDateTimeOffset,
                            overrideElmAppInterfaceConfig,
                            cancellationToken),
                        aggregateOk.initOrMigrateCmds);
            });
    }

    private record PersistentProcessLiveRepresentationDuringRestore(
        string? lastCompositionLogRecordHashBase16,
        ProcessAppConfig? lastAppConfig,
        InterfaceToHost.BackendEventResponseStruct? initOrMigrateCmds,
        PineValue? lastAppState);

    private static Result<string, PersistentProcessLiveRepresentationDuringRestore> ApplyCompositionEvent(
        CompositionEventWithResolvedDependencies compositionEvent,
        PersistentProcessLiveRepresentationDuringRestore processBefore,
        IPineVM pineVM,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig)
    {
        /*
         * 
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
                                takesState
                                ?
                                new StateShim.InterfaceToHost.StateSource.BranchStateSource("main")
                                :
                                Maybe<StateShim.InterfaceToHost.StateSource>.nothing()),
                                stateDestinationBranches: ["main"]));

                    var eventString = JsonSerializer.Serialize<StateShim.InterfaceToHost.StateShimRequestStruct>(eventStateShimRequest);

                    if (processBefore.lastElmAppVolatileProcess is not { } lastElmAppVolatileProcess)
                        return processBefore;

                    var processEventReturnValue =
                        lastElmAppVolatileProcess.ProcessEvent(eventString);

                    var processEventResult =
                        JsonSerializer.Deserialize<Result<string, object>>(processEventReturnValue);

                    return processEventResult.Map(_ => processBefore);
                });
        }

        if (compositionEvent.ApplyFunctionOnElmAppState is { } applyFunctionOnElmAppStateSerial)
        {
            var applyFunctionOnElmAppState =
                JsonSerializer.Deserialize<CompositionLogRecordInFile.ApplyFunctionOnStateEvent>(applyFunctionOnElmAppStateSerial);

            if (processBefore.lastElmAppVolatileProcess is not { } lastElmAppVolatileProcess)
                return "Process is null, no app deployed";

            return
                StateShim.StateShim.ApplyFunctionOnMainBranch(
                    process: lastElmAppVolatileProcess,
                    new AdminInterface.ApplyDatabaseFunctionRequest(
                        functionName: applyFunctionOnElmAppState.functionName,
                        serializedArgumentsJson: applyFunctionOnElmAppState.serializedArgumentsJson,
                        commitResultingState: true))
                .Map(_ => processBefore);
        }
        */

        if (compositionEvent.SetElmAppState is { } setElmAppState)
        {
            return
                new PersistentProcessLiveRepresentationDuringRestore(
                    lastCompositionLogRecordHashBase16: processBefore.lastCompositionLogRecordHashBase16,
                    lastAppConfig: processBefore.lastAppConfig,
                    initOrMigrateCmds: null,
                    lastAppState: setElmAppState);
        }

        if (compositionEvent.DeployAppConfigAndMigrateElmAppState is { } deployAppConfigAndMigrateElmAppState)
        {
            if (processBefore.lastAppConfig is not { } lastAppConfig)
            {
                return "No app config before";
            }

            if (processBefore.lastAppState is not { } lastAppState)
            {
                return "No app state before";
            }

            var compilationRootFilePath =
                overrideElmAppInterfaceConfig?.compilationRootFilePath
                ??
                ["src", "Backend", "Main.elm"];

            var appConfigTreeBefore =
                PineValueComposition.ParseAsTreeWithStringPath(lastAppConfig.appConfigComponent)
                .Extract(err => throw new Exception("Failed to parse app config: " + err));

            var appConfigParsedBefore =
                WebServiceInterface.ConfigFromSourceFilesAndEntryFileName(
                    appConfigTreeBefore,
                    compilationRootFilePath);

            var appConfigTreeNext =
                deployAppConfigAndMigrateElmAppState;

            var appConfigParsedNext =
                WebServiceInterface.ConfigFromSourceFilesAndEntryFileName(
                    appConfigTreeNext,
                    compilationRootFilePath);

            var appStateEncoded =
                appConfigParsedBefore.JsonAdapter.EncodeAppStateAsJsonValue(
                    lastAppState,
                    pineVM)
                .Extract(err => throw new Exception("Failed to encode app state: " + err));

            var appStateDecoded =
                appConfigParsedNext.JsonAdapter.DecodePreviousAppStateFromJsonValue(
                    appStateEncoded,
                    pineVM)
                .Extract(err => throw new Exception("Failed to decode app state: " + err));

            var migrateResult =
                appConfigParsedNext.JsonAdapter.MigratePreviousAppState(
                    appStateDecoded,
                    pineVM);

            {
                if (migrateResult.IsErrOrNull() is { } err)
                {
                    return "Failed to migrate app state: " + err;
                }
            }

            if (migrateResult.IsOkOrNullable() is not { } migrateOk)
            {
                throw new Exception("Unexpected result variant: " + migrateResult);
            }

            return
                processBefore
                with
                {
                    lastAppConfig =
                    new ProcessAppConfig(PineValueComposition.FromTreeWithStringPath(
                        deployAppConfigAndMigrateElmAppState)),

                    lastAppState = migrateOk.newState,
                };
        }

        if (compositionEvent.DeployAppConfigAndInitElmAppState is { } appConfig)
        {
            return
                processBefore
                with
                {
                    lastAppConfig =
                        new ProcessAppConfig(PineValueComposition.FromTreeWithStringPath(appConfig)),
                    lastAppState = null,
                    initOrMigrateCmds = null
                };
        }

        return "Unexpected shape of composition event: " + JsonSerializer.Serialize(compositionEvent);
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
            var componentHash =
                compositionEvent.SetElmAppState.HashBase16
                ??
                throw new Exception("Missing hash in SetElmAppState");

            return new CompositionEventWithResolvedDependencies
            {
                SetElmAppState =
                storeReader.LoadComponent(componentHash)
                ??
                throw new Exception("Failed to load component " + componentHash + ": Not found in store.")
            };
        }

        if (compositionEvent.DeployAppConfigAndMigrateElmAppState != null)
        {
            return new CompositionEventWithResolvedDependencies
            {
                DeployAppConfigAndMigrateElmAppState =
                loadComponentFromStoreAndAssertIsTree(
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
            Action<string>? logger = null)
    {
        var projectionResult =
            IProcessStoreReader.ProjectFileStoreReaderForAppendedCompositionLogEvent(
                originalFileStore: fileStoreReader,
                compositionLogEvent: compositionLogEvent);

        var discardingWriter = new DiscardingStoreWriter();

        try
        {
            return
                LoadFromStoreAndRestoreProcess(
                    new ProcessStoreReaderInFileStore(projectionResult.projectedReader),
                    storeWriter: discardingWriter,
                    cancellationToken: default,
                    getDateTimeOffset: () => DateTimeOffset.Now,
                    logger: message => logger?.Invoke(message))
                .Map(_ => projectionResult);
        }
        catch (Exception e)
        {
            return "Failed with exception: " + e;
        }
    }

    public Result<string, (CompositionLogRecordInFile.CompositionEvent compositionLogEvent, string)>
        SetStateOnMainBranch(JsonElement appState)
    {
        lock (processLock)
        {
            var appStateJsonString = JsonSerializer.Serialize(appState);

            var jsonStringEncoded = ElmValueEncoding.StringAsPineValue(appStateJsonString);

            var pineVMCache = new PineVMCache();

            var pineVM = new PineVM(pineVMCache.EvalCache);

            var appStateDecoded =
                appConfigParsed.JsonAdapter.DecodeAppStateFromJsonString(
                    jsonStringEncoded,
                    pineVM)
                .Extract(err => throw new Exception("Failed to decode app state: " + err));

            mutatingWebServiceApp.ResetAppState(appStateDecoded);

            return EnsurePersisted();
        }
    }

    public Result<string, JsonElement> GetAppStateOnMainBranch()
    {
        var appState = mutatingWebServiceApp.AppState;

        var pineVMCache = new PineVMCache();

        var pineVM = new PineVM(pineVMCache.EvalCache);

        var jsonStringResult =
            appConfigParsed.JsonAdapter.EncodeAppStateAsJsonString(appState, pineVM);

        {
            if (jsonStringResult.IsErrOrNull() is { } err)
            {
                return err;
            }
        }

        if (jsonStringResult.IsOkOrNull() is not { } jsonString)
        {
            throw new Exception("Unexpected result: " + jsonStringResult);
        }

        return
            JsonSerializer.Deserialize<JsonElement>(jsonString);
    }

    public Result<string, AdminInterface.ApplyDatabaseFunctionSuccess> ApplyFunctionOnMainBranch(
        AdminInterface.ApplyDatabaseFunctionRequest request)
    {
        lock (processLock)
        {
            var clock = System.Diagnostics.Stopwatch.StartNew();

            var appState = mutatingWebServiceApp.AppState;

            var pineVMCache = new PineVMCache();

            var pineVM = new PineVM(pineVMCache.EvalCache);

            var matchingExposedFunction =
                appConfigParsed.JsonAdapter.ExposedFunctions
                .FirstOrDefault(exposedFunc => exposedFunc.Key == request.functionName)
                .Value;

            if (matchingExposedFunction is null)
            {
                return "Function not found: " + request.functionName;
            }

            var arguments = new PineValue[request.serializedArgumentsJson.Count];

            for (int i = 0; i < request.serializedArgumentsJson.Count; i++)
            {
                var argumentJsonString = request.serializedArgumentsJson[i];

                var parseArgumentResult =
                    appConfigParsed.JsonAdapter.DecodeElmJsonValueFromString(
                        argumentJsonString,
                        pineVM);

                {
                    if (parseArgumentResult.IsErrOrNull() is { } err)
                    {
                        return "Failed to parse argument " + i + ": " + err;
                    }
                }

                if (parseArgumentResult.IsOkOrNull() is not { } parseArgumentOk)
                {
                    throw new Exception("Unexpected result: " + parseArgumentResult);
                }

                arguments[i] = parseArgumentOk;
            }

            var posixTimeMilli =
                getDateTimeOffset().ToUnixTimeMilliseconds();

            var applyFunctionResult =
                ElmTimeJsonAdapter.ApplyExposedFunction(
                    appStateBefore: appState,
                    arguments: arguments,
                    exposedFunction: matchingExposedFunction,
                    pineVM: pineVM,
                    posixTimeMilli: posixTimeMilli);

            {
                if (applyFunctionResult.IsErrOrNull() is { } err)
                {
                    return "Failed to apply function " + request.functionName + ": " + err;
                }
            }

            if (applyFunctionResult.IsOkOrNull() is not { } applyFunctionOk)
            {
                throw new Exception("Unexpected result: " + applyFunctionResult);
            }

            bool committedResultingState = false;

            if (request.commitResultingState)
            {
                if (applyFunctionOk.AppState is not { } appStateReturned)
                {
                    return
                        "Requested commiting new app state, but the function " +
                        request.functionName +
                        " did not return a new app state.";
                }

                mutatingWebServiceApp.ResetAppState(appStateReturned);

                EnsurePersisted();

                committedResultingState = true;
            }

            bool producedStateDifferentFromStateArgument = false;

            {
                if (applyFunctionOk.AppState is { } appStateReturned)
                {
                    producedStateDifferentFromStateArgument = !appStateReturned.Equals(appState);
                }
            }

            Maybe<JsonElement> resultLessStateJson =
                applyFunctionOk.ResponseJsonValue is { } responseJsonValue
                ?
                Maybe<JsonElement>.just(
                    ParseJsonElementFromElmJsonValue(responseJsonValue, pineVM)
                    .Extract(err => throw new Exception("Failed decoding JsonElement: " + err)))
                :
                Maybe<JsonElement>.nothing();

            return
                new AdminInterface.ApplyDatabaseFunctionSuccess(
                    new StateShim.InterfaceToHost.FunctionApplicationResult(
                        resultLessStateJson: resultLessStateJson,
                        producedStateDifferentFromStateArgument: producedStateDifferentFromStateArgument),
                    new StateShim.ProcessStateShimRequestReport(
                        SerializeTimeInMilliseconds: 0,
                        SerializedRequest: "",
                        SerializedResponse: "",
                        Response: new StateShim.InterfaceToHost.StateShimResponseStruct.ListBranchesShimResponse([]),
                        ProcessEventAndSerializeTimeInMilliseconds: (int)clock.Elapsed.TotalMilliseconds),
                    committedResultingState);
        }
    }

    private Result<string, JsonElement> ParseJsonElementFromElmJsonValue(
        PineValue elmJsonValue,
        IPineVM pineVM)
    {
        var encodeResult =
            appConfigParsed.JsonAdapter.EncodeJsonValueAsJsonString(
                elmJsonValue,
                indent: 0,
                pineVM);

        {
            if (encodeResult.IsErrOrNull() is { } err)
            {
                return err;
            }
        }

        if (encodeResult.IsOkOrNull() is not { } jsonString)
        {
            throw new Exception("Unexpected result: " + encodeResult);
        }

        return
            JsonSerializer.Deserialize<JsonElement>(jsonString);
    }

    public Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>> ListDatabaseFunctions()
    {
        return
            Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>>.ok(
            [..appConfigParsed.JsonAdapter.ExposedFunctions
            .Select(exposedFunc => new StateShim.InterfaceToHost.NamedExposedFunction(
                functionName: exposedFunc.Key,
                functionDescription: MapExposedFunctionDescription(exposedFunc.Value)))
            ]);
    }

    private static StateShim.InterfaceToHost.ExposedFunctionDescription MapExposedFunctionDescription(
       ElmTimeJsonAdapter.ExposedFunction exposedFunction)
    {
        return
            new StateShim.InterfaceToHost.ExposedFunctionDescription(
                returnType:
                new StateShim.InterfaceToHost.ExposedFunctionReturnTypeDescription(
                    sourceCodeText: exposedFunction.Description.ReturnType.SourceCodeText,
                    containsAppStateType: exposedFunction.Description.ReturnType.ContainsAppStateType),
                parameters:
                [
                    ..exposedFunction.Description.Parameters
                    .Select(param => new StateShim.InterfaceToHost.ExposedFunctionParameterDescription(
                        patternSourceCodeText: param.PatternSourceCodeText,
                        typeSourceCodeText: param.TypeSourceCodeText,
                        typeIsAppStateType: param.TypeIsAppStateType))
                ]);
    }

    /*
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
    */

    public async System.Threading.Tasks.ValueTask DisposeAsync()
    {
        await notifyTimeHasArrivedTimer.DisposeAsync();

        await volatileProcessHost.DisposeAsync();
    }
}
