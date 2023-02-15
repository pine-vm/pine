using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;
using ElmTime.WebHost.ProcessStoreSupportingMigrations;
using Pine;

namespace ElmTime.WebHost.PersistentProcess;

public interface IPersistentProcess
{
    string ProcessElmAppEvent(IProcessStoreWriter storeWriter, string serializedEvent);

    (ProvisionalReductionRecordInFile? reductionRecord, StoreProvisionalReductionReport report) StoreReductionRecordForCurrentState(IProcessStoreWriter storeWriter);
}

public record struct StoreProvisionalReductionReport(
    int lockTimeSpentMilli,
    int? serializeElmAppStateTimeSpentMilli,
    int? serializeElmAppStateLength,
    int? storeDependenciesTimeSpentMilli);

public record struct ProcessAppConfig(
    PineValue appConfigComponent,
    ProcessFromElm019Code.BuildArtifacts buildArtifacts);

public class PersistentProcessLiveRepresentation : IPersistentProcess, IDisposable
{
    readonly object processLock = new();

    string lastCompositionLogRecordHashBase16;

    public readonly ProcessAppConfig lastAppConfig;

    readonly IDisposableProcessWithStringInterface lastElmAppVolatileProcess;

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
        TreeNodeWithStringPath? DeployAppConfigAndInitElmAppState = null,
        TreeNodeWithStringPath? DeployAppConfigAndMigrateElmAppState = null);

    static public ProcessFromElm019Code.PreparedProcess ProcessFromWebAppConfig(
        TreeNodeWithStringPath appConfig,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
    {
        var sourceFiles = Composition.TreeToFlatDictionaryWithPathComparer(appConfig);

        var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
            sourceFiles: sourceFiles,
            ElmAppInterfaceConfig.Default)
            .Extract(error => throw new Exception(ElmAppCompilation.CompileCompilationErrorsDisplayText(error)));

        var (loweredAppFiles, _) = compilationResult;

        return
            ProcessFromElm019Code.ProcessFromElmCodeFiles(
                loweredAppFiles.compiledFiles,
                overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);
    }

    PersistentProcessLiveRepresentation(
        string lastCompositionLogRecordHashBase16,
        ProcessAppConfig lastAppConfig,
        IDisposableProcessWithStringInterface lastElmAppVolatileProcess)
    {
        this.lastCompositionLogRecordHashBase16 = lastCompositionLogRecordHashBase16;
        this.lastAppConfig = lastAppConfig;
        this.lastElmAppVolatileProcess = lastElmAppVolatileProcess;
    }

    static public (IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> files, string lastCompositionLogRecordHashBase16)
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

                if (fileContent != null)
                {
                    filesForProcessRestore[filePath] = fileContent.ToArray();
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

    static IEnumerable<CompositionLogRecordWithResolvedDependencies>
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
                        Composition.ParseAsTreeWithStringPath(appConfigComponent)
                        .Extract(error => throw new Exception("Unexpected content of appConfigComponent " + reductionRecord.appConfig?.HashBase16 + ": Failed to parse as tree."));

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

    static public (PersistentProcessLiveRepresentation? process, InterfaceToHost.AppEventResponseStructure? initOrMigrateCmds)
        LoadFromStoreAndRestoreProcess(
        IProcessStoreReader storeReader,
        Action<string> logger,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
    {
        var restoreStopwatch = System.Diagnostics.Stopwatch.StartNew();

        logger?.Invoke("Begin to restore the process state.");

        var compositionEventsFromLatestReduction =
            EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(storeReader)
            .ToImmutableList();

        if (!compositionEventsFromLatestReduction.Any())
        {
            logger?.Invoke("Found no composition record, default to initial state.");

            return (null, null);
        }

        logger?.Invoke("Found " + compositionEventsFromLatestReduction.Count + " composition log records to use for restore.");

        var processLiveRepresentation = RestoreFromCompositionEventSequence(
            compositionEventsFromLatestReduction,
            overrideElmAppInterfaceConfig);

        logger?.Invoke("Restored the process state in " + ((int)restoreStopwatch.Elapsed.TotalSeconds) + " seconds.");

        return processLiveRepresentation;
    }

    static public (PersistentProcessLiveRepresentation process, InterfaceToHost.AppEventResponseStructure? initOrMigrateCmds)
        RestoreFromCompositionEventSequence(
        IEnumerable<CompositionLogRecordWithResolvedDependencies> compositionLogRecords,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
    {
        var firstCompositionLogRecord =
            compositionLogRecords.FirstOrDefault();

        if (firstCompositionLogRecord.reduction == null &&
            firstCompositionLogRecord.compositionRecord.parentHashBase16 != CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16)
        {
            throw new Exception("Failed to get sufficient history: Composition log record points to parent " + firstCompositionLogRecord.compositionRecord.parentHashBase16);
        }

        string? lastCompositionLogRecordHashBase16 = null;

        var processRepresentationDuringRestore = new PersistentProcessLiveRepresentationDuringRestore(
            lastAppConfig: null,
            lastElmAppVolatileProcess: null,
            initOrMigrateCmds: null);

        foreach (var compositionLogRecord in compositionLogRecords)
        {
            try
            {
                var compositionEvent = compositionLogRecord.compositionRecord.compositionEvent;

                if (compositionLogRecord.reduction != null)
                {
                    var prepareProcessResult =
                        ProcessFromWebAppConfig(
                            compositionLogRecord.reduction.Value.appConfigAsTree,
                            overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

                    var newElmAppProcess = prepareProcessResult.startProcess();

                    var elmAppStateAsString = Encoding.UTF8.GetString(compositionLogRecord.reduction.Value.elmAppState.Span);

                    var setStateResponse =
                        AttemptProcessEvent(
                            newElmAppProcess,
                            new InterfaceToHost.AppEventStructure { SetStateEvent = elmAppStateAsString })
                        .Extract(error => throw new Exception("Failed to set state: " + error));

                    processRepresentationDuringRestore.lastElmAppVolatileProcess?.Dispose();

                    processRepresentationDuringRestore = new PersistentProcessLiveRepresentationDuringRestore(
                        lastAppConfig: new ProcessAppConfig(
                            compositionLogRecord.reduction.Value.appConfig,
                            prepareProcessResult.buildArtifacts),
                        lastElmAppVolatileProcess: newElmAppProcess,
                        initOrMigrateCmds: null);

                    continue;
                }

                if (compositionEvent.RevertProcessTo != null)
                {
                    if (compositionEvent.RevertProcessTo.HashBase16 != lastCompositionLogRecordHashBase16)
                    {
                        throw new Exception(
                            "Error in enumeration of process composition events: Got revert to " +
                            compositionEvent.RevertProcessTo.HashBase16 +
                            ", but previous version in the enumerated sequence was " + lastCompositionLogRecordHashBase16 + ".");
                    }

                    continue;
                }

                processRepresentationDuringRestore =
                    ApplyCompositionEvent(
                        compositionLogRecord.composition!.Value,
                        processRepresentationDuringRestore,
                        overrideElmAppInterfaceConfig)
                    .Extract(error => throw new Exception("Failed to apply composition event: " + error));
            }
            finally
            {
                lastCompositionLogRecordHashBase16 = compositionLogRecord.compositionRecordHashBase16;
            }
        }

        if (lastCompositionLogRecordHashBase16 == null ||
            processRepresentationDuringRestore.lastAppConfig == null ||
            processRepresentationDuringRestore.lastElmAppVolatileProcess == null)
        {
            throw new Exception("Failed to get sufficient history: " + nameof(compositionLogRecords) + " does not contain app init.");
        }

        return (new PersistentProcessLiveRepresentation(
            lastCompositionLogRecordHashBase16: lastCompositionLogRecordHashBase16,
            lastAppConfig: processRepresentationDuringRestore.lastAppConfig.Value,
            lastElmAppVolatileProcess: processRepresentationDuringRestore.lastElmAppVolatileProcess),
            processRepresentationDuringRestore.initOrMigrateCmds);
    }

    record PersistentProcessLiveRepresentationDuringRestore(
        ProcessAppConfig? lastAppConfig,
        IDisposableProcessWithStringInterface? lastElmAppVolatileProcess,
        InterfaceToHost.AppEventResponseStructure? initOrMigrateCmds);

    static Result<string, PersistentProcessLiveRepresentationDuringRestore> ApplyCompositionEvent(
        CompositionEventWithResolvedDependencies compositionEvent,
        PersistentProcessLiveRepresentationDuringRestore processBefore,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig)
    {
        if (compositionEvent.UpdateElmAppStateForEvent is byte[] updateElmAppStateForEvent)
        {
            if (processBefore.lastElmAppVolatileProcess == null)
                return Result<string, PersistentProcessLiveRepresentationDuringRestore>.ok(processBefore);

            processBefore.lastElmAppVolatileProcess.ProcessEvent(
                Encoding.UTF8.GetString(updateElmAppStateForEvent));

            return Result<string, PersistentProcessLiveRepresentationDuringRestore>.ok(processBefore);
        }

        if (compositionEvent.SetElmAppState is byte[] setElmAppState)
        {
            if (processBefore.lastElmAppVolatileProcess == null)
            {
                return Result<string, PersistentProcessLiveRepresentationDuringRestore>.err(
                    "Failed to load the serialized state with the elm app: Looks like no app was deployed so far.");
            }

            var projectedElmAppState = Encoding.UTF8.GetString(setElmAppState);

            return
                AttemptProcessEvent(processBefore.lastElmAppVolatileProcess, new InterfaceToHost.AppEventStructure { SetStateEvent = projectedElmAppState })
                .MapError(error => "Set state function in the hosted app returned an error: " + error)
                .Map(_ => processBefore);
        }

        if (compositionEvent.DeployAppConfigAndMigrateElmAppState is TreeNodeWithStringPath deployAppConfigAndMigrateElmAppState)
        {
            var elmAppStateBefore = processBefore.lastElmAppVolatileProcess?.GetSerializedState();

            var prepareProcessResult =
                ProcessFromWebAppConfig(deployAppConfigAndMigrateElmAppState, overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

            var newElmAppProcess = prepareProcessResult.startProcess();

            var migrateEventResult = AttemptProcessEvent(
                newElmAppProcess,
                new InterfaceToHost.AppEventStructure(MigrateStateEvent: elmAppStateBefore));

            return
                migrateEventResult
                .MapError(error => "Failed to process the event in the hosted app: " + error)
                .AndThen(migrateEventOk =>
                migrateEventOk.migrateResult
                .AsPineMaybe()
                .Map(migrationAttempted =>
                migrationAttempted
                .AsPineResult()
                .MapError(error => "Migration function in the hosted app returned an error: " + error)
                .Map(migrationOk =>
                {
                    processBefore.lastElmAppVolatileProcess?.Dispose();

                    return new PersistentProcessLiveRepresentationDuringRestore(
                        lastAppConfig: new ProcessAppConfig(
                            Composition.FromTreeWithStringPath(deployAppConfigAndMigrateElmAppState),
                            prepareProcessResult.buildArtifacts),
                        lastElmAppVolatileProcess: newElmAppProcess,
                        initOrMigrateCmds: migrateEventOk);
                }))
                .WithDefault(Result<string, PersistentProcessLiveRepresentationDuringRestore>.err(
                    "Unexpected shape of response: migrateResult is Nothing")
                ));
        }

        if (compositionEvent.DeployAppConfigAndInitElmAppState != null)
        {
            var appConfig = compositionEvent.DeployAppConfigAndInitElmAppState;

            var prepareProcessResult =
                ProcessFromWebAppConfig(
                    appConfig,
                    overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

            var newElmAppProcess = prepareProcessResult.startProcess();

            var initEventResult = AttemptProcessEvent(
                newElmAppProcess,
                new InterfaceToHost.AppEventStructure(InitStateEvent: new()));

            return
                initEventResult
                .MapError(error => "Failed to process the event in the hosted app: " + error)
                .Map(initEventOk =>
                {
                    processBefore.lastElmAppVolatileProcess?.Dispose();

                    return
                    new PersistentProcessLiveRepresentationDuringRestore(
                        lastAppConfig: new ProcessAppConfig(
                            Composition.FromTreeWithStringPath(appConfig),
                            prepareProcessResult.buildArtifacts),
                        lastElmAppVolatileProcess: newElmAppProcess,
                        initEventOk);
                });
        }

        return Result<string, PersistentProcessLiveRepresentationDuringRestore>.err(
            "Unexpected shape of composition event: " + JsonSerializer.Serialize(compositionEvent));
    }

    static Result<string, InterfaceToHost.AppEventResponseStructure> AttemptProcessEvent(
        IProcessWithStringInterface process,
        InterfaceToHost.AppEventStructure appEvent)
    {
        var serializedInterfaceEvent =
            JsonSerializer.Serialize(appEvent, InterfaceToHost.AppEventStructure.JsonSerializerSettings);

        var eventResponseSerial = process.ProcessEvent(serializedInterfaceEvent);

        try
        {
            var eventResponse =
                JsonSerializer.Deserialize<InterfaceToHost.ResponseOverSerialInterface>(eventResponseSerial)!;

            if (eventResponse.DecodeEventSuccess == null)
            {
                return Result<string, InterfaceToHost.AppEventResponseStructure>.err(
                    "Hosted app failed to decode the event: " + eventResponse.DecodeEventError);
            }

            return Result<string, InterfaceToHost.AppEventResponseStructure>.ok(eventResponse.DecodeEventSuccess);
        }
        catch (Exception parseException)
        {
            return Result<string, InterfaceToHost.AppEventResponseStructure>.err(
                "Failed to parse event response from the app. Looks like the loaded elm app is not compatible with the interface.\nI got following response from the app:\n" +
                eventResponseSerial + "\nException: " + parseException.ToString());
        }
    }

    static CompositionEventWithResolvedDependencies? LoadCompositionEventDependencies(
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
                Composition.ParseAsTreeWithStringPath(component)
                .Extract(error => throw new Exception("Failed to load component " + componentHash + " as tree: Failed to parse as tree."));
        }

        if (compositionEvent.UpdateElmAppStateForEvent != null)
        {
            return new CompositionEventWithResolvedDependencies
            {
                UpdateElmAppStateForEvent =
                    loadComponentFromValueInFileStructureAndAssertIsBlob(compositionEvent.UpdateElmAppStateForEvent).ToArray(),
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

    static public Result<string, FileStoreReaderProjectionResult>
        TestContinueWithCompositionEvent(
            CompositionLogRecordInFile.CompositionEvent compositionLogEvent,
            IFileStoreReader fileStoreReader,
            Action<string>? logger = null)
    {
        var projectionResult = IProcessStoreReader.ProjectFileStoreReaderForAppendedCompositionLogEvent(
            originalFileStore: fileStoreReader,
            compositionLogEvent: compositionLogEvent);

        try
        {
            using var projectedProcess =
                LoadFromStoreAndRestoreProcess(
                    new ProcessStoreReaderInFileStore(projectionResult.projectedReader),
                    logger: message => logger?.Invoke(message)).process;

            return Result<string, FileStoreReaderProjectionResult>.ok(projectionResult);
        }
        catch (Exception e)
        {
            return Result<string, FileStoreReaderProjectionResult>.err("Failed with exception: " + e.ToString());
        }
    }

    public string ProcessElmAppEvent(IProcessStoreWriter storeWriter, string serializedEvent)
    {
        lock (processLock)
        {
            var elmAppResponse =
                lastElmAppVolatileProcess!.ProcessEvent(serializedEvent);

            var compositionEvent =
                new CompositionLogRecordInFile.CompositionEvent
                {
                    UpdateElmAppStateForEvent = new ValueInFileStructure
                    {
                        LiteralStringUtf8 = serializedEvent
                    }
                };

            var recordHash = storeWriter.AppendCompositionLogRecord(compositionEvent);

            lastCompositionLogRecordHashBase16 = recordHash.recordHashBase16;

            return elmAppResponse!;
        }
    }

    public void Dispose() => lastElmAppVolatileProcess?.Dispose();

    public (ProvisionalReductionRecordInFile? reductionRecord, StoreProvisionalReductionReport report) StoreReductionRecordForCurrentState(
        IProcessStoreWriter storeWriter)
    {
        var report = new StoreProvisionalReductionReport();

        string? elmAppState = null;

        var lockStopwatch = System.Diagnostics.Stopwatch.StartNew();

        lock (processLock)
        {
            lockStopwatch.Stop();

            report.lockTimeSpentMilli = (int)lockStopwatch.ElapsedMilliseconds;

            if (lastCompositionLogRecordHashBase16 == CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16 ||
                lastCompositionLogRecordHashBase16 == null)
                return (null, report);

            var serializeStopwatch = System.Diagnostics.Stopwatch.StartNew();

            elmAppState = lastElmAppVolatileProcess?.GetSerializedState();

            report.serializeElmAppStateTimeSpentMilli = (int)serializeStopwatch.ElapsedMilliseconds;
            report.serializeElmAppStateLength = elmAppState?.Length;
        }

        var elmAppStateBlob =
            elmAppState == null
            ?
            null
            :
            Encoding.UTF8.GetBytes(elmAppState);

        var elmAppStateComponent =
            elmAppStateBlob == null
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
                        HashBase16 = CommonConversion.StringBase16(Composition.GetHash(elmAppStateComponent))
                    },
                appConfig:
                    new ValueInFileStructure
                    {
                        HashBase16 = CommonConversion.StringBase16(
                            Composition.GetHash(lastAppConfig.appConfigComponent)),
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
