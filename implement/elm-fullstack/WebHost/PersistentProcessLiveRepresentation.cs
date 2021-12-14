using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using ElmFullstack.WebHost.ProcessStoreSupportingMigrations;
using Newtonsoft.Json;
using Pine;

namespace ElmFullstack.WebHost.PersistentProcess;

public interface IPersistentProcess
{
    string ProcessElmAppEvent(IProcessStoreWriter storeWriter, string serializedEvent);

    (ProvisionalReductionRecordInFile reductionRecord, StoreProvisionalReductionReport report) StoreReductionRecordForCurrentState(IProcessStoreWriter storeWriter);
}

public struct StoreProvisionalReductionReport
{
    public int lockTimeSpentMilli;

    public int? serializeElmAppStateTimeSpentMilli;

    public int? serializeElmAppStateLength;

    public int? storeDependenciesTimeSpentMilli;
}

public record struct ProcessAppConfig(
    Composition.Component appConfigComponent,
    (string javascriptFromElmMake, string javascriptPreparedToRun) buildArtifacts);

public class PersistentProcessLiveRepresentation : IPersistentProcess, IDisposable
{
    readonly object processLock = new();

    string lastCompositionLogRecordHashBase16;

    public readonly ProcessAppConfig? lastAppConfig;

    readonly IDisposableProcessWithStringInterface lastElmAppVolatileProcess;

    public struct CompositionLogRecordWithLoadedDependencies
    {
        public CompositionLogRecordInFile compositionRecord;

        public string compositionRecordHashBase16;

        public ReductionWithLoadedDependencies? reduction;

        public CompositionEventWithLoadedDependencies? composition;
    }

    public struct ReductionWithLoadedDependencies
    {
        public byte[] elmAppState;

        public Composition.Component appConfig;

        public Composition.TreeWithStringPath appConfigAsTree;
    }

    public struct CompositionEventWithLoadedDependencies
    {
        public byte[] UpdateElmAppStateForEvent;

        public byte[] SetElmAppState;

        public Composition.TreeWithStringPath DeployAppConfigAndInitElmAppState;

        public Composition.TreeWithStringPath DeployAppConfigAndMigrateElmAppState;
    }

    static public (IDisposableProcessWithStringInterface process,
        (string javascriptFromElmMake, string javascriptPreparedToRun) buildArtifacts)
        ProcessFromWebAppConfig(
        Composition.TreeWithStringPath appConfig,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
    {
        var sourceFiles = Composition.TreeToFlatDictionaryWithPathComparer(appConfig);

        var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
            sourceFiles: sourceFiles,
            ElmAppInterfaceConfig.Default);

        if (compilationResult.Ok == null)
            throw new Exception(ElmAppCompilation.CompileCompilationErrorsDisplayText(compilationResult.Err));

        var (loweredAppFiles, _) = compilationResult.Ok;

        var (process, buildArtifacts) =
            ProcessFromElm019Code.ProcessFromElmCodeFiles(
            loweredAppFiles,
            overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

        return (process, buildArtifacts);
    }

    PersistentProcessLiveRepresentation(
        string lastCompositionLogRecordHashBase16,
        ProcessAppConfig? lastAppConfig,
        IDisposableProcessWithStringInterface lastElmAppVolatileProcess)
    {
        this.lastCompositionLogRecordHashBase16 = lastCompositionLogRecordHashBase16;
        this.lastAppConfig = lastAppConfig;
        this.lastElmAppVolatileProcess = lastElmAppVolatileProcess;
    }

    static public (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> files, string lastCompositionLogRecordHashBase16)
        GetFilesForRestoreProcess(
        IFileStoreReader fileStoreReader)
    {
        var filesForProcessRestore = new ConcurrentDictionary<IImmutableList<string>, IReadOnlyList<byte>>(EnumerableExtension.EqualityComparer<IImmutableList<string>>());

        var recordingReader = new DelegatingFileStoreReader
        (
            ListFilesInDirectoryDelegate: fileStoreReader.ListFilesInDirectory,
            GetFileContentDelegate: filePath =>
            {
                var fileContent = fileStoreReader.GetFileContent(filePath);

                if (fileContent != null)
                {
                    filesForProcessRestore[filePath] = fileContent;
                }

                return fileContent;
            }
        );

        var compositionLogRecords =
            EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(new ProcessStoreReaderInFileStore(recordingReader))
            .ToImmutableList();

        return (
            files: filesForProcessRestore.ToImmutableDictionary(EnumerableExtension.EqualityComparer<IImmutableList<string>>()),
            lastCompositionLogRecordHashBase16: compositionLogRecords.LastOrDefault().compositionRecordHashBase16);
    }

    static IEnumerable<CompositionLogRecordWithLoadedDependencies>
        EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(IProcessStoreReader storeReader) =>
            storeReader
            .EnumerateSerializedCompositionLogRecordsReverse()
            .Select(serializedCompositionLogRecord =>
            {
                var compositionRecordHashBase16 =
                    CompositionLogRecordInFile.HashBase16FromCompositionRecord(serializedCompositionLogRecord);

                var compositionRecord = JsonConvert.DeserializeObject<CompositionLogRecordInFile>(
                    Encoding.UTF8.GetString(serializedCompositionLogRecord));

                var reductionRecord = storeReader.LoadProvisionalReduction(compositionRecordHashBase16);

                ReductionWithLoadedDependencies? reduction = null;

                if (reductionRecord?.appConfig?.HashBase16 != null && reductionRecord?.elmAppState?.HashBase16 != null)
                {
                    var appConfigComponent = storeReader.LoadComponent(reductionRecord.appConfig?.HashBase16);

                    var elmAppStateComponent = storeReader.LoadComponent(reductionRecord.elmAppState?.HashBase16);

                    if (appConfigComponent != null && elmAppStateComponent != null)
                    {
                        var parseAppConfigAsTree = Composition.ParseAsTreeWithStringPath(appConfigComponent);

                        if (parseAppConfigAsTree.Ok == null)
                        {
                            throw new Exception("Unexpected content of appConfigComponent " + reductionRecord.appConfig?.HashBase16 + ": Failed to parse as tree.");
                        }

                        if (elmAppStateComponent.BlobContent == null)
                        {
                            throw new Exception("Unexpected content of elmAppStateComponent " + reductionRecord.elmAppState?.HashBase16 + ": This is not a blob.");
                        }

                        reduction = new ReductionWithLoadedDependencies
                        {
                            appConfig = appConfigComponent,
                            appConfigAsTree = parseAppConfigAsTree.Ok,
                            elmAppState = elmAppStateComponent.BlobContent.ToArray(),
                        };
                    }
                }

                return new CompositionLogRecordWithLoadedDependencies
                {
                    compositionRecord = compositionRecord,
                    compositionRecordHashBase16 = compositionRecordHashBase16,
                    composition = LoadCompositionEventDependencies(compositionRecord.compositionEvent, storeReader),
                    reduction = reduction,
                };
            })
            .TakeUntil(compositionAndReduction => compositionAndReduction.reduction != null)
            .Reverse();

    static public (PersistentProcessLiveRepresentation process, InterfaceToHost.AppEventResponseStructure initOrMigrateCmds)
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

            return
                (new PersistentProcessLiveRepresentation(
                lastCompositionLogRecordHashBase16: CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16,
                lastAppConfig: null,
                lastElmAppVolatileProcess: null),
                null);
        }

        logger?.Invoke("Found " + compositionEventsFromLatestReduction.Count + " composition log records to use for restore.");

        var processLiveRepresentation = RestoreFromCompositionEventSequence(
            compositionEventsFromLatestReduction,
            overrideElmAppInterfaceConfig);

        logger?.Invoke("Restored the process state in " + ((int)restoreStopwatch.Elapsed.TotalSeconds) + " seconds.");

        return processLiveRepresentation;
    }

    static public (PersistentProcessLiveRepresentation process, InterfaceToHost.AppEventResponseStructure initOrMigrateCmds)
        RestoreFromCompositionEventSequence(
        IEnumerable<CompositionLogRecordWithLoadedDependencies> compositionLogRecords,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
    {
        var firstCompositionLogRecord =
            compositionLogRecords.FirstOrDefault();

        if (firstCompositionLogRecord.reduction == null &&
            firstCompositionLogRecord.compositionRecord.parentHashBase16 != CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16)
        {
            throw new Exception("Failed to get sufficient history: Composition log record points to parent " + firstCompositionLogRecord.compositionRecord.parentHashBase16);
        }

        string lastCompositionLogRecordHashBase16 = null;

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
                    var (newElmAppProcess, (javascriptFromElmMake, javascriptPreparedToRun)) =
                        ProcessFromWebAppConfig(
                            compositionLogRecord.reduction.Value.appConfigAsTree,
                            overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

                    var elmAppStateAsString = Encoding.UTF8.GetString(compositionLogRecord.reduction.Value.elmAppState);

                    var setStateResult =
                        AttemptProcessEvent(
                            newElmAppProcess,
                            new InterfaceToHost.AppEventStructure { SetStateEvent = elmAppStateAsString });

                    if (setStateResult?.Ok == null)
                        throw new Exception("Failed to set state: " + setStateResult?.Err);

                    processRepresentationDuringRestore?.lastElmAppVolatileProcess?.Dispose();

                    processRepresentationDuringRestore = new PersistentProcessLiveRepresentationDuringRestore(
                        lastAppConfig: new ProcessAppConfig(compositionLogRecord.reduction.Value.appConfig, (javascriptFromElmMake, javascriptPreparedToRun)),
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

                var applyCompositionEventResult =
                    ApplyCompositionEvent(
                        compositionLogRecord.composition.Value,
                        processRepresentationDuringRestore,
                        overrideElmAppInterfaceConfig);

                if (applyCompositionEventResult?.Ok == null)
                {
                    throw new Exception("Failed to apply composition event: " + applyCompositionEventResult?.Err);
                }

                processRepresentationDuringRestore = applyCompositionEventResult?.Ok;
            }
            finally
            {
                lastCompositionLogRecordHashBase16 = compositionLogRecord.compositionRecordHashBase16;
            }
        }

        return (new PersistentProcessLiveRepresentation(
            lastCompositionLogRecordHashBase16: lastCompositionLogRecordHashBase16,
            lastAppConfig: processRepresentationDuringRestore.lastAppConfig,
            lastElmAppVolatileProcess: processRepresentationDuringRestore.lastElmAppVolatileProcess),
            processRepresentationDuringRestore.initOrMigrateCmds);
    }

    record PersistentProcessLiveRepresentationDuringRestore(
        ProcessAppConfig? lastAppConfig,
        IDisposableProcessWithStringInterface lastElmAppVolatileProcess,
        InterfaceToHost.AppEventResponseStructure initOrMigrateCmds);

    static Result<string, PersistentProcessLiveRepresentationDuringRestore> ApplyCompositionEvent(
        CompositionEventWithLoadedDependencies compositionEvent,
        PersistentProcessLiveRepresentationDuringRestore processBefore,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig)
    {
        if (compositionEvent.UpdateElmAppStateForEvent != null)
        {
            if (processBefore.lastElmAppVolatileProcess == null)
                return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Ok: processBefore);

            processBefore.lastElmAppVolatileProcess.ProcessEvent(
                Encoding.UTF8.GetString(compositionEvent.UpdateElmAppStateForEvent));

            return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Ok: processBefore);
        }

        if (compositionEvent.SetElmAppState != null)
        {
            if (processBefore.lastElmAppVolatileProcess == null)
            {
                return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Err:
                    "Failed to load the serialized state with the elm app: Looks like no app was deployed so far.");
            }

            var projectedElmAppState =
                Encoding.UTF8.GetString(compositionEvent.SetElmAppState);

            var processEventResult =
                AttemptProcessEvent(processBefore.lastElmAppVolatileProcess, new InterfaceToHost.AppEventStructure { SetStateEvent = projectedElmAppState });

            if (processEventResult?.Ok == null)
            {
                return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Err:
                    "Set state function in the hosted app returned an error: " + processEventResult?.Err);
            }

            return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Ok: processBefore);
        }

        if (compositionEvent.DeployAppConfigAndMigrateElmAppState != null)
        {
            var elmAppStateBefore = processBefore.lastElmAppVolatileProcess?.GetSerializedState();

            var appConfig = compositionEvent.DeployAppConfigAndMigrateElmAppState;

            var (newElmAppProcess, buildArtifacts) =
                ProcessFromWebAppConfig(appConfig, overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

            var migrateEventResult = AttemptProcessEvent(
                newElmAppProcess,
                new InterfaceToHost.AppEventStructure(MigrateStateEvent: elmAppStateBefore));

            if (migrateEventResult?.Ok == null)
            {
                return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Err:
                    "Failed to process the event in the hosted app: " + migrateEventResult?.Err);
            }

            if (migrateEventResult?.Ok?.migrateResult?.Just == null)
            {
                return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Err:
                    "Unexpected shape of response: migrateResult is Nothing");
            }

            if (migrateEventResult?.Ok?.migrateResult?.Just?.Ok == null)
            {
                return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Err:
                    "Migration function in the hosted app returned an error: " + migrateEventResult?.Ok?.migrateResult?.Just?.Err);
            }

            processBefore.lastElmAppVolatileProcess?.Dispose();

            return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Ok:
                new PersistentProcessLiveRepresentationDuringRestore(
                    lastAppConfig: new ProcessAppConfig(Composition.FromTreeWithStringPath(appConfig), buildArtifacts),
                    lastElmAppVolatileProcess: newElmAppProcess,
                    initOrMigrateCmds: migrateEventResult?.Ok));
        }

        if (compositionEvent.DeployAppConfigAndInitElmAppState != null)
        {
            var appConfig = compositionEvent.DeployAppConfigAndInitElmAppState;

            var (newElmAppProcess, buildArtifacts) =
                ProcessFromWebAppConfig(
                    appConfig,
                    overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

            var initEventResult = AttemptProcessEvent(
                newElmAppProcess,
                new InterfaceToHost.AppEventStructure(InitStateEvent: new()));

            if (initEventResult?.Ok == null)
            {
                return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Err:
                    "Failed to process the event in the hosted app: " + initEventResult?.Err);
            }

            processBefore.lastElmAppVolatileProcess?.Dispose();

            return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Ok:
                new PersistentProcessLiveRepresentationDuringRestore(
                    lastAppConfig: new ProcessAppConfig(Composition.FromTreeWithStringPath(appConfig), buildArtifacts),
                    lastElmAppVolatileProcess: newElmAppProcess,
                    initEventResult?.Ok));
        }

        return new Result<string, PersistentProcessLiveRepresentationDuringRestore>(Err:
            "Unexpected shape of composition event: " + JsonConvert.SerializeObject(compositionEvent));
    }

    static Result<string, InterfaceToHost.AppEventResponseStructure> AttemptProcessEvent(
        IProcessWithStringInterface process,
        InterfaceToHost.AppEventStructure appEvent)
    {
        var serializedInterfaceEvent =
            JsonConvert.SerializeObject(appEvent, InterfaceToHost.AppEventStructure.JsonSerializerSettings);

        var eventResponseSerial = process.ProcessEvent(serializedInterfaceEvent);

        try
        {
            var eventResponse =
                JsonConvert.DeserializeObject<InterfaceToHost.ResponseOverSerialInterface>(eventResponseSerial);

            if (eventResponse?.DecodeEventSuccess == null)
            {
                return new Result<string, InterfaceToHost.AppEventResponseStructure>(Err:
                    "Hosted app failed to decode the event: " + eventResponse.DecodeEventError);
            }

            return new Result<string, InterfaceToHost.AppEventResponseStructure> { Ok = eventResponse.DecodeEventSuccess };
        }
        catch (Exception parseException)
        {
            return new Result<string, InterfaceToHost.AppEventResponseStructure>
            {
                Err = "Failed to parse event response from the app. Looks like the loaded elm app is not compatible with the interface.\nI got following response from the app:\n" + eventResponseSerial + "\nException: " + parseException.ToString()
            };
        }
    }

    static CompositionEventWithLoadedDependencies? LoadCompositionEventDependencies(
        CompositionLogRecordInFile.CompositionEvent compositionEvent,
        IProcessStoreReader storeReader)
    {
        IReadOnlyList<byte> loadComponentFromValueInFileStructureAndAssertIsBlob(ValueInFileStructure valueInFileStructure)
        {
            if (valueInFileStructure.LiteralStringUtf8 != null)
                return Encoding.UTF8.GetBytes(valueInFileStructure.LiteralStringUtf8);

            return loadComponentFromStoreAndAssertIsBlob(valueInFileStructure.HashBase16);
        }

        IReadOnlyList<byte> loadComponentFromStoreAndAssertIsBlob(string componentHash)
        {
            var component = storeReader.LoadComponent(componentHash);

            if (component == null)
                throw new Exception("Failed to load component " + componentHash + ": Not found in store.");

            if (component.BlobContent == null)
                throw new Exception("Failed to load component " + componentHash + " as blob: This is not a blob.");

            return component.BlobContent;
        }

        Composition.TreeWithStringPath loadComponentFromStoreAndAssertIsTree(string componentHash)
        {
            var component = storeReader.LoadComponent(componentHash);

            if (component == null)
                throw new Exception("Failed to load component " + componentHash + ": Not found in store.");

            var parseAsTreeResult = Composition.ParseAsTreeWithStringPath(component);

            if (parseAsTreeResult.Ok == null)
                throw new Exception("Failed to load component " + componentHash + " as tree: Failed to parse as tree.");

            return parseAsTreeResult.Ok;
        }

        if (compositionEvent.UpdateElmAppStateForEvent != null)
        {
            return new CompositionEventWithLoadedDependencies
            {
                UpdateElmAppStateForEvent =
                    loadComponentFromValueInFileStructureAndAssertIsBlob(compositionEvent.UpdateElmAppStateForEvent).ToArray(),
            };
        }

        if (compositionEvent.SetElmAppState != null)
        {
            return new CompositionEventWithLoadedDependencies
            {
                SetElmAppState = loadComponentFromStoreAndAssertIsBlob(
                    compositionEvent.SetElmAppState.HashBase16).ToArray(),
            };
        }

        if (compositionEvent.DeployAppConfigAndMigrateElmAppState != null)
        {
            return new CompositionEventWithLoadedDependencies
            {
                DeployAppConfigAndMigrateElmAppState = loadComponentFromStoreAndAssertIsTree(
                    compositionEvent.DeployAppConfigAndMigrateElmAppState.HashBase16),
            };
        }

        if (compositionEvent.DeployAppConfigAndInitElmAppState != null)
        {
            return new CompositionEventWithLoadedDependencies
            {
                DeployAppConfigAndInitElmAppState = loadComponentFromStoreAndAssertIsTree(
                    compositionEvent.DeployAppConfigAndInitElmAppState.HashBase16),
            };
        }

        if (compositionEvent.RevertProcessTo != null)
            return null;

        throw new Exception("Unexpected shape of composition event: " + JsonConvert.SerializeObject(compositionEvent));
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
            return Result<string, FileStoreReaderProjectionResult>.err(
                "Failed with exception: " + e.ToString());
        }
    }

    public string ProcessElmAppEvent(IProcessStoreWriter storeWriter, string serializedEvent)
    {
        lock (processLock)
        {
            var elmAppResponse =
                lastElmAppVolatileProcess.ProcessEvent(serializedEvent);

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

            return elmAppResponse;
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

            if (lastCompositionLogRecordHashBase16 == CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16)
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
            Composition.Component.Blob(elmAppStateBlob);

        var reductionRecord =
            new ProvisionalReductionRecordInFile
            {
                reducedCompositionHashBase16 = lastCompositionLogRecordHashBase16,
                elmAppState =
                    elmAppStateComponent == null
                    ? null
                    : new ValueInFileStructure
                    {
                        HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppStateComponent))
                    },
                appConfig =
                    lastAppConfig == null
                    ? null
                    : new ValueInFileStructure
                    {
                        HashBase16 = CommonConversion.StringBase16FromByteArray(
                            Composition.GetHash(lastAppConfig.Value.appConfigComponent)),
                    },
            };

        var storeDependenciesStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var dependencies =
            new[] { elmAppStateComponent, lastAppConfig?.appConfigComponent }
            .Where(c => null != c).ToImmutableList();

        foreach (var dependency in dependencies)
            storeWriter.StoreComponent(dependency);

        storeDependenciesStopwatch.Stop();

        report.storeDependenciesTimeSpentMilli = (int)storeDependenciesStopwatch.ElapsedMilliseconds;

        storeWriter.StoreProvisionalReduction(reductionRecord);

        return (reductionRecord, report);
    }
}
