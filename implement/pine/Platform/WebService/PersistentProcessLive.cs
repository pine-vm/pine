using ElmTime.Platform.WebService.ProcessStoreSupportingMigrations;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.IO;
using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
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
    PineValue AppConfigComponent);

public sealed class PersistentProcessLive : IAsyncDisposable
{
    private static readonly TimeSpan s_storeReductionIntervalDefault = TimeSpan.FromMinutes(10);

    private readonly Pine.Core.Addressing.ConcurrentPineValueHashCache _hashCache = new();

    private readonly System.Threading.Lock _processLock = new();

    public readonly ProcessAppConfig LastAppConfig;

    private readonly Func<DateTimeOffset> _getDateTimeOffset;

    private readonly WebServiceInterface.WebServiceConfig _appConfigParsed;

    private readonly IProcessStoreWriter _storeWriter;

    private (PineValue state, CompositionLogRecordInFile.CompositionEvent compositionLogEvent, string hashBase16)? _lastAppStatePersisted;

    private StoreAppStateResetAndReductionReport? _lastStoreReduction = null;

    private DateTimeOffset StartTime { init; get; }

    private readonly ProcessLiveRepresentation _processLiveRepresentation;

    public record struct CompositionLogRecordWithResolvedDependencies(
        CompositionLogRecordInFile CompositionRecord,
        string CompositionRecordHashBase16,
        ReductionWithResolvedDependencies? Reduction,
        CompositionEventWithResolvedDependencies? Composition);

    public record struct ReductionWithResolvedDependencies(
        PineValue ElmAppState,
        PineValue AppConfig,
        BlobTreeWithStringPath AppConfigAsTree);

    public record struct CompositionEventWithResolvedDependencies(
        PineValue? SetElmAppState = null,
        BlobTreeWithStringPath? DeployAppConfigAndInitElmAppState = null,
        BlobTreeWithStringPath? DeployAppConfigAndMigrateElmAppState = null,
        ApplyFunctionOnLiteralsAndStateEvent? ApplyFunctionOnLiteralsAndState = null);

    public record ApplyFunctionOnLiteralsAndStateEvent(
        PineValue Function,
        PineValue Arguments);

    public static PersistentProcessLive Create(
        ProcessAppConfig lastAppConfig,
        PineValue? lastAppState,
        IProcessStoreWriter storeWriter,
        Func<DateTimeOffset> getDateTimeOffset,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null,
        System.Threading.CancellationToken cancellationToken = default)
    {
        return new PersistentProcessLive(
            lastAppConfig,
            lastAppState,
            storeWriter,
            getDateTimeOffset,
            overrideElmAppInterfaceConfig,
            cancellationToken);
    }

    private PersistentProcessLive(
        ProcessAppConfig lastAppConfig,
        PineValue? lastAppState,
        IProcessStoreWriter storeWriter,
        Func<DateTimeOffset> getDateTimeOffset,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null,
        System.Threading.CancellationToken cancellationToken = default)
    {
        LastAppConfig = lastAppConfig;
        _storeWriter = storeWriter;
        _getDateTimeOffset = getDateTimeOffset;
        StartTime = getDateTimeOffset();

        _appConfigParsed =
            WebServiceConfigFromDeployment(
                lastAppConfig.AppConfigComponent,
                overrideElmAppInterfaceConfig);

        var progressWriter =
            new DelegatingProgressWriter(
                DelegateFunctionApplications: EnsurePersisted,
                DelegateStoreReduction: (value) => StoreAppStateResetAndReduction(value));

        _processLiveRepresentation =
            ProcessLiveRepresentation.Create(
                webServiceConfig: _appConfigParsed,
                lastAppState: lastAppState,
                progressWriter: progressWriter,
                getDateTimeOffset: getDateTimeOffset,
                artifactSourceCompositions: [lastAppConfig.AppConfigComponent],
                cancellationToken: cancellationToken);
    }

    public async System.Threading.Tasks.Task<WebServiceInterface.HttpResponse> ProcessHttpRequestAsync(
        WebServiceInterface.HttpRequestEventStruct requestEvent)
    {
        return await _processLiveRepresentation.ProcessHttpRequestAsync(requestEvent);
    }

    private void EnsurePersisted(
        IReadOnlyList<ApplyUpdateReport<WebServiceInterface.Command>> reports)
    {
        foreach (var applyFuncReport in reports)
        {
            var elmAppState = applyFuncReport.ResponseState;

            if (_lastAppStatePersisted?.state == elmAppState)
            {
                continue;
            }

            lock (_processLock)
            {
                var asApplyFunction =
                    AsApplyFunctionOnLiteralsAndStateEvent(applyFuncReport);

                _storeWriter.StoreComponent(asApplyFunction.Function);

                var functionHash =
                    Convert.ToHexStringLower(_hashCache.GetHash(asApplyFunction.Function).Span);

                var argumentsJsonString =
                    ProcessStoreWriterInFileStore.SerializeValueToJsonString(
                        asApplyFunction.Arguments);

                var compositionEvent =
                    new CompositionLogRecordInFile.CompositionEvent
                    {
                        ApplyFunctionOnLiteralAndState =
                        new CompositionLogRecordInFile.ApplyFunctionOnLiteralAndStateEvent
                        (
                            Function:
                            new ValueInFileStructure(HashBase16: functionHash),

                            Arguments:
                            new ValueInFileStructure(LiteralStringUtf8: argumentsJsonString)
                        )
                    };

                var recordHash =
                    _storeWriter.AppendCompositionLogRecord(compositionEvent);

                _lastAppStatePersisted =
                    (elmAppState, compositionEvent, recordHash.recordHashBase16);

                var lastStoreReductionAge =
                    _getDateTimeOffset() - (_lastStoreReduction?.Time ?? StartTime);

                if (s_storeReductionIntervalDefault <= lastStoreReductionAge)
                {
                    var resetEvent = StoreAppStateResetAndReduction(elmAppState);

                    _lastAppStatePersisted =
                        (elmAppState, resetEvent.CompositionLogEvent, resetEvent.RecordHashBase16);
                }
            }
        }
    }

    private ApplyFunctionOnLiteralsAndStateEvent AsApplyFunctionOnLiteralsAndStateEvent(
        ApplyUpdateReport<WebServiceInterface.Command> applyUpdateReport)
    {
        var functionExpr =
            ApplyUpdateExpression(applyUpdateReport.Input.AppliedFunction);

        return
            new ApplyFunctionOnLiteralsAndStateEvent(
                Function:
                functionExpr,
                Arguments:
                PineValue.List([.. applyUpdateReport.Input.ArgsBeforeState]));
    }

    private readonly ConcurrentDictionary<FunctionRecordValueAndParsed, PineValue> _applyFunctionCache = new();

    private PineValue ApplyUpdateExpression(
        FunctionRecordValueAndParsed applyFunction) =>
        _applyFunctionCache.GetOrAdd(
            applyFunction,
            _ => BuildApplyUpdateExpression(applyFunction));

    private static PineValue BuildApplyUpdateExpression(
        FunctionRecordValueAndParsed applyFunction) =>
        BuildDatabaseFunctionLogEntry(
            applyFunction.Parsed,
            pathToStateInReturnValue: new ReadOnlyMemory<int>([0]));

    public static PineValue BuildDatabaseFunctionLogEntry(
        FunctionRecord appliedFunction,
        ReadOnlyMemory<int> pathToStateInReturnValue)
    {
        var parametersRemaining =
            appliedFunction.ParameterCount -
            appliedFunction.ArgumentsAlreadyCollected.Length;

        if (parametersRemaining is not 2)
        {
            throw new NotImplementedException(
                "Expected function to have two parameters.");
        }

        var getStateExpr =
            ExpressionForPath(
                pathToStateInReturnValue,
                appliedFunction.InnerFunction);

        return
            FunctionRecord.EncodeFunctionRecordInValueTagged(
                functionRecord:
                appliedFunction
                with
                {
                    InnerFunction = getStateExpr,
                });
    }

    public static Result<string, PineValue> ApplyUpdate(
        ApplyFunctionOnLiteralsAndStateEvent applyFunctionOnLiteralAndState,
        PineValue lastAppState,
        IPineVM pineVM,
        PineVMParseCache pineVMParseCache)
    {
        var parseAsFunctionRecord =
            FunctionRecord.ParseFunctionRecordTagged(
                applyFunctionOnLiteralAndState.Function,
                parseCache: pineVMParseCache);

        {
            if (parseAsFunctionRecord.IsErrOrNull() is { } err)
            {
                return "Failed to parse function: " + err;
            }
        }

        if (parseAsFunctionRecord.IsOkOrNull() is not { } parseFunctionOk)
        {
            throw new Exception("Unexpected result: " + parseAsFunctionRecord);
        }

        if (applyFunctionOnLiteralAndState.Arguments is not PineValue.ListValue argumentsBeforeState)
        {
            throw new Exception(
                "Unexpected shape of apply function event: Expected argument value to be a list value: " +
                JsonSerializer.Serialize(applyFunctionOnLiteralAndState));
        }

        return
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                parseFunctionOk,
                [.. argumentsBeforeState.Items.ToArray(), lastAppState]);
    }

    public static Expression ExpressionForPath(
        ReadOnlyMemory<int> path,
        Expression sourceExpr)
    {
        var currentNode = sourceExpr;

        for (var i = 0; i < path.Length; i++)
        {
            var nextSkipCount = path.Span[i];

            var skippedExpr =
                nextSkipCount is 0
                ?
                currentNode
                :
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.skip),
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(nextSkipCount)),
                        currentNode
                        ]));

            currentNode =
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.head),
                    skippedExpr);
        }

        return currentNode;
    }


    private static PineValue ReplaceInValue(
        PineValue pineValue,
        PineValue oldValue,
        PineValue newValue)
    {
        if (pineValue == oldValue)
            return newValue;

        if (pineValue is PineValue.ListValue listValue)
        {
            var newElements = new PineValue[listValue.Items.Length];

            for (var i = 0; i < listValue.Items.Length; i++)
            {
                newElements[i] = ReplaceInValue(listValue.Items.Span[i], oldValue, newValue);
            }

            return PineValue.List(newElements);
        }

        return pineValue;
    }

    private record struct StoreAppStateResetAndReductionReport(
        DateTimeOffset Time,
        PineValue ElmAppState,
        CompositionLogRecordInFile.CompositionEvent CompositionLogEvent,
        string RecordHashBase16,
        System.Threading.Tasks.Task TaskStoringReduction);

    private StoreAppStateResetAndReductionReport
        StoreAppStateResetAndReduction(PineValue elmAppState)
    {
        var appStateHash =
            _storeWriter.StoreComponent(elmAppState);

        var appConfigHash =
            _storeWriter.StoreComponent(LastAppConfig.AppConfigComponent);

        var compositionEvent =
            new CompositionLogRecordInFile.CompositionEvent
            {
                SetElmAppState = new ValueInFileStructure
                {
                    HashBase16 = appStateHash.hashBase16
                }
            };

        var currentTime = _getDateTimeOffset();

        string StoreCompositionRecord()
        {
            lock (_processLock)
            {
                var recordHash =
                    _storeWriter.AppendCompositionLogRecord(compositionEvent);

                _lastAppStatePersisted =
                    (elmAppState, compositionEvent, recordHash.recordHashBase16);

                return recordHash.recordHashBase16;
            }
        }

        var recordHashBase16 = StoreCompositionRecord();

        var taskStoringReduction =
            System.Threading.Tasks.Task.Run(() =>
            {
                _storeWriter.StoreProvisionalReduction(
                    new ProvisionalReductionRecordInFile(
                        reducedCompositionHashBase16: recordHashBase16,
                        elmAppState:
                        new ValueInFileStructure(
                            HashBase16: appStateHash.hashBase16),
                        appConfig:
                        new ValueInFileStructure(
                            HashBase16:
                            appConfigHash.hashBase16)));
            });

        var report =
            new StoreAppStateResetAndReductionReport(
                Time: currentTime,
                ElmAppState: elmAppState,
                CompositionLogEvent: compositionEvent,
                RecordHashBase16: recordHashBase16,
                TaskStoringReduction: taskStoringReduction);

        _lastStoreReduction = report;

        return report;
    }

    public static (IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> files, string lastCompositionLogRecordHashBase16)
        GetFilesForRestoreProcess(
        IFileStoreReader fileStoreReader)
    {
        var filesForProcessRestore =
            new ConcurrentDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>(
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

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

        Dictionary<string, PineValue> componentCache = [];

        var compositionLogRecords =
            EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(
                new ProcessStoreReaderInFileStore(recordingReader),
                componentCache)
            .ToImmutableList();

        return (
            files: filesForProcessRestore.ToImmutableDictionary(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>()),
            lastCompositionLogRecordHashBase16: compositionLogRecords.LastOrDefault().CompositionRecordHashBase16);
    }

    private static IEnumerable<CompositionLogRecordWithResolvedDependencies>
        EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(
        IProcessStoreReader storeReader,
        Dictionary<string, PineValue> componentCache) =>
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
                    var appConfigComponent =
                    LoadComponentFromStoreReader(
                        appConfigHash,
                        cache: true,
                        storeReader,
                        componentCache);

                    var elmAppStateComponent =
                    LoadComponentFromStoreReader(
                        appStateHash,
                        cache: true,
                        storeReader,
                        componentCache);

                    if (appConfigComponent is not null && elmAppStateComponent is not null)
                    {
                        var appConfigAsTree =
                        PineValueComposition.ParseAsTreeWithStringPath(appConfigComponent)
                        .Extract(_ => throw new Exception(
                            "Unexpected content of appConfigComponent " + appConfigHash + ": Failed to parse as tree."));

                        reduction = new ReductionWithResolvedDependencies
                        (
                            AppConfig: appConfigComponent,
                            AppConfigAsTree: appConfigAsTree,
                            ElmAppState: elmAppStateComponent
                        );
                    }
                }

                return new CompositionLogRecordWithResolvedDependencies
                (
                    CompositionRecord: compositionRecord,
                    CompositionRecordHashBase16: compositionRecordHashBase16,
                    Composition:
                    LoadCompositionEventDependencies(
                        compositionRecord.compositionEvent,
                        storeReader,
                        componentCache: componentCache),
                    Reduction: reduction
                );
            })
            .TakeUntil(compositionAndReduction => compositionAndReduction.Reduction is not null)
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

        Dictionary<string, PineValue> componentCache = [];

        var compositionEventsFromLatestReduction =
            EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(
                storeReader,
                componentCache)
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
        PersistentProcessLive process,
        IReadOnlyList<WebServiceInterface.Command> initOrMigrateCmds);

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

        if (firstCompositionLogRecord.Reduction is null &&
            firstCompositionLogRecord.CompositionRecord.parentHashBase16 != CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16)
        {
            return
                "Failed to get sufficient history: Composition log record points to parent " +
                firstCompositionLogRecord.CompositionRecord.parentHashBase16;
        }

        var pineVMCache = new PineVMCache();

        var pineVM = new PineVM(pineVMCache.EvalCache);

        var initialProcessRepresentation = new PersistentProcessLiveRepresentationDuringRestore(
            LastCompositionLogRecordHashBase16: null,
            LastAppConfig: null,
            InitOrMigrateCmds: [],
            LastAppState: null);


        Result<string, PersistentProcessLiveRepresentationDuringRestore> integrateCompositionLogRecord(
            PersistentProcessLiveRepresentationDuringRestore process,
            CompositionLogRecordWithResolvedDependencies compositionLogRecord)
        {
            Result<string, PersistentProcessLiveRepresentationDuringRestore> ContinueOk(
                PersistentProcessLiveRepresentationDuringRestore process) =>
                Result<string, PersistentProcessLiveRepresentationDuringRestore>.ok(process
                with
                {
                    LastCompositionLogRecordHashBase16 = compositionLogRecord.CompositionRecordHashBase16
                });

            var compositionEvent = compositionLogRecord.CompositionRecord.compositionEvent;

            if (compositionLogRecord.Reduction is { } reductionWithResolvedDependencies)
            {
                return
                    ContinueOk(
                        process
                        with
                        {
                            LastAppConfig =
                            new ProcessAppConfig(reductionWithResolvedDependencies.AppConfig),
                            LastAppState = reductionWithResolvedDependencies.ElmAppState,
                            InitOrMigrateCmds = []
                        });
            }

            if (compositionEvent.RevertProcessTo is { } revertProcessTo)
            {
                if (revertProcessTo.HashBase16 != process.LastCompositionLogRecordHashBase16)
                {
                    return
                        "Error in enumeration of process composition events: Got revert to " +
                        revertProcessTo.HashBase16 +
                        ", but previous version in the enumerated sequence was " + process.LastCompositionLogRecordHashBase16 + ".";
                }

                return ContinueOk(process);
            }

            return
                ApplyCompositionEvent(
                    compositionLogRecord.Composition!.Value,
                    process,
                    pineVM,
                    pineVMParseCache: pineVM.ParseCache,
                    overrideElmAppInterfaceConfig)
                .AndThen(ContinueOk);
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
                if (aggregateOk.LastCompositionLogRecordHashBase16 is null ||
                    aggregateOk.LastAppConfig is not { } lastAppConfig)
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
                        Create(
                            lastAppConfig: lastAppConfig,
                            lastAppState: aggregateOk.LastAppState,
                            storeWriter: storeWriter,
                            getDateTimeOffset,
                            overrideElmAppInterfaceConfig,
                            cancellationToken),
                        aggregateOk.InitOrMigrateCmds);
            });
    }

    private record PersistentProcessLiveRepresentationDuringRestore(
        string? LastCompositionLogRecordHashBase16,
        ProcessAppConfig? LastAppConfig,
        IReadOnlyList<WebServiceInterface.Command> InitOrMigrateCmds,
        PineValue? LastAppState);

    private static Result<string, PersistentProcessLiveRepresentationDuringRestore> ApplyCompositionEvent(
        CompositionEventWithResolvedDependencies compositionEvent,
        PersistentProcessLiveRepresentationDuringRestore processBefore,
        IPineVM pineVM,
        PineVMParseCache pineVMParseCache,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig)
    {
        if (compositionEvent.SetElmAppState is { } setElmAppState)
        {
            return
                new PersistentProcessLiveRepresentationDuringRestore(
                    LastCompositionLogRecordHashBase16: processBefore.LastCompositionLogRecordHashBase16,
                    LastAppConfig: processBefore.LastAppConfig,
                    InitOrMigrateCmds: [],
                    LastAppState: setElmAppState);
        }

        if (compositionEvent.DeployAppConfigAndMigrateElmAppState is { } deployAppConfigAndMigrateElmAppState)
        {
            if (processBefore.LastAppConfig is not { } lastAppConfig)
            {
                return "No app config before";
            }

            if (processBefore.LastAppState is not { } lastAppState)
            {
                return "No app state before";
            }

            var appConfigParsedBefore =
                WebServiceConfigFromDeployment(
                    lastAppConfig.AppConfigComponent,
                    overrideElmAppInterfaceConfig);

            var appConfigTreeNext =
                deployAppConfigAndMigrateElmAppState;

            var appConfigParsedNext =
                WebServiceConfigFromDeployment(
                    deployAppConfigAndMigrateElmAppState,
                    overrideElmAppInterfaceConfig);

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
                    LastAppConfig =
                    new ProcessAppConfig(PineValueComposition.FromTreeWithStringPath(
                        deployAppConfigAndMigrateElmAppState)),

                    LastAppState = migrateOk.newState,
                };
        }

        if (compositionEvent.DeployAppConfigAndInitElmAppState is { } appConfig)
        {
            var appConfigParsed =
                WebServiceConfigFromDeployment(
                    appConfig,
                    overrideElmAppInterfaceConfig);

            return
                processBefore
                with
                {
                    LastAppConfig =
                        new ProcessAppConfig(PineValueComposition.FromTreeWithStringPath(appConfig)),
                    LastAppState = appConfigParsed.Init.State,
                    InitOrMigrateCmds = []
                };
        }

        if (compositionEvent.ApplyFunctionOnLiteralsAndState is { } applyFunctionOnLiteralAndState)
        {
            var lastAppState =
                processBefore.LastAppState
                ?? throw new Exception("No app state before");

            var evalResult =
                ApplyUpdate(
                    applyFunctionOnLiteralAndState,
                    lastAppState,
                    pineVM,
                    pineVMParseCache);

            {
                if (evalResult.IsErrOrNull() is { } err)
                {
                    return "Failed to apply function: " + err;
                }
            }

            if (evalResult.IsOkOrNull() is not { } evalOk)
            {
                throw new Exception("Unexpected result: " + evalResult);
            }

            return
                processBefore
                with
                {
                    LastAppState = evalOk
                };
        }

        return "Unexpected shape of composition event: " + JsonSerializer.Serialize(compositionEvent);
    }

    public static WebServiceInterface.WebServiceConfig WebServiceConfigFromDeployment(
        PineValue deploymentValue,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig)
    {
        var appConfigTree =
            PineValueComposition.ParseAsTreeWithStringPath(deploymentValue)
            .Extract(err => throw new Exception("Failed to parse app config as file tree: " + err));

        return WebServiceConfigFromDeployment(appConfigTree, overrideElmAppInterfaceConfig);
    }

    public static WebServiceInterface.WebServiceConfig WebServiceConfigFromDeployment(
        BlobTreeWithStringPath appConfigTree,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig)
    {
        var compilationRootFilePath =
            overrideElmAppInterfaceConfig?.CompilationRootFilePath
            ??
            ["src", "Backend", "Main.elm"];

        var appConfigParsed =
            WebServiceInterface.ConfigFromSourceFilesAndEntryFileName(
                appConfigTree,
                compilationRootFilePath);

        return appConfigParsed;
    }


    private static CompositionEventWithResolvedDependencies? LoadCompositionEventDependencies(
        CompositionLogRecordInFile.CompositionEvent compositionEvent,
        IProcessStoreReader storeReader,
        Dictionary<string, PineValue> componentCache)
    {
        PineValue LoadComponentFromValueInFileStructure(
            ValueInFileStructure valueInFileStructure,
            bool cacheFromStore)
        {
            if (valueInFileStructure.HashBase16 is { } hashBase16)
            {
                return LoadComponentFromStoreReader(hashBase16, cacheFromStore, storeReader, componentCache);
            }

            if (valueInFileStructure.LiteralStringUtf8 is { } literalStringUtf8)
            {
                return ProcessStoreReaderInFileStore.DeserializeValueFromJsonString(literalStringUtf8);
            }

            throw new Exception(
                "Unexpected shape of valueInFileStructure: " +
                JsonSerializer.Serialize(valueInFileStructure));
        }

        BlobTreeWithStringPath LoadComponentFromStoreAndAssertIsTree(
            ValueInFileStructure valueInFileStructure)
        {
            var component =
                LoadComponentFromValueInFileStructure(valueInFileStructure, cacheFromStore: false);

            return
                PineValueComposition.ParseAsTreeWithStringPath(component)
                .Extract(_ => throw new Exception("Failed to load component " + component + " as tree: Failed to parse as tree."));
        }

        if (compositionEvent.SetElmAppState is { } setElmAppState)
        {
            return new CompositionEventWithResolvedDependencies
            {
                SetElmAppState =
                LoadComponentFromValueInFileStructure(
                    setElmAppState,
                    cacheFromStore: false)
            };
        }

        if (compositionEvent.DeployAppConfigAndMigrateElmAppState is { } deployAppConfigAndMigrateElmAppState)
        {
            return new CompositionEventWithResolvedDependencies
            {
                DeployAppConfigAndMigrateElmAppState =
                LoadComponentFromStoreAndAssertIsTree(deployAppConfigAndMigrateElmAppState),
            };
        }

        if (compositionEvent.DeployAppConfigAndInitElmAppState is { } deployAppConfigAndInitElmAppState)
        {
            return new CompositionEventWithResolvedDependencies
            {
                DeployAppConfigAndInitElmAppState =
                LoadComponentFromStoreAndAssertIsTree(deployAppConfigAndInitElmAppState),
            };
        }

        if (compositionEvent.ApplyFunctionOnLiteralAndState is { } applyFunctionOnLiteralAndState)
        {
            return new CompositionEventWithResolvedDependencies
            {
                ApplyFunctionOnLiteralsAndState =
                new ApplyFunctionOnLiteralsAndStateEvent(
                    Function:
                    LoadComponentFromValueInFileStructure(
                        applyFunctionOnLiteralAndState.Function,
                        cacheFromStore: true),

                    Arguments:
                    LoadComponentFromValueInFileStructure(
                        applyFunctionOnLiteralAndState.Arguments,
                        cacheFromStore: false))
            };
        }

        throw new NotImplementedException(
            "Unexpected type of composition event: " + compositionEvent.GetType().FullName);
    }

    private static PineValue LoadComponentFromStoreReader(
        string hashBase16,
        bool cache,
        IProcessStoreReader storeReader,
        Dictionary<string, PineValue> componentCache)
    {
        if (componentCache.TryGetValue(hashBase16, out var cachedComponent))
        {
            return cachedComponent;
        }

        var loaded =
            storeReader.LoadComponent(hashBase16)
            ??
            throw new Exception("Failed to load component " + hashBase16 + ": Not found in store.");

        if (cache)
        {
            componentCache.Add(hashBase16, loaded);
        }

        return loaded;
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
                    new ProcessStoreReaderInFileStore(projectionResult.ProjectedReader),
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
        SetStateOnMainBranchFromJson(JsonElement appState)
    {
        var appStateJsonString = JsonSerializer.Serialize(appState);

        var pineVMCache = new PineVMCache();

        var pineVM = new PineVM(pineVMCache.EvalCache);

        return
            SetStateOnMainBranchFromJson(
                appStateJsonString,
                pineVM);
    }

    public Result<string, (CompositionLogRecordInFile.CompositionEvent compositionLogEvent, string)>
        SetStateOnMainBranchFromJson(
        string appStateJsonString,
        PineVM pineVM)
    {
        var jsonStringEncoded = ElmValueEncoding.StringAsPineValue(appStateJsonString);

        var appStateDecodeResult =
            _appConfigParsed.JsonAdapter.DecodeAppStateFromJsonString(
                jsonStringEncoded,
                pineVM);

        if (appStateDecodeResult.IsErrOrNull() is { } err)
        {
            return "Failed to decode app state: " + err;
        }

        if (appStateDecodeResult.IsOkOrNull() is not { } appStateDecoded)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + appStateDecodeResult);
        }

        return ResetAppStateIgnoringTypeChecking(appStateDecoded);
    }

    public (CompositionLogRecordInFile.CompositionEvent compositionLogEvent, string) ResetAppStateIgnoringTypeChecking(
        PineValue appState)
    {
        _processLiveRepresentation.ResetAppStateIgnoringTypeChecking(appState);

        lock (_processLock)
        {
            var storeReductionReport =
                StoreAppStateResetAndReduction(appState);

            return (storeReductionReport.CompositionLogEvent, storeReductionReport.RecordHashBase16);
        }
    }

    public PineValue GetAppStateOnMainBranch()
    {
        return _processLiveRepresentation.GetAppStateOnMainBranch();
    }

    public Result<string, JsonElement> GetAppStateOnMainBranchAsJson()
    {
        var pineVMCache = new PineVMCache();

        var pineVM = new PineVM(pineVMCache.EvalCache);

        return
            GetAppStateOnMainBranchAsJson(pineVM)
            .Map(jsonString => JsonSerializer.Deserialize<JsonElement>(jsonString));
    }

    public Result<string, string> GetAppStateOnMainBranchAsJson(
        PineVM pineVM)
    {
        var appState = _processLiveRepresentation.GetAppStateOnMainBranch();

        var jsonStringResult =
            _appConfigParsed.JsonAdapter.EncodeAppStateAsJsonString(appState, pineVM);

        {
            if (jsonStringResult.IsErrOrNull() is { } err)
            {
                return Result<string, string>.err(err);
            }
        }

        if (jsonStringResult.IsOkOrNull() is not { } jsonString)
        {
            throw new Exception("Unexpected result: " + jsonStringResult);
        }

        return Result<string, string>.ok(jsonString);
    }

    public Result<string, AdminInterface.ApplyDatabaseFunctionSuccess> ApplyFunctionOnMainBranch(
        AdminInterface.ApplyDatabaseFunctionRequest request)
    {
        lock (_processLock)
        {
            var clock = System.Diagnostics.Stopwatch.StartNew();

            var appState = _processLiveRepresentation.GetAppStateOnMainBranch();

            var pineVMCache = new PineVMCache();

            var pineVM = new PineVM(pineVMCache.EvalCache);

            var matchingExposedFunction =
                _appConfigParsed.JsonAdapter.ExposedFunctions
                .FirstOrDefault(exposedFunc => exposedFunc.Key == request.functionName)
                .Value;

            if (matchingExposedFunction is null)
            {
                return "Function not found: " + request.functionName;
            }

            var arguments = new PineValue[request.serializedArgumentsJson.Count];

            for (var i = 0; i < request.serializedArgumentsJson.Count; i++)
            {
                var argumentJsonString = request.serializedArgumentsJson[i];

                var parseArgumentResult =
                    _appConfigParsed.JsonAdapter.DecodeElmJsonValueFromString(
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
                _getDateTimeOffset().ToUnixTimeMilliseconds();

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

            var committedResultingState = false;

            if (request.commitResultingState)
            {
                if (applyFunctionOk.AppState is not { } appStateReturned)
                {
                    return
                        "Requested commiting new app state, but the function " +
                        request.functionName +
                        " did not return a new app state.";
                }

                _processLiveRepresentation.ResetAppStateIgnoringTypeChecking(appStateReturned);

                var storeReductionReport =
                    StoreAppStateResetAndReduction(appStateReturned);

                committedResultingState = true;
            }

            var producedStateDifferentFromStateArgument = false;

            {
                if (applyFunctionOk.AppState is { } appStateReturned)
                {
                    producedStateDifferentFromStateArgument = !appStateReturned.Equals(appState);
                }
            }

            var resultLessStateJson =
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
            _appConfigParsed.JsonAdapter.EncodeJsonValueAsJsonString(
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
            [.._appConfigParsed.JsonAdapter.ExposedFunctions
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

    public async System.Threading.Tasks.ValueTask DisposeAsync()
    {
        await _processLiveRepresentation.DisposeAsync();
    }
}
