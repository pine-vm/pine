using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using ElmFullstack.WebHost.ProcessStoreSupportingMigrations;
using Newtonsoft.Json;
using Pine;

namespace ElmFullstack.WebHost.PersistentProcess
{
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


    public class PersistentProcessVolatileRepresentation : IPersistentProcess, IDisposable
    {
        static public string MigrationElmAppInterfaceModuleName => "Backend.MigrateState";

        static public string Old_MigrationElmAppInterfaceModuleName => "MigrateBackendState";

        static public string MigrationElmAppCompilationRootModuleName => MigrationElmAppInterfaceModuleName + "_Root";

        static public string MigrateElmFunctionNameInModule => "migrate";

        readonly object processLock = new object();

        string lastCompositionLogRecordHashBase16;

        public readonly (Composition.Component appConfigComponent, (string javascriptFromElmMake, string javascriptPreparedToRun) buildArtifacts)? lastAppConfig;

        readonly IDisposableProcessWithStringInterface lastElmAppVolatileProcess;

        public readonly Result<string, string> lastSetElmAppStateResult;

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

            var (loweredAppFiles, _) = ElmApp.AsCompletelyLoweredElmApp(
                sourceFiles: sourceFiles,
                ElmAppInterfaceConfig.Default);

            var (process, buildArtifacts) =
                ProcessFromElm019Code.ProcessFromElmCodeFiles(
                loweredAppFiles,
                overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

            return (process, buildArtifacts);
        }

        PersistentProcessVolatileRepresentation(
            string lastCompositionLogRecordHashBase16,
            (Composition.Component appConfigComponent, (string javascriptFromElmMake, string javascriptPreparedToRun))? lastAppConfig,
            IDisposableProcessWithStringInterface lastElmAppVolatileProcess,
            Result<string, string> lastSetElmAppStateResult)
        {
            this.lastCompositionLogRecordHashBase16 = lastCompositionLogRecordHashBase16;
            this.lastAppConfig = lastAppConfig;
            this.lastElmAppVolatileProcess = lastElmAppVolatileProcess;
            this.lastSetElmAppStateResult = lastSetElmAppStateResult;
        }

        static public (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> files, string lastCompositionLogRecordHashBase16)
            GetFilesForRestoreProcess(
            IFileStoreReader fileStoreReader)
        {
            var filesForProcessRestore = new ConcurrentDictionary<IImmutableList<string>, IReadOnlyList<byte>>(EnumerableExtension.EqualityComparer<string>());

            var recordingReader = new DelegatingFileStoreReader
            {
                GetFileContentDelegate = filePath =>
                {
                    var fileContent = fileStoreReader.GetFileContent(filePath);

                    if (fileContent != null)
                    {
                        filesForProcessRestore[filePath] = fileContent;
                    }

                    return fileContent;
                }
            };

            var compositionLogRecords =
                EnumerateCompositionLogRecordsForRestoreProcessAndLoadDependencies(new ProcessStoreReaderInFileStore(recordingReader))
                .ToImmutableList();

            return (
                files: filesForProcessRestore.ToImmutableDictionary(EnumerableExtension.EqualityComparer<string>()),
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

        static public PersistentProcessVolatileRepresentation LoadFromStoreAndRestoreProcess(
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

                return new PersistentProcessVolatileRepresentation(
                    lastCompositionLogRecordHashBase16: CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16,
                    lastAppConfig: null,
                    lastElmAppVolatileProcess: null,
                    lastSetElmAppStateResult: null);
            }

            logger?.Invoke("Found " + compositionEventsFromLatestReduction.Count + " composition log records to use for restore.");

            var processVolatileRepresentation = RestoreFromCompositionEventSequence(
                compositionEventsFromLatestReduction,
                overrideElmAppInterfaceConfig);

            logger?.Invoke("Restored the process state in " + ((int)restoreStopwatch.Elapsed.TotalSeconds) + " seconds.");

            return processVolatileRepresentation;
        }

        static public PersistentProcessVolatileRepresentation RestoreFromCompositionEventSequence(
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

            var processRepresentationDuringRestore = new PersistentProcessVolatileRepresentationDuringRestore(
                lastAppConfig: null,
                lastElmAppVolatileProcess: null,
                lastSetElmAppStateResult: null);

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

                        newElmAppProcess.SetSerializedState(elmAppStateAsString);

                        processRepresentationDuringRestore?.lastElmAppVolatileProcess?.Dispose();

                        processRepresentationDuringRestore = new PersistentProcessVolatileRepresentationDuringRestore(
                            lastAppConfig: (compositionLogRecord.reduction.Value.appConfig, (javascriptFromElmMake, javascriptPreparedToRun)),
                            lastElmAppVolatileProcess: newElmAppProcess,
                            lastSetElmAppStateResult: null);

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
                            compositionLogRecord.composition.Value,
                            processRepresentationDuringRestore,
                            overrideElmAppInterfaceConfig);
                }
                finally
                {
                    lastCompositionLogRecordHashBase16 = compositionLogRecord.compositionRecordHashBase16;
                }
            }

            return new PersistentProcessVolatileRepresentation(
                lastCompositionLogRecordHashBase16: lastCompositionLogRecordHashBase16,
                lastAppConfig: processRepresentationDuringRestore.lastAppConfig,
                lastElmAppVolatileProcess: processRepresentationDuringRestore.lastElmAppVolatileProcess,
                lastSetElmAppStateResult: processRepresentationDuringRestore.lastSetElmAppStateResult);
        }

        class PersistentProcessVolatileRepresentationDuringRestore
        {
            public readonly (Composition.Component appConfigComponent, (string javascriptFromElmMake, string javascriptPreparedToRun) buildArtifacts)? lastAppConfig;

            public readonly IDisposableProcessWithStringInterface lastElmAppVolatileProcess;

            public readonly Result<string, string> lastSetElmAppStateResult;

            public PersistentProcessVolatileRepresentationDuringRestore(
                (Composition.Component appConfigComponent, (string javascriptFromElmMake, string javascriptPreparedToRun) buildArtifacts)? lastAppConfig,
                IDisposableProcessWithStringInterface lastElmAppVolatileProcess,
                Result<string, string> lastSetElmAppStateResult)
            {
                this.lastAppConfig = lastAppConfig;
                this.lastElmAppVolatileProcess = lastElmAppVolatileProcess;
                this.lastSetElmAppStateResult = lastSetElmAppStateResult;
            }

            public PersistentProcessVolatileRepresentationDuringRestore WithLastSetElmAppStateResult(
                Result<string, string> lastSetElmAppStateResult) =>
                new PersistentProcessVolatileRepresentationDuringRestore(
                    lastAppConfig: lastAppConfig,
                    lastElmAppVolatileProcess: lastElmAppVolatileProcess,
                    lastSetElmAppStateResult: lastSetElmAppStateResult);
        }

        static PersistentProcessVolatileRepresentationDuringRestore ApplyCompositionEvent(
            CompositionEventWithLoadedDependencies compositionEvent,
            PersistentProcessVolatileRepresentationDuringRestore processBefore,
            ElmAppInterfaceConfig? overrideElmAppInterfaceConfig)
        {
            if (compositionEvent.UpdateElmAppStateForEvent != null)
            {
                if (processBefore.lastElmAppVolatileProcess == null)
                    return processBefore;

                processBefore.lastElmAppVolatileProcess.ProcessEvent(
                    Encoding.UTF8.GetString(compositionEvent.UpdateElmAppStateForEvent));

                return processBefore;
            }

            if (compositionEvent.SetElmAppState != null)
            {
                if (processBefore.lastElmAppVolatileProcess == null)
                {
                    return
                        processBefore
                        .WithLastSetElmAppStateResult(new Result<string, string>
                        {
                            Err = "Failed to load the serialized state with the elm app: Looks like no app was deployed so far."
                        });
                }

                var projectedElmAppState =
                    Encoding.UTF8.GetString(compositionEvent.SetElmAppState);

                processBefore.lastElmAppVolatileProcess.SetSerializedState(projectedElmAppState);

                var resultingElmAppState = processBefore.lastElmAppVolatileProcess.GetSerializedState();

                var lastSetElmAppStateResult =
                    ApplyCommonFormattingToJson(resultingElmAppState) == ApplyCommonFormattingToJson(projectedElmAppState)
                    ?
                    new Result<string, string>
                    {
                        Ok = "Successfully loaded the elm app state.",
                    }
                    :
                    new Result<string, string>
                    {
                        Err = "Failed to load the serialized state with the elm app. resulting State:\n" + resultingElmAppState
                    };

                return
                    processBefore
                    .WithLastSetElmAppStateResult(lastSetElmAppStateResult);
            }

            if (compositionEvent.DeployAppConfigAndMigrateElmAppState != null)
            {
                var elmAppStateBefore = processBefore.lastElmAppVolatileProcess?.GetSerializedState();

                var appConfig = compositionEvent.DeployAppConfigAndMigrateElmAppState;

                var prepareMigrateResult =
                    PrepareMigrateSerializedValue(destinationAppConfigTree: appConfig);

                var (newElmAppProcess, buildArtifacts) =
                    ProcessFromWebAppConfig(
                        appConfig,
                        overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

                string migratedSerializedState = null;
                Result<string, string> setElmAppStateResult = null;

                if (prepareMigrateResult.Ok == null)
                {
                    setElmAppStateResult = new Result<string, string> { Err = "Failed to prepare elm app state migration: " + prepareMigrateResult.Err };
                }
                else
                {
                    var migrateResult = prepareMigrateResult.Ok(elmAppStateBefore);

                    //  TODO: Simplify: Remove the check currently in PrepareMigrateSerializedValue and assign anyway.

                    if (migrateResult.Ok == null)
                    {
                        setElmAppStateResult = new Result<string, string> { Err = "Failed to migrate the elm app state:" + migrateResult.Err };
                    }
                    else
                    {
                        migratedSerializedState = migrateResult.Ok;
                        setElmAppStateResult = new Result<string, string> { Ok = "Successfully migrated the elm app state." };
                    }
                }

                newElmAppProcess.SetSerializedState(migratedSerializedState ?? "");

                processBefore.lastElmAppVolatileProcess?.Dispose();

                return new PersistentProcessVolatileRepresentationDuringRestore(
                    lastAppConfig: (Composition.FromTreeWithStringPath(appConfig), buildArtifacts),
                    lastElmAppVolatileProcess: newElmAppProcess,
                    lastSetElmAppStateResult: setElmAppStateResult);
            }

            if (compositionEvent.DeployAppConfigAndInitElmAppState != null)
            {
                var appConfig = compositionEvent.DeployAppConfigAndInitElmAppState;

                var (newElmAppProcess, buildArtifacts) =
                    ProcessFromWebAppConfig(
                        appConfig,
                        overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

                processBefore.lastElmAppVolatileProcess?.Dispose();

                return new PersistentProcessVolatileRepresentationDuringRestore(
                    lastAppConfig: (Composition.FromTreeWithStringPath(appConfig), buildArtifacts),
                    lastElmAppVolatileProcess: newElmAppProcess,
                    lastSetElmAppStateResult: null);
            }

            throw new Exception("Unexpected shape of composition event: " + JsonConvert.SerializeObject(compositionEvent));
        }

        static CompositionEventWithLoadedDependencies? LoadCompositionEventDependencies(
            CompositionLogRecordInFile.CompositionEvent compositionEvent,
            IProcessStoreReader storeReader)
        {
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
                    UpdateElmAppStateForEvent = loadComponentFromStoreAndAssertIsBlob(
                        compositionEvent.UpdateElmAppStateForEvent.HashBase16).ToArray(),
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

        public class Result<ErrT, OkT>
        {
            public ErrT Err;

            public OkT Ok;
        }

        static public Composition.Result<string, (string parentHashBase16, IEnumerable<(IImmutableList<string> filePath, IReadOnlyList<byte> fileContent)> projectedFiles, IFileStoreReader projectedReader)>
            TestContinueWithCompositionEvent(
                CompositionLogRecordInFile.CompositionEvent compositionLogEvent,
                IFileStoreReader fileStoreReader)
        {
            var projectionResult = IProcessStoreReader.ProjectFileStoreReaderForAppendedCompositionLogEvent(
                originalFileStore: fileStoreReader,
                compositionLogEvent: compositionLogEvent);

            using (var projectedProcess =
                LoadFromStoreAndRestoreProcess(new ProcessStoreReaderInFileStore(projectionResult.projectedReader), _ => { }))
            {
                if (compositionLogEvent.DeployAppConfigAndMigrateElmAppState != null ||
                    compositionLogEvent.SetElmAppState != null)
                {
                    if (projectedProcess.lastSetElmAppStateResult?.Ok == null)
                    {
                        return Composition.Result<string, (string parentHashBase16, IEnumerable<(IImmutableList<string> filePath, IReadOnlyList<byte> fileContent)> projectedFiles, IFileStoreReader projectedReader)>.err(
                            "Failed to migrate Elm app state for this deployment: " + projectedProcess.lastSetElmAppStateResult?.Err);
                    }
                }
            }

            return Composition.Result<string, (string parentHashBase16, IEnumerable<(IImmutableList<string> filePath, IReadOnlyList<byte> fileContent)> projectedFiles, IFileStoreReader projectedReader)>.ok(
                projectionResult);
        }

        static Result<string, Func<string, Result<string, string>>> PrepareMigrateSerializedValue(
            Composition.TreeWithStringPath destinationAppConfigTree)
        {
            var appConfigFiles =
                Composition.TreeToFlatDictionaryWithPathComparer(destinationAppConfigTree);

            var pathToInterfaceModuleFile = ElmApp.FilePathFromModuleName(MigrationElmAppInterfaceModuleName);
            var pathToCompilationRootModuleFile = ElmApp.FilePathFromModuleName(MigrationElmAppCompilationRootModuleName);

            if (!appConfigFiles.TryGetValue(pathToInterfaceModuleFile, out var _) &&
                !appConfigFiles.TryGetValue(ElmApp.FilePathFromModuleName(Old_MigrationElmAppInterfaceModuleName), out var _))
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Did not find interface module at '" + string.Join("/", pathToInterfaceModuleFile) + "'",
                };

            try
            {
                var (migrateElmAppFiles, _) = ElmApp.AsCompletelyLoweredElmApp(
                    sourceFiles: appConfigFiles,
                    interfaceConfig: ElmAppInterfaceConfig.Default);

                var javascriptFromElmMake = ElmFullstack.ProcessFromElm019Code.CompileElmToJavascript(
                    migrateElmAppFiles,
                    pathToCompilationRootModuleFile);

                var javascriptMinusCrashes = ElmFullstack.ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

                var javascriptToRun =
                    ElmFullstack.ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                        javascriptMinusCrashes,
                        new[]
                        {
                            (functionNameInElm: MigrationElmAppCompilationRootModuleName + ".decodeMigrateAndEncodeAndSerializeResult",
                            publicName: "interface_migrate",
                            arity: 1),
                        });

                var attemptMigrateFunc = new Func<string, Result<string, string>>(elmAppStateBeforeSerialized =>
                {
                    string migrateResultString = null;

                    using (var javascriptEngine = ProcessHostedWithV8.ConstructJsEngine())
                    {
                        var initAppResult = javascriptEngine.Evaluate(javascriptToRun);

                        var migrateResult = javascriptEngine.CallFunction(
                            "interface_migrate", elmAppStateBeforeSerialized);

                        migrateResultString = migrateResult.ToString();
                    }

                    var migrateResultStructure =
                        JsonConvert.DeserializeObject<ElmFullstack.ElmValueCommonJson.Result<string, string>>(migrateResultString);

                    var elmAppStateMigratedSerialized = migrateResultStructure?.Ok?.FirstOrDefault();

                    if (elmAppStateMigratedSerialized == null)
                    {
                        return new Result<string, string>
                        {
                            Err = "Decoding of serialized state failed for this migration:\n" + migrateResultStructure?.Err?.FirstOrDefault()
                        };
                    }

                    using (var testProcess = ProcessFromWebAppConfig(
                        appConfig: destinationAppConfigTree,
                        overrideElmAppInterfaceConfig: null).process)
                    {
                        testProcess.SetSerializedState(elmAppStateMigratedSerialized);

                        var resultingState = testProcess.GetSerializedState();

                        if (ApplyCommonFormattingToJson(resultingState) != ApplyCommonFormattingToJson(elmAppStateMigratedSerialized))
                            return new Result<string, string>
                            {
                                Err = "Failed to load the migrated serialized state with the destination public app configuration. resulting State:\n" + resultingState
                            };
                    }

                    return new Result<string, string>
                    {
                        Ok = elmAppStateMigratedSerialized
                    };
                });

                return new Result<string, Func<string, Result<string, string>>>
                {
                    Ok = attemptMigrateFunc
                };
            }
            catch (Exception e)
            {
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Failed with exception:\n" + e.ToString()
                };
            }
        }

        public string ProcessElmAppEvent(IProcessStoreWriter storeWriter, string serializedEvent)
        {
            lock (processLock)
            {
                var eventElmAppEventComponent = Composition.Component.Blob(Encoding.UTF8.GetBytes(serializedEvent));

                storeWriter.StoreComponent(eventElmAppEventComponent);

                var elmAppResponse =
                    lastElmAppVolatileProcess.ProcessEvent(serializedEvent);

                var compositionRecord = new CompositionLogRecordInFile
                {
                    parentHashBase16 = lastCompositionLogRecordHashBase16,
                    compositionEvent =
                        new CompositionLogRecordInFile.CompositionEvent
                        {
                            UpdateElmAppStateForEvent = new ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(eventElmAppEventComponent))
                            }
                        },
                };

                var serializedCompositionLogRecord =
                    ProcessStoreInFileStore.Serialize(compositionRecord);

                var recordHash = storeWriter.SetCompositionLogHeadRecord(serializedCompositionLogRecord);

                lastCompositionLogRecordHashBase16 = recordHash.recordHashBase16;

                return elmAppResponse;
            }
        }

        static string ApplyCommonFormattingToJson(string originalJson)
        {
            try
            {
                return JsonConvert.SerializeObject(JsonConvert.DeserializeObject(originalJson));
            }
            catch
            {
                return originalJson;
            }
        }

        public void Dispose() => lastElmAppVolatileProcess?.Dispose();

        public (ProvisionalReductionRecordInFile reductionRecord, StoreProvisionalReductionReport report) StoreReductionRecordForCurrentState(
            IProcessStoreWriter storeWriter)
        {
            var report = new StoreProvisionalReductionReport();

            string elmAppState = null;

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
}
