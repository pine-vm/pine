using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations;
using Newtonsoft.Json;

namespace Kalmit.PersistentProcess.WebHost.PersistentProcess
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
        static public string MigrationElmAppInterfaceModuleName => "MigrateBackendState";

        static public string MigrationElmAppCompilationRootModuleName => MigrationElmAppInterfaceModuleName + "Root";

        static public string MigrateElmFunctionNameInModule => "migrate";

        readonly object processLock = new object();

        string lastCompositionLogRecordHashBase16;

        public readonly (Composition.Component appConfigComponent, (string javascriptFromElmMake, string javascriptPreparedToRun) buildArtifacts)? lastAppConfig;

        readonly IDisposableProcessWithStringInterface lastElmAppVolatileProcess;

        public readonly Result<string, string> lastSetElmAppStateResult;

        class LoadedReduction
        {
            public byte[] elmAppState;

            public Composition.Component appConfig;

            public Composition.TreeWithStringPath appConfigAsTree;
        }

        static public (IDisposableProcessWithStringInterface process,
            (string javascriptFromElmMake, string javascriptPreparedToRun) buildArtifacts,
            IReadOnlyList<string> log)
            ProcessFromWebAppConfig(
            Composition.TreeWithStringPath appConfig,
            ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
        {
            var log = new List<string>();

            var sourceFiles = TreeToFlatDictionaryWithPathComparer(appConfig);

            var loweredAppFiles = ElmApp.AsCompletelyLoweredElmApp(
                sourceFiles: sourceFiles,
                ElmAppInterfaceConfig.Default,
                log.Add);

            var processFromLoweredElmApp =
                ProcessFromElm019Code.ProcessFromElmCodeFiles(
                loweredAppFiles,
                overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

            return (processFromLoweredElmApp.process, processFromLoweredElmApp.buildArtifacts, log);
        }

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> TreeToFlatDictionaryWithPathComparer(
            Composition.TreeWithStringPath tree)
        {
            return
                ElmApp.ToFlatDictionaryWithPathComparer(tree.EnumerateBlobsTransitive());
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

        static public (IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> files, string lastCompositionLogRecordHashBase16)
            GetFilesForRestoreProcess(
            IFileStoreReader fileStoreReader)
        {
            var filesForProcessRestore = new ConcurrentDictionary<IImmutableList<string>, IImmutableList<byte>>(EnumerableExtension.EqualityComparer<string>());

            var recordingReader = new Kalmit.DelegatingFileStoreReader
            {
                GetFileContentDelegate = filePath =>
                {
                    var fileContent = fileStoreReader.GetFileContent(filePath);

                    if (fileContent != null)
                    {
                        filesForProcessRestore[filePath] = fileContent.ToImmutableList();
                    }

                    return fileContent;
                }
            };

            /*
            Following part can be made less expensive when we have an implementation that evaluates all readings but skips the
            rest of the restoring. Something like an extension of `EnumerateCompositionLogRecordsForRestoreProcess` resolving all
            dependencies on the store.
            */
            using (var restoredProcess = Restore(new ProcessStoreReaderInFileStore(recordingReader), _ => { }))
            {
                return (
                    files: filesForProcessRestore.ToImmutableDictionary(EnumerableExtension.EqualityComparer<string>()),
                    lastCompositionLogRecordHashBase16: restoredProcess.lastCompositionLogRecordHashBase16);
            }
        }

        static IEnumerable<(CompositionLogRecordInFile compositionRecord, string compositionRecordHashBase16, LoadedReduction reduction)>
            EnumerateCompositionLogRecordsForRestoreProcess(IProcessStoreReader storeReader) =>
                storeReader
                .EnumerateSerializedCompositionLogRecordsReverse()
                .Select(serializedCompositionLogRecord =>
                {
                    var compositionRecordHashBase16 =
                        CompositionLogRecordInFile.HashBase16FromCompositionRecord(serializedCompositionLogRecord);

                    var compositionRecord = JsonConvert.DeserializeObject<CompositionLogRecordInFile>(
                        System.Text.Encoding.UTF8.GetString(serializedCompositionLogRecord));

                    var reductionRecord = storeReader.LoadProvisionalReduction(compositionRecordHashBase16);

                    LoadedReduction loadedReduction = null;

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

                            loadedReduction = new LoadedReduction
                            {
                                appConfig = appConfigComponent,
                                appConfigAsTree = parseAppConfigAsTree.Ok,
                                elmAppState = elmAppStateComponent.BlobContent.ToArray(),
                            };
                        }
                    }

                    return (
                        compositionRecord: compositionRecord,
                        compositionRecordHashBase16: compositionRecordHashBase16,
                        reduction: loadedReduction);
                })
                .TakeUntil(compositionAndReduction => compositionAndReduction.reduction != null);

        static public PersistentProcessVolatileRepresentation Restore(
            IProcessStoreReader storeReader,
            Action<string> logger,
            ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
        {
            var restoreStopwatch = System.Diagnostics.Stopwatch.StartNew();

            logger?.Invoke("Begin to restore the process state.");

            var compositionEventsToLatestReductionReversed =
                EnumerateCompositionLogRecordsForRestoreProcess(storeReader)
                .ToImmutableList();

            if (!compositionEventsToLatestReductionReversed.Any())
            {
                logger?.Invoke("Found no composition record, default to initial state.");

                return new PersistentProcessVolatileRepresentation(
                    lastCompositionLogRecordHashBase16: CompositionLogRecordInFile.compositionLogFirstRecordParentHashBase16,
                    lastAppConfig: null,
                    lastElmAppVolatileProcess: null,
                    lastSetElmAppStateResult: null);
            }

            logger?.Invoke("Found " + compositionEventsToLatestReductionReversed.Count + " composition log records to use for restore.");

            var firstCompositionEventRecord =
                compositionEventsToLatestReductionReversed.LastOrDefault();

            if (firstCompositionEventRecord.reduction == null &&
                firstCompositionEventRecord.compositionRecord.parentHashBase16 != CompositionLogRecordInFile.compositionLogFirstRecordParentHashBase16)
            {
                throw new Exception("Failed to get sufficient history: Composition log record points to parent " + firstCompositionEventRecord.compositionRecord.parentHashBase16);
            }

            string lastCompositionLogRecordHashBase16 = null;

            var processRepresentationDuringRestore = new PersistentProcessVolatileRepresentationDuringRestore(
                lastAppConfig: null,
                lastElmAppVolatileProcess: null,
                lastSetElmAppStateResult: null);

            foreach (var compositionLogRecord in compositionEventsToLatestReductionReversed.Reverse())
            {
                try
                {
                    var compositionEvent = compositionLogRecord.compositionRecord.compositionEvent;

                    if (compositionLogRecord.reduction != null)
                    {
                        var (newElmAppProcess, (javascriptFromElmMake, javascriptPreparedToRun), _) =
                            ProcessFromWebAppConfig(
                                compositionLogRecord.reduction.appConfigAsTree,
                                overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

                        var elmAppStateAsString = Encoding.UTF8.GetString(compositionLogRecord.reduction.elmAppState);

                        newElmAppProcess.SetSerializedState(elmAppStateAsString);

                        processRepresentationDuringRestore?.lastElmAppVolatileProcess?.Dispose();

                        processRepresentationDuringRestore = new PersistentProcessVolatileRepresentationDuringRestore(
                            lastAppConfig: (compositionLogRecord.reduction.appConfig, (javascriptFromElmMake, javascriptPreparedToRun)),
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
                            compositionEvent,
                            processRepresentationDuringRestore,
                            storeReader,
                            overrideElmAppInterfaceConfig);
                }
                finally
                {
                    lastCompositionLogRecordHashBase16 = compositionLogRecord.compositionRecordHashBase16;
                }
            }

            logger?.Invoke("Restored the process state in " + ((int)restoreStopwatch.Elapsed.TotalSeconds) + " seconds.");

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
            CompositionLogRecordInFile.CompositionEvent compositionEvent,
            PersistentProcessVolatileRepresentationDuringRestore processBefore,
            IProcessStoreReader storeReader,
            ElmAppInterfaceConfig? overrideElmAppInterfaceConfig)
        {
            IImmutableList<byte> loadComponentFromStoreAndAssertIsBlob(string componentHash)
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
                if (processBefore.lastElmAppVolatileProcess == null)
                    return processBefore;

                processBefore.lastElmAppVolatileProcess.ProcessEvent(
                    Encoding.UTF8.GetString(loadComponentFromStoreAndAssertIsBlob(
                        compositionEvent.UpdateElmAppStateForEvent.HashBase16).ToArray()));

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
                    Encoding.UTF8.GetString(loadComponentFromStoreAndAssertIsBlob(
                        compositionEvent.SetElmAppState.HashBase16).ToArray());

                processBefore.lastElmAppVolatileProcess.SetSerializedState(projectedElmAppState);

                var resultingElmAppState = processBefore.lastElmAppVolatileProcess.GetSerializedState();

                var lastSetElmAppStateResult =
                    applyCommonFormattingToJson(resultingElmAppState) == applyCommonFormattingToJson(projectedElmAppState)
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

                var appConfig = loadComponentFromStoreAndAssertIsTree(
                    compositionEvent.DeployAppConfigAndMigrateElmAppState.HashBase16);

                var prepareMigrateResult =
                    PrepareMigrateSerializedValue(destinationAppConfigTree: appConfig);

                var (newElmAppProcess, buildArtifacts, _) =
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
                var appConfig = loadComponentFromStoreAndAssertIsTree(
                    compositionEvent.DeployAppConfigAndInitElmAppState.HashBase16);

                var (newElmAppProcess, buildArtifacts, _) =
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

        public class Result<ErrT, OkT>
        {
            public ErrT Err;

            public OkT Ok;
        }

        static public Composition.Result<string, (string parentHashBase16, IEnumerable<(IImmutableList<string> filePath, byte[] fileContent)> projectedFiles, IFileStoreReader projectedReader)>
            TestContinueWithCompositionEvent(
                ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent compositionLogEvent,
                IFileStoreReader fileStoreReader)
        {
            var projectionResult = IProcessStoreReader.ProjectFileStoreReaderForAppendedCompositionLogEvent(
                originalFileStore: fileStoreReader,
                compositionLogEvent: compositionLogEvent);

            using (var projectedProcess =
                PersistentProcess.PersistentProcessVolatileRepresentation.Restore(
                    new ProcessStoreReaderInFileStore(projectionResult.projectedReader),
                    _ => { }))
            {
                if (compositionLogEvent.DeployAppConfigAndMigrateElmAppState != null ||
                    compositionLogEvent.SetElmAppState != null)
                {
                    if (projectedProcess.lastSetElmAppStateResult?.Ok == null)
                    {
                        return Composition.Result<string, (string parentHashBase16, IEnumerable<(IImmutableList<string> filePath, byte[] fileContent)> projectedFiles, IFileStoreReader projectedReader)>.err(
                            "Failed to migrate Elm app state for this deployment: " + projectedProcess.lastSetElmAppStateResult?.Err);
                    }
                }
            }

            return Composition.Result<string, (string parentHashBase16, IEnumerable<(IImmutableList<string> filePath, byte[] fileContent)> projectedFiles, IFileStoreReader projectedReader)>.ok(
                projectionResult);
        }

        static Result<string, Func<string, Result<string, string>>> PrepareMigrateSerializedValue(
            Composition.TreeWithStringPath destinationAppConfigTree)
        {
            var appConfigFiles =
                ElmApp.ToFlatDictionaryWithPathComparer(destinationAppConfigTree.EnumerateBlobsTransitive());

            var pathToInterfaceModuleFile = ElmApp.FilePathFromModuleName(MigrationElmAppInterfaceModuleName);
            var pathToCompilationRootModuleFile = ElmApp.FilePathFromModuleName(MigrationElmAppCompilationRootModuleName);

            appConfigFiles.TryGetValue(pathToInterfaceModuleFile, out var migrateElmAppInterfaceModuleOriginalFile);

            if (migrateElmAppInterfaceModuleOriginalFile == null)
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Did not find interface module at '" + string.Join("/", pathToInterfaceModuleFile) + "'",
                };

            var migrateElmAppInterfaceModuleOriginalText =
                Encoding.UTF8.GetString(migrateElmAppInterfaceModuleOriginalFile.ToArray());

            var migrateFunctionTypeAnnotation =
                CompileElm.TypeAnnotationFromFunctionName(MigrateElmFunctionNameInModule, migrateElmAppInterfaceModuleOriginalText);

            if (migrateFunctionTypeAnnotation == null)
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Did not find type annotation for function '" + MigrateElmFunctionNameInModule + "'"
                };

            var typeAnnotationMatch = Regex.Match(migrateFunctionTypeAnnotation, @"^\s*([\w\d_\.]+)\s*->\s*([\w\d_\.]+)\s*$");

            if (!typeAnnotationMatch.Success)
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Type annotation did not match expected pattern: '" + migrateFunctionTypeAnnotation + "'"
                };

            var inputTypeText = typeAnnotationMatch.Groups[1].Value;
            var returnTypeText = typeAnnotationMatch.Groups[2].Value;

            var inputTypeCanonicalName =
                inputTypeText.Contains(".") ?
                inputTypeText :
                MigrationElmAppInterfaceModuleName + "." + inputTypeText;

            var returnTypeCanonicalName =
                returnTypeText.Contains(".") ?
                returnTypeText :
                MigrationElmAppInterfaceModuleName + "." + returnTypeText;

            var compilationRootModuleInitialText = @"
module " + MigrationElmAppCompilationRootModuleName + @" exposing(decodeMigrateAndEncodeAndSerializeResult, main)

import " + MigrationElmAppInterfaceModuleName + @"
import Json.Decode
import Json.Encode


decodeMigrateAndEncode : String -> Result String String
decodeMigrateAndEncode =
    Json.Decode.decodeString jsonDecodeBackendState
        >> Result.map (" + MigrationElmAppInterfaceModuleName + "." + MigrateElmFunctionNameInModule + @" >> jsonEncodeBackendState >> Json.Encode.encode 0)
        >> Result.mapError Json.Decode.errorToString


decodeMigrateAndEncodeAndSerializeResult : String -> String
decodeMigrateAndEncodeAndSerializeResult =
    decodeMigrateAndEncode
        >> jsonEncodeResult Json.Encode.string Json.Encode.string
        >> Json.Encode.encode 0


jsonEncodeResult : (err -> Json.Encode.Value) -> (ok -> Json.Encode.Value) -> Result err ok -> Json.Encode.Value
jsonEncodeResult encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( ""Err"", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( ""Ok"", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object


main : Program Int {} String
main =
    Platform.worker
        { init = \_ -> ( {}, Cmd.none )
        , update =
            \_ _ ->
                ( decodeMigrateAndEncodeAndSerializeResult |> always {}, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
";
            var compileCodingFunctionsLogLines = new System.Collections.Generic.List<string>();

            try
            {
                var migrateElmAppFilesBeforeAddingCodingSupport =
                    appConfigFiles.SetItem(
                        pathToCompilationRootModuleFile,
                        Encoding.UTF8.GetBytes(compilationRootModuleInitialText).ToImmutableList());

                var appFilesWithInputCodingFunctions =
                    ElmApp.WithSupportForCodingElmType(
                        migrateElmAppFilesBeforeAddingCodingSupport,
                        inputTypeCanonicalName,
                        MigrationElmAppCompilationRootModuleName,
                        compileCodingFunctionsLogLines.Add,
                        out var inputTypeFunctionNames);

                var appFilesWithCodingFunctions =
                    ElmApp.WithSupportForCodingElmType(
                        appFilesWithInputCodingFunctions,
                        returnTypeCanonicalName,
                        MigrationElmAppCompilationRootModuleName,
                        compileCodingFunctionsLogLines.Add,
                        out var returnTypeFunctionNames);

                var rootModuleTextWithSupportAdded =
                    Encoding.UTF8.GetString(appFilesWithCodingFunctions[pathToCompilationRootModuleFile].ToArray());

                var rootModuleText =
                    new[]
                    {
                        "jsonDecodeBackendState = " + inputTypeFunctionNames.decodeFunctionName,
                        "jsonEncodeBackendState = " + returnTypeFunctionNames.encodeFunctionName
                    }
                    .Aggregate(rootModuleTextWithSupportAdded, (intermediateModuleText, functionToAdd) =>
                        CompileElm.WithFunctionAdded(intermediateModuleText, functionToAdd));

                var migrateElmAppFiles = appFilesWithCodingFunctions.SetItem(
                    pathToCompilationRootModuleFile,
                    Encoding.UTF8.GetBytes(rootModuleText).ToImmutableList());

                var javascriptFromElmMake = Kalmit.ProcessFromElm019Code.CompileElmToJavascript(
                    migrateElmAppFiles,
                    pathToCompilationRootModuleFile);

                var javascriptMinusCrashes = Kalmit.ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

                var javascriptToRun =
                    Kalmit.ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
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

                    using (var javascriptEngine = ProcessHostedWithChakraCore.ConstructJsEngine())
                    {
                        var initAppResult = javascriptEngine.Evaluate(javascriptToRun);

                        var migrateResult = javascriptEngine.CallFunction(
                            "interface_migrate", elmAppStateBeforeSerialized);

                        migrateResultString = migrateResult.ToString();
                    }

                    var migrateResultStructure =
                        Newtonsoft.Json.JsonConvert.DeserializeObject<Kalmit.ElmValueCommonJson.Result<string, string>>(
                            migrateResultString);

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

                        if (applyCommonFormattingToJson(resultingState) != applyCommonFormattingToJson(elmAppStateMigratedSerialized))
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
                    Err = "Failed with exception:\n" + e.ToString() + "\ncompileCodingFunctionsLogLines:\n" + String.Join("\n", compileCodingFunctionsLogLines)
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

        static string applyCommonFormattingToJson(string originalJson)
        {
            try
            {
                return Newtonsoft.Json.JsonConvert.SerializeObject(Newtonsoft.Json.JsonConvert.DeserializeObject(originalJson));
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

                if (lastCompositionLogRecordHashBase16 == CompositionLogRecordInFile.compositionLogFirstRecordParentHashBase16)
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
