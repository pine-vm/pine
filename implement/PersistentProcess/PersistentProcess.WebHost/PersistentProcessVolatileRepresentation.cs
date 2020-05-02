using System;
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
        IImmutableList<string> ProcessElmAppEvents(IProcessStoreWriter storeWriter, IReadOnlyList<string> serializedEvents);

        ProvisionalReductionRecordInFile StoreReductionRecordForCurrentState(IProcessStoreWriter storeWriter);
    }

    //  A provisional special case for a process from an elm app.
    public class PersistentProcessVolatileRepresentation : IPersistentProcess, IDisposable
    {
        static public string MigrationElmAppInterfaceModuleName => "MigrateBackendState";

        static public string MigrationElmAppCompilationRootModuleName => MigrationElmAppInterfaceModuleName + "Root";

        static public string MigrateElmFunctionNameInModule => "migrate";

        static public string compositionLogEmptyInitHashBase16 =>
            CommonConversion.StringBase16FromByteArray(
                CompositionLogRecordInFile.HashFromSerialRepresentation(new byte[0]));

        readonly object processLock = new object();

        string lastCompositionLogRecordHashBase16;

        public readonly (Composition.Component appConfigComponent, (string javascriptFromElmMake, string javascriptPreparedToRun) buildArtifacts)? lastAppConfig;

        readonly IDisposableProcessWithStringInterface lastElmAppVolatileProcess;

        public readonly Result<string, string> lastSetElmAppStateResult;

        class LoadedReduction
        {
            public byte[] elmAppState;

            public Composition.Component appConfig;

            public Composition.TreeComponent appConfigAsTree;
        }

        static public (IDisposableProcessWithStringInterface process,
            (string javascriptFromElmMake, string javascriptPreparedToRun) buildArtifacts)
            ProcessFromWebAppConfig(
            Composition.TreeComponent appConfig,
            ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
        {
            var elmAppFilesNamesAndContents =
                SubtreeElmAppFromAppConfig(appConfig).EnumerateBlobsTransitive()
                .Select(blobPathAndContent => (
                    fileName: (IImmutableList<string>)blobPathAndContent.path.Select(name => Encoding.UTF8.GetString(name.ToArray())).ToImmutableList(),
                    fileContent: blobPathAndContent.blobContent))
                .ToImmutableList();

            return ProcessFromElm019Code.ProcessFromElmCodeFiles(
                elmAppFilesNamesAndContents,
                overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);
        }

        static Composition.TreeComponent SubtreeElmAppFromAppConfig(Composition.TreeComponent appConfig) =>
            appConfig.TreeContent
            .FirstOrDefault(c => c.name.SequenceEqual(Encoding.UTF8.GetBytes("elm-app"))).component;

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

        static public PersistentProcessVolatileRepresentation Restore(
            IProcessStoreReader storeReader,
            Action<string> logger,
            ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
        {
            var restoreStopwatch = System.Diagnostics.Stopwatch.StartNew();

            logger?.Invoke("Begin to restore the process state.");

            IImmutableList<byte> loadComponentFromStoreAndAssertIsBlob(string componentHash)
            {
                var component = storeReader.LoadComponent(componentHash);

                if (component == null)
                    throw new Exception("Failed to load component " + componentHash + ": Not found in store.");

                if (component.BlobContent == null)
                    throw new Exception("Failed to load component " + componentHash + " as blob: This is not a blob.");

                return component.BlobContent;
            }

            Composition.TreeComponent loadComponentFromStoreAndAssertIsTree(string componentHash)
            {
                var component = storeReader.LoadComponent(componentHash);

                if (component == null)
                    throw new Exception("Failed to load component " + componentHash + ": Not found in store.");

                var parseAsTreeResult = Composition.ParseAsTree(component);

                if (parseAsTreeResult.Ok == null)
                    throw new Exception("Failed to load component " + componentHash + " as tree: Failed to parse as tree.");

                return parseAsTreeResult.Ok;
            }

            var compositionRecords = new Dictionary<string, (string compositionRecordHash, CompositionLogRecordInFile compositionRecord)>();

            var compositionChain = new Stack<(string hash, CompositionLogRecordInFile composition)>();

            foreach (var serializedCompositionRecord in storeReader.EnumerateSerializedCompositionLogRecordsReverse())
            {
                {
                    var compositionRecordFromFile = JsonConvert.DeserializeObject<CompositionLogRecordInFile>(
                        System.Text.Encoding.UTF8.GetString(serializedCompositionRecord));

                    var compositionRecordHash =
                        CommonConversion.StringBase16FromByteArray(
                            CompositionLogRecordInFile.HashFromSerialRepresentation(serializedCompositionRecord));

                    var compositionChainElement = (compositionRecordHash, compositionRecordFromFile);

                    if (!compositionChain.Any())
                        compositionChain.Push(compositionChainElement);
                    else
                        compositionRecords[compositionRecordHash] = compositionChainElement;
                }

                while (true)
                {
                    var (compositionRecordHash, compositionRecord) = compositionChain.Peek();

                    var reductionRecord = storeReader.LoadProvisionalReduction(compositionRecordHash);

                    LoadedReduction loadedReduction = null;

                    if (reductionRecord?.appConfig?.HashBase16 != null && reductionRecord?.elmAppState?.HashBase16 != null)
                    {
                        var appConfigComponent = storeReader.LoadComponent(reductionRecord.appConfig?.HashBase16);

                        var elmAppStateComponent = storeReader.LoadComponent(reductionRecord.elmAppState?.HashBase16);

                        if (appConfigComponent != null && elmAppStateComponent != null)
                        {
                            var parseAppConfigAsTree = Composition.ParseAsTree(appConfigComponent);

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

                    if ((loadedReduction?.appConfig != null && loadedReduction?.elmAppState != null) ||
                        compositionLogEmptyInitHashBase16 == compositionRecord.parentHashBase16)
                    {
                        IDisposableProcessWithStringInterface lastElmAppVolatileProcess = null;
                        (Composition.Component appConfigComponent, (string javascriptFromElmMake, string javascriptPreparedToRun))? lastAppConfig = null;
                        string lastCompositionLogRecordHashBase16 = null;
                        Result<string, string> lastSetElmAppStateResult = null;

                        if (reductionRecord != null)
                        {
                            compositionChain.Pop();

                            var (newElmAppProcess, (javascriptFromElmMake, javascriptPreparedToRun)) =
                                ProcessFromWebAppConfig(
                                    loadedReduction.appConfigAsTree,
                                    overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

                            var elmAppStateAsString = Encoding.UTF8.GetString(loadedReduction.elmAppState);

                            newElmAppProcess.SetSerializedState(elmAppStateAsString);

                            lastElmAppVolatileProcess?.Dispose();
                            lastElmAppVolatileProcess = newElmAppProcess;
                            lastAppConfig = (loadedReduction.appConfig, (javascriptFromElmMake, javascriptPreparedToRun));

                            lastCompositionLogRecordHashBase16 = reductionRecord.reducedCompositionHashBase16;
                        }

                        foreach (var followingCompositionLogRecord in compositionChain)
                        {
                            foreach (var compositionEvent in followingCompositionLogRecord.composition.events)
                            {
                                if (compositionEvent.SetElmAppState != null)
                                {
                                    var projectedElmAppState =
                                        Encoding.UTF8.GetString(loadComponentFromStoreAndAssertIsBlob(
                                            compositionEvent.SetElmAppState.HashBase16).ToArray());

                                    lastElmAppVolatileProcess.SetSerializedState(projectedElmAppState);

                                    var resultingElmAppState = lastElmAppVolatileProcess.GetSerializedState();

                                    lastSetElmAppStateResult =
                                        resultingElmAppState == projectedElmAppState
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
                                }

                                if (compositionEvent.UpdateElmAppStateForEvent != null)
                                {
                                    lastElmAppVolatileProcess.ProcessEvent(
                                        Encoding.UTF8.GetString(loadComponentFromStoreAndAssertIsBlob(
                                            compositionEvent.UpdateElmAppStateForEvent.HashBase16).ToArray()));
                                }

                                if (compositionEvent.DeployAppConfigAndInitElmAppState != null)
                                {
                                    var appConfig = loadComponentFromStoreAndAssertIsTree(
                                        compositionEvent.DeployAppConfigAndInitElmAppState.HashBase16);

                                    var (newElmAppProcess, buildArtifacts) =
                                        ProcessFromWebAppConfig(
                                            appConfig,
                                            overrideElmAppInterfaceConfig: overrideElmAppInterfaceConfig);

                                    lastAppConfig = (Composition.FromTree(appConfig), buildArtifacts);
                                    lastElmAppVolatileProcess?.Dispose();
                                    lastElmAppVolatileProcess = newElmAppProcess;
                                    lastSetElmAppStateResult = null;
                                }

                                if (compositionEvent.DeployAppConfigAndMigrateElmAppState != null)
                                {
                                    var elmAppStateBefore = lastElmAppVolatileProcess?.GetSerializedState();

                                    var appConfig = loadComponentFromStoreAndAssertIsTree(
                                        compositionEvent.DeployAppConfigAndMigrateElmAppState.HashBase16);

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

                                    lastAppConfig = (Composition.FromTree(appConfig), buildArtifacts);
                                    lastElmAppVolatileProcess?.Dispose();
                                    lastElmAppVolatileProcess = newElmAppProcess;
                                    lastSetElmAppStateResult = setElmAppStateResult;
                                }
                            }

                            lastCompositionLogRecordHashBase16 = followingCompositionLogRecord.hash;
                        }

                        logger?.Invoke("Restored the process state in " + ((int)restoreStopwatch.Elapsed.TotalSeconds) + " seconds.");

                        return new PersistentProcessVolatileRepresentation(
                            lastCompositionLogRecordHashBase16: lastCompositionLogRecordHashBase16,
                            lastAppConfig: lastAppConfig,
                            lastElmAppVolatileProcess: lastElmAppVolatileProcess,
                            lastSetElmAppStateResult: lastSetElmAppStateResult);
                    }

                    var parentKey = compositionRecord.parentHashBase16;

                    if (!compositionRecords.TryGetValue(parentKey, out var compositionChainElementFromPool))
                        break;

                    compositionChain.Push(compositionChainElementFromPool);
                    compositionRecords.Remove(parentKey);
                }
            }

            if (compositionChain.Any())
                throw new NotImplementedException(
                    "I did not find a reduction for any composition on the chain to the last composition (" +
                    compositionChain.Last().hash +
                    ").");

            logger?.Invoke("Found no composition record, default to initial state.");

            return new PersistentProcessVolatileRepresentation(
                lastCompositionLogRecordHashBase16: compositionLogEmptyInitHashBase16,
                lastAppConfig: null,
                lastElmAppVolatileProcess: null,
                lastSetElmAppStateResult: null);
        }

        public class Result<ErrT, OkT>
        {
            public ErrT Err;

            public OkT Ok;
        }

        static Result<string, Func<string, Result<string, string>>> PrepareMigrateSerializedValue(
            Composition.TreeComponent destinationAppConfigTree)
        {
            var elmAppTree = SubtreeElmAppFromAppConfig(destinationAppConfigTree);

            var migrateElmAppOriginalFiles =
                ElmApp.ToFlatDictionaryWithPathComparer(
                    elmAppTree.EnumerateBlobsTransitive()
                    .Select(blobPathAndContent => (
                        fileName: (IImmutableList<string>)blobPathAndContent.path.Select(name => Encoding.UTF8.GetString(name.ToArray())).ToImmutableList(),
                        fileContent: blobPathAndContent.blobContent))
                    .ToImmutableList());

            var pathToInterfaceModuleFile = ElmApp.FilePathFromModuleName(MigrationElmAppInterfaceModuleName);
            var pathToCompilationRootModuleFile = ElmApp.FilePathFromModuleName(MigrationElmAppCompilationRootModuleName);

            migrateElmAppOriginalFiles.TryGetValue(pathToInterfaceModuleFile, out var migrateElmAppInterfaceModuleOriginalFile);

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
                    migrateElmAppOriginalFiles.SetItem(
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

                        if (resultingState != elmAppStateMigratedSerialized)
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

        public IImmutableList<string> ProcessElmAppEvents(IProcessStoreWriter storeWriter, IReadOnlyList<string> serializedEvents)
        {
            lock (processLock)
            {
                var elmAppEvents =
                    serializedEvents.Select(serializedEvent =>
                    {
                        var eventComponent = Composition.Component.Blob(Encoding.UTF8.GetBytes(serializedEvent));

                        return new
                        {
                            eventAsString = serializedEvent,
                            eventComponent = eventComponent
                        };
                    }).ToImmutableList();

                var responses =
                    serializedEvents.Select(serializedEvent => lastElmAppVolatileProcess.ProcessEvent(serializedEvent))
                    .ToImmutableList();

                var compositionRecord = new CompositionLogRecordInFile
                {
                    parentHashBase16 = lastCompositionLogRecordHashBase16,
                    events = elmAppEvents.Select(elmAppEvent =>
                        new CompositionLogRecordInFile.CompositionEvent
                        {
                            UpdateElmAppStateForEvent = new ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppEvent.eventComponent))
                            }
                        }).ToImmutableList(),
                };

                var serializedCompositionLogRecord =
                    ProcessStoreInFileStore.Serialize(compositionRecord);

                var compositionLogRecordHash = CompositionLogRecordInFile.HashFromSerialRepresentation(serializedCompositionLogRecord);

                foreach (var elmAppEvent in elmAppEvents)
                {
                    storeWriter.StoreComponent(elmAppEvent.eventComponent);
                }

                storeWriter.AppendSerializedCompositionLogRecord(serializedCompositionLogRecord);

                lastCompositionLogRecordHashBase16 = CommonConversion.StringBase16FromByteArray(compositionLogRecordHash);

                return responses;
            }
        }

        public void Dispose() => lastElmAppVolatileProcess?.Dispose();

        public ProvisionalReductionRecordInFile StoreReductionRecordForCurrentState(IProcessStoreWriter storeWriter)
        {
            lock (processLock)
            {
                var elmAppState = lastElmAppVolatileProcess?.GetSerializedState();

                var elmAppStateBlob =
                    elmAppState == null
                    ?
                    new byte[0]
                    :
                    Encoding.UTF8.GetBytes(elmAppState);

                var elmAppStateComponent = Composition.Component.Blob(elmAppStateBlob);

                var reductionRecord =
                    new ProvisionalReductionRecordInFile
                    {
                        reducedCompositionHashBase16 = lastCompositionLogRecordHashBase16,
                        elmAppState = new ValueInFileStructure
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

                var dependencies =
                    new[] { elmAppStateComponent, lastAppConfig?.appConfigComponent }
                    .Where(c => null != c).ToImmutableList();

                foreach (var dependency in dependencies)
                    storeWriter.StoreComponent(dependency);

                storeWriter.StoreProvisionalReduction(reductionRecord);

                return reductionRecord;
            }
        }
    }
}
