using ElmTime.ElmInteractive;
using ElmTime.ElmSyntax;
using Pine.Core;
using Pine.Core.PineVM;
using Pine.ElmInteractive;
using Pine.PineVM;
using System.Collections.Generic;
using System.Linq;

namespace ElmTime;

/// <summary>
/// Json adapter implements a bridge from the JSON-based interface to Elm backend apps as used from 2018 to 2024.
/// </summary>
public class ElmTimeJsonAdapter
{
    public static IReadOnlyList<string> RootFilePath =>
        ["src", "Backend", "InterfaceToHost_Root.elm"];

    public const string RootModuleName = "Backend.InterfaceToHost_Root";

    public record Parsed(
        /*
         * jsonEncodeAppState : Backend.Main.State -> Json.Encode.Value
         * */
        ElmInteractiveEnvironment.FunctionRecord JsonEncodeAppState,
        /*
         * jsonDecodeAppState : Json.Decode.Decoder Backend.Main.State
         * */
        PineValue JsonDecodeAppState,
        /*
         * The Elm app compiler emits the 'jsonDecodeMigratePreviousState' declaration only if the Elm app has a migration module.
         * 
         * jsonDecodeMigratePreviousState : Json.Decode.Decoder prevState
         * */
        PineValue? JsonDecodeMigratePreviousState,
        /*
         * Backend.MigrateState.migrate : PreviousBackendState -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
         * */
        ElmInteractiveEnvironment.FunctionRecord? Migrate,
        /*
         * https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#decodeValue
         * Decoder a -> Value -> Result Error a
         * */
        ElmInteractiveEnvironment.FunctionRecord JsonDecodeDecodeValue,
        /*
         * https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#decodeString
         * decodeString : Decoder a -> String -> Result Error a
         * */
        ElmInteractiveEnvironment.FunctionRecord JsonDecodeDecodeString)
    {
        public static Result<string, Parsed> ParseFromCompiled(
            PineValue compiledApp,
            PineVMParseCache parseCache)
        {
            var parseEnvResult =
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledApp);

            {
                if (parseEnvResult.IsErrOrNull() is { } err)
                {
                    return err;
                }
            }

            if (parseEnvResult.IsOkOrNull() is not { } parseEnvOk)
            {
                throw new System.Exception(
                    "Unexpected null parseEnvResult: " + parseEnvResult);
            }

            var moduleRoot =
                parseEnvOk.Modules
                .FirstOrDefault(module => module.moduleName is RootModuleName);

            if (moduleRoot.moduleValue is null)
            {
                return
                    "Root module " + RootModuleName + " not found among " +
                    parseEnvOk.Modules.Count + " modules: " +
                    string.Join(", ", parseEnvOk.Modules.Select(module => module.moduleName));
            }

            /*
            jsonDecodeAppState : Json.Decode.Decoder Backend.Main.State
            jsonDecodeAppState =
                Backend.InterfaceToHost_Root.Generated_JsonConverters.jsonDecode_1615808600


            jsonDecodeMigratePreviousState : Json.Decode.Decoder Backend.MigrateState.PreviousBackendState
            jsonDecodeMigratePreviousState =
                Backend.InterfaceToHost_Root.Generated_JsonConverters.jsonDecode_4168357374


            jsonEncodeAppState : Backend.Main.State -> Json.Encode.Value
            jsonEncodeAppState =
                Backend.InterfaceToHost_Root.Generated_JsonConverters.jsonEncode_1615808600

             * */

            moduleRoot.moduleContent.FunctionDeclarations.TryGetValue(
                "jsonEncodeAppState",
                out var jsonEncodeAppStateValue);

            moduleRoot.moduleContent.FunctionDeclarations.TryGetValue(
                "jsonDecodeAppState",
                out var jsonDecodeAppStateValue);

            moduleRoot.moduleContent.FunctionDeclarations.TryGetValue(
                "jsonDecodeMigratePreviousState",
                out var jsonDecodeMigratePreviousStateValue);

            if (jsonEncodeAppStateValue is null)
            {
                return "Function 'jsonEncodeAppState' not found in root module " + RootModuleName;
            }

            if (jsonDecodeAppStateValue is null)
            {
                return "Function 'jsonDecodeAppState' not found in root module " + RootModuleName;
            }

            var parseJsonEncodeAppStateResult =
                ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                    jsonEncodeAppStateValue,
                    parseCache);

            {
                if (parseJsonEncodeAppStateResult.IsErrOrNull() is { } err)
                {
                    return
                        "Failed to parse 'jsonEncodeAppState' function: " + err;
                }
            }

            if (parseJsonEncodeAppStateResult.IsOkOrNull() is not { } parseJsonEncodeAppStateOk)
            {
                throw new System.Exception(
                    "Unexpected parseJsonEncodeAppStateResult: " + parseJsonEncodeAppStateResult);
            }

            var moduleJsonDecode =
                parseEnvOk.Modules
                .FirstOrDefault(module => module.moduleName is "Json.Decode");

            if (moduleJsonDecode.moduleValue is null)
            {
                return
                    "Module 'Json.Decode' not found among " +
                    parseEnvOk.Modules.Count + " modules: " +
                    string.Join(", ", parseEnvOk.Modules.Select(module => module.moduleName));
            }

            moduleJsonDecode.moduleContent.FunctionDeclarations.TryGetValue(
                "decodeValue",
                out var decodeValueFunctionValue);

            if (decodeValueFunctionValue is null)
            {
                return "Function 'decodeValue' not found in module 'Json.Decode'";
            }

            var parseDecodeValueFunctionResult =
                ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                    decodeValueFunctionValue,
                    parseCache);
            {
                if (parseDecodeValueFunctionResult.IsErrOrNull() is { } err)
                {
                    return
                        "Failed to parse 'decodeValue' function: " + err;
                }
            }

            if (parseDecodeValueFunctionResult.IsOkOrNull() is not { } parseDecodeValueFunctionOk)
            {
                throw new System.Exception(
                    "Unexpected parseDecodeValueFunctionResult: " + parseDecodeValueFunctionResult);
            }

            moduleJsonDecode.moduleContent.FunctionDeclarations.TryGetValue(
                "decodeString",
                out var decodeStringFunctionValue);

            if (decodeStringFunctionValue is null)
            {
                return "Function 'decodeString' not found in module 'Json.Decode'";
            }

            var parseDecodeStringFunctionResult =
                ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                    decodeStringFunctionValue,
                    parseCache);

            {
                if (parseDecodeStringFunctionResult.IsErrOrNull() is { } err)
                {
                    return
                        "Failed to parse 'decodeString' function: " + err;
                }
            }

            if (parseDecodeStringFunctionResult.IsOkOrNull() is not { } parseDecodeStringFunctionOk)
            {
                throw new System.Exception(
                    "Unexpected parseDecodeStringFunctionResult: " + parseDecodeStringFunctionResult);
            }


            PineValue? migrateFunctionValue = null;

            var migrateStateModule =
                parseEnvOk.Modules
                .FirstOrDefault(module => module.moduleName is "Backend.MigrateState");

            if (migrateStateModule.moduleValue is not null)
            {
                migrateStateModule.moduleContent.FunctionDeclarations.TryGetValue(
                    "migrate",
                    out migrateFunctionValue);
            }

            ElmInteractiveEnvironment.FunctionRecord? migrateFunctionRecord =
                migrateFunctionValue is null
                ?
                null
                :
                ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                    migrateFunctionValue,
                    parseCache)
                .Extract(err => throw new System.Exception("Failed to parse migrate function: " + err));

            return
                new Parsed(
                    JsonEncodeAppState: parseJsonEncodeAppStateOk,
                    JsonDecodeAppState: jsonDecodeAppStateValue,
                    JsonDecodeMigratePreviousState: jsonDecodeMigratePreviousStateValue,
                    Migrate: migrateFunctionRecord,
                    JsonDecodeDecodeValue: parseDecodeValueFunctionOk,
                    JsonDecodeDecodeString: parseDecodeStringFunctionOk);
        }

        public Result<string, PineValue> EncodeAppStateAsJsonValue(
            PineValue appState,
            IPineVM pineVM)
        {
            return ElmInteractiveEnvironment.ApplyFunction(pineVM, JsonEncodeAppState, [appState]);
        }

        public Result<string, PineValue> DecodeAppStateFromJsonValue(
            PineValue jsonValue,
            IPineVM pineVM)
        {
            var jsonDecodeApplyFunctionResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    JsonDecodeDecodeValue,
                    [JsonDecodeAppState, jsonValue]);

            {
                if (jsonDecodeApplyFunctionResult.IsErrOrNull() is { } err)
                {
                    return err;
                }
            }

            if (jsonDecodeApplyFunctionResult.IsOkOrNull() is not { } jsonDecodeApplyFunctionOk)
            {
                throw new System.Exception(
                    "Unexpected jsonDecodeApplyFunctionResult: " + jsonDecodeApplyFunctionResult);
            }

            return
                ElmValueInterop.ParseElmResultValue(
                    jsonDecodeApplyFunctionOk,
                    err => "Failed to decode JSON value: " + err,
                    Result<string, PineValue>.ok,
                    invalid:
                    err => throw new System.Exception("Invalid: " + err));
        }

        public Result<string, PineValue> DecodeAppStateFromJsonString(
            PineValue jsonString,
            IPineVM pineVM)
        {
            var jsonDecodeApplyFunctionResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    JsonDecodeDecodeString,
                    [JsonDecodeAppState, jsonString]);

            {
                if (jsonDecodeApplyFunctionResult.IsErrOrNull() is { } err)
                {
                    return err;
                }
            }

            if (jsonDecodeApplyFunctionResult.IsOkOrNull() is not { } jsonDecodeApplyFunctionOk)
            {
                throw new System.Exception(
                    "Unexpected jsonDecodeApplyFunctionResult: " + jsonDecodeApplyFunctionResult);
            }

            return
                ElmValueInterop.ParseElmResultValue(
                    jsonDecodeApplyFunctionOk,
                    err => "Failed to decode JSON value: " + err,
                    Result<string, PineValue>.ok,
                    invalid:
                    err => throw new System.Exception("Invalid: " + err));
        }

        public Result<string, PineValue> DecodePreviousAppStateFromJsonValue(
            PineValue jsonValue,
            IPineVM pineVM)
        {
            if (JsonDecodeMigratePreviousState is null)
            {
                return "No migration function available";
            }

            var jsonDecodeApplyFunctionResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    JsonDecodeDecodeValue,
                    [JsonDecodeMigratePreviousState, jsonValue]);

            {
                if (jsonDecodeApplyFunctionResult.IsErrOrNull() is { } err)
                {
                    return err;
                }
            }

            if (jsonDecodeApplyFunctionResult.IsOkOrNull() is not { } jsonDecodeApplyFunctionOk)
            {
                throw new System.Exception(
                    "Unexpected jsonDecodeApplyFunctionResult: " + jsonDecodeApplyFunctionResult);
            }

            return
                ElmValueInterop.ParseElmResultValue(
                    jsonDecodeApplyFunctionOk,
                    err => "Failed to decode JSON value: " + err,
                    Result<string, PineValue>.ok,
                    invalid:
                    err => throw new System.Exception("Invalid: " + err));
        }
    }

    /// <summary>
    /// The original lowering implementation added a 'main' declaration to account for DCE.
    /// </summary>
    public static TreeNodeWithStringPath CleanUpFromLoweredForJavaScript(
        TreeNodeWithStringPath loweredForJavaScript)
    {
        /*
        {-| Support function-level dead code elimination (<https://elm-lang.org/blog/small-assets-without-the-headache>) Elm code needed to inform the Elm compiler about our entry points.
        -}
        main : Program Int () String
        main =
            Platform.worker
                { init = always ( (), Cmd.none )
                , update =
                    { a = interfaceToHost_processEvent
                    , b = interfaceToHost_initState
                    }
                        |> always ( (), Cmd.none )
                        |> always
                        |> always
                , subscriptions = always Sub.none
                }
         * */

        var rootFile =
            loweredForJavaScript.GetNodeAtPath(RootFilePath);

        if (rootFile is not TreeNodeWithStringPath.BlobNode rootFileNode)
        {
            throw new System.Exception("Root file not found at " + string.Join("/", RootFilePath));
        }

        var rootFileText =
            System.Text.Encoding.UTF8.GetString(rootFileNode.Bytes.Span);

        var inMainDeclaration = false;

        IEnumerable<string> linesFiltered()
        {
            foreach (var line in rootFileText.ModuleLines())
            {
                if (line.StartsWith("main "))
                {
                    inMainDeclaration = true;
                }

                if (line.Trim().Length is 0)
                {
                    inMainDeclaration = false;
                }

                if (!inMainDeclaration)
                {
                    yield return line;
                }
            }
        }

        var newRootFileText = string.Join("\n", linesFiltered());

        return
            loweredForJavaScript
            .SetNodeAtPathSorted(
                RootFilePath,
                new TreeNodeWithStringPath.BlobNode(
                    System.Text.Encoding.UTF8.GetBytes(newRootFileText)));
    }
}
