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
        ElmInteractiveEnvironment.FunctionRecord JsonDecodeDecodeString,
        /*
         * https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#encode
         * encode : Int -> Value -> String
         * */
        ElmInteractiveEnvironment.FunctionRecord JsonEncodeEncode)
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

            var moduleJsonEncode =
                parseEnvOk.Modules
                .FirstOrDefault(module => module.moduleName is "Json.Encode");

            if (moduleJsonEncode.moduleValue is null)
            {
                return
                    "Module 'Json.Encode' not found among " +
                    parseEnvOk.Modules.Count + " modules: " +
                    string.Join(", ", parseEnvOk.Modules.Select(module => module.moduleName));
            }

            moduleJsonEncode.moduleContent.FunctionDeclarations.TryGetValue(
                "encode",
                out var encodeFunctionValue);

            if (encodeFunctionValue is null)
            {
                return "Function 'encode' not found in module 'Json.Encode'";
            }

            var parseEncodeFunctionResult =
                ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                    encodeFunctionValue,
                    parseCache);
            {
                if (parseEncodeFunctionResult.IsErrOrNull() is { } err)
                {
                    return
                        "Failed to parse 'encode' function: " + err;
                }
            }

            if (parseEncodeFunctionResult.IsOkOrNull() is not { } parseEncodeFunctionOk)
            {
                throw new System.Exception(
                    "Unexpected parseEncodeFunctionResult: " + parseEncodeFunctionResult);
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
                    JsonDecodeDecodeString: parseDecodeStringFunctionOk,
                    JsonEncodeEncode: parseEncodeFunctionOk);
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
                    err => "Failed to decode JSON value: " + JsonDecodeErrorDisplayText(err),
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
                    err => "Failed to decode JSON value: " + JsonDecodeErrorDisplayText(err),
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
                    err => "Failed to decode JSON value: " + JsonDecodeErrorDisplayText(err),
                    Result<string, PineValue>.ok,
                    invalid:
                    err => throw new System.Exception("Invalid: " + err));
        }

        public Result<string, (PineValue newState, IReadOnlyList<PineValue> cmds)>
            MigratePreviousAppState(
            PineValue previousAppState,
            IPineVM pineVM)
        {
            if (Migrate is null)
            {
                return "No migration function available";
            }

            var applyResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    Migrate,
                    [previousAppState]);
            {
                if (applyResult.IsErrOrNull() is { } err)
                {
                    return "Failed apply migrate function: " + err;
                }
            }

            if (applyResult.IsOkOrNull() is not { } applyOk)
            {
                throw new System.Exception("Unexpected applyResult: " + applyResult);
            }

            if (applyOk is not PineValue.ListValue applyList)
            {
                return "Unexpected apply result: Is not list but: " + applyResult;
            }

            if (applyList.Elements.Length is not 2)
            {
                return "Unexpected apply result: Expected 2 elements but got: " + applyList.Elements.Length;
            }

            var newState = applyList.Elements.Span[0];

            var cmdsValue = applyList.Elements.Span[1];

            if (cmdsValue is not PineValue.ListValue cmdsList)
            {
                return "Unexpected apply result: Commands is not a list but: " + cmdsValue;
            }

            return
                Result<string, (PineValue newState, IReadOnlyList<PineValue> cmds)>
                .ok((newState, cmdsList.Elements.ToArray()));
        }

        public Result<string, string> EncodeAppStateAsJsonString(
            PineValue appState,
            IPineVM pineVM)
        {
            var indentArgument = PineValueAsInteger.ValueFromSignedInteger(0);

            var encodeResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    JsonEncodeEncode,
                    [indentArgument, appState]);

            {
                if (encodeResult.IsErrOrNull() is { } err)
                {
                    return Result<string, string>.err(err);
                }
            }

            if (encodeResult.IsOkOrNull() is not { } encodeOk)
            {
                throw new System.Exception("Unexpected encodeResult: " + encodeResult);
            }

            var decodeStringResult =
                PineValueAsString.StringFromValue(encodeOk);

            {
                if (decodeStringResult.IsErrOrNull() is { } err)
                {
                    return Result<string, string>.err(err);
                }
            }

            if (decodeStringResult.IsOkOrNull() is not { } decodeStringOk)
            {
                throw new System.Exception("Unexpected decodeStringResult: " + decodeStringResult);
            }

            return
                Result<string, string>.ok(decodeStringOk);
        }

        public static string JsonDecodeErrorDisplayText(
            PineValue jsonDecodeErrorValue)
        {
            var decodeResult =
                ElmValueEncoding.PineValueAsElmValue(jsonDecodeErrorValue, null, null);

            if (decodeResult.IsErrOrNull() is { } err)
            {
                return "Failed decoding error value: " + err;
            }

            if (decodeResult.IsOkOrNull() is not { } decodeOk)
            {
                throw new System.Exception("Unexpected decode result: " + decodeResult);
            }

            return ElmValue.RenderAsElmExpression(decodeOk).expressionString;
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
