using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.PineVM;
using Pine.Elm;
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

    public const string RootModuleExposedFunctionsDeclName = "config_exposedFunctions";

    public record ExposedFunction(
        ExposedFunctionDescription Description,
        FunctionRecord Handler);

    public record ExposedFunctionDescription(
        ExposedFunctionDescriptionReturnType ReturnType,
        IReadOnlyList<ExposedFunctionDescriptionParameter> Parameters)
    {
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(ReturnType);

            for (var i = 0; i < Parameters.Count; ++i)
            {
                hashCode.Add(Parameters[i]);
            }

            return hashCode.ToHashCode();
        }

        public virtual bool Equals(ExposedFunctionDescription? other)
        {
            if (other is null)
            {
                return false;
            }

            if (!ReturnType.Equals(other.ReturnType))
            {
                return false;
            }

            if (Parameters.Count != other.Parameters.Count)
            {
                return false;
            }

            for (var i = 0; i < Parameters.Count; ++i)
            {
                if (!Parameters[i].Equals(other.Parameters[i]))
                {
                    return false;
                }
            }

            return true;
        }
    }

    public record ExposedFunctionDescriptionReturnType(
        string SourceCodeText,
        bool ContainsAppStateType);

    public record ExposedFunctionDescriptionParameter(
        string PatternSourceCodeText,
        string TypeSourceCodeText,
        bool TypeIsAppStateType);

    public record Parsed(
        /*
         * jsonEncodeAppState : Backend.Main.State -> Json.Encode.Value
         * */
        FunctionRecord JsonEncodeAppState,
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
        FunctionRecord? Migrate,
        /*
         * https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#decodeValue
         * Decoder a -> Value -> Result Error a
         * */
        FunctionRecord JsonDecodeDecodeValue,
        /*
         * https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#decodeString
         * decodeString : Decoder a -> String -> Result Error a
         * */
        FunctionRecord JsonDecodeDecodeString,
        /*
         * https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#value
         * value : Decoder Value
         * */
        PineValue JsonDecodeValue,
        /*
         * https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#encode
         * encode : Int -> Value -> String
         * */
        FunctionRecord JsonEncodeEncode,
        IReadOnlyList<KeyValuePair<string, ExposedFunction>> ExposedFunctions)
    {
        public static Result<string, Parsed> ParseFromCompiled(
            PineValue compiledApp,
            PineVMParseCache parseCache)
        {
            ElmCompilerCache elmEncodingCache = new();

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
                FunctionRecord.ParseFunctionRecordTagged(
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
                FunctionRecord.ParseFunctionRecordTagged(
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
                FunctionRecord.ParseFunctionRecordTagged(
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

            moduleJsonDecode.moduleContent.FunctionDeclarations.TryGetValue(
                "value",
                out var jsonDecodeValueDecl);

            if (jsonDecodeValueDecl is null)
            {
                return "Declaration 'value' not found in module 'Json.Decode'";
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
                out var jsonEncodeFunctionValue);

            if (jsonEncodeFunctionValue is null)
            {
                return "Function 'encode' not found in module 'Json.Encode'";
            }

            var parseJsonEncodeFunctionResult =
                FunctionRecord.ParseFunctionRecordTagged(
                    jsonEncodeFunctionValue,
                    parseCache);
            {
                if (parseJsonEncodeFunctionResult.IsErrOrNull() is { } err)
                {
                    return
                        "Failed to parse 'encode' function: " + err;
                }
            }

            if (parseJsonEncodeFunctionResult.IsOkOrNull() is not { } parseJsonEncodeFunctionOk)
            {
                throw new System.Exception(
                    "Unexpected parseEncodeFunctionResult: " + parseJsonEncodeFunctionResult);
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

            var migrateFunctionRecord =
                migrateFunctionValue is null
                ?
                null
                :
                FunctionRecord.ParseFunctionRecordTagged(
                    migrateFunctionValue,
                    parseCache)
                .Extract(err => throw new System.Exception("Failed to parse migrate function: " + err));

            var exposedFunctionsDeclDictValue =
                moduleRoot.moduleContent.FunctionDeclarations
                .FirstOrDefault(kvp => kvp.Key == RootModuleExposedFunctionsDeclName)
                .Value;

            if (exposedFunctionsDeclDictValue is null)
            {
                return "Exposed functions declaration not found in root module " + RootModuleName;
            }

            var exposedFunctionsList = Precompiled.DictToListRecursive(exposedFunctionsDeclDictValue);

            var exposedFunctions = new KeyValuePair<string, ExposedFunction>[exposedFunctionsList.Length];

            for (var i = 0; i < exposedFunctionsList.Length; ++i)
            {
                var parseExposedFunctionResult =
                    ParseExposedFunctionDictEntryValue(
                        exposedFunctionsList.Span[i],
                        parseCache,
                        elmEncodingCache);

                {
                    if (parseExposedFunctionResult.IsErrOrNull() is { } err)
                    {
                        return "Failed to parse exposed function: " + err;
                    }
                }

                if (parseExposedFunctionResult.IsOkOrNullable() is not { } parseExposedFunctionOk)
                {
                    throw new System.Exception("Unexpected parseExposedFunctionResult: " + parseExposedFunctionResult);
                }

                exposedFunctions[i] = parseExposedFunctionOk;
            }

            return
                new Parsed(
                    JsonEncodeAppState: parseJsonEncodeAppStateOk,
                    JsonDecodeAppState: jsonDecodeAppStateValue,
                    JsonDecodeMigratePreviousState: jsonDecodeMigratePreviousStateValue,
                    Migrate: migrateFunctionRecord,
                    JsonDecodeDecodeValue: parseDecodeValueFunctionOk,
                    JsonDecodeDecodeString: parseDecodeStringFunctionOk,
                    JsonDecodeValue: jsonDecodeValueDecl,
                    JsonEncodeEncode: parseJsonEncodeFunctionOk,
                    ExposedFunctions: exposedFunctions);
        }

        /*
         * 
        config_exposedFunctions :
            Dict.Dict
                String
                { description :
                    { returnType : { sourceCodeText : String, containsAppStateType : Bool }, parameters : List { patternSourceCodeText : String, typeSourceCodeText : String, typeIsAppStateType : Bool } }
                , handler :
                    Backend.Generated.StateShimTypes.ApplyFunctionArguments
                        (Maybe
                            { nextTaskIndex : Int
                            , posixTimeMilli : Int
                            , createVolatileProcessTasks :
                                Dict.Dict
                                    Backend.Generated.WebServiceShimTypes.TaskId
                                    (Platform.WebService.CreateVolatileProcessResult
                                     -> Backend.Main.State
                                     -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
                                    )
                            , requestToVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId (Platform.WebService.RequestToVolatileProcessResult -> Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State ))
                            , terminateVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId ()
                            , stateLessFramework : Backend.Main.State
                            }
                        )
                    ->
                        Result
                            String
                            ( Maybe
                                { nextTaskIndex : Int
                                , posixTimeMilli : Int
                                , createVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId (Platform.WebService.CreateVolatileProcessResult -> Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State ))
                                , requestToVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId (Platform.WebService.RequestToVolatileProcessResult -> Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State ))
                                , terminateVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId ()
                                , stateLessFramework : Backend.Main.State
                                }
                            , Maybe Json.Encode.Value
                            )
                }
        config_exposedFunctions =
            [...]
         * */

        public static Result<string, KeyValuePair<string, ExposedFunction>>
            ParseExposedFunctionDictEntryValue(
            PineValue dictItemValue,
            PineVMParseCache parseCache,
            ElmCompilerCache elmEncodingCache)
        {
            if (dictItemValue is not PineValue.ListValue dictItem)
            {
                return "Expected list value but got: " + dictItemValue;
            }

            if (dictItem.Items.Length is not 2)
            {
                return "Expected 2 elements but got: " + dictItem.Items.Length;
            }

            var dictItemSpan = dictItem.Items.Span;

            var exposedFunctionNameValue = dictItemSpan[0];

            var exposedFunctionDescriptionAndHandlerValue = dictItemSpan[1];

            var parseNameResult =
                elmEncodingCache.PineValueDecodedAsElmValue(exposedFunctionNameValue);

            {
                if (parseNameResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse exposed function name: " + err;
                }
            }

            if (parseNameResult.IsOkOrNull() is not { } parseNameOk)
            {
                throw new System.Exception("Unexpected parseNameResult: " + parseNameResult);
            }

            if (parseNameOk is not ElmValue.ElmString declName)
            {
                return "Expected string but got: " + parseNameOk;
            }


            var parseDescAndHandlerRecordResult =
                ElmValueEncoding.ParsePineValueAsRecordTagged(exposedFunctionDescriptionAndHandlerValue);

            {
                if (parseDescAndHandlerRecordResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse exposed function description and handler: " + err;
                }
            }

            if (parseDescAndHandlerRecordResult.IsOkOrNull() is not { } parseDescAndHandlerRecordOk)
            {
                throw new System.Exception("Unexpected parseDescAndHandlerRecordResult: " + parseDescAndHandlerRecordResult);
            }

            var exposedFunctionHandlerValue =
                parseDescAndHandlerRecordOk
                .FirstOrDefault(kvp => kvp.fieldName is "handler")
                .fieldValue;

            if (exposedFunctionHandlerValue is null)
            {
                return "Expected 'handler' field in exposed function description and handler record";
            }

            var parseHandlerResult =
                FunctionRecord.ParseFunctionRecordTagged(
                    exposedFunctionHandlerValue,
                    parseCache);
            {
                if (parseHandlerResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse exposed function handler: " + err;
                }
            }

            if (parseHandlerResult.IsOkOrNull() is not { } parseHandlerOk)
            {
                throw new System.Exception("Unexpected parseHandlerResult: " + parseHandlerResult);
            }

            var exposedFunctionDescriptionValue =
                parseDescAndHandlerRecordOk
                .FirstOrDefault(kvp => kvp.fieldName is "description")
                .fieldValue;

            if (exposedFunctionDescriptionValue is null)
            {
                return "Expected 'description' field in exposed function description and handler record";
            }

            var parseDescriptionResult =
                ParseExposedFunctionDescription(
                    exposedFunctionDescriptionValue,
                    elmEncodingCache);

            {
                if (parseDescriptionResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse exposed function description: " + err;
                }
            }

            if (parseDescriptionResult.IsOkOrNull() is not { } parseDescriptionOk)
            {
                throw new System.Exception("Unexpected parseDescriptionResult: " + parseDescriptionResult);
            }

            return
                Result<string, KeyValuePair<string, ExposedFunction>>
                .ok(
                    KeyValuePair.Create(
                        declName.Value,
                        new ExposedFunction(
                            Description: parseDescriptionOk,
                            Handler: parseHandlerOk)));
        }

        public static Result<string, ExposedFunctionDescription> ParseExposedFunctionDescription(
            PineValue exposedFunctionDescriptionValue,
            ElmCompilerCache elmEncodingCache)
        {
            var asElmValueResult =
                elmEncodingCache.PineValueDecodedAsElmValue(exposedFunctionDescriptionValue);

            {
                if (asElmValueResult.IsErrOrNull() is { } err)
                {
                    return "Failed to decode exposed function description as Elm value: " + err;
                }
            }

            if (asElmValueResult.IsOkOrNull() is not { } asElmValueOk)
            {
                throw new System.Exception(
                    "Unexpected asElmValueResult: " + asElmValueResult);
            }

            if (asElmValueOk is not ElmValue.ElmRecord exposedFunctionDescriptionRecord)
            {
                return "Expected record but got: " + asElmValueOk;
            }

            var returnTypeField =
                exposedFunctionDescriptionRecord["returnType"];

            if (returnTypeField is null)
            {
                return "Expected 'returnType' field in exposed function description record";
            }

            if (returnTypeField is not ElmValue.ElmRecord returnTypeRecord)
            {
                return "Expected record in 'returnType' but got: " + returnTypeField;
            }

            var sourceCodeTextField =
                returnTypeRecord["sourceCodeText"];

            if (sourceCodeTextField is null)
            {
                return "Expected 'sourceCodeText' field in exposed function description record";
            }

            var containsAppStateTypeField =
                returnTypeRecord["containsAppStateType"];

            if (containsAppStateTypeField is null)
            {
                return "Expected 'containsAppStateType' field in exposed function description record";
            }

            if (sourceCodeTextField is not ElmValue.ElmString sourceCodeTextString)
            {
                return "Expected string in 'sourceCodeText' but got: " + sourceCodeTextField;
            }

            if (containsAppStateTypeField is not ElmValue.ElmTag containsAppStateTypeTag)
            {
                return "Expected bool in 'containsAppStateType' but got: " + containsAppStateTypeField;
            }

            var parametersField =
                exposedFunctionDescriptionRecord["parameters"];

            if (parametersField is null)
            {
                return "Expected 'parameters' field in exposed function description record";
            }

            if (parametersField is not ElmValue.ElmList parametersList)
            {
                return "Expected list in 'parameters' but got: " + parametersField;
            }

            var parameters = new ExposedFunctionDescriptionParameter[parametersList.Items.Count];

            for (var i = 0; i < parametersList.Items.Count; i++)
            {
                var parseParamResult =
                    ParseExposedFunctionDescriptionParameter(
                        parametersList.Items[i]);

                {
                    if (parseParamResult.IsErrOrNull() is { } err)
                    {
                        return "Failed to parse parameter [" + i + "]: " + err;
                    }
                }

                if (parseParamResult.IsOkOrNull() is not { } parseParamOk)
                {
                    throw new System.Exception("Unexpected parseParamResult: " + parseParamResult);
                }

                parameters[i] = parseParamOk;
            }

            return
                new ExposedFunctionDescription(
                    ReturnType:
                    new ExposedFunctionDescriptionReturnType(
                        SourceCodeText: sourceCodeTextString.Value,
                        ContainsAppStateType: containsAppStateTypeTag.TagName is "True"),
                    Parameters: parameters);
        }

        public static Result<string, ExposedFunctionDescriptionParameter> ParseExposedFunctionDescriptionParameter(
            ElmValue parameterValue)
        {
            if (parameterValue is not ElmValue.ElmRecord parameterRecord)
            {
                return "Expected record but got: " + parameterValue;
            }

            var patternSourceCodeTextField =
                parameterRecord["patternSourceCodeText"];

            if (patternSourceCodeTextField is null)
            {
                return "Expected 'patternSourceCodeText' field in parameter record";
            }

            var typeSourceCodeTextField =
                parameterRecord["typeSourceCodeText"];

            if (typeSourceCodeTextField is null)
            {
                return "Expected 'typeSourceCodeText' field in parameter record";
            }

            var typeIsAppStateTypeField =
                parameterRecord["typeIsAppStateType"];

            if (typeIsAppStateTypeField is null)
            {
                return "Expected 'typeIsAppStateType' field in parameter record";
            }

            if (patternSourceCodeTextField is not ElmValue.ElmString patternSourceCodeTextString)
            {
                return "Expected string in 'patternSourceCodeText' but got: " + patternSourceCodeTextField;
            }

            if (typeSourceCodeTextField is not ElmValue.ElmString typeSourceCodeTextString)
            {
                return "Expected string in 'typeSourceCodeText' but got: " + typeSourceCodeTextField;
            }

            if (typeIsAppStateTypeField is not ElmValue.ElmTag typeIsAppStateTypeTag)
            {
                return "Expected bool in 'typeIsAppStateType' but got: " + typeIsAppStateTypeField;
            }

            return
                new ExposedFunctionDescriptionParameter(
                    PatternSourceCodeText: patternSourceCodeTextString.Value,
                    TypeSourceCodeText: typeSourceCodeTextString.Value,
                    TypeIsAppStateType: typeIsAppStateTypeTag.TagName is "True");
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

            if (applyList.Items.Length is not 2)
            {
                return "Unexpected apply result: Expected 2 elements but got: " + applyList.Items.Length;
            }

            var newState = applyList.Items.Span[0];

            var cmdsValue = applyList.Items.Span[1];

            if (cmdsValue is not PineValue.ListValue cmdsList)
            {
                return "Unexpected apply result: Commands is not a list but: " + cmdsValue;
            }

            return
                Result<string, (PineValue newState, IReadOnlyList<PineValue> cmds)>
                .ok((newState, cmdsList.Items.ToArray()));
        }

        public Result<string, PineValue> DecodeElmJsonValueFromString(
            string jsonString,
            IPineVM pineVM)
        {
            var jsonStringEncoded =
                ElmValueEncoding.StringAsPineValue(jsonString);

            var jsonDecodeApplyFunctionResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    JsonDecodeDecodeString,
                    [JsonDecodeValue, jsonStringEncoded]);

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
                    err =>
                    "Failed to decode JSON value: " + JsonDecodeErrorDisplayText(err),
                    Result<string, PineValue>.ok,
                    invalid:
                    err => throw new System.Exception("Invalid: " + err));
        }

        public Result<string, string> EncodeAppStateAsJsonString(
            PineValue appState,
            IPineVM pineVM)
        {
            var appStateJsonResult = EncodeAppStateAsJsonValue(appState, pineVM);

            {
                if (appStateJsonResult.IsErrOrNull() is { } err)
                {
                    return Result<string, string>.err("Failed to encode app state as JSON value: " + err);
                }
            }

            if (appStateJsonResult.IsOkOrNull() is not { } appStateJsonOk)
            {
                throw new System.Exception("Unexpected appStateJsonResult: " + appStateJsonResult);
            }

            return
                EncodeJsonValueAsJsonString(
                    appStateJsonOk,
                    indent: 0,
                    pineVM);
        }

        public Result<string, string> EncodeJsonValueAsJsonString(
            PineValue elmJsonValue,
            int indent,
            IPineVM pineVM)
        {
            var indentArgument = IntegerEncoding.EncodeSignedInteger(indent);

            var encodeResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    JsonEncodeEncode,
                    [indentArgument, elmJsonValue]);

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
                ElmValueEncoding.PineValueAsElmValue(encodeOk, null, null);

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

            if (decodeStringOk is not ElmValue.ElmString decodeStringOkString)
            {
                return Result<string, string>.err("Expected string but got: " + decodeStringOk);
            }

            return
                Result<string, string>.ok(decodeStringOkString.Value);
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

    public record ApplyExposedFunctionResponse(
        PineValue? AppState,
        PineValue? ResponseJsonValue);

    public static Result<string, ApplyExposedFunctionResponse> ApplyExposedFunction(
        PineValue? appStateBefore,
        IReadOnlyList<ElmValue> arguments,
        ExposedFunction exposedFunction,
        IPineVM pineVM,
        long posixTimeMilli)
    {
        var serializedArgumentsJson =
            arguments
            .Select(ElmValueJsonValueEncoding.EncodeAsJsonValuePineValue)
            .ToArray();

        return
            ApplyExposedFunction(
                appStateBefore: appStateBefore,
                arguments: serializedArgumentsJson,
                exposedFunction,
                pineVM,
                posixTimeMilli: posixTimeMilli);
    }

    public static Result<string, ApplyExposedFunctionResponse> ApplyExposedFunction(
        PineValue? appStateBefore,
        IReadOnlyList<PineValue> arguments,
        ExposedFunction exposedFunction,
        IPineVM pineVM,
        long posixTimeMilli)
    {
        /*

        type alias ApplyFunctionArguments state =
            { stateArgument : state
            , serializedArgumentsJson : List Json.Encode.Value
            }


        config_exposedFunctions :
            Dict.Dict
                String
                { description :
                    { returnType : { sourceCodeText : String, containsAppStateType : Bool }, parameters : List { patternSourceCodeText : String, typeSourceCodeText : String, typeIsAppStateType : Bool } }
                , handler :
                    Backend.Generated.StateShimTypes.ApplyFunctionArguments
                        (Maybe
                            { nextTaskIndex : Int
                            , posixTimeMilli : Int
                            , createVolatileProcessTasks :
                                Dict.Dict
                                    Backend.Generated.WebServiceShimTypes.TaskId
                                    (Platform.WebService.CreateVolatileProcessResult
                                     -> Backend.Main.State
                                     -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
                                    )
                            , requestToVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId (Platform.WebService.RequestToVolatileProcessResult -> Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State ))
                            , terminateVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId ()
                            , stateLessFramework : Backend.Main.State
                            }
                        )
                    ->
                        Result
                            String
                            ( Maybe
                                { nextTaskIndex : Int
                                , posixTimeMilli : Int
                                , createVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId (Platform.WebService.CreateVolatileProcessResult -> Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State ))
                                , requestToVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId (Platform.WebService.RequestToVolatileProcessResult -> Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State ))
                                , terminateVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId ()
                                , stateLessFramework : Backend.Main.State
                                }
                            , Maybe Json.Encode.Value
                            )
                }
        config_exposedFunctions =
         * */

        PineValue functionArgument()
        {
            if (appStateBefore is null)
            {
                return
                    ElmValueEncoding.TagAsPineValue("Nothing", []);
            }

            var appStateIncludingShim =
                ElmValueEncoding.ElmRecordAsPineValue(
                    [
                    ("nextTaskIndex", IntegerEncoding.EncodeSignedInteger(0)),
                    ("posixTimeMilli", IntegerEncoding.EncodeSignedInteger(posixTimeMilli)),
                    ("createVolatileProcessTasks", EmptyDictAsPineValue),
                    ("requestToVolatileProcessTasks", EmptyDictAsPineValue),
                    ("terminateVolatileProcessTasks", EmptyDictAsPineValue),
                    ("stateLessFramework", appStateBefore)
                    ]);

            return
                ElmValueEncoding.ElmRecordAsPineValue(
                    [
                    ("stateArgument",
                    ElmValueEncoding.TagAsPineValue(
                        "Just",
                        [appStateIncludingShim])),

                    ("serializedArgumentsJson",
                    PineValue.List([..arguments]))
                    ]);
        }

        var applyResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                exposedFunction.Handler,
                [functionArgument()]);

        {
            if (applyResult.IsErrOrNull() is { } err)
            {
                return "Failed to apply exposed function: " + err;
            }
        }

        if (applyResult.IsOkOrNull() is not { } applyOk)
        {
            throw new System.Exception("Unexpected applyResult: " + applyResult);
        }

        return
            ParseApplyExposedFunctionResult(applyOk);
    }

    public static Result<string, ApplyExposedFunctionResponse> ParseApplyExposedFunctionResult(
        PineValue applyResult)
    {
        return
            ElmValueInterop.ParseElmResultValue(
                applyResult,
                err =>
                ElmValueEncoding.PineValueAsElmValue(err, null, null)
                .Unpack(
                    fromErr:
                    err => "Failed decoding error value: " + err,
                    fromOk:
                    ok => ElmValue.RenderAsElmExpression(ok).expressionString),
                ok: ParseApplyExposedFunctionOk,
                invalid:
                err => throw new System.Exception("Invalid: " + err));
    }

    public static Result<string, ApplyExposedFunctionResponse> ParseApplyExposedFunctionOk(
        PineValue applyResponse)
    {
        if (applyResponse is not PineValue.ListValue applyResponseList)
        {
            return "Expected list value but got: " + applyResponse;
        }

        if (applyResponseList.Items.Length is not 2)
        {
            return "Expected 2 elements but got: " + applyResponseList.Items.Length;
        }

        var applyResponseSpan = applyResponseList.Items.Span;

        var maybeStateIncludingShim = applyResponseSpan[0];

        var parseAppStateResult =
            ElmValueInterop.ParseElmMaybeValue(
                maybeStateIncludingShim,
                nothing: () => Result<string, PineValue>.ok(null),
                just: ParseApplyExposedFunctionOkStateIncludingShim,
                invalid:
                err => throw new System.Exception("Invalid: " + err));

        if (parseAppStateResult.IsErrOrNull() is { } err)
        {
            return "Failed to parse app state: " + err;
        }

        var responseJsonValue =
            ElmValueInterop.ParseElmMaybeValue(
                applyResponseSpan[1],
                nothing: () => null,
                just: value => value,
                invalid:
                err => throw new System.Exception("Invalid: " + err));

        return
            new ApplyExposedFunctionResponse(
                AppState: parseAppStateResult.IsOkOrNull(),
                ResponseJsonValue: responseJsonValue);
    }

    public static Result<string, PineValue> ParseApplyExposedFunctionOkStateIncludingShim(
        PineValue applyResponse)
    {
        /*
        { nextTaskIndex : Int
        , posixTimeMilli : Int
        , createVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId (Platform.WebService.CreateVolatileProcessResult -> Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State ))
        , requestToVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId (Platform.WebService.RequestToVolatileProcessResult -> Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State ))
        , terminateVolatileProcessTasks : Dict.Dict Backend.Generated.WebServiceShimTypes.TaskId ()
        , stateLessFramework : Backend.Main.State
        }
        */

        var asRecordResult = ElmValueEncoding.ParsePineValueAsRecordTagged(applyResponse);

        {
            if (asRecordResult.IsErrOrNull() is { } err)
            {
                return "Failed to decode apply response as record: " + err;
            }
        }

        if (asRecordResult.IsOkOrNull() is not { } asRecordOk)
        {
            throw new System.Exception("Unexpected asRecordResult: " + asRecordResult);
        }

        var stateLessFrameworkField =
            asRecordOk
            .FirstOrDefault(kvp => kvp.fieldName is "stateLessFramework")
            .fieldValue;

        if (stateLessFrameworkField is null)
        {
            return "Expected 'stateLessFramework' field in apply response record";
        }

        return Result<string, PineValue>.ok(stateLessFrameworkField);
    }

    private static readonly PineValue EmptyDictAsPineValue =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.EmptyDict);

    /// <summary>
    /// The original lowering implementation added a 'main' declaration to account for DCE.
    /// </summary>
    public static FileTree CleanUpFromLoweredForJavaScript(
        FileTree loweredForJavaScript)
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

        if (rootFile is not FileTree.FileNode rootFileNode)
        {
            throw new System.Exception("Root file not found at " + string.Join("/", RootFilePath));
        }

        var rootFileText =
            System.Text.Encoding.UTF8.GetString(rootFileNode.Bytes.Span);

        IEnumerable<string> linesFiltered()
        {
            var inMainDeclaration = false;

            foreach (var line in rootFileText.ModuleLines())
            {
                if (line.StartsWith("main "))
                {
                    inMainDeclaration = true;
                }
                else if (string.IsNullOrWhiteSpace(line))
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
                new FileTree.FileNode(
                    System.Text.Encoding.UTF8.GetBytes(newRootFileText)));
    }
}
