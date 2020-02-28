using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Kalmit
{
    public struct ElmAppInterfaceConfig
    {
        public string RootModuleName;

        static public ElmAppInterfaceConfig Default => new ElmAppInterfaceConfig
        {
            RootModuleName = "Backend.Main"
        };
    }

    public struct ElmAppInterfaceConvention
    {
        public const string InitialStateFunctionName = "interfaceToHost_initState";

        public const string ProcessSerializedEventFunctionName = "interfaceToHost_processEvent";

        public const string SerializeStateFunctionName = "interfaceToHost_serializeState";

        public const string DeserializeStateFunctionName = "interfaceToHost_deserializeState";
    }

    public class ElmApp
    {
        static public bool FilePathMatchesPatternOfFilesInElmApp(string filePath) =>
            Regex.IsMatch(
                Path.GetFileName(filePath),
                "(^" + Regex.Escape("elm.json") + "|" + Regex.Escape(".elm") + ")$",
                RegexOptions.IgnoreCase);

        static public IEnumerable<(string filePath, IImmutableList<byte> fileContent)> FilesFilteredForElmApp(
            IEnumerable<(string filePath, IImmutableList<byte> fileContent)> files) =>
            files
            .Where(file => 0 < file.filePath?.Length && FilePathMatchesPatternOfFilesInElmApp(file.filePath));

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> ToFlatDictionaryWithPathComparer(
          IEnumerable<(IImmutableList<string> filePath, IImmutableList<byte> fileContent)> fileList) =>
          fileList.ToImmutableDictionary(
              entry => entry.filePath, entry => entry.fileContent)
          .WithComparers(EnumerableExtension.EqualityComparer<string>());

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> AsCompletelyLoweredElmApp(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> originalAppFiles,
            ElmAppInterfaceConfig interfaceConfig) =>
            LoweredElmAppForBackendStateSerializer(
                LoweredElmAppToGenerateJsonCoders(originalAppFiles), interfaceConfig);

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> LoweredElmAppForBackendStateSerializer(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> originalAppFiles,
            ElmAppInterfaceConfig interfaceConfig)
        {
            if (originalAppFiles.ContainsKey(InterfaceToHostRootModuleFilePath))
            {
                //  Support integrating applications supplying their own lowered version.
                return originalAppFiles;
            }

            var backendMainFilePath = FilePathFromModuleName(interfaceConfig.RootModuleName);

            var backendMainOriginalFile = originalAppFiles[backendMainFilePath];

            var stateTypeNameInModule =
                StateTypeNameFromRootElmModule(Encoding.UTF8.GetString(backendMainOriginalFile.ToArray()));

            string getModuleText(string moduleName)
            {
                var filePath = FilePathFromModuleName(moduleName);

                originalAppFiles.TryGetValue(filePath, out var moduleFile);

                if (moduleFile == null)
                    throw new Exception("Did not find the module named '" + moduleFile + "'");

                return Encoding.UTF8.GetString(moduleFile.ToArray());
            }

            var allOriginalElmModules =
                originalAppFiles
                .Select(originalAppFilePathAndContent =>
                {
                    var fileName = originalAppFilePathAndContent.Key.Last();

                    if (originalAppFilePathAndContent.Key.First() != "src" || !fileName.EndsWith(".elm"))
                        return null;

                    return
                        (IImmutableList<string>)
                        originalAppFilePathAndContent.Key.Skip(1).Reverse().Skip(1).Reverse()
                        .Concat(new[] { fileName.Substring(0, fileName.Length - 4) })
                        .ToImmutableList();
                })
                .Where(module => module != null)
                .OrderBy(module => string.Join(".", module))
                .ToImmutableList();

            var getExpressionsAndDependenciesForType = new Func<string, CompileElmValueSerializer.ResolveTypeResult>(canonicalTypeName =>
            {
                return
                    CompileElmValueSerializer.ResolveType(
                        canonicalTypeName,
                        InterfaceToHostRootModuleName,
                        moduleName =>
                        {
                            if (moduleName == InterfaceToHostRootModuleName)
                            {
                                var newRootModuleNameImportStatements =
                                    String.Join("\n",
                                        allOriginalElmModules.Select(elmModule => "import " + String.Join(".", elmModule)));

                                return "module " + InterfaceToHostRootModuleName + "\n\n" + newRootModuleNameImportStatements;
                            }

                            return getModuleText(moduleName);
                        });
            });

            var canonicalStateTypeName =
                getExpressionsAndDependenciesForType(interfaceConfig.RootModuleName + "." + stateTypeNameInModule).canonicalTypeText;

            var allStateCodingExpressions =
                CompileElmValueSerializer.EnumerateExpressionsResolvingAllDependencies(
                    getExpressionsAndDependenciesForType,
                    ImmutableHashSet.Create(canonicalStateTypeName))
                .ToImmutableList();

            Console.WriteLine("allStateCodingExpressions.expressions.Count: " + allStateCodingExpressions.Count);

            var appFilesAfterExposingCustomTypesInModules =
                allStateCodingExpressions
                .Select(exprResult => exprResult.elmType)
                .Aggregate(
                    originalAppFiles,
                    (partiallyUpdatedAppFiles, elmType) =>
                    {
                        {
                            var enclosingParenthesesMatch = Regex.Match(elmType.Trim(), @"^\(([^,^\)]+)\)$");

                            if (enclosingParenthesesMatch.Success)
                                elmType = enclosingParenthesesMatch.Groups[1].Value;
                        }

                        var qualifiedMatch = Regex.Match(elmType.Trim(), @"^(.+)\.([^\s^\.]+)(\s+[a-z][^\s^\.]*)*$");

                        if (!qualifiedMatch.Success)
                            return partiallyUpdatedAppFiles;

                        var moduleName = qualifiedMatch.Groups[1].Value;
                        var localTypeName = qualifiedMatch.Groups[2].Value;

                        var expectedFilePath = FilePathFromModuleName(moduleName);

                        var moduleBefore =
                            partiallyUpdatedAppFiles
                            .FirstOrDefault(candidate => candidate.Key.SequenceEqual(expectedFilePath));

                        if (moduleBefore.Value == null)
                            return partiallyUpdatedAppFiles;

                        var moduleTextBefore = Encoding.UTF8.GetString(moduleBefore.Value.ToArray());

                        var isCustomTypeMatch = Regex.Match(
                            moduleTextBefore,
                            @"^type\s+" + localTypeName + @"(\s+[a-z][^\s]*){0,}\s*=", RegexOptions.Multiline);

                        if (!isCustomTypeMatch.Success)
                            return partiallyUpdatedAppFiles;

                        var moduleText = CompileElm.ExposeCustomTypeAllTagsInElmModule(moduleTextBefore, localTypeName);

                        return partiallyUpdatedAppFiles.SetItem(moduleBefore.Key, Encoding.UTF8.GetBytes(moduleText).ToImmutableList());
                    });

            var stateCodingJsonFunctionsText =
                String.Join("\n\n",
                allStateCodingExpressions
                .Select(typeResult => CompileElmValueSerializer.BuildJsonCodingFunctionTexts(
                    typeResult.elmType,
                    typeResult.result.encodeExpression,
                    typeResult.result.decodeExpression))
                .SelectMany(encodeAndDecodeFunctions => new[] { encodeAndDecodeFunctions.encodeFunction, encodeAndDecodeFunctions.decodeFunction }));

            var stateCodingFunctionNames =
                CompileElmValueSerializer.GetFunctionNamesAndTypeParametersFromTypeText(canonicalStateTypeName).functionNames;

            return
                appFilesAfterExposingCustomTypesInModules.SetItem(
                    InterfaceToHostRootModuleFilePath,
                    Encoding.UTF8.GetBytes(LoweredRootElmModuleCode(
                        interfaceConfig.RootModuleName,
                        stateTypeNameInModule,
                        stateCodingJsonFunctionsText,
                        stateCodingFunctionNames.encodeFunctionName,
                        stateCodingFunctionNames.decodeFunctionName,
                        allOriginalElmModules)).ToImmutableList());
        }

        /*
        TODO: Add lowering for exposing types in defining modules, analogous to lowering for the backend state serializer (`appFilesAfterExposingCustomTypesInModules`).
        */
        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> LoweredElmAppToGenerateJsonCoders(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> originalAppFiles)
        {
            var generateSerializerInterfaceModuleName = "ElmFullstackLoweringInterface.GenerateJsonCoders";

            var interfaceModuleFilePath = FilePathFromModuleName(generateSerializerInterfaceModuleName);

            if (!originalAppFiles.ContainsKey(interfaceModuleFilePath))
            {
                return originalAppFiles;
            }

            var interfaceModuleOriginalFile = originalAppFiles[interfaceModuleFilePath];

            var interfaceModuleOriginalFileText =
                Encoding.UTF8.GetString(interfaceModuleOriginalFile.ToArray());

            var originalFunctions = CompileElm.ParseAllFunctionsFromModule(interfaceModuleOriginalFileText);

            var intermediateModuleText = interfaceModuleOriginalFileText;

            var functionsReplacements =
                originalFunctions
                .Select(originalFunction =>
                {
                    var functionType = parseFunctionType(originalFunction.functionText);

                    var replacement = computeFunctionReplacement(functionType.typeCanonicalName, functionType.isDecoder);

                    var originalFunctionTextLines =
                        originalFunction.functionText.Replace("\r", "").Split("\n");

                    var newFunctionText =
                        String.Join("\n", originalFunctionTextLines.Take(2).Concat(new[] { replacement.functionBody }));

                    return new
                    {
                        functionName = originalFunction.functionName,
                        newFunctionText = newFunctionText,
                        supportingCodingExpressions = replacement.supportingCodingExpressions,
                    };
                })
                .ToImmutableList();

            static (string typeCanonicalName, bool isDecoder) parseFunctionType(string functionText)
            {
                //  Assume all are on one line:
                var parametersTextMatch = Regex.Match(functionText, @"^[^\s]+\s*\:(.*)$", RegexOptions.Multiline);

                if (!parametersTextMatch.Success)
                    throw new NotImplementedException("Did not find parametersTextMatch");

                var parameterTexts = parametersTextMatch.Groups[1].Value;

                var parametersTexts =
                    parameterTexts.Split("->").Select(paramText => paramText.Trim())
                    .ToImmutableList();

                if (parametersTexts.Count == 1)
                {
                    var decoderMatch = Regex.Match(parametersTexts[0], Regex.Escape("Json.Decode.Decoder") + @"\s+([\w\d_\.]+)");

                    if (!decoderMatch.Success)
                        throw new NotImplementedException("Did not match Decoder pattern: " + parameterTexts);

                    return (typeCanonicalName: decoderMatch.Groups[1].Value, isDecoder: true);
                }

                if (parametersTexts.Count == 2)
                {
                    if (parametersTexts[1] != "Json.Encode.Value")
                        throw new NotImplementedException("Did not match Encode pattern: " + parameterTexts);

                    return (typeCanonicalName: parametersTexts[0], isDecoder: false);
                }

                throw new NotImplementedException("Unexpected number of parameters: " + parameterTexts);
            }

            (string functionBody,
            IImmutableDictionary<string, (string encodeExpression, string decodeExpression, IImmutableSet<string> referencedModules)> supportingCodingExpressions)
                computeFunctionReplacement(
                string serializedTypeName, bool isDecoder)
            {
                string getModuleText(string moduleName)
                {
                    var filePath = FilePathFromModuleName(moduleName);

                    originalAppFiles.TryGetValue(filePath, out var moduleFile);

                    if (moduleFile == null)
                        throw new Exception("Did not find the module named '" + moduleFile + "'");

                    return Encoding.UTF8.GetString(moduleFile.ToArray());
                }

                var allOriginalElmModules =
                    originalAppFiles
                    .Select(originalAppFilePathAndContent =>
                    {
                        var fileName = originalAppFilePathAndContent.Key.Last();

                        if (originalAppFilePathAndContent.Key.First() != "src" || !fileName.EndsWith(".elm"))
                            return null;

                        return
                            (IImmutableList<string>)
                            originalAppFilePathAndContent.Key.Skip(1).Reverse().Skip(1).Reverse()
                            .Concat(new[] { fileName.Substring(0, fileName.Length - 4) })
                            .ToImmutableList();
                    })
                    .Where(module => module != null)
                    .OrderBy(module => string.Join(".", module))
                    .ToImmutableList();

                var getExpressionsAndDependenciesForType = new Func<string, CompileElmValueSerializer.ResolveTypeResult>(canonicalTypeName =>
                {
                    return
                        CompileElmValueSerializer.ResolveType(
                            canonicalTypeName,
                            InterfaceToHostRootModuleName,
                            moduleName =>
                            {
                                if (moduleName == InterfaceToHostRootModuleName)
                                {
                                    var newRootModuleNameImportStatements =
                                        String.Join("\n",
                                            allOriginalElmModules.Select(elmModule => "import " + String.Join(".", elmModule)));

                                    return "module " + InterfaceToHostRootModuleName + "\n\n" + newRootModuleNameImportStatements;
                                }

                                return getModuleText(moduleName);
                            });
                });

                var currentFunctionCodingExpressions =
                    CompileElmValueSerializer.EnumerateExpressionsResolvingAllDependencies(
                        getExpressionsAndDependenciesForType,
                        ImmutableHashSet.Create(serializedTypeName))
                    .ToImmutableList();

                var serializedTypeCanonicalText = getExpressionsAndDependenciesForType(serializedTypeName).canonicalTypeText;

                var currentFunctionCodingExpressionsDict =
                    currentFunctionCodingExpressions
                    .ToImmutableDictionary(entry => entry.elmType, entry => entry.result);

                var resultForCurrentFunction = currentFunctionCodingExpressionsDict[serializedTypeCanonicalText];

                var codingFunctionNames =
                    CompileElmValueSerializer.GetFunctionNamesAndTypeParametersFromTypeText(serializedTypeCanonicalText);

                var codeTypeExpression =
                    isDecoder
                    ?
                    codingFunctionNames.functionNames.decodeFunctionName
                    :
                    codingFunctionNames.functionNames.encodeFunctionName;

                return (CompileElmValueSerializer.IndentElmCodeLines(1, codeTypeExpression), currentFunctionCodingExpressionsDict);
            }

            string replaceFunctionInModule(string moduleText, string functionName, string newFunctionText)
            {
                var allFunctions = CompileElm.ParseAllFunctionsFromModule(moduleText).ToImmutableList();

                var originalFunction = allFunctions.FirstOrDefault(fun => fun.functionName == functionName);

                if (originalFunction.functionText == null)
                    throw new NotImplementedException("Did not find function '" + functionName + "' in module text.");

                return moduleText.Replace(originalFunction.functionText, newFunctionText);
            }

            var interfaceModuleWithReplacedFunctions =
                functionsReplacements
                .Aggregate(interfaceModuleOriginalFileText, (intermediateModuleText, replacement) =>
                    replaceFunctionInModule(intermediateModuleText, replacement.functionName, replacement.newFunctionText));

            var allSupportingCodingExpressions =
                functionsReplacements
                .Select(functionReplacement => functionReplacement.supportingCodingExpressions)
                .Aggregate(ImmutableDictionary<string, (string encodeExpression, string decodeExpression, IImmutableSet<string> referencedModules)>.Empty,
                (a, b) => a.SetItems(b));

            var appFilesAfterExposingCustomTypesInModules =
                allSupportingCodingExpressions
                .Select(exprResult => exprResult.Key)
                .Aggregate(
                    originalAppFiles,
                    (partiallyUpdatedAppFiles, elmType) =>
                    {
                        {
                            var enclosingParenthesesMatch = Regex.Match(elmType.Trim(), @"^\(([^,^\)]+)\)$");

                            if (enclosingParenthesesMatch.Success)
                                elmType = enclosingParenthesesMatch.Groups[1].Value;
                        }

                        var qualifiedMatch = Regex.Match(elmType.Trim(), @"^(.+)\.([^\s^\.]+)(\s+[a-z][^\s^\.]*)*$");

                        if (!qualifiedMatch.Success)
                            return partiallyUpdatedAppFiles;

                        var moduleName = qualifiedMatch.Groups[1].Value;
                        var localTypeName = qualifiedMatch.Groups[2].Value;

                        var expectedFilePath = FilePathFromModuleName(moduleName);

                        var moduleBefore =
                            partiallyUpdatedAppFiles
                            .FirstOrDefault(candidate => candidate.Key.SequenceEqual(expectedFilePath));

                        if (moduleBefore.Value == null)
                            return partiallyUpdatedAppFiles;

                        var moduleTextBefore = Encoding.UTF8.GetString(moduleBefore.Value.ToArray());

                        var isCustomTypeMatch = Regex.Match(
                            moduleTextBefore,
                            @"^type\s+" + localTypeName + @"(\s+[a-z][^\s]*){0,}\s*=", RegexOptions.Multiline);

                        if (!isCustomTypeMatch.Success)
                            return partiallyUpdatedAppFiles;

                        var moduleText = CompileElm.ExposeCustomTypeAllTagsInElmModule(moduleTextBefore, localTypeName);

                        return partiallyUpdatedAppFiles.SetItem(moduleBefore.Key, Encoding.UTF8.GetBytes(moduleText).ToImmutableList());
                    });

            var supportingCodingFunctionsText =
                String.Join("\n\n",
                allSupportingCodingExpressions
                .Select(typeResult => CompileElmValueSerializer.BuildJsonCodingFunctionTexts(
                    typeResult.Key,
                    typeResult.Value.encodeExpression,
                    typeResult.Value.decodeExpression))
                .SelectMany(encodeAndDecodeFunctions => new[] { encodeAndDecodeFunctions.encodeFunction, encodeAndDecodeFunctions.decodeFunction }));

            var interfaceModuleText =
                interfaceModuleWithReplacedFunctions + "\n\n\n" +
                supportingCodingFunctionsText + "\n\n\n" +
                String.Join("\n\n", CompileElmValueSerializer.generalSupportingFunctionsTexts) + "\n";

            return
                appFilesAfterExposingCustomTypesInModules.SetItem(
                    interfaceModuleFilePath,
                    Encoding.UTF8.GetBytes(interfaceModuleText).ToImmutableList());
        }

        static IImmutableList<string> FilePathFromModuleName(string moduleName)
        {
            var pathComponents = moduleName.Split(new[] { '.' });

            var fileName = pathComponents.Last() + ".elm";
            var directoryNames = pathComponents.Reverse().Skip(1).Reverse();

            return new[] { "src" }.Concat(directoryNames).Concat(new[] { fileName }).ToImmutableList();
        }

        static public string StateTypeNameFromRootElmModule(string elmModuleText)
        {
            var match = Regex.Match(
                elmModuleText,
                "^" + ElmAppInterfaceConvention.ProcessSerializedEventFunctionName +
                @"\s*:\s*String\s*->\s*([\w\d_]+)\s*->\s*\(\s*",
                RegexOptions.Multiline);

            if (!match.Success)
                throw new System.Exception("Did not find the expected type anotation for function " + ElmAppInterfaceConvention.ProcessSerializedEventFunctionName);

            return match.Groups[1].Value;
        }

        static public string InterfaceToHostRootModuleName => "Backend.InterfaceToHost_Root";

        static public IImmutableList<string> InterfaceToHostRootModuleFilePath => FilePathFromModuleName(InterfaceToHostRootModuleName);

        static public string LoweredRootElmModuleCode(
            string rootModuleNameBeforeLowering,
            string stateTypeNameInRootModuleBeforeLowering,
            string stateCodingFunctions,
            string stateEncodingFunctionName,
            string stateDecodingFunctionName,
            IImmutableList<IImmutableList<string>> modulesToImport) =>
            $@"
module " + InterfaceToHostRootModuleName + $@" exposing
    (State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import " + rootModuleNameBeforeLowering + $@"
import Set
import Dict
import Platform
import Json.Encode
import Json.Decode
" + String.Join("\n", modulesToImport.Select(moduleName => "import " + String.Join(".", moduleName)))
        + $@"

type alias DeserializedState = " + rootModuleNameBeforeLowering + "." + stateTypeNameInRootModuleBeforeLowering + $@"


type State
    = DeserializeFailed String
    | DeserializeSuccessful DeserializedState


interfaceToHost_initState = " + rootModuleNameBeforeLowering + $@".interfaceToHost_initState |> DeserializeSuccessful


interfaceToHost_processEvent hostEvent stateBefore =
    case stateBefore of
        DeserializeFailed _ ->
            ( stateBefore, ""[]"" )

        DeserializeSuccessful deserializedState ->
            deserializedState
                |> " + rootModuleNameBeforeLowering + $@".interfaceToHost_processEvent hostEvent
                |> Tuple.mapFirst DeserializeSuccessful


interfaceToHost_serializeState = jsonEncodeState >> Json.Encode.encode 0


interfaceToHost_deserializeState = deserializeState


-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        {{ init = \_ -> ( interfaceToHost_initState, Cmd.none )
        , update =
            \event stateBefore ->
                interfaceToHost_processEvent event (stateBefore |> interfaceToHost_serializeState |> interfaceToHost_deserializeState) |> Tuple.mapSecond (always Cmd.none)
        , subscriptions = \_ -> Sub.none
        }}


-- Inlined helpers -->


{{-| Turn a `Result e a` to an `a`, by applying the conversion
function specified to the `e`.
-}}
result_Extra_Extract : (e -> a) -> Result e a -> a
result_Extra_Extract f x =
    case x of
        Ok a ->
            a

        Err e ->
            f e


-- Remember and communicate errors from state deserialization -->


jsonEncodeState : State -> Json.Encode.Value
jsonEncodeState state =
    case state of
        DeserializeFailed error ->
            [ ( ""Interface_DeserializeFailed"", [ ( ""error"", error |> Json.Encode.string ) ] |> Json.Encode.object ) ] |> Json.Encode.object

        DeserializeSuccessful deserializedState ->
            deserializedState |> jsonEncodeDeserializedState


jsonEncodeDeserializedState =
    " + stateEncodingFunctionName + $@"


deserializeState : String -> State
deserializeState serializedState =
    serializedState
        |> Json.Decode.decodeString jsonDecodeState
        |> Result.mapError Json.Decode.errorToString
        |> result_Extra_Extract DeserializeFailed


jsonDecodeState : Json.Decode.Decoder State
jsonDecodeState =
    Json.Decode.oneOf
        [ Json.Decode.field ""Interface_DeserializeFailed"" (Json.Decode.field ""error"" Json.Decode.string |> Json.Decode.map DeserializeFailed)
        , jsonDecodeDeserializedState |> Json.Decode.map DeserializeSuccessful
        ]


jsonDecodeDeserializedState : Json.Decode.Decoder DeserializedState
jsonDecodeDeserializedState =
    " + stateDecodingFunctionName + $@"


-- State encoding and decoding functions -->

" + stateCodingFunctions + "\n\n\n" + String.Join("\n\n", CompileElmValueSerializer.generalSupportingFunctionsTexts) + "\n";
    }
}