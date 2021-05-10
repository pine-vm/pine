using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Pine;

namespace ElmFullstack
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

        static public string ElmMakeInterfaceModuleName => "ElmFullstackCompilerInterface.ElmMake";

        static public string GenerateJsonCodersInterfaceModuleName => "ElmFullstackCompilerInterface.GenerateJsonCoders";

        static public IImmutableList<string> CompilationInterfaceModuleNamePrefixes => ImmutableList.Create("ElmFullstackCompilerInterface", "CompilationInterface");

        static public string FrontendWebElmModuleName => "FrontendWeb.Main";
    }

    public class ElmApp
    {
        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> ToFlatDictionaryWithPathComparer(
            IEnumerable<(IImmutableList<string> filePath, IImmutableList<byte> fileContent)> filesBeforeSorting) =>
            filesBeforeSorting.ToImmutableSortedDictionary(
                entry => entry.filePath,
                entry => entry.fileContent,
                EnumerableExtension.Comparer<IImmutableList<string>>());

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> AsCompletelyLoweredElmApp(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> sourceFiles,
            ElmAppInterfaceConfig interfaceConfig,
            Action<string> logWriteLine) =>
            LoweredElmAppForBackendStateSerializer(
                originalAppFiles: LoweredElmAppForSourceFilesAndJsonCodersAndElmMake(sourceFiles),
                interfaceConfig,
                logWriteLine: logWriteLine);

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> LoweredElmAppForBackendStateSerializer(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> originalAppFiles,
            ElmAppInterfaceConfig interfaceConfig,
            Action<string> logWriteLine)
        {
            var interfaceToHostRootModuleFilePath = InterfaceToHostRootModuleFilePathFromSourceFiles(originalAppFiles);

            if (originalAppFiles.ContainsKey(interfaceToHostRootModuleFilePath))
            {
                //  Support integrating applications supplying their own lowered version.
                return originalAppFiles;
            }

            var backendMainFilePath = FilePathFromModuleName(interfaceConfig.RootModuleName);

            if (!originalAppFiles.TryGetValue(backendMainFilePath, out var backendMainOriginalFile))
            {
                //  App contains no backend.
                return originalAppFiles;
            }

            var stateTypeNameInModule =
                StateTypeNameFromRootElmModule(Encoding.UTF8.GetString(backendMainOriginalFile.ToArray()));

            var initialRootModuleText = InitialRootElmModuleText(
                interfaceConfig.RootModuleName, stateTypeNameInModule);

            var appFilesWithInitialRootModule =
                originalAppFiles
                .SetItem(interfaceToHostRootModuleFilePath, Encoding.UTF8.GetBytes(initialRootModuleText).ToImmutableList());

            var appFilesWithSupportAdded =
                WithSupportForCodingElmType(
                    appFilesWithInitialRootModule,
                    interfaceConfig.RootModuleName + "." + stateTypeNameInModule,
                    InterfaceToHostRootModuleName,
                    logWriteLine,
                    out var functionNames);

            var rootModuleTextWithSupportAdded =
                Encoding.UTF8.GetString(appFilesWithSupportAdded[interfaceToHostRootModuleFilePath].ToArray());

            var rootModuleText =
                new[]
                {
                    "jsonEncodeDeserializedState = " + functionNames.encodeFunctionName,
                    "jsonDecodeDeserializedState = " + functionNames.decodeFunctionName
                }
                .Aggregate(rootModuleTextWithSupportAdded, (intermediateModuleText, functionToAdd) =>
                    CompileElm.WithFunctionAdded(intermediateModuleText, functionToAdd));

            return appFilesWithSupportAdded.SetItem(
                interfaceToHostRootModuleFilePath,
                Encoding.UTF8.GetBytes(rootModuleText).ToImmutableList());
        }

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> WithSupportForCodingElmType(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> originalAppFiles,
            string elmTypeName,
            string elmModuleToAddFunctionsIn,
            Action<string> logWriteLine,
            out (string encodeFunctionName, string decodeFunctionName) functionNames)
        {
            var interfaceModuleFilePath = FilePathFromModuleName(elmModuleToAddFunctionsIn);

            if (!originalAppFiles.ContainsKey(interfaceModuleFilePath))
            {
                throw new ArgumentException("Did not find the module '" + elmModuleToAddFunctionsIn + "'.");
            }

            var interfaceModuleOriginalFile = originalAppFiles[interfaceModuleFilePath];

            var interfaceModuleOriginalFileText =
                Encoding.UTF8.GetString(interfaceModuleOriginalFile.ToArray());

            string getOriginalModuleText(string moduleName)
            {
                var filePath = FilePathFromModuleName(moduleName);

                originalAppFiles.TryGetValue(filePath, out var moduleFile);

                if (moduleFile == null)
                    throw new Exception("Did not find the module named '" + moduleFile + "'");

                return Encoding.UTF8.GetString(moduleFile.ToArray());
            }

            var allOriginalElmModulesNames =
                originalAppFiles
                .Select(originalAppFilePathAndContent =>
                {
                    var fileName = originalAppFilePathAndContent.Key.Last();

                    if (originalAppFilePathAndContent.Key.First() != "src" || !fileName.EndsWith(".elm"))
                        return null;

                    return
                        (IEnumerable<string>)
                        originalAppFilePathAndContent.Key.Skip(1).Reverse().Skip(1).Reverse()
                        .Concat(new[] { fileName.Substring(0, fileName.Length - 4) })
                        .ToImmutableList();
                })
                .Where(module => module != null)
                .OrderBy(module => string.Join(".", module))
                .ToImmutableHashSet();

            var sourceModules =
                allOriginalElmModulesNames
                .Select(moduleName => string.Join(".", moduleName))
                .ToImmutableDictionary(
                    moduleName => moduleName,
                    moduleName =>
                    {
                        var originalModuleText = getOriginalModuleText(moduleName);

                        if (moduleName == elmModuleToAddFunctionsIn)
                        {
                            return
                                CompileElm.WithImportsAdded(originalModuleText, allOriginalElmModulesNames);
                        }

                        return originalModuleText;
                    });

            var getExpressionsAndDependenciesForType = new Func<string, CompileElmValueSerializer.ResolveTypeResult>(canonicalTypeName =>
            {
                return
                    CompileElmValueSerializer.ResolveType(
                        canonicalTypeName,
                        elmModuleToAddFunctionsIn,
                        sourceModules,
                        logWriteLine);
            });

            var functionCodingExpressions =
                CompileElmValueSerializer.EnumerateExpressionsResolvingAllDependencies(
                    getExpressionsAndDependenciesForType,
                    ImmutableHashSet.Create(elmTypeName))
                .ToImmutableList();

            var functionCodingExpressionsDict =
                functionCodingExpressions
                .ToImmutableDictionary(entry => entry.elmType, entry => entry.result);

            var appFilesAfterExposingCustomTypesInModules =
                functionCodingExpressionsDict
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

            var supportingCodingFunctions =
                functionCodingExpressionsDict
                .Select(typeResult => CompileElmValueSerializer.BuildJsonCodingFunctionTexts(
                    typeResult.Key,
                    typeResult.Value.encodeExpression,
                    typeResult.Value.decodeExpression))
                .SelectMany(encodeAndDecodeFunctions => new[] { encodeAndDecodeFunctions.encodeFunction, encodeAndDecodeFunctions.decodeFunction })
                .ToImmutableHashSet()
                .Union(CompileElmValueSerializer.generalSupportingFunctionsTexts);

            var modulesToImport =
                functionCodingExpressions
                .SelectMany(functionReplacement =>
                    functionReplacement.result.referencedModules.Select(moduleName => moduleName.Split(".")))
                .ToImmutableHashSet(EnumerableExtension.EqualityComparer<string>())
                .Remove(elmModuleToAddFunctionsIn.Split("."))
                .Add(new[] { "Set" })
                .Add(new[] { "Dict" })
                .Add(new[] { "Json.Decode" })
                .Add(new[] { "Json.Encode" })
                .Add(new[] { "Base64" })
                .Add(new[] { "Bytes" })
                .Add(new[] { "Bytes.Encode" })
                .Add(new[] { "Bytes.Decode" });

            var interfaceModuleWithImports =
                CompileElm.WithImportsAdded(interfaceModuleOriginalFileText, modulesToImport);

            var interfaceModuleWithSupportingFunctions =
                supportingCodingFunctions
                .Aggregate(
                    interfaceModuleWithImports,
                    (intermediateModuleText, supportingFunction) => CompileElm.WithFunctionAdded(intermediateModuleText, supportingFunction));

            var elmTypeCodingFunctionNames =
                CompileElmValueSerializer.GetFunctionNamesAndTypeParametersFromTypeText(
                    getExpressionsAndDependenciesForType(elmTypeName).canonicalTypeText);

            functionNames =
                (encodeFunctionName: elmTypeCodingFunctionNames.functionNames.encodeFunctionName,
                decodeFunctionName: elmTypeCodingFunctionNames.functionNames.decodeFunctionName);

            return
                appFilesAfterExposingCustomTypesInModules.SetItem(
                    interfaceModuleFilePath,
                    Encoding.UTF8.GetBytes(interfaceModuleWithSupportingFunctions).ToImmutableList());
        }

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> LoweredElmAppForSourceFilesAndJsonCodersAndElmMake(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> sourceFiles) =>
            LoweredElmAppForSourceFilesAndJsonCodersAndElmMake(
                sourceFiles,
                ImmutableStack<IImmutableList<(CompilerSerialInterface.DependencyKey key, IImmutableList<byte> value)>>.Empty);

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> LoweredElmAppForSourceFilesAndJsonCodersAndElmMake(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> sourceFiles,
            IImmutableStack<IImmutableList<(CompilerSerialInterface.DependencyKey key, IImmutableList<byte> value)>> dependenciesStack)
        {
            if (10 < dependenciesStack.Count())
                throw new Exception("Dependencies stack depth > 10");

            using (var jsEngine = PrepareJsEngineToCompileElmApp())
            {
                var sourceFilesJson =
                    sourceFiles
                    .Select(appCodeFile => new CompilerSerialInterface.AppCodeEntry
                    {
                        path = appCodeFile.Key,
                        content = new CompilerSerialInterface.BytesJson { AsBase64 = Convert.ToBase64String(appCodeFile.Value.ToArray()) },
                    })
                    .ToImmutableList();

                var dependencies =
                    dependenciesStack.SelectMany(frame => frame)
                    .ToImmutableList();

                var dependenciesJson =
                    dependencies
                    .Select(dependency =>
                    new
                    {
                        key = dependency.key,
                        value = CompilerSerialInterface.BytesJson.AsJson(dependency.value),
                    })
                    .ToImmutableList();

                var argumentsJson = Newtonsoft.Json.JsonConvert.SerializeObject(
                    new
                    {
                        sourceFiles = sourceFilesJson,
                        compilationInterfaceElmModuleNamePrefixes = ElmAppInterfaceConvention.CompilationInterfaceModuleNamePrefixes,
                        dependencies = dependenciesJson,
                    }
                );

                var responseJson =
                    jsEngine.CallFunction("lowerForSourceFilesAndJsonCodersAndElmMakeSerialized", argumentsJson)
                    ?.ToString();

                var responseStructure =
                    Newtonsoft.Json.JsonConvert.DeserializeObject<ElmValueCommonJson.Result<IReadOnlyList<CompilerSerialInterface.CompilationError>, IReadOnlyList<CompilerSerialInterface.AppCodeEntry>>>(
                        responseJson);

                if (responseStructure.Ok?.FirstOrDefault() == null)
                {
                    var compilationErrors =
                        responseStructure.Err?.FirstOrDefault();

                    if (compilationErrors == null)
                        throw new Exception("Failed compilation: Protocol error: Missing error descriptions.");

                    var compilationErrorsMissingElmMakeDependencies =
                        compilationErrors
                        .Where(error => error?.MissingDependencyError?.FirstOrDefault().ElmMakeDependency != null)
                        .ToImmutableList();

                    var otherErrors =
                        compilationErrors.Except(compilationErrorsMissingElmMakeDependencies)
                        .ToImmutableList();

                    if (0 < otherErrors.Count)
                    {
                        var otherErrorsText =
                            string.Join("\n", otherErrors.Select(DescribeCompilationError));

                        throw new Exception("Failed compilation with " + compilationErrors.Count + " errors:\n" + otherErrorsText);
                    }

                    byte[] ElmMake(
                        IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> elmCodeFiles,
                        IImmutableList<string> pathToFileWithElmEntryPoint,
                        bool makeJavascript,
                        bool enableDebug)
                    {
                        var elmMakeCommandAppendix =
                            enableDebug ? "--debug" : null;

                        var fileAsString = ProcessFromElm019Code.CompileElm(
                            elmCodeFiles,
                            pathToFileWithElmEntryPoint: pathToFileWithElmEntryPoint,
                            outputFileName: "output." + (makeJavascript ? "js" : "html"),
                            elmMakeCommandAppendix: elmMakeCommandAppendix);

                        return Encoding.UTF8.GetBytes(fileAsString);
                    }

                    var newDependencies =
                        compilationErrorsMissingElmMakeDependencies
                        .Select(error =>
                        {
                            var dependencyKey =
                                error.MissingDependencyError.FirstOrDefault();

                            var elmMakeRequest =
                                dependencyKey.ElmMakeDependency.FirstOrDefault();

                            var elmMakeRequestFiles =
                                elmMakeRequest.files
                                .ToImmutableDictionary(
                                    entry => (IImmutableList<string>)entry.path.ToImmutableList(),
                                    entry => (IImmutableList<byte>)Convert.FromBase64String(entry.content.AsBase64).ToImmutableList())
                                .WithComparers(EnumerableExtension.EqualityComparer<string>());

                            var value = ElmMake(
                                elmCodeFiles: elmMakeRequestFiles,
                                pathToFileWithElmEntryPoint: elmMakeRequest.entryPointFilePath.ToImmutableList(),
                                makeJavascript: elmMakeRequest.outputType.ElmMakeOutputTypeJs?.FirstOrDefault() != null,
                                enableDebug: elmMakeRequest.enableDebug);

                            return (key: dependencyKey, value: (IImmutableList<byte>)value.ToImmutableList());
                        })
                        .ToImmutableList();

                    return LoweredElmAppForSourceFilesAndJsonCodersAndElmMake(
                        sourceFiles: sourceFiles,
                        dependenciesStack: dependenciesStack.Push(newDependencies));
                }

                var resultFiles =
                    responseStructure.Ok?.FirstOrDefault()
                    .ToImmutableDictionary(
                        entry => (IImmutableList<string>)entry.path.ToImmutableList(),
                        entry => (IImmutableList<byte>)Convert.FromBase64String(entry.content.AsBase64).ToImmutableList())
                    .WithComparers(EnumerableExtension.EqualityComparer<string>());

                return resultFiles;
            }
        }

        static Composition.Result<string, KeyValuePair<IImmutableList<string>, IImmutableList<byte>>> FindFileWithPathMatchingRepresentationInFunctionName(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> sourceFiles,
            string filePathRepresentation)
        {
            var sourceFilesWithRepresentations =
                sourceFiles
                .Select(sourceFilePathAndContent => (sourceFilePathAndContent, filePathRepresentation: functionNameInCompilationInterfaceFromFilePath(sourceFilePathAndContent.Key)))
                .ToImmutableList();

            var matchingFiles =
                sourceFilesWithRepresentations
                .Where(sourceFileAndPathRepresentation => sourceFileAndPathRepresentation.filePathRepresentation == filePathRepresentation)
                .Select(sourceFileAndPathRepresentation => sourceFileAndPathRepresentation.sourceFilePathAndContent)
                .ToImmutableList();

            if (matchingFiles.Count < 1)
            {
                return Composition.Result<string, KeyValuePair<IImmutableList<string>, IImmutableList<byte>>>.err(
                    "Did not find any source file with a path matching the representation '" + filePathRepresentation + "'. Here is a list of the available files:\n" +
                    string.Join("\n", sourceFilesWithRepresentations.Select(
                        sourceFileAndRepr => "'" + string.Join("/", sourceFileAndRepr.sourceFilePathAndContent.Key) + "' ('" + sourceFileAndRepr.filePathRepresentation + "')")));
            }

            if (matchingFiles.Count > 1)
            {
                return Composition.Result<string, KeyValuePair<IImmutableList<string>, IImmutableList<byte>>>.err(
                    "The file path representation '" + filePathRepresentation +
                    "' is not unique because it matches " + matchingFiles.Count + " of the source files:" +
                    string.Join(", ", matchingFiles.Select(matchingFile => "\"" + string.Join("/", matchingFile.Key) + "\"")));
            }

            return Composition.Result<string, KeyValuePair<IImmutableList<string>, IImmutableList<byte>>>.ok(
                matchingFiles.Single());
        }

        static string functionNameInCompilationInterfaceFromFilePath(IImmutableList<string> filePath) =>
            Regex.Replace(string.Join("/", filePath), "[^a-zA-Z0-9]", "_");

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> ImportModuleInModule(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> previousAppFiles,
            string moduleName, IEnumerable<string> moduleToImport)
        {
            var moduleFilePath = FilePathFromModuleName(moduleName);

            var moduleTextBefore = Encoding.UTF8.GetString(previousAppFiles[moduleFilePath].ToArray());

            var moduleText = CompileElm.WithImportsAdded(moduleTextBefore, ImmutableHashSet.Create(moduleToImport));

            return
                previousAppFiles
                .SetItem(moduleFilePath, Encoding.UTF8.GetBytes(moduleText).ToImmutableList());
        }

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> ReplaceFunctionInModule(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> previousAppFiles,
            string moduleName, string functionName, string newFunctionText)
        {
            var moduleFilePath = FilePathFromModuleName(moduleName);

            var moduleTextBefore = Encoding.UTF8.GetString(previousAppFiles[moduleFilePath].ToArray());

            var moduleText = ReplaceFunctionInModuleText(moduleTextBefore, functionName, newFunctionText);

            return
                previousAppFiles
                .SetItem(moduleFilePath, Encoding.UTF8.GetBytes(moduleText).ToImmutableList());
        }

        static string ReplaceFunctionInModuleText(string moduleText, string functionName, string newFunctionText)
        {
            var allFunctions = CompileElm.ParseAllFunctionsFromModule(moduleText).ToImmutableList();

            var originalFunction = allFunctions.FirstOrDefault(fun => fun.functionName == functionName);

            if (originalFunction.functionText == null)
                throw new NotImplementedException("Did not find function '" + functionName + "' in module text.");

            return moduleText.Replace(originalFunction.functionText, newFunctionText);
        }

        static public IImmutableList<string> FilePathFromModuleName(IReadOnlyList<string> moduleName)
        {
            var fileName = moduleName.Last() + ".elm";
            var directoryNames = moduleName.Reverse().Skip(1).Reverse();

            return new[] { "src" }.Concat(directoryNames).Concat(new[] { fileName }).ToImmutableList();
        }

        static public IImmutableList<string> FilePathFromModuleName(string moduleName) =>
            FilePathFromModuleName(moduleName.Split(new[] { '.' }).ToImmutableList());

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

        static public IImmutableList<string> InterfaceToHostRootModuleFilePathFromSourceFiles(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> sourceFiles) =>
            FilePathFromModuleName(InterfaceToHostRootModuleName);

        static public string InitialRootElmModuleText(
            string rootModuleNameBeforeLowering,
            string stateTypeNameInRootModuleBeforeLowering) =>
            $@"
module " + InterfaceToHostRootModuleName + $@" exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import " + rootModuleNameBeforeLowering + $@"
import Platform

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

";

        static public JavaScriptEngineSwitcher.Core.IJsEngine PrepareJsEngineToCompileElmApp()
        {
            var javascript = JavascriptToCompileElmApp.Value;

            var javascriptEngine = ProcessHostedWithV8.ConstructJsEngine();

            var initAppResult = javascriptEngine.Evaluate(javascript);

            return javascriptEngine;
        }

        static readonly Lazy<string> JavascriptToCompileElmApp = new Lazy<string>(BuildJavascriptToCompileElmApp);

        static string BuildJavascriptToCompileElmApp()
        {
            var javascriptFromElmMake =
                ProcessFromElm019Code.CompileElmToJavascript(
                    CompileElmProgramAppCodeFiles(),
                    ImmutableList.Create("src", "Main.elm"));

            var javascriptMinusCrashes = ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

            var listFunctionToPublish =
                new[]
                {
                    (functionNameInElm: "Main.lowerForSourceFilesSerialized",
                    publicName: "lowerForSourceFilesSerialized",
                    arity: 1),
                    (functionNameInElm: "Main.lowerForSourceFilesAndJsonCodersSerialized",
                    publicName: "lowerForSourceFilesAndJsonCodersSerialized",
                    arity: 1),
                    (functionNameInElm: "Main.lowerForSourceFilesAndJsonCodersAndElmMakeSerialized",
                    publicName: "lowerForSourceFilesAndJsonCodersAndElmMakeSerialized",
                    arity: 1),
                };

            return
                ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                    javascriptMinusCrashes,
                    listFunctionToPublish);
        }

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> CompileElmProgramAppCodeFiles() =>
            ImmutableDictionary<IImmutableList<string>, IImmutableList<byte>>.Empty
            .WithComparers(EnumerableExtension.EqualityComparer<string>())
            .SetItem(ImmutableList.Create("elm.json"), GetManifestResourceStreamContent("elm_fullstack.ElmFullstack.compile_elm_program.elm.json").ToImmutableList())
            .SetItem(ImmutableList.Create("src", "CompileFullstackApp.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmFullstack.compile_elm_program.src.CompileFullstackApp.elm").ToImmutableList())
            .SetItem(ImmutableList.Create("src", "Main.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmFullstack.compile_elm_program.src.Main.elm").ToImmutableList());

        static byte[] GetManifestResourceStreamContent(string name)
        {
            using (var stream = typeof(ElmApp).Assembly.GetManifestResourceStream(name))
            {
                using (var memoryStream = new System.IO.MemoryStream())
                {
                    stream.CopyTo(memoryStream);

                    return memoryStream.ToArray();
                }
            }
        }

        static string DescribeCompilationError(CompilerSerialInterface.CompilationError compilationError) =>
            Newtonsoft.Json.JsonConvert.SerializeObject(compilationError, Newtonsoft.Json.Formatting.Indented);
    }

    namespace CompilerSerialInterface
    {
        class CompilationError
        {
            public IReadOnlyList<string> OtherCompilationError;

            public IReadOnlyList<DependencyKey> MissingDependencyError;
        }

        struct DependencyKey
        {
            public IReadOnlyList<ElmMakeRequestStructure> ElmMakeDependency;
        }

        class ElmMakeRequestStructure
        {
            public IReadOnlyList<CompilerSerialInterface.AppCodeEntry> files;

            public IReadOnlyList<string> entryPointFilePath;

            public ElmMakeOutputType outputType;

            public bool enableDebug;
        }

        struct ElmMakeOutputType
        {
            public IReadOnlyList<object> ElmMakeOutputTypeHtml;

            public IReadOnlyList<object> ElmMakeOutputTypeJs;
        }

        struct AppCodeEntry
        {
            public IReadOnlyList<string> path;

            public BytesJson content;
        }

        struct BytesJson
        {
            public string AsBase64;

            static public BytesJson AsJson(IImmutableList<byte> bytes) =>
                new CompilerSerialInterface.BytesJson { AsBase64 = Convert.ToBase64String(bytes.ToArray()) };
        }
    }
}