module CompileFullstackApp exposing
    ( AppFiles
    , CompilationArguments
    , CompilationError(..)
    , DependencyKey(..)
    , ElmCustomTypeStruct
    , ElmMakeOutputType(..)
    , ElmMakeRequestStructure
    , ElmTypeAnnotation(..)
    , LeafElmTypeStruct(..)
    , appendLineAndStringInLogFile
    , asCompletelyLoweredElmApp
    , buildTypeAnnotationText
    , elmModulesDictFromFilesTexts
    , jsonCodingExpressionFromType
    , jsonCodingFunctionFromCustomType
    , parseAppStateElmTypeAndDependenciesRecursively
    , parseElmMakeModuleFunctionName
    , parseElmModuleText
    , parseElmTypeAndDependenciesRecursivelyFromAnnotation
    , parserDeadEndsToString
    )

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.Signature
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import Json.Encode
import List
import Maybe
import Parser
import Result.Extra
import SHA256
import Set


type alias CompilationArguments =
    { sourceFiles : AppFiles
    , compilationInterfaceElmModuleNamePrefixes : List String
    , dependencies : List ( DependencyKey, Bytes.Bytes )
    , rootModuleName : List String
    , interfaceToHostRootModuleName : List String
    }


type alias ElmAppInterfaceConvention =
    { initialStateFunctionName : String
    , processSerializedEventFunctionName : String
    , serializeStateFunctionName : String
    , deserializeStateFunctionName : String
    }


elmAppInterfaceConvention : ElmAppInterfaceConvention
elmAppInterfaceConvention =
    { initialStateFunctionName = "interfaceToHost_processEvent"
    , processSerializedEventFunctionName = "interfaceToHost_processEvent"
    , serializeStateFunctionName = "interfaceToHost_serializeState"
    , deserializeStateFunctionName = "interfaceToHost_deserializeState"
    }


type alias AppFiles =
    Dict.Dict (List String) Bytes.Bytes


type CompilationError
    = MissingDependencyError DependencyKey
    | OtherCompilationError String


type DependencyKey
    = ElmMakeDependency ElmMakeRequestStructure


type alias ElmMakeRequestStructure =
    { files : AppFiles
    , entryPointFilePath : List String
    , outputType : ElmMakeOutputType
    , enableDebug : Bool
    }


type alias ParseElmMakeFileNameResult =
    { filePathRepresentation : String
    , outputType : ElmMakeOutputType
    , enableDebug : Bool
    , base64 : Bool
    }


type ElmMakeOutputType
    = ElmMakeOutputTypeHtml
    | ElmMakeOutputTypeJs


{-| This function returns an Err if the needed dependencies for ElmMake are not yet in the arguments.
The integrating software can then perform the ElmMake, insert it into the dependencies dict and retry.
-}
asCompletelyLoweredElmApp : CompilationArguments -> Result (List CompilationError) AppFiles
asCompletelyLoweredElmApp { sourceFiles, compilationInterfaceElmModuleNamePrefixes, dependencies, rootModuleName, interfaceToHostRootModuleName } =
    let
        sourceModules =
            elmModulesDictFromAppFiles sourceFiles
                |> Dict.map (always Tuple.second)
    in
    loweredForSourceFiles compilationInterfaceElmModuleNamePrefixes sourceFiles
        |> Result.andThen (loweredForJsonCoders { originalSourceModules = sourceModules } compilationInterfaceElmModuleNamePrefixes)
        |> Result.mapError (OtherCompilationError >> List.singleton)
        |> Result.andThen (loweredForElmMake compilationInterfaceElmModuleNamePrefixes dependencies)
        |> Result.andThen
            (loweredForAppStateSerializer
                { originalSourceModules = sourceModules
                , rootModuleName = rootModuleName
                , interfaceToHostRootModuleName = interfaceToHostRootModuleName
                }
            )


loweredForSourceFiles : List String -> AppFiles -> Result String AppFiles
loweredForSourceFiles compilationInterfaceElmModuleNamePrefixes sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    identity
                    (compilationInterfaceElmModuleNamePrefix ++ ".SourceFiles")
                    mapSourceFilesModuleText
                    files
            )
            (Ok sourceFiles)


loweredForJsonCoders :
    { originalSourceModules : Dict.Dict String Elm.Syntax.File.File }
    -> List String
    -> AppFiles
    -> Result String AppFiles
loweredForJsonCoders context compilationInterfaceElmModuleNamePrefixes sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    identity
                    (compilationInterfaceElmModuleNamePrefix ++ ".GenerateJsonCoders")
                    (mapJsonCodersModuleText context)
                    files
            )
            (Ok sourceFiles)


loweredForElmMake : List String -> List ( DependencyKey, Bytes.Bytes ) -> AppFiles -> Result (List CompilationError) AppFiles
loweredForElmMake compilationInterfaceElmModuleNamePrefixes dependencies sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    (OtherCompilationError >> List.singleton)
                    (compilationInterfaceElmModuleNamePrefix ++ ".ElmMake")
                    (mapElmMakeModuleText dependencies)
                    files
            )
            (Ok sourceFiles)


loweredForAppStateSerializer :
    { rootModuleName : List String
    , interfaceToHostRootModuleName : List String
    , originalSourceModules : Dict.Dict String Elm.Syntax.File.File
    }
    -> AppFiles
    -> Result (List CompilationError) AppFiles
loweredForAppStateSerializer { rootModuleName, interfaceToHostRootModuleName, originalSourceModules } sourceFiles =
    let
        backendMainFilePath =
            filePathFromElmModuleName (String.join "." rootModuleName)

        interfaceToHostRootFilePath =
            filePathFromElmModuleName (String.join "." interfaceToHostRootModuleName)
    in
    case Dict.get backendMainFilePath sourceFiles of
        Nothing ->
            -- App contains no backend.
            Ok sourceFiles

        Just backendMainFile ->
            if Dict.get interfaceToHostRootFilePath sourceFiles /= Nothing then
                -- Support integrating applications supplying their own lowered version.
                Ok sourceFiles

            else
                case stringFromFileContent backendMainFile of
                    Nothing ->
                        Err [ OtherCompilationError "Failed to map file content to text" ]

                    Just backendMainModuleText ->
                        parseAppStateElmTypeAndDependenciesRecursively originalSourceModules backendMainModuleText
                            |> Result.mapError ((++) "Failed to parse state type name: ")
                            |> Result.andThen
                                (\( stateTypeAnnotation, stateTypeDependencies ) ->
                                    let
                                        initialRootElmModuleText =
                                            composeInitialRootElmModuleText
                                                { interfaceToHostRootModuleName = String.join "." interfaceToHostRootModuleName
                                                , rootModuleNameBeforeLowering = String.join "." rootModuleName
                                                , stateTypeAnnotation = stateTypeAnnotation
                                                }
                                    in
                                    mapAppFilesAndModuleTextToSupportJsonCoding
                                        [ stateTypeAnnotation ]
                                        stateTypeDependencies
                                        ( sourceFiles, initialRootElmModuleText )
                                        |> Result.map
                                            (\( appFiles, interfaceModuleTextWithSupportingFunctions, generatedModuleName ) ->
                                                let
                                                    functionsNamesInGeneratedModules =
                                                        buildJsonCodingFunctionsForTypeAnnotation stateTypeAnnotation

                                                    encodeFunction =
                                                        "jsonEncodeDeserializedState =\n"
                                                            ++ indentElmCodeLines 1
                                                                (generatedModuleName ++ "." ++ functionsNamesInGeneratedModules.encodeFunction.name)

                                                    decodeFunction =
                                                        "jsonDecodeDeserializedState =\n"
                                                            ++ indentElmCodeLines 1
                                                                (generatedModuleName ++ "." ++ functionsNamesInGeneratedModules.decodeFunction.name)

                                                    interfaceModuleText =
                                                        [ interfaceModuleTextWithSupportingFunctions
                                                        , encodeFunction
                                                        , decodeFunction
                                                        ]
                                                            |> String.join "\n\n"
                                                in
                                                appFiles
                                                    |> updateFileContentAtPath
                                                        (always (fileContentFromString interfaceModuleText))
                                                        interfaceToHostRootFilePath
                                            )
                                )
                            |> Result.mapError (OtherCompilationError >> List.singleton)


parseAppStateElmTypeAndDependenciesRecursively : Dict.Dict String Elm.Syntax.File.File -> String -> Result String ( ElmTypeAnnotation, Dict.Dict String ElmCustomTypeStruct )
parseAppStateElmTypeAndDependenciesRecursively sourceModules moduleText =
    case parseElmModuleText moduleText of
        Err error ->
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText error)

        Ok parsedModule ->
            case stateTypeAnnotationFromRootElmModule parsedModule of
                Err error ->
                    Err ("Did not find state type annotation: " ++ error)

                Ok stateTypeAnnotation ->
                    parseElmTypeAndDependenciesRecursivelyFromAnnotation
                        sourceModules
                        ( parsedModule, stateTypeAnnotation )


stateTypeAnnotationFromRootElmModule : Elm.Syntax.File.File -> Result String Elm.Syntax.TypeAnnotation.TypeAnnotation
stateTypeAnnotationFromRootElmModule parsedModule =
    parsedModule.declarations
        |> List.filterMap
            (\declaration ->
                case Elm.Syntax.Node.value declaration of
                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                        if
                            Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                == elmAppInterfaceConvention.processSerializedEventFunctionName
                        then
                            Just functionDeclaration

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.map
            (\functionDeclaration ->
                case functionDeclaration.signature of
                    Nothing ->
                        Err "Missing function signature"

                    Just functionSignature ->
                        case Elm.Syntax.Node.value (Elm.Syntax.Node.value functionSignature).typeAnnotation of
                            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation _ afterFirstArg ->
                                case Elm.Syntax.Node.value afterFirstArg of
                                    Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation secondArgument _ ->
                                        Ok (Elm.Syntax.Node.value secondArgument)

                                    _ ->
                                        Err "Unexpected type annotation in second argument"

                            _ ->
                                Err "Unexpected type annotation"
            )
        |> Maybe.withDefault (Err "Did not find function with matching name")


composeInitialRootElmModuleText :
    { interfaceToHostRootModuleName : String
    , rootModuleNameBeforeLowering : String
    , stateTypeAnnotation : ElmTypeAnnotation
    }
    -> String
composeInitialRootElmModuleText { interfaceToHostRootModuleName, rootModuleNameBeforeLowering, stateTypeAnnotation } =
    "module " ++ interfaceToHostRootModuleName ++ """ exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import """ ++ rootModuleNameBeforeLowering ++ """
import Platform

type alias DeserializedState =
    (""" ++ buildTypeAnnotationText stateTypeAnnotation ++ """)


type State
    = DeserializeFailed String
    | DeserializeSuccessful DeserializedState


interfaceToHost_initState = """ ++ rootModuleNameBeforeLowering ++ """.interfaceToHost_initState |> DeserializeSuccessful


interfaceToHost_processEvent hostEvent stateBefore =
    case stateBefore of
        DeserializeFailed _ ->
            ( stateBefore, "[]" )

        DeserializeSuccessful deserializedState ->
            deserializedState
                |> """ ++ rootModuleNameBeforeLowering ++ """.interfaceToHost_processEvent hostEvent
                |> Tuple.mapFirst DeserializeSuccessful


interfaceToHost_serializeState = jsonEncodeState >> Json.Encode.encode 0


interfaceToHost_deserializeState = deserializeState


-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        { init = \\_ -> ( interfaceToHost_initState, Cmd.none )
        , update =
            \\event stateBefore ->
                interfaceToHost_processEvent event (stateBefore |> interfaceToHost_serializeState |> interfaceToHost_deserializeState) |> Tuple.mapSecond (always Cmd.none)
        , subscriptions = \\_ -> Sub.none
        }


-- Inlined helpers -->


{-| Turn a `Result e a` to an `a`, by applying the conversion
function specified to the `e`.
-}
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
            [ ( "Interface_DeserializeFailed", [ ( "error", error |> Json.Encode.string ) ] |> Json.Encode.object ) ] |> Json.Encode.object

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
        [ Json.Decode.field "Interface_DeserializeFailed" (Json.Decode.field "error" Json.Decode.string |> Json.Decode.map DeserializeFailed)
        , jsonDecodeDeserializedState |> Json.Decode.map DeserializeSuccessful
        ]

"""


sourceFileFunctionNameStart : String
sourceFileFunctionNameStart =
    "file"


elmMakeFunctionNameStart : String
elmMakeFunctionNameStart =
    "elm_make"


functionNameFlagsSeparator : String
functionNameFlagsSeparator =
    "____"


mapJsonCodersModuleText :
    { originalSourceModules : Dict.Dict String Elm.Syntax.File.File }
    -> ( AppFiles, String )
    -> Result String ( AppFiles, String )
mapJsonCodersModuleText { originalSourceModules } ( sourceFiles, moduleText ) =
    case parseElmModuleText moduleText of
        Err error ->
            -- TODO: Consolidate branch to parse with `mapSourceFilesModuleText`
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText error)

        Ok parsedModule ->
            parsedModule.declarations
                -- TODO: Also share the 'map all functions' part with `mapSourceFilesModuleText`
                |> List.filterMap
                    (\declaration ->
                        case Elm.Syntax.Node.value declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                Just functionDeclaration

                            _ ->
                                Nothing
                    )
                |> List.map
                    (\functionDeclaration ->
                        case functionDeclaration.signature of
                            Nothing ->
                                Err "Missing function signature"

                            Just functionSignature ->
                                let
                                    functionName =
                                        Elm.Syntax.Node.value (Elm.Syntax.Node.value functionSignature).name
                                in
                                (case parseJsonCodingFunctionType moduleText (Elm.Syntax.Node.value functionSignature) of
                                    Err error ->
                                        Err ("Failed parsing json coding function type: " ++ error)

                                    Ok functionType ->
                                        case
                                            parseElmTypeAndDependenciesRecursivelyFromAnnotation
                                                originalSourceModules
                                                ( parsedModule, functionType.typeAnnotation )
                                        of
                                            Err error ->
                                                Err ("Failed to parse type annotation: " ++ error)

                                            Ok ( parsedTypeAnnotation, dependencies ) ->
                                                Ok
                                                    { functionName = functionName
                                                    , functionType = functionType
                                                    , parsedTypeAnnotation = parsedTypeAnnotation
                                                    , dependencies = dependencies
                                                    }
                                )
                                    |> Result.mapError (\error -> "Failed to prepare mapping '" ++ functionName ++ "': " ++ error)
                    )
                |> Result.Extra.combine
                |> Result.andThen
                    (\functionsToReplace ->
                        mapAppFilesAndModuleTextToSupportJsonCoding
                            (functionsToReplace |> List.map .parsedTypeAnnotation)
                            (functionsToReplace |> List.map .dependencies |> List.foldl Dict.union Dict.empty)
                            ( sourceFiles, moduleText )
                            |> Result.andThen
                                (\( appFiles, interfaceModuleText, generatedModuleName ) ->
                                    functionsToReplace
                                        |> listFoldlToAggregateResult
                                            (\functionToReplace previousModuleText ->
                                                let
                                                    functionName =
                                                        functionToReplace.functionName

                                                    functionsNamesInGeneratedModules =
                                                        buildJsonCodingFunctionsForTypeAnnotation functionToReplace.parsedTypeAnnotation

                                                    newFunction =
                                                        functionName
                                                            ++ " =\n    "
                                                            ++ generatedModuleName
                                                            ++ "."
                                                            ++ (if functionToReplace.functionType.isDecoder then
                                                                    functionsNamesInGeneratedModules.decodeFunction.name

                                                                else
                                                                    functionsNamesInGeneratedModules.encodeFunction.name
                                                               )

                                                    mapFunctionDeclarationLines originalFunctionTextLines =
                                                        [ originalFunctionTextLines |> List.take 1
                                                        , [ newFunction ]
                                                        ]
                                                            |> List.concat
                                                in
                                                addOrUpdateFunctionInElmModuleText
                                                    { functionName = functionName
                                                    , mapFunctionLines = Maybe.withDefault [] >> mapFunctionDeclarationLines
                                                    }
                                                    previousModuleText
                                                    |> Result.mapError ((++) "Failed to replace function text: ")
                                            )
                                            (Ok interfaceModuleText)
                                        |> Result.map (Tuple.pair appFiles)
                                )
                    )


mapAppFilesAndModuleTextToSupportJsonCoding :
    List ElmTypeAnnotation
    -> Dict.Dict String ElmCustomTypeStruct
    -> ( AppFiles, String )
    -> Result String ( AppFiles, String, String )
mapAppFilesAndModuleTextToSupportJsonCoding typeAnnotationsBeforeDeduplicating customTypes ( appFilesBefore, moduleText ) =
    case parseElmModuleText moduleText of
        Err error ->
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText error)

        Ok parsedModule ->
            let
                interfaceModuleName =
                    Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedModule.moduleDefinition)

                modulesToImportForCustomTypes =
                    customTypes
                        |> Dict.keys
                        |> List.map moduleNameFromTypeName
                        |> Set.fromList
                        |> Set.toList
                        |> List.map (String.split ".")

                modulesToImport =
                    [ [ "Base64" ]
                    , [ "Dict" ]
                    , [ "Set" ]
                    , [ "Json", "Decode" ]
                    , [ "Json", "Encode" ]
                    , [ "Bytes" ]
                    , [ "Bytes", "Decode" ]
                    , [ "Bytes", "Encode" ]
                    ]
                        ++ modulesToImportForCustomTypes
                        |> List.filter ((==) interfaceModuleName >> not)

                appFilesAfterExposingCustomTypesInModules =
                    modulesToImportForCustomTypes
                        |> List.foldl exposeAllInElmModuleInAppFiles appFilesBefore

                typeAnnotationsFunctions =
                    typeAnnotationsBeforeDeduplicating
                        |> listRemoveDuplicates
                        |> List.map buildJsonCodingFunctionsForTypeAnnotation

                typeAnnotationsFunctionsForGeneratedModule =
                    typeAnnotationsFunctions
                        |> List.concatMap
                            (\functionsForType ->
                                [ functionsForType.encodeFunction, functionsForType.decodeFunction ]
                            )
                        |> List.map (\function -> { functionName = function.name, functionText = function.text })

                dependenciesFunctions =
                    customTypes
                        |> Dict.toList
                        |> List.map
                            (\( customTypeName, customType ) ->
                                jsonCodingFunctionFromCustomType
                                    { customTypeName = customTypeName
                                    , encodeValueExpression = jsonEncodeParamName
                                    , typeArgLocalName = "type_arg"
                                    }
                                    customType
                            )
                        |> List.concatMap
                            (\functionsForType ->
                                [ functionsForType.encodeFunction, functionsForType.decodeFunction ]
                            )
                        |> List.map (\function -> { functionName = function.name, functionText = function.text })

                functionsForGeneratedModule =
                    typeAnnotationsFunctionsForGeneratedModule ++ dependenciesFunctions ++ generalSupportingFunctionsTexts

                generatedModuleTextWithoutModuleDeclaration =
                    [ modulesToImport |> List.map (String.join "." >> (++) "import ")
                    , functionsForGeneratedModule |> List.map .functionText
                    ]
                        |> List.concat
                        |> String.join "\n\n"

                generatedModuleHash =
                    generatedModuleTextWithoutModuleDeclaration
                        |> SHA256.fromString
                        |> SHA256.toHex

                generatedModuleName =
                    (interfaceModuleName |> String.join ".")
                        ++ ".Generated_"
                        ++ String.left 8 generatedModuleHash

                generatedModulePath =
                    filePathFromElmModuleName generatedModuleName

                generatedModuleText =
                    [ "module " ++ generatedModuleName ++ " exposing (..)"
                    , generatedModuleTextWithoutModuleDeclaration
                    ]
                        |> String.join "\n\n"

                appFiles =
                    appFilesAfterExposingCustomTypesInModules
                        |> updateFileContentAtPath
                            (always (fileContentFromString generatedModuleText))
                            generatedModulePath
            in
            addImportsInElmModuleText
                ([ String.split "." generatedModuleName ] ++ modulesToImport)
                moduleText
                |> Result.map (\moduleTextWithImport -> ( appFiles, moduleTextWithImport, generatedModuleName ))


buildJsonCodingFunctionsForTypeAnnotation :
    ElmTypeAnnotation
    -> { encodeFunction : { name : String, text : String }, decodeFunction : { name : String, text : String } }
buildJsonCodingFunctionsForTypeAnnotation typeAnnotation =
    let
        jsonCodingExpressions =
            jsonCodingExpressionFromType
                { encodeValueExpression = jsonEncodeParamName
                , typeArgLocalName = "type_arg"
                }
                ( typeAnnotation, [] )

        typeAnnotationText =
            buildTypeAnnotationText typeAnnotation

        nameCommonPart =
            typeAnnotationText
                |> SHA256.fromString
                |> SHA256.toHex
                |> String.left 10

        encodeFunctionName =
            jsonEncodeFunctionNamePrefix ++ nameCommonPart

        decodeFunctionName =
            jsonDecodeFunctionNamePrefix ++ nameCommonPart

        encodeFunctionText =
            encodeFunctionName
                ++ " "
                ++ jsonEncodeParamName
                ++ " =\n"
                ++ indentElmCodeLines 1 jsonCodingExpressions.encodeExpression

        decodeFunctionText =
            decodeFunctionName
                ++ " =\n"
                ++ indentElmCodeLines 1 jsonCodingExpressions.decodeExpression
    in
    { encodeFunction = { name = encodeFunctionName, text = encodeFunctionText }
    , decodeFunction = { name = decodeFunctionName, text = decodeFunctionText }
    }


mapSourceFilesModuleText : ( AppFiles, String ) -> Result String ( AppFiles, String )
mapSourceFilesModuleText ( sourceFiles, moduleText ) =
    case parseElmModuleText moduleText of
        Err error ->
            -- TODO: Consolidate branch to parse with `mapJsonCodersModuleText`
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText error)

        Ok parsedModule ->
            parsedModule.declarations
                -- TODO: Also share the 'map all functions' part with `mapJsonCodersModuleText`
                -- Remember: The module to interface with git services will probably use similar functionality.
                |> List.filterMap
                    (\declaration ->
                        case Elm.Syntax.Node.value declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                Just
                                    (Elm.Syntax.Node.value
                                        (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                    )

                            _ ->
                                Nothing
                    )
                |> listFoldlToAggregateResult
                    (\functionName previousAggregate ->
                        replaceFunctionInSourceFilesModuleText
                            sourceFiles
                            { functionName = functionName }
                            previousAggregate
                            |> Result.mapError
                                (\replaceFunctionError ->
                                    "Failed to replace function '"
                                        ++ functionName
                                        ++ "': "
                                        ++ replaceFunctionError
                                )
                    )
                    (addImportsInElmModuleText [ [ "Base64" ] ] moduleText)
                |> Result.map (Tuple.pair sourceFiles)


mapElmMakeModuleText :
    List ( DependencyKey, Bytes.Bytes )
    -> ( AppFiles, String )
    -> Result (List CompilationError) ( AppFiles, String )
mapElmMakeModuleText dependencies ( sourceFiles, moduleText ) =
    case parseElmModuleText moduleText of
        Err error ->
            -- TODO: Consolidate branch to parse with `mapSourceFilesModuleText`
            Err [ OtherCompilationError ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText error) ]

        Ok parsedModule ->
            parsedModule.declarations
                -- TODO: Also share the 'map all functions' part with `mapJsonCodersModuleText`
                |> List.filterMap
                    (\declaration ->
                        case Elm.Syntax.Node.value declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                Just functionDeclaration

                            _ ->
                                Nothing
                    )
                |> List.map
                    (\functionDeclaration ->
                        prepareReplaceFunctionInElmMakeModuleText
                            dependencies
                            sourceFiles
                            { functionName =
                                Elm.Syntax.Node.value
                                    (Elm.Syntax.Node.value functionDeclaration.declaration).name
                            }
                    )
                |> resultCombineConcatenatingErrors
                |> Result.andThen
                    (\functionsToReplaceFunction ->
                        let
                            interfaceModuleName =
                                Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedModule.moduleDefinition)

                            generatedModuleTextWithoutModuleDeclaration =
                                functionsToReplaceFunction
                                    |> List.map .valueFunctionText
                                    |> String.join "\n\n"

                            generatedModuleHash =
                                generatedModuleTextWithoutModuleDeclaration
                                    |> SHA256.fromString
                                    |> SHA256.toHex

                            generatedModuleName =
                                (interfaceModuleName |> String.join ".")
                                    ++ ".Generated_"
                                    ++ String.left 8 generatedModuleHash

                            generatedModulePath =
                                filePathFromElmModuleName generatedModuleName

                            generatedModuleText =
                                [ "module " ++ generatedModuleName ++ " exposing (..)"
                                , generatedModuleTextWithoutModuleDeclaration
                                ]
                                    |> String.join "\n\n"

                            appFiles =
                                sourceFiles
                                    |> updateFileContentAtPath
                                        (always (fileContentFromString generatedModuleText))
                                        generatedModulePath
                        in
                        functionsToReplaceFunction
                            |> List.map .updateInterfaceModuleText
                            |> listFoldlToAggregateResult
                                (\replaceFunction previousAggregate -> replaceFunction { generatedModuleName = generatedModuleName } previousAggregate)
                                (addImportsInElmModuleText [ [ "Base64" ], String.split "." generatedModuleName ] moduleText
                                    |> Result.mapError (OtherCompilationError >> List.singleton)
                                )
                            |> Result.map (Tuple.pair appFiles)
                    )


exposeAllInElmModuleInAppFiles : List String -> AppFiles -> AppFiles
exposeAllInElmModuleInAppFiles moduleName appFiles =
    let
        moduleFilePath =
            filePathFromElmModuleName (String.join "." moduleName)
    in
    case
        appFiles
            |> Dict.get moduleFilePath
            |> Maybe.andThen stringFromFileContent
    of
        Nothing ->
            appFiles

        Just originalModuleText ->
            let
                moduleText =
                    exposeAllInElmModule originalModuleText
            in
            appFiles |> Dict.insert moduleFilePath (fileContentFromString moduleText)


exposeAllInElmModule : String -> String
exposeAllInElmModule moduleText =
    case parseElmModuleText moduleText of
        Err _ ->
            moduleText

        Ok parsedModule ->
            let
                exposingListNode =
                    case Elm.Syntax.Node.value parsedModule.moduleDefinition of
                        Elm.Syntax.Module.NormalModule normalModule ->
                            normalModule.exposingList

                        Elm.Syntax.Module.PortModule portModule ->
                            portModule.exposingList

                        Elm.Syntax.Module.EffectModule effectModule ->
                            effectModule.exposingList

                exposingListRange =
                    Elm.Syntax.Node.range exposingListNode
            in
            case Elm.Syntax.Node.value exposingListNode of
                Elm.Syntax.Exposing.All _ ->
                    moduleText

                Elm.Syntax.Exposing.Explicit _ ->
                    replaceRangeInText exposingListRange "exposing (..)" moduleText


appendLineAndStringInLogFile : String -> AppFiles -> AppFiles
appendLineAndStringInLogFile logLine =
    updateFileContentAtPath
        (\fileContentBefore ->
            [ Maybe.withDefault (fileContentFromString "") fileContentBefore, fileContentFromString ("\n" ++ logLine) ]
                |> List.map Bytes.Encode.bytes
                |> Bytes.Encode.sequence
                |> Bytes.Encode.encode
        )
        [ "compilation-log.txt" ]


updateFileContentAtPath : (Maybe Bytes.Bytes -> Bytes.Bytes) -> List String -> AppFiles -> AppFiles
updateFileContentAtPath updateFileContent filePath appFiles =
    let
        fileContent =
            appFiles
                |> Dict.get filePath
                |> updateFileContent
    in
    appFiles |> Dict.insert filePath fileContent


type ElmTypeAnnotation
    = CustomElmType String
    | RecordElmType { fields : List ( String, ElmTypeAnnotation ) }
    | InstanceElmType { instantiated : ElmTypeAnnotation, arguments : List ElmTypeAnnotation }
    | TupleElmType (List ElmTypeAnnotation)
    | LeafElmType LeafElmTypeStruct
    | GenericType String
    | UnitType


type alias ElmCustomTypeStruct =
    { parameters : List String
    , tags : Dict.Dict String (List ElmTypeAnnotation)
    }


type LeafElmTypeStruct
    = StringLeaf
    | IntLeaf
    | BoolLeaf
    | FloatLeaf
    | BytesLeaf
    | ListLeaf
    | SetLeaf
    | MaybeLeaf
    | ResultLeaf
    | DictLeaf


parseElmTypeAndDependenciesRecursivelyFromAnnotation :
    Dict.Dict String Elm.Syntax.File.File
    -> ( Elm.Syntax.File.File, Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Result String ( ElmTypeAnnotation, Dict.Dict String ElmCustomTypeStruct )
parseElmTypeAndDependenciesRecursivelyFromAnnotation =
    parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal { typesToIgnore = Set.empty }


parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal :
    { typesToIgnore : Set.Set String }
    -> Dict.Dict String Elm.Syntax.File.File
    -> ( Elm.Syntax.File.File, Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Result String ( ElmTypeAnnotation, Dict.Dict String ElmCustomTypeStruct )
parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal stack modules ( currentModule, typeAnnotation ) =
    case typeAnnotation of
        Elm.Syntax.TypeAnnotation.Typed instantiatedNode argumentsNodes ->
            let
                ( instantiatedModuleAlias, instantiatedLocalName ) =
                    Elm.Syntax.Node.value instantiatedNode

                instantiatedResult =
                    case
                        parseElmTypeLeavesNames
                            |> Dict.get (instantiatedModuleAlias ++ [ instantiatedLocalName ] |> String.join ".")
                    of
                        Just leaf ->
                            Ok ( LeafElmType leaf, Dict.empty )

                        Nothing ->
                            let
                                maybeInstantiatedModule =
                                    if instantiatedModuleAlias == [] then
                                        currentModule.imports
                                            |> List.map Elm.Syntax.Node.value
                                            |> List.filterMap
                                                (\moduleImport ->
                                                    case Maybe.map Elm.Syntax.Node.value moduleImport.exposingList of
                                                        Nothing ->
                                                            Nothing

                                                        Just importExposing ->
                                                            let
                                                                containsMatchingExposition =
                                                                    case importExposing of
                                                                        Elm.Syntax.Exposing.All _ ->
                                                                            -- TODO: Add lookup into that module
                                                                            False

                                                                        Elm.Syntax.Exposing.Explicit topLevelExpositions ->
                                                                            topLevelExpositions
                                                                                |> List.any
                                                                                    (\topLevelExpose ->
                                                                                        case Elm.Syntax.Node.value topLevelExpose of
                                                                                            Elm.Syntax.Exposing.InfixExpose _ ->
                                                                                                False

                                                                                            Elm.Syntax.Exposing.FunctionExpose _ ->
                                                                                                False

                                                                                            Elm.Syntax.Exposing.TypeOrAliasExpose typeOrAliasExpose ->
                                                                                                typeOrAliasExpose == instantiatedLocalName

                                                                                            Elm.Syntax.Exposing.TypeExpose typeExpose ->
                                                                                                typeExpose.name == instantiatedLocalName
                                                                                    )
                                                            in
                                                            if containsMatchingExposition then
                                                                modules |> Dict.get (Elm.Syntax.Node.value moduleImport.moduleName |> String.join ".")

                                                            else
                                                                Nothing
                                                )
                                            |> List.head
                                            |> Maybe.withDefault currentModule
                                            |> Just

                                    else
                                        currentModule.imports
                                            |> List.map Elm.Syntax.Node.value
                                            |> List.filter
                                                (\moduleImport ->
                                                    moduleImport.moduleAlias
                                                        |> Maybe.withDefault moduleImport.moduleName
                                                        |> Elm.Syntax.Node.value
                                                        |> (==) instantiatedModuleAlias
                                                )
                                            |> List.head
                                            |> Maybe.andThen
                                                (\matchingImport ->
                                                    modules |> Dict.get (String.join "." (Elm.Syntax.Node.value matchingImport.moduleName))
                                                )
                            in
                            case maybeInstantiatedModule of
                                Nothing ->
                                    Err ("Did not find referenced module '" ++ String.join "." instantiatedModuleAlias ++ "'")

                                Just instantiatedModule ->
                                    instantiatedModule.declarations
                                        |> List.map Elm.Syntax.Node.value
                                        |> List.filterMap
                                            (\declaration ->
                                                case declaration of
                                                    Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
                                                        if Elm.Syntax.Node.value aliasDeclaration.name /= instantiatedLocalName then
                                                            Nothing

                                                        else
                                                            Just (AliasDeclaration aliasDeclaration)

                                                    Elm.Syntax.Declaration.CustomTypeDeclaration customTypeDeclaration ->
                                                        if Elm.Syntax.Node.value customTypeDeclaration.name /= instantiatedLocalName then
                                                            Nothing

                                                        else
                                                            Just (CustomTypeDeclaration customTypeDeclaration)

                                                    _ ->
                                                        Nothing
                                            )
                                        |> List.head
                                        |> Maybe.map
                                            (\declaration ->
                                                case declaration of
                                                    AliasDeclaration aliasDeclaration ->
                                                        case
                                                            parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal
                                                                stack
                                                                modules
                                                                ( instantiatedModule, Elm.Syntax.Node.value aliasDeclaration.typeAnnotation )
                                                        of
                                                            Err error ->
                                                                Err
                                                                    ("Failed to parse alias '"
                                                                        ++ Elm.Syntax.Node.value aliasDeclaration.name
                                                                        ++ "' type annotation: "
                                                                        ++ error
                                                                    )

                                                            Ok ( aliasedType, aliasedTypeDeps ) ->
                                                                Ok ( aliasedType, aliasedTypeDeps )

                                                    CustomTypeDeclaration customTypeDeclaration ->
                                                        let
                                                            typeName =
                                                                Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value instantiatedModule.moduleDefinition)
                                                                    ++ [ Elm.Syntax.Node.value customTypeDeclaration.name ]
                                                                    |> String.join "."

                                                            parameters =
                                                                customTypeDeclaration.generics
                                                                    |> List.map Elm.Syntax.Node.value
                                                        in
                                                        if stack.typesToIgnore |> Set.member typeName then
                                                            Ok ( CustomElmType typeName, Dict.empty )

                                                        else
                                                            customTypeDeclaration.constructors
                                                                |> List.map Elm.Syntax.Node.value
                                                                |> List.map
                                                                    (\constructor ->
                                                                        constructor.arguments
                                                                            |> List.map
                                                                                (\constructorArgument ->
                                                                                    parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal
                                                                                        { typesToIgnore = stack.typesToIgnore |> Set.insert typeName }
                                                                                        modules
                                                                                        ( instantiatedModule, Elm.Syntax.Node.value constructorArgument )
                                                                                        |> Result.mapError ((++) "Failed to parse argument: ")
                                                                                )
                                                                            |> Result.Extra.combine
                                                                            |> Result.mapError
                                                                                ((++)
                                                                                    ("Failed to parse constructor '"
                                                                                        ++ Elm.Syntax.Node.value constructor.name
                                                                                        ++ "': "
                                                                                    )
                                                                                )
                                                                            |> Result.map listTupleSecondDictUnion
                                                                            |> Result.map
                                                                                (\( argumentsTypes, argumentsDeps ) ->
                                                                                    ( ( Elm.Syntax.Node.value constructor.name
                                                                                      , argumentsTypes
                                                                                      )
                                                                                    , argumentsDeps
                                                                                    )
                                                                                )
                                                                    )
                                                                |> Result.Extra.combine
                                                                |> Result.map listTupleSecondDictUnion
                                                                |> Result.map
                                                                    (\( constructors, constructorsDeps ) ->
                                                                        ( CustomElmType typeName
                                                                        , constructorsDeps
                                                                            |> Dict.insert
                                                                                typeName
                                                                                { parameters = parameters
                                                                                , tags = constructors |> Dict.fromList
                                                                                }
                                                                        )
                                                                    )
                                            )
                                        |> Maybe.withDefault
                                            (Err
                                                ("Did not find declaration of '"
                                                    ++ instantiatedLocalName
                                                    ++ "' in module '"
                                                    ++ String.join "." instantiatedModuleAlias
                                                    ++ "'"
                                                )
                                            )
            in
            instantiatedResult
                |> Result.andThen
                    (\( instantiatedType, instantiatedDependencies ) ->
                        argumentsNodes
                            |> List.map (Elm.Syntax.Node.value >> Tuple.pair currentModule)
                            |> List.map (parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal stack modules)
                            |> Result.Extra.combine
                            |> Result.map listTupleSecondDictUnion
                            |> Result.map
                                (\( instanceArguments, instanceArgumentsDeps ) ->
                                    if instanceArguments == [] then
                                        ( instantiatedType, instantiatedDependencies )

                                    else
                                        ( InstanceElmType
                                            { instantiated = instantiatedType
                                            , arguments = instanceArguments
                                            }
                                        , instanceArgumentsDeps |> Dict.union instantiatedDependencies
                                        )
                                )
                    )

        Elm.Syntax.TypeAnnotation.Record fieldsNodes ->
            fieldsNodes
                |> List.map Elm.Syntax.Node.value
                |> List.map
                    (\( fieldNameNode, fieldAnnotation ) ->
                        case
                            parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal
                                stack
                                modules
                                ( currentModule, Elm.Syntax.Node.value fieldAnnotation )
                        of
                            Err error ->
                                Err
                                    ("Failed to parse annotation of field '"
                                        ++ Elm.Syntax.Node.value fieldNameNode
                                        ++ "': "
                                        ++ error
                                    )

                            Ok ( fieldType, fieldTypeDeps ) ->
                                Ok ( ( Elm.Syntax.Node.value fieldNameNode, fieldType ), fieldTypeDeps )
                    )
                |> Result.Extra.combine
                |> Result.map listTupleSecondDictUnion
                |> Result.map
                    (\( fields, fieldsDependencies ) -> ( RecordElmType { fields = fields }, fieldsDependencies ))

        Elm.Syntax.TypeAnnotation.Tupled tupled ->
            tupled
                |> List.map (Elm.Syntax.Node.value >> Tuple.pair currentModule)
                |> List.map (parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal stack modules)
                |> Result.Extra.combine
                |> Result.map listTupleSecondDictUnion
                |> Result.map
                    (\( items, itemsDependencies ) -> ( TupleElmType items, itemsDependencies ))

        Elm.Syntax.TypeAnnotation.GenericType genericName ->
            Ok ( GenericType genericName, Dict.empty )

        Elm.Syntax.TypeAnnotation.Unit ->
            Ok ( UnitType, Dict.empty )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation _ _ ->
            Err "FunctionTypeAnnotation not implemented"

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
            Err "GenericRecord not implemented"


jsonCodingExpressionFromType :
    { encodeValueExpression : String, typeArgLocalName : String }
    -> ( ElmTypeAnnotation, List ElmTypeAnnotation )
    -> { encodeExpression : String, decodeExpression : String }
jsonCodingExpressionFromType { encodeValueExpression, typeArgLocalName } ( typeAnnotation, typeArguments ) =
    let
        typeArgumentsExpressions =
            typeArguments
                |> List.map
                    (\typeArgument ->
                        let
                            typeArgumentExpressions =
                                jsonCodingExpressionFromType
                                    { encodeValueExpression = typeArgLocalName
                                    , typeArgLocalName = typeArgLocalName ++ "_"
                                    }
                                    ( typeArgument, [] )

                            decode =
                                if String.contains " " typeArgumentExpressions.decodeExpression then
                                    "(" ++ typeArgumentExpressions.decodeExpression ++ ")"

                                else
                                    typeArgumentExpressions.decodeExpression
                        in
                        { encode =
                            "(\\"
                                ++ typeArgLocalName
                                ++ " -> "
                                ++ typeArgumentExpressions.encodeExpression
                                ++ ")"
                        , decode = decode
                        }
                    )

        typeArgumentsEncodeExpressionsText =
            typeArgumentsExpressions
                |> List.map .encode
                |> String.join " "

        typeArgumentsDecodeExpressionsText =
            typeArgumentsExpressions
                |> List.map .decode
                |> String.join " "

        continueWithAtomInJsonCore atomName =
            { encodeExpression =
                [ "Json.Encode." ++ atomName
                , typeArgumentsEncodeExpressionsText
                , encodeValueExpression
                ]
                    |> List.filter (String.isEmpty >> not)
                    |> String.join " "
            , decodeExpression =
                String.trim ("Json.Decode." ++ atomName ++ " " ++ typeArgumentsDecodeExpressionsText)
            }

        continueWithLocalNameAndCommonPrefix localName =
            { encodeExpression =
                [ jsonEncodeFunctionNamePrefix ++ localName
                , typeArgumentsEncodeExpressionsText
                , encodeValueExpression
                ]
                    |> List.filter (String.isEmpty >> not)
                    |> String.join " "
            , decodeExpression =
                String.trim (jsonDecodeFunctionNamePrefix ++ localName ++ " " ++ typeArgumentsDecodeExpressionsText)
            }
    in
    case typeAnnotation of
        CustomElmType custom ->
            let
                typeNameRepresentation =
                    jsonCodingFunctionNameCommonPartFromTypeName custom
            in
            { encodeExpression =
                [ jsonEncodeFunctionNamePrefix ++ typeNameRepresentation
                , typeArgumentsEncodeExpressionsText
                , encodeValueExpression
                ]
                    |> List.filter (String.isEmpty >> not)
                    |> String.join " "
            , decodeExpression =
                String.trim
                    (jsonDecodeFunctionNamePrefix
                        ++ typeNameRepresentation
                        ++ " "
                        ++ typeArgumentsDecodeExpressionsText
                    )
            }

        RecordElmType record ->
            if record.fields == [] then
                { encodeExpression = "Json.Encode.object []"
                , decodeExpression = "Json.Decode.succeed {}"
                }

            else
                let
                    fieldsExpressions =
                        record.fields
                            |> List.map
                                (\( fieldName, fieldType ) ->
                                    let
                                        fieldExpression =
                                            jsonCodingExpressionFromType
                                                { encodeValueExpression = encodeValueExpression ++ "." ++ fieldName
                                                , typeArgLocalName = typeArgLocalName
                                                }
                                                ( fieldType, [] )
                                    in
                                    { fieldName = fieldName
                                    , encode =
                                        "( \""
                                            ++ fieldName
                                            ++ "\"\n  , "
                                            ++ String.trimLeft (indentElmCodeLines 1 fieldExpression.encodeExpression)
                                            ++ "\n  )"
                                    , decode = fieldExpression.decodeExpression
                                    }
                                )

                    decodeMap =
                        "(\\"
                            ++ (record.fields |> List.map Tuple.first |> String.join " ")
                            ++ " -> { "
                            ++ (record.fields
                                    |> List.map (\( fieldName, _ ) -> fieldName ++ " = " ++ fieldName)
                                    |> String.join ", "
                               )
                            ++ " })"

                    decodeFieldsLines =
                        fieldsExpressions
                            |> List.map
                                (\field ->
                                    let
                                        fieldDecode =
                                            if field.decode |> String.contains " " then
                                                "( " ++ field.decode ++ "\n)"

                                            else
                                                field.decode

                                        fieldMapLines =
                                            [ "( Json.Decode.field \"" ++ field.fieldName ++ "\""
                                            , indentElmCodeLines 1 fieldDecode
                                            , ")"
                                            ]
                                    in
                                    "|> jsonDecode_andMap\n"
                                        ++ (fieldMapLines |> String.join "\n" |> indentElmCodeLines 1)
                                )

                    encodeListExpression =
                        "[ "
                            ++ (fieldsExpressions |> List.map .encode |> String.join "\n, ")
                            ++ "\n]"
                in
                { encodeExpression =
                    [ "Json.Encode.object"
                    , indentElmCodeLines 1 encodeListExpression
                    ]
                        |> String.join "\n"
                , decodeExpression =
                    "Json.Decode.succeed "
                        ++ decodeMap
                        ++ "\n"
                        ++ (decodeFieldsLines |> String.join "\n" |> indentElmCodeLines 1)
                }

        InstanceElmType instance ->
            jsonCodingExpressionFromType
                { encodeValueExpression = encodeValueExpression, typeArgLocalName = typeArgLocalName }
                ( instance.instantiated, instance.arguments )

        TupleElmType tuple ->
            let
                itemsNames =
                    List.range 0 (List.length tuple - 1)
                        |> List.map (String.fromInt >> (++) "item_")

                decodeMap =
                    "(\\" ++ (itemsNames |> String.join " ") ++ " -> ( " ++ (itemsNames |> String.join ", ") ++ " ))"

                getItemFunctionFromIndex itemIndex =
                    "(\\( " ++ (itemsNames |> String.join ", ") ++ " ) -> item_" ++ String.fromInt itemIndex ++ ")"

                itemsExpressions =
                    tuple
                        |> List.indexedMap
                            (\i itemType ->
                                let
                                    localName =
                                        "item_" ++ String.fromInt i

                                    getItemFunction =
                                        getItemFunctionFromIndex i

                                    itemExpression =
                                        jsonCodingExpressionFromType
                                            { encodeValueExpression =
                                                "("
                                                    ++ getItemFunction
                                                    ++ " "
                                                    ++ encodeValueExpression
                                                    ++ ")"
                                            , typeArgLocalName = typeArgLocalName
                                            }
                                            ( itemType, [] )

                                    itemDecodeExpr =
                                        if String.contains " " itemExpression.decodeExpression then
                                            "(" ++ itemExpression.decodeExpression ++ ")"

                                        else
                                            itemExpression.decodeExpression
                                in
                                { localName = localName
                                , encode = itemExpression.encodeExpression
                                , decode = "(Json.Decode.index " ++ String.fromInt i ++ " " ++ itemDecodeExpr ++ ")"
                                }
                            )

                encodeListExpression =
                    "[ "
                        ++ (itemsExpressions |> List.map .encode |> String.join "\n, ")
                        ++ "\n]"
            in
            { encodeExpression =
                "Json.Encode.list identity\n"
                    ++ indentElmCodeLines 1 encodeListExpression
            , decodeExpression =
                "Json.Decode.map"
                    ++ String.fromInt (List.length tuple)
                    ++ " "
                    ++ decodeMap
                    ++ "\n"
                    ++ (itemsExpressions |> List.map .decode |> String.join "\n" |> indentElmCodeLines 1)
            }

        GenericType name ->
            let
                functionsNames =
                    jsonCodingFunctionNameFromTypeParameterName name
            in
            { encodeExpression = functionsNames.encodeName ++ " " ++ encodeValueExpression
            , decodeExpression = functionsNames.decodeName
            }

        UnitType ->
            { encodeExpression = "Json.Encode.list (always (Json.Encode.object [])) []"
            , decodeExpression = "Json.Decode.succeed ()"
            }

        LeafElmType leaf ->
            case leaf of
                StringLeaf ->
                    continueWithAtomInJsonCore "string"

                IntLeaf ->
                    continueWithAtomInJsonCore "int"

                BoolLeaf ->
                    continueWithAtomInJsonCore "bool"

                FloatLeaf ->
                    continueWithAtomInJsonCore "float"

                BytesLeaf ->
                    { encodeExpression = "json_encode_Bytes " ++ encodeValueExpression
                    , decodeExpression = "json_decode_Bytes"
                    }

                ListLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonCodeListFunctionNameCommonPart

                SetLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonCodeSetFunctionNameCommonPart

                ResultLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonCodeResultFunctionNameCommonPart

                MaybeLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonCodeMaybeFunctionNameCommonPart

                DictLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonCodeDictFunctionNameCommonPart


jsonCodingFunctionFromCustomType :
    { customTypeName : String, encodeValueExpression : String, typeArgLocalName : String }
    -> ElmCustomTypeStruct
    -> { encodeFunction : { name : String, text : String }, decodeFunction : { name : String, text : String } }
jsonCodingFunctionFromCustomType { customTypeName, encodeValueExpression, typeArgLocalName } customType =
    let
        encodeParametersText =
            customType.parameters
                |> List.map (jsonCodingFunctionNameFromTypeParameterName >> .encodeName)
                |> String.join " "

        decodeParametersText =
            customType.parameters
                |> List.map (jsonCodingFunctionNameFromTypeParameterName >> .decodeName)
                |> String.join " "

        moduleName =
            moduleNameFromTypeName customTypeName

        typeNameRepresentation =
            jsonCodingFunctionNameCommonPartFromTypeName customTypeName

        tagsExpressions =
            customType.tags
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map
                    (\( tagName, tagParameters ) ->
                        let
                            tagParametersExpressions =
                                tagParameters
                                    |> List.indexedMap
                                        (\i tagParamType ->
                                            let
                                                argumentLocalName =
                                                    "tagArgument" ++ String.fromInt i

                                                tagParamExpr =
                                                    jsonCodingExpressionFromType
                                                        { encodeValueExpression = argumentLocalName
                                                        , typeArgLocalName = typeArgLocalName
                                                        }
                                                        ( tagParamType, [] )
                                            in
                                            { localName = argumentLocalName
                                            , encode = tagParamExpr.encodeExpression
                                            , decode = tagParamExpr.decodeExpression
                                            }
                                        )

                            decodeInField =
                                if tagParametersExpressions == [] then
                                    "jsonDecodeSucceedWhenNotNull " ++ moduleName ++ "." ++ tagName

                                else
                                    "Json.Decode.lazy (\\_ -> Json.Decode.map"
                                        ++ (if List.length tagParametersExpressions == 1 then
                                                ""

                                            else
                                                String.fromInt (List.length tagParametersExpressions)
                                           )
                                        ++ " "
                                        ++ moduleName
                                        ++ "."
                                        ++ tagName
                                        ++ " "
                                        ++ (tagParametersExpressions
                                                |> List.indexedMap
                                                    (\i tagParamExpr ->
                                                        let
                                                            tagParamDecodeExpr =
                                                                if String.contains " " tagParamExpr.decode then
                                                                    "(" ++ tagParamExpr.decode ++ ")"

                                                                else
                                                                    tagParamExpr.decode
                                                        in
                                                        "(Json.Decode.index "
                                                            ++ String.fromInt i
                                                            ++ " "
                                                            ++ tagParamDecodeExpr
                                                            ++ ")"
                                                    )
                                                |> String.join " "
                                           )
                                        ++ ")"

                            encodeArguments =
                                tagParametersExpressions
                                    |> List.map .localName
                                    |> String.join " "

                            encodeFirstLine =
                                [ moduleName ++ "." ++ tagName
                                , encodeArguments
                                , "->"
                                ]
                                    |> List.filter (String.isEmpty >> not)
                                    |> String.join " "

                            encodeSecondLine =
                                "Json.Encode.object [ ( \""
                                    ++ tagName
                                    ++ "\", Json.Encode.list identity ["
                                    ++ (if tagParametersExpressions == [] then
                                            ""

                                        else
                                            " " ++ (tagParametersExpressions |> List.map .encode |> String.join ", ") ++ " "
                                       )
                                    ++ "] ) ]"
                        in
                        { encode =
                            [ encodeFirstLine
                            , indentElmCodeLines 1 encodeSecondLine
                            ]
                                |> String.join "\n"
                        , decode = "Json.Decode.field \"" ++ tagName ++ "\" (" ++ decodeInField ++ ")"
                        }
                    )

        encodeListExpression =
            tagsExpressions |> List.map .encode |> String.join "\n"

        encodeExpression =
            [ "case " ++ encodeValueExpression ++ " of"
            , indentElmCodeLines 1 encodeListExpression
            ]
                |> String.join "\n"

        decodeListExpression =
            "[ "
                ++ (tagsExpressions |> List.map .decode |> String.join "\n, ")
                ++ "\n]"

        decodeExpression =
            [ "Json.Decode.oneOf"
            , indentElmCodeLines 1 decodeListExpression
            ]
                |> String.join "\n"

        encodeFunctionName =
            jsonEncodeFunctionNamePrefix ++ typeNameRepresentation

        decodeFunctionName =
            jsonDecodeFunctionNamePrefix ++ typeNameRepresentation
    in
    { encodeFunction =
        { name = encodeFunctionName
        , text =
            [ [ encodeFunctionName
              , encodeParametersText
              , encodeValueExpression
              , "="
              ]
                |> List.filter (String.isEmpty >> not)
                |> String.join " "
            , indentElmCodeLines 1 encodeExpression
            ]
                |> String.join "\n"
        }
    , decodeFunction =
        { name = decodeFunctionName
        , text =
            [ [ decodeFunctionName
              , decodeParametersText
              , "="
              ]
                |> List.filter (String.isEmpty >> not)
                |> String.join " "
            , indentElmCodeLines 1 decodeExpression
            ]
                |> String.join "\n"
        }
    }


jsonCodingFunctionNameFromTypeParameterName : String -> { encodeName : String, decodeName : String }
jsonCodingFunctionNameFromTypeParameterName paramName =
    { encodeName = jsonEncodeFunctionNamePrefix ++ "type_parameter_" ++ paramName
    , decodeName = jsonDecodeFunctionNamePrefix ++ "type_parameter_" ++ paramName
    }


jsonCodingFunctionNameCommonPartFromTypeName : String -> String
jsonCodingFunctionNameCommonPartFromTypeName =
    String.toList
        >> List.map
            (\char ->
                if Char.isAlphaNum char then
                    char

                else
                    '_'
            )
        >> String.fromList


moduleNameFromTypeName : String -> String
moduleNameFromTypeName =
    String.split "."
        >> List.reverse
        >> List.drop 1
        >> List.reverse
        >> String.join "."


buildTypeAnnotationText : ElmTypeAnnotation -> String
buildTypeAnnotationText typeAnnotation =
    case typeAnnotation of
        CustomElmType custom ->
            custom

        RecordElmType { fields } ->
            "{ "
                ++ (fields |> List.map (\( fieldName, fieldType ) -> fieldName ++ " : " ++ buildTypeAnnotationText fieldType) |> String.join ", ")
                ++ " }"

        InstanceElmType instance ->
            "( "
                ++ (instance.instantiated
                        :: instance.arguments
                        |> List.map buildTypeAnnotationText
                        |> String.join " "
                   )
                ++ " )"

        TupleElmType tuple ->
            "(" ++ (tuple |> List.map buildTypeAnnotationText |> String.join ", ") ++ ")"

        GenericType name ->
            name

        UnitType ->
            "()"

        LeafElmType leaf ->
            case leaf of
                StringLeaf ->
                    "String"

                IntLeaf ->
                    "Int"

                BoolLeaf ->
                    "Bool"

                FloatLeaf ->
                    "Float"

                BytesLeaf ->
                    "Bytes.Bytes"

                ListLeaf ->
                    "List"

                SetLeaf ->
                    "Set.Set"

                ResultLeaf ->
                    "Result"

                MaybeLeaf ->
                    "Maybe"

                DictLeaf ->
                    "Dict.Dict"


parseElmTypeLeavesNames : Dict.Dict String LeafElmTypeStruct
parseElmTypeLeavesNames =
    [ ( "String", StringLeaf )
    , ( "Int", IntLeaf )
    , ( "Bool", BoolLeaf )
    , ( "Float", FloatLeaf )
    , ( "Bytes.Bytes", BytesLeaf )
    , ( "List", ListLeaf )
    , ( "Set.Set", SetLeaf )
    , ( "Result", ResultLeaf )
    , ( "Maybe", MaybeLeaf )
    , ( "Dict.Dict", DictLeaf )
    ]
        |> Dict.fromList


listTupleSecondDictUnion : List ( a, Dict.Dict comparable v ) -> ( List a, Dict.Dict comparable v )
listTupleSecondDictUnion list =
    ( list |> List.map Tuple.first
    , list |> List.map Tuple.second |> List.foldl Dict.union Dict.empty
    )


type Declaration
    = AliasDeclaration Elm.Syntax.TypeAlias.TypeAlias
    | CustomTypeDeclaration Elm.Syntax.Type.Type


generalSupportingFunctionsTexts : List { functionName : String, functionText : String }
generalSupportingFunctionsTexts =
    (generalSupportingFunctionsTextsWithCommonNamePattern
        |> List.concatMap
            (\generalSupportingFunction ->
                [ { name =
                        jsonEncodeFunctionNamePrefix
                            ++ generalSupportingFunction.functionNameCommonPart
                  , afterName = generalSupportingFunction.encodeSyntax
                  }
                , { name =
                        jsonDecodeFunctionNamePrefix
                            ++ generalSupportingFunction.functionNameCommonPart
                  , afterName = generalSupportingFunction.decodeSyntax
                  }
                ]
            )
        |> List.map
            (\function ->
                { functionName = function.name
                , functionText = function.name ++ " " ++ function.afterName
                }
            )
    )
        ++ [ { functionName = "jsonDecode_andMap"
             , functionText = """
jsonDecode_andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
jsonDecode_andMap =
    Json.Decode.map2 (|>)"""
             }
           , { functionName = "json_encode_Bytes"
             , functionText = """
json_encode_Bytes : Bytes.Bytes -> Json.Encode.Value
json_encode_Bytes bytes =
    [ ( "AsBase64", bytes |> Base64.fromBytes |> Maybe.withDefault "Error encoding to base64" |> Json.Encode.string ) ]
        |> Json.Encode.object"""
             }
           , { functionName = "json_decode_Bytes"
             , functionText = """
json_decode_Bytes : Json.Decode.Decoder Bytes.Bytes
json_decode_Bytes =
    Json.Decode.field "AsBase64"
        (Json.Decode.string
            |> Json.Decode.andThen
                (Base64.toBytes >> Maybe.map Json.Decode.succeed >> Maybe.withDefault (Json.Decode.fail "Failed to decode base64."))
        )
              """
             }
           , { functionName = "jsonDecodeSucceedWhenNotNull"
             , functionText = """
jsonDecodeSucceedWhenNotNull : a -> Json.Decode.Decoder a
jsonDecodeSucceedWhenNotNull valueIfNotNull =
    Json.Decode.value
        |> Json.Decode.andThen
            (\\asValue ->
                if asValue == Json.Encode.null then
                    Json.Decode.fail "Is null."

                else
                    Json.Decode.succeed valueIfNotNull
            )"""
             }
           ]


generalSupportingFunctionsTextsWithCommonNamePattern :
    List
        { functionNameCommonPart : String
        , encodeSyntax : String
        , decodeSyntax : String
        }
generalSupportingFunctionsTextsWithCommonNamePattern =
    [ { functionNameCommonPart = jsonCodeMaybeFunctionNameCommonPart
      , encodeSyntax = """encodeJust valueToEncode =
    case valueToEncode of
        Nothing ->
            [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object

        Just just ->
            [ ( "Just", [ just ] |> Json.Encode.list encodeJust ) ] |> Json.Encode.object
"""
      , decodeSyntax = """decoder =
    Json.Decode.oneOf
        [ Json.Decode.field "Nothing" (Json.Decode.succeed Nothing)
        , Json.Decode.field "Just" ((Json.Decode.index 0 decoder) |> Json.Decode.map Just)
        , Json.Decode.field "Just" (decoder |> Json.Decode.map Just) -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.null Nothing -- Temporary backwardscompatibility: Map 'null' to Nothing
        ]
     """
      }
    , { functionNameCommonPart = jsonCodeListFunctionNameCommonPart
      , encodeSyntax = """ = Json.Encode.list"""
      , decodeSyntax = """ = Json.Decode.list"""
      }
    , { functionNameCommonPart = jsonCodeSetFunctionNameCommonPart
      , encodeSyntax = """encoder =
    Set.toList >> Json.Encode.list encoder"""
      , decodeSyntax = """decoder =
    Json.Decode.list decoder |> Json.Decode.map Set.fromList"""
      }
    , { functionNameCommonPart = jsonCodeDictFunctionNameCommonPart
      , encodeSyntax = """encodeKey encodeValue =
    Dict.toList >> Json.Encode.list (""" ++ jsonEncodeFunctionNamePrefix ++ jsonCodeTupleFunctionNameCommonPart ++ "2 encodeKey encodeValue)"
      , decodeSyntax = """decodeKey decodeValue =
        (Json.Decode.list (""" ++ jsonDecodeFunctionNamePrefix ++ jsonCodeTupleFunctionNameCommonPart ++ """2 decodeKey decodeValue))
            |> Json.Decode.map Dict.fromList"""
      }
    , { functionNameCommonPart = jsonCodeResultFunctionNameCommonPart
      , encodeSyntax = """encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( "Err", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( "Ok", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object"""
      , decodeSyntax = """decodeErr decodeOk =
    Json.Decode.oneOf
        [ Json.Decode.field "Err" (Json.Decode.index 0 decodeErr) |> Json.Decode.map Err
        , Json.Decode.field "Ok" (Json.Decode.index 0 decodeOk) |> Json.Decode.map Ok
        , Json.Decode.field "Err" decodeErr |> Json.Decode.map Err -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.field "Ok" decodeOk |> Json.Decode.map Ok -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        ]"""
      }
    , { functionNameCommonPart = jsonCodeTupleFunctionNameCommonPart ++ "2"
      , encodeSyntax = """encodeA encodeB ( a, b ) =
    [ a |> encodeA, b |> encodeB ]
        |> Json.Encode.list identity"""
      , decodeSyntax = """decodeA decodeB =
    Json.Decode.map2 (\\a b -> ( a, b ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)"""
      }
    , { functionNameCommonPart = jsonCodeTupleFunctionNameCommonPart ++ "3"
      , encodeSyntax = """encodeA encodeB encodeC ( a, b, c ) =
    [ a |> encodeA, b |> encodeB, c |> encodeC ]
        |> Json.Encode.list identity"""
      , decodeSyntax = """decodeA decodeB decodeC =
    Json.Decode.map3 (\\a b c -> ( a, b, c ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)
        (Json.Decode.index 2 decodeC)"""
      }
    ]


jsonCodeMaybeFunctionNameCommonPart : String
jsonCodeMaybeFunctionNameCommonPart =
    "_generic_Maybe"


jsonCodeListFunctionNameCommonPart : String
jsonCodeListFunctionNameCommonPart =
    "_generic_List"


jsonCodeSetFunctionNameCommonPart : String
jsonCodeSetFunctionNameCommonPart =
    "_generic_Set"


jsonCodeDictFunctionNameCommonPart : String
jsonCodeDictFunctionNameCommonPart =
    "_generic_Dict"


jsonCodeResultFunctionNameCommonPart : String
jsonCodeResultFunctionNameCommonPart =
    "_generic_Result"


jsonCodeTupleFunctionNameCommonPart : String
jsonCodeTupleFunctionNameCommonPart =
    "_tuple_"


jsonEncodeParamName : String
jsonEncodeParamName =
    "valueToEncode"


jsonEncodeFunctionNamePrefix : String
jsonEncodeFunctionNamePrefix =
    "jsonEncode_"


jsonDecodeFunctionNamePrefix : String
jsonDecodeFunctionNamePrefix =
    "jsonDecode_"


elmModulesDictFromAppFiles : AppFiles -> Dict.Dict String ( String, Elm.Syntax.File.File )
elmModulesDictFromAppFiles =
    Dict.toList
        >> List.filter
            (Tuple.first
                >> List.reverse
                >> List.head
                >> Maybe.map (String.toLower >> String.endsWith ".elm")
                >> Maybe.withDefault False
            )
        >> List.filterMap (Tuple.second >> stringFromFileContent)
        >> elmModulesDictFromFilesTexts


elmModulesDictFromFilesTexts : List String -> Dict.Dict String ( String, Elm.Syntax.File.File )
elmModulesDictFromFilesTexts =
    List.filterMap
        (\fileText ->
            case parseElmModuleText fileText of
                Err _ ->
                    Nothing

                Ok elmFile ->
                    Just
                        ( String.join "." (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value elmFile.moduleDefinition))
                        , ( fileText, elmFile )
                        )
        )
        >> Dict.fromList


parseJsonCodingFunctionType :
    String
    -> Elm.Syntax.Signature.Signature
    -> Result String { typeCanonicalName : String, isDecoder : Bool, typeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation }
parseJsonCodingFunctionType moduleText signature =
    let
        errorValue detail =
            Err
                ("Unexpected type signature ("
                    ++ detail
                    ++ "): "
                    ++ (Elm.Syntax.Node.value signature.typeAnnotation
                            |> Elm.Syntax.TypeAnnotation.encode
                            |> Json.Encode.encode 0
                       )
                )
    in
    case Elm.Syntax.Node.value signature.typeAnnotation of
        Elm.Syntax.TypeAnnotation.Typed genericType typeArguments ->
            if Tuple.second (Elm.Syntax.Node.value genericType) /= "Decoder" then
                errorValue "Generic type is not 'Decoder'"

            else
                case typeArguments of
                    [ singleTypeArgument ] ->
                        Ok
                            { typeCanonicalName =
                                getTextLinesFromRange
                                    (Elm.Syntax.Node.range singleTypeArgument)
                                    moduleText
                                    |> String.join " "
                            , isDecoder = True
                            , typeAnnotation = Elm.Syntax.Node.value singleTypeArgument
                            }

                    _ ->
                        errorValue "Unexpected number of type arguments for 'Decoder'"

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation leftType rightType ->
            case Elm.Syntax.Node.value rightType of
                Elm.Syntax.TypeAnnotation.Typed rightNameNode _ ->
                    if Tuple.second (Elm.Syntax.Node.value rightNameNode) /= "Value" then
                        errorValue "right side of function type is not 'Value'"

                    else
                        Ok
                            { typeCanonicalName =
                                getTextLinesFromRange
                                    (Elm.Syntax.Node.range leftType)
                                    moduleText
                                    |> String.join " "
                            , isDecoder = False
                            , typeAnnotation = Elm.Syntax.Node.value leftType
                            }

                _ ->
                    errorValue "right side of function type"

        _ ->
            errorValue ""


replaceFunctionInSourceFilesModuleText : AppFiles -> { functionName : String } -> String -> Result String String
replaceFunctionInSourceFilesModuleText sourceFiles { functionName } moduleText =
    moduleText
        |> getDeclarationFromFunctionNameAndElmModuleText { functionName = functionName }
        |> Result.andThen (Maybe.map Ok >> Maybe.withDefault (Err ("Did not find the function '" ++ functionName ++ "'")))
        |> Result.andThen
            (\functionDeclaration ->
                case parseSourceFileFunctionName functionName of
                    Err error ->
                        Err ("Failed to parse function name: " ++ error)

                    Ok { filePathRepresentation, base64 } ->
                        case findFileWithPathMatchingRepresentationInFunctionName sourceFiles filePathRepresentation of
                            Err error ->
                                Err ("Failed to identify file for '" ++ filePathRepresentation ++ "': " ++ error)

                            Ok ( _, fileContent ) ->
                                let
                                    functionLines =
                                        moduleText |> getTextLinesFromRange (Elm.Syntax.Node.range functionDeclaration)

                                    fileExpression =
                                        buildBytesElmExpression { encodeAsBase64 = base64 } fileContent

                                    newFunctionLines =
                                        List.take 2 functionLines ++ [ indentElmCodeLines 1 fileExpression ]
                                in
                                addOrUpdateFunctionInElmModuleText
                                    { functionName = functionName, mapFunctionLines = always newFunctionLines }
                                    moduleText
            )


prepareReplaceFunctionInElmMakeModuleText :
    List ( DependencyKey, Bytes.Bytes )
    -> AppFiles
    -> { functionName : String }
    -> Result (List CompilationError) { valueFunctionText : String, updateInterfaceModuleText : { generatedModuleName : String } -> String -> Result (List CompilationError) String }
prepareReplaceFunctionInElmMakeModuleText dependencies sourceFiles { functionName } =
    case parseElmMakeModuleFunctionName functionName of
        Err error ->
            Err [ OtherCompilationError ("Failed to parse function name: " ++ error) ]

        Ok { filePathRepresentation, base64, outputType, enableDebug } ->
            case findFileWithPathMatchingRepresentationInFunctionName sourceFiles filePathRepresentation of
                Err error ->
                    Err [ OtherCompilationError ("Failed to identify file for '" ++ filePathRepresentation ++ "': " ++ error) ]

                Ok ( entryPointFilePath, _ ) ->
                    let
                        sourceFilesForElmMake =
                            sourceFiles
                                |> Dict.filter (\filePath _ -> includeFilePathInElmMakeRequest filePath)

                        elmMakeRequest =
                            { files = sourceFilesForElmMake
                            , entryPointFilePath = entryPointFilePath
                            , outputType = outputType
                            , enableDebug = enableDebug
                            }

                        dependencyKey =
                            ElmMakeDependency elmMakeRequest
                    in
                    case
                        dependencies
                            |> List.filter (Tuple.first >> dependencyKeysAreEqual dependencyKey)
                            |> List.head
                    of
                        Nothing ->
                            Err [ MissingDependencyError dependencyKey ]

                        Just ( _, dependencyValue ) ->
                            let
                                valueFunctionName =
                                    "bytes_as_base64_" ++ SHA256.toHex (SHA256.fromBytes dependencyValue)

                                valueFunctionText =
                                    valueFunctionName
                                        ++ " =\n"
                                        ++ indentElmCodeLines 1 (buildBytesElmExpression { encodeAsBase64 = True } dependencyValue)
                            in
                            Ok
                                { valueFunctionText = valueFunctionText
                                , updateInterfaceModuleText =
                                    \{ generatedModuleName } moduleText ->
                                        moduleText
                                            |> getDeclarationFromFunctionNameAndElmModuleText { functionName = functionName }
                                            |> Result.andThen (Maybe.map Ok >> Maybe.withDefault (Err ("Did not find the function '" ++ functionName ++ "'")))
                                            |> Result.mapError (OtherCompilationError >> List.singleton)
                                            |> Result.andThen
                                                (\functionDeclaration ->
                                                    let
                                                        functionLines =
                                                            moduleText |> getTextLinesFromRange (Elm.Syntax.Node.range functionDeclaration)

                                                        fileExpression =
                                                            buildBytesElmExpressionFromBase64Expression
                                                                { encodeAsBase64 = base64 }
                                                                (generatedModuleName ++ "." ++ valueFunctionName)

                                                        newFunctionLines =
                                                            List.take 2 functionLines ++ [ indentElmCodeLines 1 fileExpression ]
                                                    in
                                                    addOrUpdateFunctionInElmModuleText
                                                        { functionName = functionName, mapFunctionLines = always newFunctionLines }
                                                        moduleText
                                                        |> Result.mapError (OtherCompilationError >> List.singleton)
                                                )
                                }


buildBytesElmExpression : { encodeAsBase64 : Bool } -> Bytes.Bytes -> String
buildBytesElmExpression { encodeAsBase64 } bytes =
    let
        base64Expression =
            "\"" ++ (bytes |> Base64.fromBytes |> Maybe.withDefault "Error encoding to base64") ++ "\""
    in
    buildBytesElmExpressionFromBase64Expression
        { encodeAsBase64 = encodeAsBase64 }
        base64Expression


buildBytesElmExpressionFromBase64Expression : { encodeAsBase64 : Bool } -> String -> String
buildBytesElmExpressionFromBase64Expression { encodeAsBase64 } base64Expression =
    if encodeAsBase64 then
        base64Expression

    else
        [ base64Expression
        , "|> Base64.toBytes"
        , "|> Maybe.withDefault (\"Failed to convert from base64\" |> Bytes.Encode.string |> Bytes.Encode.encode)"
        ]
            |> String.join "\n"


includeFilePathInElmMakeRequest : List String -> Bool
includeFilePathInElmMakeRequest path =
    case List.head (List.reverse path) of
        Nothing ->
            False

        Just fileName ->
            (fileName == "elm.json")
                || String.endsWith ".elm" fileName


getDeclarationFromFunctionNameAndElmModuleText : { functionName : String } -> String -> Result String (Maybe (Elm.Syntax.Node.Node Elm.Syntax.Expression.Function))
getDeclarationFromFunctionNameAndElmModuleText { functionName } =
    parseAndMapElmModuleText
        (\parsedModule ->
            parsedModule.declarations
                |> List.filterMap
                    (\declaration ->
                        case Elm.Syntax.Node.value declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                Just (Elm.Syntax.Node.Node (Elm.Syntax.Node.range declaration) functionDeclaration)

                            _ ->
                                Nothing
                    )
                |> List.filter
                    (Elm.Syntax.Node.value
                        >> .declaration
                        >> Elm.Syntax.Node.value
                        >> .name
                        >> Elm.Syntax.Node.value
                        >> (==) functionName
                    )
                |> List.head
                |> Ok
        )


getTextLinesFromRange : Elm.Syntax.Range.Range -> String -> List String
getTextLinesFromRange range text =
    let
        lines =
            String.lines text
    in
    lines
        |> List.take range.end.row
        |> List.drop (range.start.row - 1)
        |> List.reverse
        |> listMapFirstElement (String.left (range.end.column - 1))
        |> List.reverse
        |> listMapFirstElement (String.dropLeft (range.start.column - 1))


listMapFirstElement : (a -> a) -> List a -> List a
listMapFirstElement mapElement list =
    case list of
        firstElement :: followingElements ->
            mapElement firstElement :: followingElements

        _ ->
            list


indentElmCodeLines : Int -> String -> String
indentElmCodeLines level =
    String.lines
        >> List.map ((++) (String.repeat level "    "))
        >> String.join "\n"


findFileWithPathMatchingRepresentationInFunctionName : AppFiles -> String -> Result String ( List String, Bytes.Bytes )
findFileWithPathMatchingRepresentationInFunctionName sourceFiles pathPattern =
    let
        matchingFiles =
            sourceFiles
                |> Dict.toList
                |> List.filter (Tuple.first >> pathMatchesPatternFromFunctionName pathPattern)
    in
    case matchingFiles of
        [ matchingFile ] ->
            Ok matchingFile

        [] ->
            Err
                ("Did not find any source file with a path matching the representation '"
                    ++ pathPattern
                    ++ "'. Here is a list of the available files: "
                    ++ String.join ", " (List.map (String.join "/") (Dict.keys sourceFiles))
                )

        _ ->
            Err
                ("The file path representation '"
                    ++ pathPattern
                    ++ "' is not unique because it matches "
                    ++ String.fromInt (List.length matchingFiles)
                    ++ " of the source files: "
                    ++ String.join ", " (List.map (Tuple.first >> String.join "/") matchingFiles)
                )


addOrUpdateFunctionInElmModuleText : { functionName : String, mapFunctionLines : Maybe (List String) -> List String } -> String -> Result String String
addOrUpdateFunctionInElmModuleText { functionName, mapFunctionLines } moduleText =
    moduleText
        |> parseAndMapElmModuleText
            (\parsedModule ->
                case
                    parsedModule.declarations
                        |> List.filter
                            (\declaration ->
                                case Elm.Syntax.Node.value declaration of
                                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                        let
                                            candidateFunctionName =
                                                Elm.Syntax.Node.value
                                                    (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                        in
                                        functionName == candidateFunctionName

                                    _ ->
                                        False
                            )
                        |> List.head
                of
                    Nothing ->
                        Ok (String.join "\n\n\n" [ moduleText, String.join "\n" (mapFunctionLines Nothing) ])

                    Just declaration ->
                        let
                            declarationRange =
                                Elm.Syntax.Node.range declaration

                            originalFunctionLines =
                                getTextLinesFromRange declarationRange moduleText
                        in
                        Ok
                            (replaceRangeInText declarationRange
                                (String.join "\n" (mapFunctionLines (Just originalFunctionLines)))
                                moduleText
                            )
            )


replaceRangeInText : Elm.Syntax.Range.Range -> String -> String -> String
replaceRangeInText range rangeReplacement originalText =
    let
        originalTextLines =
            String.lines originalText

        startLines =
            originalTextLines |> List.take (range.start.row - 1)

        lineStart =
            originalTextLines
                |> List.drop (List.length startLines)
                |> List.head
                |> Maybe.withDefault ""
                |> String.left (range.start.column - 1)
    in
    (originalTextLines |> List.take (range.start.row - 1))
        ++ [ lineStart ++ rangeReplacement ]
        ++ (originalTextLines |> List.drop range.end.row)
        |> String.join "\n"


addImportsInElmModuleText : List (List String) -> String -> Result String String
addImportsInElmModuleText importsModuleName moduleText =
    moduleText
        |> parseAndMapElmModuleText
            (\parsedModule ->
                let
                    moduleTextLines =
                        String.lines moduleText

                    insertionRow =
                        parsedModule.imports
                            |> List.map (Elm.Syntax.Node.range >> .end >> .row)
                            |> List.maximum
                            |> Maybe.withDefault (Elm.Syntax.Node.range parsedModule.moduleDefinition).end.row

                    importStatementsLines =
                        importsModuleName
                            |> List.map (String.join "." >> (++) "import ")
                in
                (List.take insertionRow moduleTextLines
                    ++ importStatementsLines
                    ++ List.drop insertionRow moduleTextLines
                )
                    |> String.join "\n"
                    |> Ok
            )


parseAndMapElmModuleText : (Elm.Syntax.File.File -> Result String ok) -> String -> Result String ok
parseAndMapElmModuleText mapDependingOnParsing moduleText =
    case parseElmModuleText moduleText of
        Err error ->
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText error)

        Ok parsedModule ->
            mapDependingOnParsing parsedModule


{-| Consider restricting the interface of `mapElmModuleWithNameIfExists` to not support arbitrary changes to app code but only addition of expose syntax.
-}
mapElmModuleWithNameIfExists : (String -> err) -> String -> (( AppFiles, String ) -> Result err ( AppFiles, String )) -> AppFiles -> Result err AppFiles
mapElmModuleWithNameIfExists errFromString elmModuleName tryMapModuleText appCode =
    let
        elmModuleFilePath =
            filePathFromElmModuleName elmModuleName
    in
    case Dict.get elmModuleFilePath appCode of
        Nothing ->
            Ok appCode

        Just elmModuleFile ->
            case stringFromFileContent elmModuleFile of
                Nothing ->
                    Err (errFromString "Failed to decode file content as string")

                Just moduleText ->
                    tryMapModuleText ( appCode, moduleText )
                        |> Result.map
                            (\( newAppCode, newModuleText ) ->
                                newAppCode
                                    |> Dict.insert elmModuleFilePath (Bytes.Encode.encode (Bytes.Encode.string newModuleText))
                            )


parseSourceFileFunctionName : String -> Result String { filePathRepresentation : String, base64 : Bool }
parseSourceFileFunctionName functionName =
    parseFlagsAndPathPatternFromFunctionName sourceFileFunctionNameStart functionName
        |> Result.map
            (\( flags, filePathRepresentation ) ->
                { filePathRepresentation = filePathRepresentation
                , base64 = flags |> List.member "base64"
                }
            )


parseElmMakeModuleFunctionName : String -> Result String ParseElmMakeFileNameResult
parseElmMakeModuleFunctionName functionName =
    parseFlagsAndPathPatternFromFunctionName elmMakeFunctionNameStart functionName
        |> Result.map
            (\( flags, filePathRepresentation ) ->
                { filePathRepresentation = filePathRepresentation
                , outputType =
                    if flags |> List.member "javascript" then
                        ElmMakeOutputTypeJs

                    else
                        ElmMakeOutputTypeHtml
                , base64 = flags |> List.member "base64"
                , enableDebug = flags |> List.member "debug"
                }
            )


parseFlagsAndPathPatternFromFunctionName : String -> String -> Result String ( List String, String )
parseFlagsAndPathPatternFromFunctionName requiredPrefix functionName =
    if not (String.startsWith requiredPrefix functionName) then
        Err ("Did not start with expected prefix of '" ++ requiredPrefix ++ "'")

    else
        let
            partAfterPrefix =
                String.dropLeft (String.length requiredPrefix) functionName
        in
        case String.indices functionNameFlagsSeparator partAfterPrefix of
            [] ->
                Err ("Missing separator '" ++ functionNameFlagsSeparator ++ "'")

            separatorIndex :: _ ->
                let
                    flags =
                        String.slice 0 separatorIndex partAfterPrefix
                            |> String.split "__"
                            |> List.filter (String.length >> (<) 0)
                in
                Ok
                    ( flags
                    , String.dropLeft (separatorIndex + String.length functionNameFlagsSeparator) partAfterPrefix
                    )


pathMatchesPatternFromFunctionName : String -> List String -> Bool
pathMatchesPatternFromFunctionName pathPattern path =
    filePathRepresentationInFunctionName path == pathPattern


filePathRepresentationInFunctionName : List String -> String
filePathRepresentationInFunctionName =
    String.join "/"
        >> String.toList
        >> List.map
            (\char ->
                if Char.isAlphaNum char then
                    char

                else
                    '_'
            )
        >> String.fromList


filePathFromElmModuleName : String -> List String
filePathFromElmModuleName elmModuleName =
    case elmModuleName |> String.split "." |> List.reverse of
        [] ->
            []

        moduleLastName :: reversedDirectoryNames ->
            [ "src" ] ++ List.reverse ((moduleLastName ++ ".elm") :: reversedDirectoryNames)


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    Elm.Parser.parse >> Result.map (Elm.Processing.process Elm.Processing.init)


stringFromFileContent : Bytes.Bytes -> Maybe String
stringFromFileContent bytes =
    Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes


fileContentFromString : String -> Bytes.Bytes
fileContentFromString =
    Bytes.Encode.string >> Bytes.Encode.encode


parserDeadEndsToString : String -> List Parser.DeadEnd -> String
parserDeadEndsToString parsedText deadEnds =
    String.concat (List.intersperse "; " (List.map (parserDeadEndToString parsedText) deadEnds))


parserDeadEndToString : String -> Parser.DeadEnd -> String
parserDeadEndToString parsedText deadend =
    parserProblemToString deadend.problem
        ++ " at row "
        ++ String.fromInt deadend.row
        ++ ", col "
        ++ String.fromInt deadend.col
        ++ ": '"
        ++ (parsedText |> String.lines |> List.drop (deadend.row - 2) |> List.take 4 |> String.join "\n")
        ++ "'"


parserProblemToString : Parser.Problem -> String
parserProblemToString p =
    case p of
        Parser.Expecting s ->
            "expecting '" ++ s ++ "'"

        Parser.ExpectingInt ->
            "expecting int"

        Parser.ExpectingHex ->
            "expecting hex"

        Parser.ExpectingOctal ->
            "expecting octal"

        Parser.ExpectingBinary ->
            "expecting binary"

        Parser.ExpectingFloat ->
            "expecting float"

        Parser.ExpectingNumber ->
            "expecting number"

        Parser.ExpectingVariable ->
            "expecting variable"

        Parser.ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        Parser.ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        Parser.ExpectingEnd ->
            "expecting end"

        Parser.UnexpectedChar ->
            "unexpected char"

        Parser.Problem s ->
            "problem " ++ s

        Parser.BadRepeat ->
            "bad repeat"


stringStartsWithLowercaseLetter : String -> Bool
stringStartsWithLowercaseLetter =
    String.toList >> List.head >> Maybe.map Char.isLower >> Maybe.withDefault False


listFoldlToAggregateResult : (a -> b -> Result e b) -> Result e b -> List a -> Result e b
listFoldlToAggregateResult getElementResult =
    List.foldl
        (\element previousAggregateResult ->
            previousAggregateResult |> Result.andThen (\previousAggregate -> getElementResult element previousAggregate)
        )


dependencyKeysAreEqual : DependencyKey -> DependencyKey -> Bool
dependencyKeysAreEqual a b =
    case ( a, b ) of
        ( ElmMakeDependency elmMakeA, ElmMakeDependency elmMakeB ) ->
            elmMakeRequestsAreEqual elmMakeA elmMakeB


elmMakeRequestsAreEqual : ElmMakeRequestStructure -> ElmMakeRequestStructure -> Bool
elmMakeRequestsAreEqual a b =
    (a == b) && areAppFilesEqual a.files b.files


areAppFilesEqual : AppFiles -> AppFiles -> Bool
areAppFilesEqual a b =
    {- Avoid bug in Elm core library as reported at https://github.com/elm/bytes/issues/15 :
       Convert to other representation before comparing.
    -}
    let
        representationForComparison =
            Dict.map (always (SHA256.fromBytes >> SHA256.toHex))
    in
    representationForComparison a == representationForComparison b


resultCombineConcatenatingErrors : List (Result (List err) ok) -> Result (List err) (List ok)
resultCombineConcatenatingErrors =
    List.foldl
        (\result previousAggregate ->
            case previousAggregate of
                Err previousErrors ->
                    Err (previousErrors ++ (result |> Result.Extra.error |> Maybe.withDefault []))

                Ok previousList ->
                    case result of
                        Err error ->
                            Err error

                        Ok newItem ->
                            Ok (previousList ++ [ newItem ])
        )
        (Ok [])


listRemoveDuplicates : List a -> List a
listRemoveDuplicates =
    List.foldr
        (\item list ->
            if list |> List.member item then
                list

            else
                item :: list
        )
        []
