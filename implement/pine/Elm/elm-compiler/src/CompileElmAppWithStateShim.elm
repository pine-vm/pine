module CompileElmAppWithStateShim exposing (..)

import CompileElmApp
    exposing
        ( AppFiles
        , CompilationError(..)
        , CompileEntryPointConfig
        , ElmChoiceTypeStruct
        , ElmMakeEntryPointKind(..)
        , ElmMakeEntryPointStruct
        , ElmTypeAnnotation
        , LocatedInSourceFiles(..)
        , SourceDirectories
        , SourceParsedElmModule
        , addModulesFromTextToAppFiles
        , buildEstimateJsonEncodeLengthFunctionForTypeAnnotation
        , buildJsonConverterFunctionsForTypeAnnotation
        , buildTypeAnnotationText
        , fileContentFromString
        , filePathFromElmModuleName
        , importSyntaxTextFromModuleNameAndAlias
        , indentElmCodeLines
        , mapAppFilesToSupportJsonConverters
        , mapLocatedInSourceFiles
        , modulesToAddForBase64Coding
        , updateFileContentAtPath
        )
import Dict
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Result.Extra
import Set


type alias StateShimConfig =
    { jsonConverterDeclarationsConfigs : Dict.Dict String StateShimConfigJsonConverterConfig
    , jsonConverterDeclarations : Dict.Dict String StateShimConfigJsonConverter
    , appStateType : StateShimConfigSupportingType
    , initAppShimStateExpression : String
    , appStateLessShimExpression : String
    , exposedFunctions : Dict.Dict String ExposedFunctionConfig
    , supportingModules : List String
    , rootModuleSupportingFunctions : List String
    , modulesToImport : List (List String)
    , appStateWithPlatformShimTypeAnnotationFromAppStateAnnotation : String -> String
    , stateShimConfigExpression : String
    }


type alias RootModuleConfig =
    { interfaceToHostRootModuleName : String
    , appRootDeclarationModuleName : String
    , appStateTypeAnnotation : ElmTypeAnnotation
    , modulesToImport : List (List String)
    , stateShimRequestDecodeFunction : ( List String, String )
    , stateShimResponseEncodeSerialFunction : ( List String, String )
    , supportingJsonConverterFunctions : Dict.Dict String ( List String, String )
    , otherSupportingFunctions : List String
    , appStateWithPlatformShimTypeAnnotationFromAppStateAnnotation : String -> String
    , stateShimConfigExpression : String
    }


type alias ExposedFunctionConfig =
    { description : ExposedFunctionDescription
    , handlerExpression : String
    }


type alias ExposedFunctionDescription =
    { returnType : ExposedFunctionReturnTypeDescription
    , parameters : List ExposedFunctionParameterDescription
    }


type alias ExposedFunctionReturnTypeDescription =
    { sourceCodeText : String
    , containsAppStateType : Bool
    }


type alias ExposedFunctionParameterDescription =
    { patternSourceCodeText : String
    , typeSourceCodeText : String
    , typeIsAppStateType : Bool
    }


type alias StateShimConfigJsonConverterConfig =
    { isDecoder : Bool
    , moduleName : List String
    , declarationName : String
    }


type alias StateShimConfigJsonConverter =
    { isDecoder : Bool
    , typeAnnotation : ElmTypeAnnotation
    , dependencies : Dict.Dict String ElmChoiceTypeStruct
    }


type alias StateShimConfigSupportingType =
    { typeAnnotation : ElmTypeAnnotation
    , dependencies : Dict.Dict String ElmChoiceTypeStruct
    }


loweredForAppInStateManagementShim :
    SourceDirectories
    -> StateShimConfig
    -> CompileEntryPointConfig
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) ( AppFiles, ElmMakeEntryPointStruct )
loweredForAppInStateManagementShim sourceDirs stateShimConfig config sourceFiles =
    let
        interfaceToHostRootFilePath =
            filePathFromElmModuleName sourceDirs config.interfaceToHostRootModuleName

        entryPoint =
            { elmMakeJavaScriptFunctionName =
                String.join "." (config.interfaceToHostRootModuleName ++ [ "interfaceToHost_processEvent" ])
            }
    in
    config.originalSourceModules
        |> supportingTypesModules sourceDirs stateShimConfig
        |> Result.mapError
            (mapLocatedInSourceFiles
                ((++) "Failed to prepare supporting types modules: " >> OtherCompilationError)
                >> List.singleton
            )
        |> Result.map
            (\supportingTypes ->
                let
                    appStateType =
                        stateShimConfig.appStateType

                    jsonConverterDeclarations =
                        supportingTypes.jsonConverterDeclarations
                            |> Dict.union stateShimConfig.jsonConverterDeclarations

                    supportingJsonConverterFunctionsDependencies =
                        jsonConverterDeclarations
                            |> Dict.values
                            |> List.concatMap (.dependencies >> Dict.toList)
                            |> Dict.fromList

                    jsonConvertedTypesDependencies =
                        appStateType.dependencies
                            |> Dict.union (Tuple.second supportingTypes.stateShimRequestType)
                            |> Dict.union (Tuple.second supportingTypes.stateShimResponseResultType)
                            |> Dict.union supportingJsonConverterFunctionsDependencies

                    typeToGenerateSerializersFor : List ElmTypeAnnotation
                    typeToGenerateSerializersFor =
                        appStateType.typeAnnotation
                            :: Tuple.first supportingTypes.stateShimRequestType
                            :: Tuple.first supportingTypes.stateShimResponseResultType
                            :: (jsonConverterDeclarations
                                    |> Dict.values
                                    |> List.map .typeAnnotation
                               )

                    modulesToAdd =
                        modulesToAddForBase64Coding
                            ++ List.map .fileText (Dict.values supportingTypes.modules)

                    ( appFiles, generateSerializersResult ) =
                        sourceFiles
                            |> addModulesFromTextToAppFiles sourceDirs modulesToAdd
                            |> mapAppFilesToSupportJsonConverters
                                { generatedModuleNamePrefix = config.interfaceToHostRootModuleName
                                , sourceDirs = sourceDirs
                                }
                                typeToGenerateSerializersFor
                                jsonConvertedTypesDependencies

                    modulesToImport =
                        Set.toList generateSerializersResult.modulesToImport
                            ++ stateShimConfig.modulesToImport
                            ++ List.map .moduleName (Dict.values supportingTypes.modules)

                    stateShimRequestFunctionsNamesInGeneratedModules =
                        buildJsonConverterFunctionsForTypeAnnotation (Tuple.first supportingTypes.stateShimRequestType)

                    stateShimResponseResultFunctionsNamesInGeneratedModules =
                        buildJsonConverterFunctionsForTypeAnnotation (Tuple.first supportingTypes.stateShimResponseResultType)

                    stateShimRequestDecodeFunction =
                        ( generateSerializersResult.generatedModuleName
                        , stateShimRequestFunctionsNamesInGeneratedModules.decodeFunction.name
                        )

                    stateShimResponseEncodeSerialFunction =
                        ( generateSerializersResult.generatedModuleName
                        , stateShimResponseResultFunctionsNamesInGeneratedModules.encodeFunction.name
                        )

                    supportingJsonConverterFunctions =
                        jsonConverterDeclarations
                            |> Dict.map
                                (\_ jsonConverter ->
                                    let
                                        jsonConverterFunctionNames =
                                            buildJsonConverterFunctionsForTypeAnnotation jsonConverter.typeAnnotation

                                        getEncodeOrDecode =
                                            if jsonConverter.isDecoder then
                                                .decodeFunction

                                            else
                                                .encodeFunction
                                    in
                                    ( generateSerializersResult.generatedModuleName
                                    , jsonConverterFunctionNames |> getEncodeOrDecode |> .name
                                    )
                                )

                    appRootDeclarationModuleName =
                        config.compilationRootModule.parsedSyntax.moduleDefinition
                            |> Elm.Syntax.Node.value
                            |> Elm.Syntax.Module.moduleName
                            |> String.join "."

                    exposedFunctionsListSyntax =
                        stateShimConfig.exposedFunctions
                            |> Dict.toList
                            |> List.map composeExposedFunctionListEntrySyntax
                            |> String.join "\n,"

                    exposedFunctionsRootModuleSupportingFunction =
                        [ "config_exposedFunctions ="
                        , [ "[" ++ exposedFunctionsListSyntax
                          , "]"
                          , "|> Dict.fromList"
                          ]
                            |> String.join "\n"
                            |> indentElmCodeLines 1
                        ]
                            |> String.join "\n"

                    estimateJsonEncodeStateLengthGenerated =
                        CompileElmApp.buildEstimateJsonEncodeLengthFunctionsForMultipleTypes
                            [ appStateType.typeAnnotation ]
                            appStateType.dependencies

                    estimateAppStateJsonEncodeLengthFunctionName =
                        (buildEstimateJsonEncodeLengthFunctionForTypeAnnotation appStateType.typeAnnotation).name

                    estimateJsonEncodeLengthSupportingFunctionsText =
                        [ """
estimateJsonEncodeAppStateLength : AppState -> Int
estimateJsonEncodeAppStateLength = """ ++ estimateAppStateJsonEncodeLengthFunctionName
                        ]
                            ++ List.map .functionText estimateJsonEncodeStateLengthGenerated.generatedFunctions

                    rootElmModuleText =
                        composeRootElmModuleTextWithStateShim
                            { interfaceToHostRootModuleName = String.join "." config.interfaceToHostRootModuleName
                            , appRootDeclarationModuleName = appRootDeclarationModuleName
                            , appStateTypeAnnotation = appStateType.typeAnnotation
                            , modulesToImport = modulesToImport
                            , stateShimRequestDecodeFunction = stateShimRequestDecodeFunction
                            , stateShimResponseEncodeSerialFunction = stateShimResponseEncodeSerialFunction
                            , supportingJsonConverterFunctions = supportingJsonConverterFunctions
                            , otherSupportingFunctions =
                                exposedFunctionsRootModuleSupportingFunction
                                    :: stateShimConfig.rootModuleSupportingFunctions
                                    ++ estimateJsonEncodeLengthSupportingFunctionsText
                            , appStateWithPlatformShimTypeAnnotationFromAppStateAnnotation =
                                stateShimConfig.appStateWithPlatformShimTypeAnnotationFromAppStateAnnotation
                            , stateShimConfigExpression = stateShimConfig.stateShimConfigExpression
                            }
                in
                ( appFiles
                    |> updateFileContentAtPath
                        (always (fileContentFromString rootElmModuleText))
                        interfaceToHostRootFilePath
                , entryPoint
                )
            )


composeExposedFunctionListEntrySyntax : ( String, ExposedFunctionConfig ) -> String
composeExposedFunctionListEntrySyntax ( functionName, functionConfig ) =
    let
        syntaxFromBool bool =
            if bool then
                "True"

            else
                "False"

        parametersTexts =
            functionConfig.description.parameters
                |> List.map
                    (\parameterDescription ->
                        "{"
                            ++ ([ ( "patternSourceCodeText"
                                  , "\"" ++ parameterDescription.patternSourceCodeText ++ "\""
                                  )
                                , ( "typeSourceCodeText"
                                  , "\"" ++ parameterDescription.typeSourceCodeText ++ "\""
                                  )
                                , ( "typeIsAppStateType"
                                  , if parameterDescription.typeIsAppStateType then
                                        "True"

                                    else
                                        "False"
                                  )
                                ]
                                    |> List.map (\( key, value ) -> key ++ " = " ++ value)
                                    |> String.join ", "
                               )
                            ++ "}"
                    )
    in
    [ "(\"" ++ functionName ++ "\""
    , ", { description = { "
        ++ " returnType = { sourceCodeText = \""
        ++ functionConfig.description.returnType.sourceCodeText
        ++ "\", containsAppStateType = "
        ++ syntaxFromBool functionConfig.description.returnType.containsAppStateType
        ++ " }"
        ++ ", parameters = [ "
        ++ String.join ", " parametersTexts
        ++ " ]"
        ++ " }"
    , "  , handler ="
    , indentElmCodeLines 1 functionConfig.handlerExpression
    , "  }"
    , ")"
    ]
        |> String.join "\n"


supportingTypesModules :
    SourceDirectories
    -> StateShimConfig
    -> Dict.Dict (List String) SourceParsedElmModule
    ->
        Result
            (LocatedInSourceFiles String)
            { modules : Dict.Dict (List String) SourceParsedElmModule
            , stateShimRequestType : ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
            , stateShimResponseResultType : ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
            , jsonConverterDeclarations : Dict.Dict String StateShimConfigJsonConverter
            }
supportingTypesModules sourceDirs stateShimConfig originalSourceModules =
    [ stateShimTypesModuleText
    , stateShimModuleText
    ]
        ++ stateShimConfig.supportingModules
        |> CompileElmApp.elmModulesDictFromModuleTexts (CompileElmApp.filePathFromElmModuleName sourceDirs)
        |> Result.mapError
            (LocatedInSourceFiles
                { filePath = []
                , locationInModuleText = Elm.Syntax.Range.emptyRange
                }
            )
        |> Result.andThen
            (\typesModules ->
                let
                    moduleFromName moduleName =
                        typesModules
                            |> Dict.toList
                            |> List.filter (Tuple.second >> .moduleName >> (==) moduleName)
                            |> List.head
                in
                case moduleFromName [ "Backend", "Generated", "StateShimTypes" ] of
                    Nothing ->
                        Err
                            (LocatedInSourceFiles
                                { filePath = []
                                , locationInModuleText = Elm.Syntax.Range.emptyRange
                                }
                                "Did not find state shim types module"
                            )

                    Just ( stateShimTypesModuleFilePath, stateShimTypesModule ) ->
                        let
                            syntaxNodeFromEmptyRange =
                                Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange

                            fakeTypeAnnotationFromLocalName localName =
                                Elm.Syntax.TypeAnnotation.Typed (syntaxNodeFromEmptyRange ( [], localName )) []

                            stateShimRequestFakeTypeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation
                            stateShimRequestFakeTypeAnnotation =
                                fakeTypeAnnotationFromLocalName "StateShimRequest"

                            stateShimResponseResultFakeTypeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation
                            stateShimResponseResultFakeTypeAnnotation =
                                fakeTypeAnnotationFromLocalName "ResponseOverSerialInterface"

                            fromStateShimConfigJsonConverterConfig :
                                StateShimConfigJsonConverterConfig
                                -> Result (LocatedInSourceFiles String) StateShimConfigJsonConverter
                            fromStateShimConfigJsonConverterConfig converterConfig =
                                case moduleFromName converterConfig.moduleName of
                                    Nothing ->
                                        Err
                                            (LocatedInSourceFiles
                                                { filePath = [], locationInModuleText = Elm.Syntax.Range.emptyRange }
                                                ("Did not find module " ++ String.join "." converterConfig.moduleName)
                                            )

                                    Just ( converterTypeModuleFilePath, converterTypeModule ) ->
                                        CompileElmApp.parseElmTypeAndDependenciesRecursivelyFromAnnotation
                                            (Dict.union typesModules originalSourceModules)
                                            ( ( converterTypeModuleFilePath, converterTypeModule.parsedSyntax )
                                            , syntaxNodeFromEmptyRange
                                                (fakeTypeAnnotationFromLocalName converterConfig.declarationName)
                                            )
                                            |> Result.map
                                                (\( typeAnnotation, dependencies ) ->
                                                    { isDecoder = converterConfig.isDecoder
                                                    , typeAnnotation = typeAnnotation
                                                    , dependencies = dependencies
                                                    }
                                                )

                            jsonConverterDeclarationsResults =
                                stateShimConfig.jsonConverterDeclarationsConfigs
                                    |> Dict.toList
                                    |> List.map
                                        (\( declarationName, converterConfig ) ->
                                            fromStateShimConfigJsonConverterConfig converterConfig
                                                |> Result.map (Tuple.pair declarationName)
                                        )
                        in
                        CompileElmApp.parseElmTypeAndDependenciesRecursivelyFromAnnotation
                            (Dict.union typesModules originalSourceModules)
                            ( ( stateShimTypesModuleFilePath, stateShimTypesModule.parsedSyntax )
                            , syntaxNodeFromEmptyRange stateShimRequestFakeTypeAnnotation
                            )
                            |> Result.andThen
                                (\stateShimRequestType ->
                                    CompileElmApp.parseElmTypeAndDependenciesRecursivelyFromAnnotation
                                        (Dict.union typesModules originalSourceModules)
                                        ( ( stateShimTypesModuleFilePath, stateShimTypesModule.parsedSyntax )
                                        , syntaxNodeFromEmptyRange stateShimResponseResultFakeTypeAnnotation
                                        )
                                        |> Result.andThen
                                            (\stateShimResponseResultType ->
                                                jsonConverterDeclarationsResults
                                                    |> Result.Extra.combine
                                                    |> Result.map
                                                        (\jsonConverterDeclarations ->
                                                            { modules = typesModules
                                                            , stateShimRequestType = stateShimRequestType
                                                            , stateShimResponseResultType = stateShimResponseResultType
                                                            , jsonConverterDeclarations = Dict.fromList jsonConverterDeclarations
                                                            }
                                                        )
                                            )
                                )
            )


composeRootElmModuleTextWithStateShim : RootModuleConfig -> String
composeRootElmModuleTextWithStateShim config =
    let
        supportingJsonConverterFunctionsAliases =
            Dict.toList config.supportingJsonConverterFunctions

        functionAliases =
            [ ( "jsonDecodeStateShimRequest"
              , config.stateShimRequestDecodeFunction
              )
            , ( "jsonEncodeStateShimResultResponse"
              , config.stateShimResponseEncodeSerialFunction
              )
            ]
                ++ supportingJsonConverterFunctionsAliases

        supportingFunctionsText =
            (functionAliases
                |> List.map
                    (\( localDeclarationName, ( remoteModuleName, remoteDeclarationName ) ) ->
                        localDeclarationName
                            ++ " =\n    "
                            ++ String.join "." (remoteModuleName ++ [ remoteDeclarationName ])
                    )
            )
                ++ config.otherSupportingFunctions
                |> String.join "\n\n\n"

        importsText =
            config.modulesToImport
                |> Set.fromList
                |> Set.toList
                |> List.map (Tuple.pair >> (|>) Nothing >> importSyntaxTextFromModuleNameAndAlias)
                |> String.join "\n"

        appStateWithPlatformShim =
            config.appStateWithPlatformShimTypeAnnotationFromAppStateAnnotation "AppState"
    in
    "module "
        ++ config.interfaceToHostRootModuleName
        ++ """ exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , main
    )

import """
        ++ config.appRootDeclarationModuleName
        ++ """
"""
        ++ importsText
        ++ """
import Platform
import Backend.Generated.StateShim exposing (StateShimConfig, StateShimState)


type alias AppState =
    ("""
        ++ buildTypeAnnotationText config.appStateTypeAnnotation
        ++ """)


type alias AppStateWithPlatformShim =
    """
        ++ appStateWithPlatformShim
        ++ """


config_stateShim =
    """
        ++ indentElmCodeLines 1 config.stateShimConfigExpression
        ++ """


type alias State =
    StateShimState AppStateWithPlatformShim


interfaceToHost_initState =
    Backend.Generated.StateShim.init


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    wrapForSerialInterface_processEvent config_stateShim


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


wrapForSerialInterface_processEvent :
    StateShimConfig appState appStateLessShim
    -> String
    -> StateShimState appState
    -> ( StateShimState appState, String )
wrapForSerialInterface_processEvent config serializedEvent stateBefore =
    (case serializedEvent |> Json.Decode.decodeString jsonDecodeStateShimRequest of
        Err error ->
            ( stateBefore
            , Err ("Failed to deserialize event: " ++ Json.Decode.errorToString error)
            )

        Ok hostEvent ->
            stateBefore
                |> Backend.Generated.StateShim.processEvent config hostEvent
                |> Tuple.mapSecond Ok
    )
        |> Tuple.mapSecond (jsonEncodeStateShimResultResponse >> Json.Encode.encode 0)


"""
        ++ supportingFunctionsText


stateShimModuleText : String
stateShimModuleText =
    String.trimLeft """
module Backend.Generated.StateShim exposing (..)

import Backend.Generated.StateShimTypes exposing (..)
import Dict
import Json.Decode
import Json.Encode


type alias StateShimConfig appState appStateLessShim =
    { exposedFunctions : Dict.Dict String (ExposedFunctionRecord appState)
    , jsonEncodeAppState : appStateLessShim -> Json.Encode.Value
    , jsonDecodeAppState : Json.Decode.Decoder appStateLessShim
    , initAppShimState : appStateLessShim -> appState
    , appStateLessShim : appState -> appStateLessShim
    , estimateJsonEncodeAppStateLength : appStateLessShim -> Int
    }


type alias StateShimState appState =
    { branches : Dict.Dict String appState }


type alias ExposedFunctionHandler appState =
    ApplyFunctionArguments (Maybe appState) -> Result String ( Maybe appState, Maybe Json.Encode.Value )


type alias ExposedFunctionRecord appState =
    { description : ExposedFunctionDescription
    , handler : ExposedFunctionHandler appState
    }


init : StateShimState appState
init =
    { branches = Dict.empty }


processEvent :
    StateShimConfig appState appStateLessShim
    -> StateShimRequest
    -> StateShimState appState
    -> ( StateShimState appState, StateShimResponse )
processEvent config hostEvent stateBefore =
    case hostEvent of
        ListExposedFunctionsShimRequest ->
            ( stateBefore
            , config.exposedFunctions
                |> Dict.toList
                |> List.map (Tuple.mapSecond .description)
                |> List.map
                    (\\( functionName, functionDescription ) ->
                        { functionName = functionName, functionDescription = functionDescription }
                    )
                |> ListExposedFunctionsShimResponse
            )

        ApplyFunctionShimRequest applyFunction ->
            let
                returnError errorText =
                    ( stateBefore
                    , errorText
                        |> Err
                        |> ApplyFunctionShimResponse
                    )
            in
            case config.exposedFunctions |> Dict.get applyFunction.functionName of
                Nothing ->
                    returnError
                        ("None of the exposed functions matches name '"
                            ++ applyFunction.functionName
                            ++ "'. This app only exposes the following "
                            ++ String.fromInt (Dict.size config.exposedFunctions)
                            ++ " functions: "
                            ++ String.join ", " (Dict.keys config.exposedFunctions)
                        )

                Just exposedFunction ->
                    let
                        resolveStateSourceResult =
                            applyFunction.arguments.stateArgument
                                |> Maybe.map (resolveStateSource config stateBefore >> Result.map Just)
                                |> Maybe.withDefault (Ok Nothing)
                    in
                    case resolveStateSourceResult of
                        Err err ->
                            returnError ("Failed to resolve state source: " ++ err)

                        Ok stateArgument ->
                            case
                                exposedFunction.handler
                                    { stateArgument = stateArgument
                                    , serializedArgumentsJson = applyFunction.arguments.serializedArgumentsJson
                                    }
                            of
                                Err err ->
                                    returnError err

                                Ok ( maybeFunctionResultState, maybeFunctionResultOther ) ->
                                    let
                                        producedStateDifferentFromStateArgument =
                                            (maybeFunctionResultState /= Nothing)
                                                && (maybeFunctionResultState /= stateArgument)

                                        updateStateResult =
                                            case applyFunction.stateDestinationBranches of
                                                [] ->
                                                    Ok stateBefore

                                                destBranches ->
                                                    case maybeFunctionResultState of
                                                        Nothing ->
                                                            Err "Function did not return a new state"

                                                        Just newAppState ->
                                                            stateBefore
                                                                |> setStateOnBranches destBranches newAppState
                                                                |> Ok
                                    in
                                    case updateStateResult of
                                        Err err ->
                                            returnError err

                                        Ok state ->
                                            ( state
                                            , ApplyFunctionShimResponse
                                                (Ok
                                                    { resultLessStateJson = maybeFunctionResultOther
                                                    , producedStateDifferentFromStateArgument = producedStateDifferentFromStateArgument
                                                    }
                                                )
                                            )

        SerializeStateShimRequest stateSource ->
            case resolveStateSource config stateBefore stateSource of
                Err err ->
                    ( stateBefore
                    , ("Failed to resolve state source: " ++ err)
                        |> Err
                        |> SerializeStateShimResponse
                    )

                Ok appState ->
                    ( stateBefore
                    , appState
                        |> config.appStateLessShim
                        |> config.jsonEncodeAppState
                        |> Ok
                        |> SerializeStateShimResponse
                    )

        SetBranchesStateShimRequest stateSource branches ->
            case resolveStateSource config stateBefore stateSource of
                Err err ->
                    ( stateBefore
                    , ("Failed to resolve state source: " ++ err)
                        |> Err
                        |> SetBranchesStateShimResponse
                    )

                Ok appState ->
                    ( setStateOnBranches branches appState stateBefore
                    , "" |> Ok |> SetBranchesStateShimResponse
                    )

        EstimateSerializedStateLengthShimRequest stateSource ->
            case resolveStateSource config stateBefore stateSource of
                Err err ->
                    ( stateBefore
                    , ("Failed to resolve state source: " ++ err)
                        |> Err
                        |> EstimateSerializedStateLengthShimResponse
                    )

                Ok appState ->
                    ( stateBefore
                    , appState
                        |> config.appStateLessShim
                        |> config.estimateJsonEncodeAppStateLength
                        |> Ok
                        |> EstimateSerializedStateLengthShimResponse
                    )

        ListBranchesShimRequest ->
            ( stateBefore
            , stateBefore.branches
                |> Dict.keys
                |> ListBranchesShimResponse
            )

        RemoveBranchesShimRequest branchesToRemove ->
            let
                branches =
                    stateBefore.branches
                        |> Dict.filter (List.member >> (|>) branchesToRemove >> not >> always)

                removedCount =
                    Dict.size stateBefore.branches - Dict.size branches
            in
            ( { stateBefore | branches = branches }
            , RemoveBranchesShimResponse { removedCount = removedCount }
            )

        TestAreStatesEqualRequest statesSources ->
            statesSources
                |> List.map (resolveStateSource config stateBefore)
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> Result.mapError ((++) "Failed to resolve state source: ")
                |> Result.map (\\states ->
                    case states of
                        [] ->
                            True

                        first :: others ->
                            List.all ((==) first) others)
                |> TestAreStatesEqualResponse
                |> Tuple.pair stateBefore


setStateOnBranches : List String -> appState -> StateShimState appState -> StateShimState appState
setStateOnBranches branches appState stateBefore =
    { stateBefore
        | branches =
            branches
                |> List.foldl
                    (\\branchName -> Dict.insert branchName appState)
                    stateBefore.branches
    }


resolveStateSource : StateShimConfig appState appStateLessShim -> StateShimState appState -> StateSource -> Result String appState
resolveStateSource config shimState stateSource =
    case stateSource of
        JsonStateSource stateJson ->
            stateJson
                |> Json.Decode.decodeValue config.jsonDecodeAppState
                |> Result.map config.initAppShimState
                |> Result.mapError (Json.Decode.errorToString >> (++) "Failed to decode: ")

        BranchStateSource branchName ->
            case Dict.get branchName shimState.branches of
                Nothing ->
                    Err ("Branch named '" ++ branchName ++ "' does not exist")

                Just appState ->
                    Ok appState


exposedFunctionExpectingSingleArgument :
    Json.Decode.Decoder arg
    -> (arg -> Result String ( Maybe appState, Maybe Json.Encode.Value ))
    -> ExposedFunctionHandler appState
exposedFunctionExpectingSingleArgument argumentDecoder funcAfterDecode genericArguments =
    case genericArguments.serializedArgumentsJson of
        [ singleArgumentJson ] ->
            case Json.Decode.decodeValue argumentDecoder singleArgumentJson of
                Err err ->
                    Err ("Failed to JSON decode argument: " ++ Json.Decode.errorToString err)

                Ok argument ->
                    funcAfterDecode argument

        serializedArgumentsJson ->
            Err
                ("Unexpected number of arguments: "
                    ++ String.fromInt (List.length serializedArgumentsJson)
                    ++ " instead of 1"
                )


exposedFunctionExpectingSingleArgumentAndAppState :
    Json.Decode.Decoder arg
    -> (arg -> appState -> Result String ( Maybe appState, Maybe Json.Encode.Value ))
    -> ExposedFunctionHandler appState
exposedFunctionExpectingSingleArgumentAndAppState argumentDecoder funcAfterDecode genericArguments =
    case genericArguments.stateArgument of
        Nothing ->
            Err "Type mismatch: Missing state argument"

        Just appState ->
            case genericArguments.serializedArgumentsJson of
                [ singleArgumentJson ] ->
                    case Json.Decode.decodeValue argumentDecoder singleArgumentJson of
                        Err err ->
                            Err ("Failed to JSON decode argument: " ++ Json.Decode.errorToString err)

                        Ok argument ->
                            funcAfterDecode argument appState

                serializedArgumentsJson ->
                    Err
                        ("Unexpected number of arguments: "
                            ++ String.fromInt (List.length serializedArgumentsJson)
                            ++ " instead of 1"
                        )

"""


stateShimTypesModuleText : String
stateShimTypesModuleText =
    String.trimLeft """
module Backend.Generated.StateShimTypes exposing (..)

import Json.Encode


type alias ResponseOverSerialInterface =
    Result String StateShimResponse


type StateShimRequest
    = ListExposedFunctionsShimRequest
    | ApplyFunctionShimRequest ApplyFunctionShimRequestStruct
    | SerializeStateShimRequest StateSource
    | SetBranchesStateShimRequest StateSource (List String)
    | EstimateSerializedStateLengthShimRequest StateSource
    | ListBranchesShimRequest
    | RemoveBranchesShimRequest (List String)
    | TestAreStatesEqualRequest (List StateSource)


type StateShimResponse
    = ListExposedFunctionsShimResponse (List { functionName : String, functionDescription : ExposedFunctionDescription })
    | ApplyFunctionShimResponse (Result String FunctionApplicationResult)
    | SerializeStateShimResponse (Result String Json.Encode.Value)
    | SetBranchesStateShimResponse (Result String String)
    | EstimateSerializedStateLengthShimResponse (Result String Int)
    | ListBranchesShimResponse (List String)
    | RemoveBranchesShimResponse { removedCount : Int }
    | TestAreStatesEqualResponse (Result String Bool)


type alias ApplyFunctionShimRequestStruct =
    { functionName : String
    , arguments : ApplyFunctionArguments (Maybe StateSource)
    , stateDestinationBranches : List String
    }


type alias ApplyFunctionArguments state =
    { stateArgument : state
    , serializedArgumentsJson : List Json.Encode.Value
    }


type StateSource
    = JsonStateSource Json.Encode.Value
    | BranchStateSource String


type alias ExposedFunctionDescription =
    { returnType : ExposedFunctionReturnTypeDescription
    , parameters : List ExposedFunctionParameterDescription
    }


type alias ExposedFunctionReturnTypeDescription =
    { sourceCodeText : String
    , containsAppStateType : Bool
    }


type alias ExposedFunctionParameterDescription =
    { patternSourceCodeText : String
    , typeSourceCodeText : String
    , typeIsAppStateType : Bool
    }


type alias FunctionApplicationResult =
    { resultLessStateJson : Maybe Json.Encode.Value
    , producedStateDifferentFromStateArgument : Bool
    }

"""
