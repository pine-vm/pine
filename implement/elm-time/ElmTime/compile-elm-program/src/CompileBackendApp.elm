module CompileBackendApp exposing (..)

import CompileFullstackApp
    exposing
        ( AppFiles
        , CompilationError(..)
        , CompileEntryPointConfig
        , ElmChoiceTypeStruct
        , ElmMakeEntryPointKind(..)
        , ElmMakeEntryPointStruct
        , ElmTypeAnnotation
        , EntryPointClass
        , LocatedInSourceFiles(..)
        , SourceParsedElmModule
        , addModulesFromTextToAppFiles
        , buildJsonCodingFunctionsForTypeAnnotation
        , buildTypeAnnotationText
        , entryPointClassFromSetOfEquallyProcessedFunctionNames
        , fileContentFromString
        , filePathFromElmModuleName
        , findModuleByName
        , importSyntaxTextFromModuleNameAndAlias
        , indentElmCodeLines
        , locatedInSourceFilesFromRange
        , mapAppFilesToSupportJsonCoding
        , mapLocatedInSourceFiles
        , modulesToAddForBase64Coding
        , parseElmTypeAndDependenciesRecursivelyFromAnnotation
        , syntaxRangeCoveringCompleteModule
        , updateFileContentAtPath
        )
import Dict
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Set


type alias BackendRootModuleConfig =
    { interfaceToHostRootModuleName : String
    , rootModuleNameBeforeLowering : String
    , mainDeclarationName : String
    , stateTypeAnnotation : ElmTypeAnnotation
    , modulesToImport : List (List String)
    , stateEncodeFunction : ( List String, String )
    , stateDecodeFunction : ( List String, String )
    , stateShimRequestDecodeFunction : ( List String, String )
    , stateShimResponseEncodeFunction : ( List String, String )
    , stateFromStringExpression : String
    , migrateFromStringExpression : String
    }


type alias MigrationConfig =
    { inputType : ElmTypeAnnotation
    , returnType : ElmTypeAnnotation
    , dependencies : Dict.Dict String ElmChoiceTypeStruct
    , migrateFunctionModuleName : List String
    , migrateFunctionDeclarationLocalName : String
    }


type alias ElmAppInterfaceConvention =
    { serializeStateFunctionName : String
    , deserializeStateFunctionName : String
    }


elmAppInterfaceConvention : ElmAppInterfaceConvention
elmAppInterfaceConvention =
    { serializeStateFunctionName = "interfaceToHost_serializeState"
    , deserializeStateFunctionName = "interfaceToHost_deserializeState"
    }


appStateMigrationInterfaceModuleName : List String
appStateMigrationInterfaceModuleName =
    [ "Backend", "MigrateState" ]


appStateMigrationInterfaceFunctionName : String
appStateMigrationInterfaceFunctionName =
    "migrate"


entryPoints : List EntryPointClass
entryPoints =
    [ entryPointClassFromSetOfEquallyProcessedFunctionNames
        (Set.fromList [ "webServerMain", "backendMain" ])
        (\functionDeclaration entryPointConfig ->
            loweredForBackendApp functionDeclaration entryPointConfig
                >> Result.map
                    (\( compiledFiles, entryPoint ) ->
                        { compiledFiles = compiledFiles
                        , rootModuleEntryPointKind = ClassicMakeEntryPoint entryPoint
                        }
                    )
        )
    ]


supportingTypesModules :
    Dict.Dict (List String) SourceParsedElmModule
    ->
        Result
            (LocatedInSourceFiles String)
            { modules : Dict.Dict (List String) SourceParsedElmModule
            , stateShimRequestType : ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
            , stateShimResponseType : ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
            }
supportingTypesModules originalSourceModules =
    [ stateShimTypesModuleText
    , webServerShimTypesModuleText
    ]
        |> CompileFullstackApp.elmModulesDictFromModuleTexts CompileFullstackApp.filePathFromElmModuleName
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
                case moduleFromName [ "Backend", "Generated", "WebServerShimTypes" ] of
                    Nothing ->
                        Err
                            (LocatedInSourceFiles
                                { filePath = []
                                , locationInModuleText = Elm.Syntax.Range.emptyRange
                                }
                                "Did not find web server shim types module"
                            )

                    Just ( webServerShimTypesModuleFilePath, webServerShimTypesModule ) ->
                        case moduleFromName [ "Backend", "Generated", "StateShimTypes" ] of
                            Nothing ->
                                Err
                                    (LocatedInSourceFiles
                                        { filePath = []
                                        , locationInModuleText = Elm.Syntax.Range.emptyRange
                                        }
                                        "Did not find web server shim types module"
                                    )

                            Just ( stateShimTypesModuleFilePath, stateShimTypesModule ) ->
                                let
                                    syntaxNodeFromEmptyRange =
                                        Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange

                                    stateShimRequestFakeTypeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation
                                    stateShimRequestFakeTypeAnnotation =
                                        Elm.Syntax.TypeAnnotation.Typed (syntaxNodeFromEmptyRange ( [], "StateShimRequest" )) []

                                    backendEventResponseFakeTypeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation
                                    backendEventResponseFakeTypeAnnotation =
                                        Elm.Syntax.TypeAnnotation.Typed (syntaxNodeFromEmptyRange ( [], "BackendEventResponseStruct" )) []

                                    stateShimResponseFakeTypeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation
                                    stateShimResponseFakeTypeAnnotation =
                                        Elm.Syntax.TypeAnnotation.Typed (syntaxNodeFromEmptyRange ( [], "StateShimResponse" )) []
                                in
                                CompileFullstackApp.parseElmTypeAndDependenciesRecursivelyFromAnnotation
                                    (Dict.union typesModules originalSourceModules)
                                    ( ( stateShimTypesModuleFilePath, stateShimTypesModule.parsedSyntax )
                                    , syntaxNodeFromEmptyRange stateShimRequestFakeTypeAnnotation
                                    )
                                    |> Result.andThen
                                        (\stateShimRequestType ->
                                            CompileFullstackApp.parseElmTypeAndDependenciesRecursivelyFromAnnotation
                                                (Dict.union typesModules originalSourceModules)
                                                ( ( stateShimTypesModuleFilePath, stateShimTypesModule.parsedSyntax )
                                                , syntaxNodeFromEmptyRange stateShimResponseFakeTypeAnnotation
                                                )
                                                |> Result.map
                                                    (\stateShimResponseType ->
                                                        { modules = typesModules
                                                        , stateShimRequestType = stateShimRequestType
                                                        , stateShimResponseType = stateShimResponseType
                                                        }
                                                    )
                                        )
            )


loweredForBackendApp :
    Elm.Syntax.Expression.Function
    -> CompileEntryPointConfig
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) ( AppFiles, ElmMakeEntryPointStruct )
loweredForBackendApp appDeclaration config sourceFiles =
    let
        interfaceToHostRootFilePath =
            filePathFromElmModuleName config.interfaceToHostRootModuleName

        entryPoint =
            { elmMakeJavaScriptFunctionName =
                String.join "." (config.interfaceToHostRootModuleName ++ [ "interfaceToHost_processEvent" ])
            }
    in
    if Dict.get interfaceToHostRootFilePath sourceFiles /= Nothing then
        -- Support integrating applications supplying their own lowered version.
        Ok ( sourceFiles, entryPoint )

    else
        config.originalSourceModules
            |> supportingTypesModules
            |> Result.mapError
                (mapLocatedInSourceFiles
                    ((++) "Failed to prepare supporting types modules: " >> OtherCompilationError)
                    >> List.singleton
                )
            |> Result.andThen
                (\supportingTypes ->
                    parseAppStateElmTypeAndDependenciesRecursively
                        appDeclaration
                        config.originalSourceModules
                        ( config.compilationRootFilePath, config.compilationRootModule.parsedSyntax )
                        |> Result.mapError (mapLocatedInSourceFiles ((++) "Failed to parse state type: "))
                        |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)
                        |> Result.andThen
                            (\stateType ->
                                case Dict.get stateType.instantiatedConfigTypeName composeBackendRootElmModuleTextFromTypeName of
                                    Nothing ->
                                        Err
                                            [ LocatedInSourceFiles
                                                { filePath = config.compilationRootFilePath
                                                , locationInModuleText = Elm.Syntax.Range.emptyRange
                                                }
                                                (OtherCompilationError
                                                    ("Unknown instantiated config type name: "
                                                        ++ String.join "." stateType.instantiatedConfigTypeName
                                                    )
                                                )
                                            ]

                                    Just composeBackendRootElmModuleText ->
                                        parseMigrationConfig { originalSourceModules = config.originalSourceModules }
                                            |> Result.map
                                                (\maybeMigrationConfig ->
                                                    let
                                                        jsonConvertedTypesDependencies =
                                                            stateType.dependencies
                                                                |> Dict.union
                                                                    (maybeMigrationConfig
                                                                        |> Maybe.map .dependencies
                                                                        |> Maybe.withDefault Dict.empty
                                                                    )
                                                                |> Dict.union (Tuple.second supportingTypes.stateShimRequestType)
                                                                |> Dict.union (Tuple.second supportingTypes.stateShimResponseType)

                                                        typeToGenerateSerializersFor : List ElmTypeAnnotation
                                                        typeToGenerateSerializersFor =
                                                            stateType.stateTypeAnnotation
                                                                :: Tuple.first supportingTypes.stateShimRequestType
                                                                :: Tuple.first supportingTypes.stateShimResponseType
                                                                :: typeToGenerateSerializersForMigration

                                                        modulesToAdd =
                                                            modulesToAddForBase64Coding
                                                                ++ List.map .fileText (Dict.values supportingTypes.modules)

                                                        ( appFiles, generateSerializersResult ) =
                                                            sourceFiles
                                                                |> addModulesFromTextToAppFiles modulesToAdd
                                                                |> mapAppFilesToSupportJsonCoding
                                                                    { generatedModuleNamePrefix = config.interfaceToHostRootModuleName }
                                                                    typeToGenerateSerializersFor
                                                                    jsonConvertedTypesDependencies

                                                        modulesToImport =
                                                            generateSerializersResult.modulesToImport
                                                                ++ (maybeMigrationConfig
                                                                        |> Maybe.map (.migrateFunctionModuleName >> List.singleton)
                                                                        |> Maybe.withDefault []
                                                                   )

                                                        stateFunctionsNamesInGeneratedModules =
                                                            buildJsonCodingFunctionsForTypeAnnotation stateType.stateTypeAnnotation

                                                        stateShimRequestFunctionsNamesInGeneratedModules =
                                                            buildJsonCodingFunctionsForTypeAnnotation (Tuple.first supportingTypes.stateShimRequestType)

                                                        stateShimResponseFunctionsNamesInGeneratedModules =
                                                            buildJsonCodingFunctionsForTypeAnnotation (Tuple.first supportingTypes.stateShimResponseType)

                                                        stateEncodeFunction =
                                                            ( generateSerializersResult.generatedModuleName
                                                            , stateFunctionsNamesInGeneratedModules.encodeFunction.name
                                                            )

                                                        stateDecodeFunction =
                                                            ( generateSerializersResult.generatedModuleName
                                                            , stateFunctionsNamesInGeneratedModules.decodeFunction.name
                                                            )

                                                        stateShimRequestDecodeFunction =
                                                            ( generateSerializersResult.generatedModuleName
                                                            , stateShimRequestFunctionsNamesInGeneratedModules.decodeFunction.name
                                                            )

                                                        stateShimResponseEncodeFunction =
                                                            ( generateSerializersResult.generatedModuleName
                                                            , stateShimResponseFunctionsNamesInGeneratedModules.encodeFunction.name
                                                            )

                                                        ( migrateFromStringExpressionFromGenerateModuleName, typeToGenerateSerializersForMigration ) =
                                                            case maybeMigrationConfig of
                                                                Nothing ->
                                                                    ( always "always (Err \"Did not find a migration function in the program code\")"
                                                                    , []
                                                                    )

                                                                Just migrationConfig ->
                                                                    let
                                                                        inputTypeFunctionNames =
                                                                            buildJsonCodingFunctionsForTypeAnnotation migrationConfig.inputType
                                                                    in
                                                                    ( \generatedModuleName ->
                                                                        [ "Json.Decode.decodeString "
                                                                            ++ String.join "." (generatedModuleName ++ [ inputTypeFunctionNames.decodeFunction.name ])
                                                                        , ">> Result.mapError Json.Decode.errorToString"
                                                                        , ">> Result.map " ++ String.join "." (migrationConfig.migrateFunctionModuleName ++ [ migrationConfig.migrateFunctionDeclarationLocalName ])
                                                                        ]
                                                                            |> String.join "\n"
                                                                    , [ migrationConfig.inputType, migrationConfig.returnType ]
                                                                    )

                                                        stateFromStringExpression =
                                                            [ "Json.Decode.decodeString "
                                                                ++ String.join "." (generateSerializersResult.generatedModuleName ++ [ stateFunctionsNamesInGeneratedModules.decodeFunction.name ])
                                                            , ">> Result.mapError Json.Decode.errorToString"
                                                            ]
                                                                |> String.join "\n"

                                                        rootElmModuleText =
                                                            composeBackendRootElmModuleText
                                                                { interfaceToHostRootModuleName = String.join "." config.interfaceToHostRootModuleName
                                                                , rootModuleNameBeforeLowering =
                                                                    config.compilationRootModule.parsedSyntax.moduleDefinition
                                                                        |> Elm.Syntax.Node.value
                                                                        |> Elm.Syntax.Module.moduleName
                                                                        |> String.join "."
                                                                , mainDeclarationName =
                                                                    appDeclaration.declaration
                                                                        |> Elm.Syntax.Node.value
                                                                        |> .name
                                                                        |> Elm.Syntax.Node.value
                                                                , stateTypeAnnotation = stateType.stateTypeAnnotation
                                                                , modulesToImport = modulesToImport
                                                                , stateEncodeFunction = stateEncodeFunction
                                                                , stateDecodeFunction = stateDecodeFunction
                                                                , stateShimRequestDecodeFunction = stateShimRequestDecodeFunction
                                                                , stateShimResponseEncodeFunction = stateShimResponseEncodeFunction
                                                                , stateFromStringExpression = stateFromStringExpression
                                                                , migrateFromStringExpression =
                                                                    generateSerializersResult.generatedModuleName
                                                                        |> migrateFromStringExpressionFromGenerateModuleName
                                                                }
                                                    in
                                                    ( appFiles
                                                        |> updateFileContentAtPath
                                                            (always (fileContentFromString rootElmModuleText))
                                                            interfaceToHostRootFilePath
                                                    , entryPoint
                                                    )
                                                )
                            )
                )


composeBackendRootElmModuleTextFromTypeName : Dict.Dict (List String) (BackendRootModuleConfig -> String)
composeBackendRootElmModuleTextFromTypeName =
    [ ( [ "Platform", "WebServer", "WebServerConfig" ]
      , composeBackendRootElmModuleTextPlatformWebServer
      )
    ]
        |> Dict.fromList


parseMigrationConfig :
    { originalSourceModules : Dict.Dict (List String) SourceParsedElmModule }
    -> Result (List (LocatedInSourceFiles CompilationError)) (Maybe MigrationConfig)
parseMigrationConfig { originalSourceModules } =
    case findModuleByName appStateMigrationInterfaceModuleName originalSourceModules of
        Nothing ->
            Ok Nothing

        Just ( originalInterfaceModuleFilePath, originalInterfaceModule ) ->
            parseAppStateMigrateElmTypeAndDependenciesRecursively
                originalSourceModules
                ( originalInterfaceModuleFilePath, originalInterfaceModule.parsedSyntax )
                |> Result.mapError (mapLocatedInSourceFiles ((++) "Failed to parse migration state type name: "))
                |> Result.map
                    (\( ( inputType, returnType ), stateTypeDependencies ) ->
                        Just
                            { inputType = inputType
                            , returnType = returnType
                            , dependencies = stateTypeDependencies
                            , migrateFunctionModuleName = appStateMigrationInterfaceModuleName
                            , migrateFunctionDeclarationLocalName = appStateMigrationInterfaceFunctionName
                            }
                    )
                |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)


parseAppStateElmTypeAndDependenciesRecursively :
    Elm.Syntax.Expression.Function
    -> Dict.Dict (List String) SourceParsedElmModule
    -> ( List String, Elm.Syntax.File.File )
    ->
        Result
            (LocatedInSourceFiles String)
            { stateTypeAnnotation : ElmTypeAnnotation
            , dependencies : Dict.Dict String ElmChoiceTypeStruct
            , instantiatedConfigTypeName : List String
            }
parseAppStateElmTypeAndDependenciesRecursively rootFunctionDeclaration sourceModules ( parsedModuleFilePath, parsedModule ) =
    stateTypeAnnotationFromRootFunctionDeclaration rootFunctionDeclaration
        |> Result.mapError
            ((++) "Did not find state type annotation: "
                >> LocatedInSourceFiles
                    { filePath = parsedModuleFilePath
                    , locationInModuleText = syntaxRangeCoveringCompleteModule parsedModule
                    }
            )
        |> Result.andThen
            (\stateTypeAnnotation ->
                parseElmTypeAndDependenciesRecursivelyFromAnnotation
                    sourceModules
                    ( ( parsedModuleFilePath, parsedModule ), stateTypeAnnotation.parameter )
                    |> Result.map
                        (\( stateType, dependencies ) ->
                            { stateTypeAnnotation = stateType
                            , dependencies = dependencies
                            , instantiatedConfigTypeName =
                                Tuple.first (Elm.Syntax.Node.value stateTypeAnnotation.instantiated)
                                    ++ [ Tuple.second (Elm.Syntax.Node.value stateTypeAnnotation.instantiated) ]
                            }
                        )
            )


parseAppStateMigrateElmTypeAndDependenciesRecursively :
    Dict.Dict (List String) SourceParsedElmModule
    -> ( List String, Elm.Syntax.File.File )
    -> Result (LocatedInSourceFiles String) ( ( ElmTypeAnnotation, ElmTypeAnnotation ), Dict.Dict String ElmChoiceTypeStruct )
parseAppStateMigrateElmTypeAndDependenciesRecursively sourceModules ( parsedModuleFilePath, parsedModule ) =
    migrateStateTypeAnnotationFromElmModule parsedModule
        |> Result.mapError
            (Elm.Syntax.Node.map ((++) "Did not find state type annotation: ")
                >> locatedInSourceFilesFromRange parsedModuleFilePath
            )
        |> Result.andThen
            (\( originTypeNode, destinationTypeNode ) ->
                parseElmTypeAndDependenciesRecursivelyFromAnnotation
                    sourceModules
                    ( ( parsedModuleFilePath, parsedModule ), originTypeNode )
                    |> Result.andThen
                        (\( originTypeAnnotation, originTypeDependencies ) ->
                            parseElmTypeAndDependenciesRecursivelyFromAnnotation
                                sourceModules
                                ( ( parsedModuleFilePath, parsedModule ), destinationTypeNode )
                                |> Result.map
                                    (\( destinationTypeAnnotation, destinationTypeDependencies ) ->
                                        ( ( originTypeAnnotation, destinationTypeAnnotation )
                                        , originTypeDependencies |> Dict.union destinationTypeDependencies
                                        )
                                    )
                        )
            )


stateTypeAnnotationFromRootFunctionDeclaration :
    Elm.Syntax.Expression.Function
    ->
        Result
            String
            { instantiated : Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String )
            , parameter : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
            }
stateTypeAnnotationFromRootFunctionDeclaration rootFunctionDeclaration =
    case rootFunctionDeclaration.signature of
        Nothing ->
            Err "Missing function signature"

        Just signature ->
            case Elm.Syntax.Node.value (Elm.Syntax.Node.value signature).typeAnnotation of
                Elm.Syntax.TypeAnnotation.Typed instantiated typeArguments ->
                    case typeArguments of
                        [ singleTypeArgument ] ->
                            Ok
                                { instantiated = instantiated
                                , parameter = singleTypeArgument
                                }

                        _ ->
                            Err ("Unexpected number of type arguments: " ++ String.fromInt (List.length typeArguments))

                _ ->
                    Err "Unexpected type annotation: Not an instance"


migrateStateTypeAnnotationFromElmModule :
    Elm.Syntax.File.File
    ->
        Result
            (Elm.Syntax.Node.Node String)
            ( Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation, Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation )
migrateStateTypeAnnotationFromElmModule parsedModule =
    parsedModule.declarations
        |> List.filterMap
            (\declaration ->
                case Elm.Syntax.Node.value declaration of
                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                        if
                            Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                == appStateMigrationInterfaceFunctionName
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
                (case functionDeclaration.signature of
                    Nothing ->
                        Err "Missing function signature"

                    Just functionSignature ->
                        case Elm.Syntax.Node.value (Elm.Syntax.Node.value functionSignature).typeAnnotation of
                            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputType returnType ->
                                case Elm.Syntax.Node.value returnType of
                                    Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation _ _ ->
                                        Err "Too many parameters."

                                    Elm.Syntax.TypeAnnotation.Tupled [ stateTypeAnnotation, _ ] ->
                                        Ok ( inputType, stateTypeAnnotation )

                                    _ ->
                                        Err "Unexpected return type: Not a tuple."

                            _ ->
                                Err "Unexpected type annotation"
                )
                    |> Result.mapError (Elm.Syntax.Node.Node (Elm.Syntax.Node.range functionDeclaration.declaration))
            )
        |> Maybe.withDefault
            (Err
                (Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteModule parsedModule) "Did not find function with matching name")
            )


composeBackendRootElmModuleTextPlatformWebServer : BackendRootModuleConfig -> String
composeBackendRootElmModuleTextPlatformWebServer config =
    let
        mainDeclarationNameQualifiedName =
            config.rootModuleNameBeforeLowering ++ "." ++ config.mainDeclarationName

        functionAliases =
            [ ( "jsonEncodeDeserializedState"
              , config.stateEncodeFunction
              )
            , ( "jsonDecodeDeserializedState"
              , config.stateDecodeFunction
              )
            , ( "jsonDecodeStateShimRequest"
              , config.stateShimRequestDecodeFunction
              )
            , ( "jsonEncodeStateShimResponse"
              , config.stateShimResponseEncodeFunction
              )
            ]

        functionsAliasesText =
            functionAliases
                |> List.map
                    (\( localDeclarationName, ( remoteModuleName, remoteDeclarationName ) ) ->
                        localDeclarationName
                            ++ " =\n    "
                            ++ String.join "." (remoteModuleName ++ [ remoteDeclarationName ])
                    )
                |> String.join "\n\n\n"
    in
    "module "
        ++ config.interfaceToHostRootModuleName
        ++ """ exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import """
        ++ config.rootModuleNameBeforeLowering
        ++ """
"""
        ++ (config.modulesToImport |> List.map (Tuple.pair >> (|>) Nothing >> importSyntaxTextFromModuleNameAndAlias) |> String.join "\n")
        ++ """
import Platform
import Platform.WebServer
import Backend.Generated.StateShimTypes exposing (..)
import Backend.Generated.WebServerShimTypes exposing (..)

type alias DeserializedState =
    ("""
        ++ buildTypeAnnotationText config.stateTypeAnnotation
        ++ """)


type alias DeserializedStateWithTaskFramework =
    { stateLessFramework : DeserializedState
    , nextTaskIndex : Int
    , posixTimeMilli : Int
    , createVolatileProcessTasks : Dict.Dict TaskId (Platform.WebServer.CreateVolatileProcessResult -> DeserializedState -> ( DeserializedState, Platform.WebServer.Commands DeserializedState ))
    , requestToVolatileProcessTasks : Dict.Dict TaskId (Platform.WebServer.RequestToVolatileProcessResult -> DeserializedState -> ( DeserializedState, Platform.WebServer.Commands DeserializedState ))
    , terminateVolatileProcessTasks : Dict.Dict TaskId ()
    }


initDeserializedStateWithTaskFramework : DeserializedState -> DeserializedStateWithTaskFramework
initDeserializedStateWithTaskFramework stateLessFramework =
    { stateLessFramework = stateLessFramework
    , nextTaskIndex = 0
    , posixTimeMilli = 0
    , createVolatileProcessTasks = Dict.empty
    , requestToVolatileProcessTasks = Dict.empty
    , terminateVolatileProcessTasks = Dict.empty
    }


type State
    = DeserializeFailed String
    | DeserializeSuccessful DeserializedStateWithTaskFramework


interfaceToHost_initState =
    """
        ++ mainDeclarationNameQualifiedName
        ++ """.init
        |> Tuple.first
        |> initDeserializedStateWithTaskFramework
        |> DeserializeSuccessful


interfaceToHost_processEvent hostEvent stateBefore =
    case stateBefore of
        DeserializeFailed _ ->
            ( stateBefore, "[]" )

        DeserializeSuccessful deserializedState ->
            deserializedState
                |> wrapForSerialInterface_processEvent """
        ++ mainDeclarationNameQualifiedName
        ++ """.subscriptions hostEvent
                |> Tuple.mapFirst DeserializeSuccessful


interfaceToHost_serializeState =
    jsonEncodeState >> Json.Encode.encode 0


interfaceToHost_deserializeState =
    deserializeState



-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        { init = always ( interfaceToHost_initState, Cmd.none )
        , update =
            { a = interfaceToHost_processEvent
            , b = interfaceToHost_serializeState
            , c = interfaceToHost_deserializeState
            }
                |> always ( interfaceToHost_initState, Cmd.none )
                |> always
                |> always
        , subscriptions = always Sub.none
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
            [ ( "Interface_DeserializeFailed"
              , [ ( "error", error |> Json.Encode.string ) ] |> Json.Encode.object
              )
            ]
                |> Json.Encode.object

        DeserializeSuccessful deserializedState ->
            deserializedState.stateLessFramework |> jsonEncodeDeserializedState


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
        , jsonDecodeDeserializedState |> Json.Decode.map (initDeserializedStateWithTaskFramework >> DeserializeSuccessful)
        ]



----


wrapForSerialInterface_processEvent :
    (DeserializedState -> Platform.WebServer.Subscriptions DeserializedState)
    -> String
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, String )
wrapForSerialInterface_processEvent subscriptions serializedEvent stateBefore =
    let
        ( state, response ) =
            case serializedEvent |> Json.Decode.decodeString jsonDecodeStateShimRequest of
                Err error ->
                    ( stateBefore
                    , ("Failed to deserialize event: " ++ (error |> Json.Decode.errorToString))
                        |> Err
                    )

                Ok hostEvent ->
                    stateBefore
                        |> processEvent subscriptions hostEvent
                        |> Tuple.mapSecond Ok
    in
    ( state, response |> encodeResponseOverSerialInterface |> Json.Encode.encode 0 )


processEvent :
    (DeserializedState -> Platform.WebServer.Subscriptions DeserializedState)
    -> StateShimRequest
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, StateShimResponse )
processEvent subscriptions hostEvent stateBefore =
    let
        maybeEventPosixTimeMilli =
            case hostEvent of
                AppEventShimRequest (HttpRequestEvent httpRequestEvent) ->
                    Just httpRequestEvent.posixTimeMilli

                AppEventShimRequest (PosixTimeHasArrivedEvent posixTimeHasArrivedEvent) ->
                    Just posixTimeHasArrivedEvent.posixTimeMilli

                _ ->
                    Nothing

        state =
            case maybeEventPosixTimeMilli of
                Nothing ->
                    stateBefore

                Just eventPosixTimeMilli ->
                    { stateBefore | posixTimeMilli = max stateBefore.posixTimeMilli eventPosixTimeMilli }
    in
    processEventLessRememberTime subscriptions hostEvent state


processEventLessRememberTime :
    (DeserializedState -> Platform.WebServer.Subscriptions DeserializedState)
    -> StateShimRequest
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, StateShimResponse )
processEventLessRememberTime subscriptions hostEvent stateBefore =
    let
        continueWithState newState =
            ( newState
            , backendEventResponseFromSubscriptions (subscriptions newState.stateLessFramework)
            )

        discardEvent =
            continueWithState stateBefore
                |> Tuple.mapSecond AppEventShimResponse

        continueWithUpdateToTasks updateToTasks stateBeforeUpdateToTasks =
            let
                ( stateLessFramework, runtimeTasks ) =
                    updateToTasks stateBeforeUpdateToTasks.stateLessFramework
            in
            backendEventResponseFromRuntimeTasksAndSubscriptions
                subscriptions
                runtimeTasks
                { stateBeforeUpdateToTasks | stateLessFramework = stateLessFramework }
    in
    case hostEvent of
        AppEventShimRequest (HttpRequestEvent httpRequestEvent) ->
            continueWithUpdateToTasks
                ((subscriptions stateBefore.stateLessFramework).httpRequest httpRequestEvent)
                stateBefore
                |> Tuple.mapSecond AppEventShimResponse

        AppEventShimRequest (PosixTimeHasArrivedEvent posixTimeHasArrivedEvent) ->
            case (subscriptions stateBefore.stateLessFramework).posixTimeIsPast of
                Nothing ->
                    discardEvent

                Just posixTimeIsPastSub ->
                    if posixTimeHasArrivedEvent.posixTimeMilli < posixTimeIsPastSub.minimumPosixTimeMilli then
                        discardEvent

                    else
                        continueWithUpdateToTasks
                            (posixTimeIsPastSub.update { currentPosixTimeMilli = posixTimeHasArrivedEvent.posixTimeMilli })
                            stateBefore
                            |> Tuple.mapSecond AppEventShimResponse

        AppEventShimRequest (TaskCompleteEvent taskCompleteEvent) ->
            case taskCompleteEvent.taskResult of
                CreateVolatileProcessResponse createVolatileProcessResponse ->
                    case Dict.get taskCompleteEvent.taskId stateBefore.createVolatileProcessTasks of
                        Nothing ->
                            discardEvent

                        Just taskEntry ->
                            continueWithUpdateToTasks
                                (taskEntry createVolatileProcessResponse)
                                { stateBefore
                                    | createVolatileProcessTasks =
                                        stateBefore.createVolatileProcessTasks |> Dict.remove taskCompleteEvent.taskId
                                }
                                |> Tuple.mapSecond AppEventShimResponse

                RequestToVolatileProcessResponse requestToVolatileProcessResponse ->
                    case Dict.get taskCompleteEvent.taskId stateBefore.requestToVolatileProcessTasks of
                        Nothing ->
                            discardEvent

                        Just taskEntry ->
                            continueWithUpdateToTasks
                                (taskEntry requestToVolatileProcessResponse)
                                { stateBefore
                                    | requestToVolatileProcessTasks =
                                        stateBefore.requestToVolatileProcessTasks |> Dict.remove taskCompleteEvent.taskId
                                }
                                |> Tuple.mapSecond AppEventShimResponse

                CompleteWithoutResult ->
                    continueWithState
                        { stateBefore
                            | terminateVolatileProcessTasks =
                                stateBefore.terminateVolatileProcessTasks |> Dict.remove taskCompleteEvent.taskId
                        }
                        |> Tuple.mapSecond AppEventShimResponse

        InitStateEvent ->
            continueWithUpdateToTasks (always """
        ++ mainDeclarationNameQualifiedName
        ++ """.init) stateBefore
                |> Tuple.mapSecond AppEventShimResponse

        SetStateEvent stateString ->
            let
                ( ( state, responseBeforeAddingMigrateResult ), migrateResult ) =
                    case setStateFromString stateString of
                        Err migrateError ->
                            ( continueWithState
                                stateBefore
                            , Err migrateError
                            )

                        Ok appState ->
                            ( continueWithUpdateToTasks
                                (always ( appState, [] ))
                                stateBefore
                            , Ok ()
                            )
            in
            ( state
            , { responseBeforeAddingMigrateResult | migrateResult = Just migrateResult }
            )
                |> Tuple.mapSecond AppEventShimResponse

        MigrateStateEvent stateString ->
            let
                ( ( state, responseBeforeAddingMigrateResult ), migrateResult ) =
                    case migrateFromString stateString of
                        Err migrateError ->
                            ( continueWithState
                                stateBefore
                            , Err migrateError
                            )

                        Ok stateAndCmds ->
                            ( continueWithUpdateToTasks
                                (always stateAndCmds)
                                stateBefore
                            , Ok ()
                            )
            in
            ( state
            , { responseBeforeAddingMigrateResult | migrateResult = Just migrateResult }
            )
                |> Tuple.mapSecond AppEventShimResponse


setStateFromString : String -> Result String DeserializedState
setStateFromString =
"""
        ++ indentElmCodeLines 2 config.stateFromStringExpression
        ++ """

migrateFromString : String -> Result String ( DeserializedState, Platform.WebServer.Commands DeserializedState )
migrateFromString =
"""
        ++ indentElmCodeLines 2 config.migrateFromStringExpression
        ++ """


backendEventResponseFromRuntimeTasksAndSubscriptions :
    (DeserializedState -> Platform.WebServer.Subscriptions DeserializedState)
    -> List (Platform.WebServer.Command DeserializedState)
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, BackendEventResponseStruct )
backendEventResponseFromRuntimeTasksAndSubscriptions subscriptions tasks stateBefore =
    tasks
        |> List.foldl
            (\\task ( previousState, previousResponse ) ->
                let
                    ( newState, newResponse ) =
                        backendEventResponseFromRuntimeTask task previousState
                in
                ( newState, newResponse :: previousResponse )
            )
            ( stateBefore
            , [ backendEventResponseFromSubscriptions (subscriptions stateBefore.stateLessFramework) ]
            )
        |> Tuple.mapSecond concatBackendEventResponse


backendEventResponseFromSubscriptions : Platform.WebServer.Subscriptions DeserializedState -> BackendEventResponseStruct
backendEventResponseFromSubscriptions subscriptions =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived =
        subscriptions.posixTimeIsPast
            |> Maybe.map (\\posixTimeIsPast -> { minimumPosixTimeMilli = posixTimeIsPast.minimumPosixTimeMilli })
    , migrateResult = Nothing
    }


backendEventResponseFromRuntimeTask :
    Platform.WebServer.Command DeserializedState
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, BackendEventResponseStruct )
backendEventResponseFromRuntimeTask task stateBefore =
    let
        createTaskId stateBeforeCreateTaskId =
            let
                taskId =
                    String.join "-"
                        [ String.fromInt stateBeforeCreateTaskId.posixTimeMilli
                        , String.fromInt stateBeforeCreateTaskId.nextTaskIndex
                        ]
            in
            ( { stateBeforeCreateTaskId
                | nextTaskIndex = stateBeforeCreateTaskId.nextTaskIndex + 1
              }
            , taskId
            )
    in
    case task of
        Platform.WebServer.RespondToHttpRequest respondToHttpRequest ->
            ( stateBefore
            , passiveBackendEventResponse
                |> withCompleteHttpResponsesAdded [ respondToHttpRequest ]
            )

        Platform.WebServer.CreateVolatileProcess createVolatileProcess ->
            let
                ( stateAfterCreateTaskId, taskId ) =
                    createTaskId stateBefore
            in
            ( { stateAfterCreateTaskId
                | createVolatileProcessTasks =
                    stateAfterCreateTaskId.createVolatileProcessTasks
                        |> Dict.insert taskId createVolatileProcess.update
              }
            , passiveBackendEventResponse
                |> withStartTasksAdded
                    [ { taskId = taskId
                      , task = CreateVolatileProcess { programCode = createVolatileProcess.programCode }
                      }
                    ]
            )

        Platform.WebServer.RequestToVolatileProcess requestToVolatileProcess ->
            let
                ( stateAfterCreateTaskId, taskId ) =
                    createTaskId stateBefore
            in
            ( { stateAfterCreateTaskId
                | requestToVolatileProcessTasks =
                    stateAfterCreateTaskId.requestToVolatileProcessTasks
                        |> Dict.insert taskId requestToVolatileProcess.update
              }
            , passiveBackendEventResponse
                |> withStartTasksAdded
                    [ { taskId = taskId
                      , task =
                            RequestToVolatileProcess
                                { processId = requestToVolatileProcess.processId
                                , request = requestToVolatileProcess.request
                                }
                      }
                    ]
            )

        Platform.WebServer.TerminateVolatileProcess terminateVolatileProcess ->
            let
                ( stateAfterCreateTaskId, taskId ) =
                    createTaskId stateBefore
            in
            ( { stateAfterCreateTaskId
                | terminateVolatileProcessTasks =
                    stateAfterCreateTaskId.terminateVolatileProcessTasks |> Dict.insert taskId ()
              }
            , passiveBackendEventResponse
                |> withStartTasksAdded
                    [ { taskId = taskId
                      , task = TerminateVolatileProcess terminateVolatileProcess
                      }
                    ]
            )


concatBackendEventResponse : List BackendEventResponseStruct -> BackendEventResponseStruct
concatBackendEventResponse responses =
    let
        notifyWhenPosixTimeHasArrived =
            responses
                |> List.filterMap .notifyWhenPosixTimeHasArrived
                |> List.map .minimumPosixTimeMilli
                |> List.minimum
                |> Maybe.map (\\posixTimeMilli -> { minimumPosixTimeMilli = posixTimeMilli })

        startTasks =
            responses |> List.concatMap .startTasks

        completeHttpResponses =
            responses |> List.concatMap .completeHttpResponses
    in
    { notifyWhenPosixTimeHasArrived = notifyWhenPosixTimeHasArrived
    , startTasks = startTasks
    , completeHttpResponses = completeHttpResponses
    , migrateResult = Nothing
    }


passiveBackendEventResponse : BackendEventResponseStruct
passiveBackendEventResponse =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived = Nothing
    , migrateResult = Nothing
    }


withStartTasksAdded : List StartTaskStructure -> BackendEventResponseStruct -> BackendEventResponseStruct
withStartTasksAdded startTasksToAdd responseBefore =
    { responseBefore | startTasks = responseBefore.startTasks ++ startTasksToAdd }


withCompleteHttpResponsesAdded :
    List Platform.WebServer.RespondToHttpRequestStruct
    -> BackendEventResponseStruct
    -> BackendEventResponseStruct
withCompleteHttpResponsesAdded httpResponsesToAdd responseBefore =
    { responseBefore | completeHttpResponses = responseBefore.completeHttpResponses ++ httpResponsesToAdd }


encodeResponseOverSerialInterface : ResponseOverSerialInterface -> Json.Encode.Value
encodeResponseOverSerialInterface responseOverSerialInterface =
    (case responseOverSerialInterface of
        Err error ->
            ( "Err", error |> Json.Encode.string )

        Ok response ->
            ( "Ok", response |> jsonEncodeStateShimResponse )
    )
        |> Tuple.mapSecond (List.singleton >> Json.Encode.list identity)
        |> List.singleton
        |> Json.Encode.object


"""
        ++ functionsAliasesText


stateShimTypesModuleText : String
stateShimTypesModuleText =
    String.trimLeft """
module Backend.Generated.StateShimTypes exposing (..)

import Backend.Generated.WebServerShimTypes


type alias ResponseOverSerialInterface =
    Result String StateShimResponse


type StateShimRequest
    = AppEventShimRequest Backend.Generated.WebServerShimTypes.BackendEvent
    | InitStateEvent
    | SetStateEvent String
    | MigrateStateEvent String


type StateShimResponse
    = AppEventShimResponse Backend.Generated.WebServerShimTypes.BackendEventResponseStruct

"""


webServerShimTypesModuleText : String
webServerShimTypesModuleText =
    String.trimLeft """
module Backend.Generated.WebServerShimTypes exposing (..)

import Platform.WebServer


type alias ExposedFunctionArguments =
    { serializedArguments : List String
    }


type BackendEvent
    = HttpRequestEvent Platform.WebServer.HttpRequestEventStruct
    | TaskCompleteEvent TaskCompleteEventStruct
    | PosixTimeHasArrivedEvent { posixTimeMilli : Int }


type alias BackendEventResponseStruct =
    { startTasks : List StartTaskStructure
    , notifyWhenPosixTimeHasArrived : Maybe { minimumPosixTimeMilli : Int }
    , completeHttpResponses : List Platform.WebServer.RespondToHttpRequestStruct
    , migrateResult : Maybe (Result String ())
    }


type alias TaskCompleteEventStruct =
    { taskId : TaskId
    , taskResult : TaskResultStructure
    }


type TaskResultStructure
    = CreateVolatileProcessResponse (Result Platform.WebServer.CreateVolatileProcessErrorStruct Platform.WebServer.CreateVolatileProcessComplete)
    | RequestToVolatileProcessResponse (Result Platform.WebServer.RequestToVolatileProcessError Platform.WebServer.RequestToVolatileProcessComplete)
    | CompleteWithoutResult


type alias StartTaskStructure =
    { taskId : TaskId
    , task : Task
    }


type Task
    = CreateVolatileProcess CreateVolatileProcessLessUpdateStruct
    | RequestToVolatileProcess RequestToVolatileProcessLessUpdateStruct
    | TerminateVolatileProcess Platform.WebServer.TerminateVolatileProcessStruct


type alias CreateVolatileProcessLessUpdateStruct =
    { programCode : String
    }


type alias RequestToVolatileProcessLessUpdateStruct =
    { processId : String
    , request : String
    }


type alias TaskId =
    String

"""
