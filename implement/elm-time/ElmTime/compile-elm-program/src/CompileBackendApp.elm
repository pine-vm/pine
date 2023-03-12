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
    , stateShimResponseEncodeSerialFunction : ( List String, String )
    , backendEventDecodeFunction : ( List String, String )
    , backendEventResponseEncodeFunction : ( List String, String )
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
            , stateShimResponseResultType : ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
            , backendEventType : ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
            , backendResponseEventType : ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
            }
supportingTypesModules originalSourceModules =
    [ stateShimTypesModuleText
    , stateShimModuleText
    , webServerShimModuleText
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

                                    fakeTypeAnnotationFromLocalName localName =
                                        Elm.Syntax.TypeAnnotation.Typed (syntaxNodeFromEmptyRange ( [], localName )) []

                                    stateShimRequestFakeTypeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation
                                    stateShimRequestFakeTypeAnnotation =
                                        fakeTypeAnnotationFromLocalName "StateShimRequest"

                                    backendEventResponseFakeTypeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation
                                    backendEventResponseFakeTypeAnnotation =
                                        fakeTypeAnnotationFromLocalName "BackendEventResponseStruct"

                                    stateShimResponseResultFakeTypeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation
                                    stateShimResponseResultFakeTypeAnnotation =
                                        fakeTypeAnnotationFromLocalName "ResponseOverSerialInterface"

                                    backendEventFakeTypeAnnotation : Elm.Syntax.TypeAnnotation.TypeAnnotation
                                    backendEventFakeTypeAnnotation =
                                        fakeTypeAnnotationFromLocalName "BackendEvent"
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
                                                ( ( webServerShimTypesModuleFilePath, webServerShimTypesModule.parsedSyntax )
                                                , syntaxNodeFromEmptyRange backendEventResponseFakeTypeAnnotation
                                                )
                                                |> Result.andThen
                                                    (\backendResponseEventType ->
                                                        CompileFullstackApp.parseElmTypeAndDependenciesRecursivelyFromAnnotation
                                                            (Dict.union typesModules originalSourceModules)
                                                            ( ( stateShimTypesModuleFilePath, stateShimTypesModule.parsedSyntax )
                                                            , syntaxNodeFromEmptyRange stateShimResponseResultFakeTypeAnnotation
                                                            )
                                                            |> Result.andThen
                                                                (\stateShimResponseResultType ->
                                                                    CompileFullstackApp.parseElmTypeAndDependenciesRecursivelyFromAnnotation
                                                                        (Dict.union typesModules originalSourceModules)
                                                                        ( ( webServerShimTypesModuleFilePath, webServerShimTypesModule.parsedSyntax )
                                                                        , syntaxNodeFromEmptyRange backendEventFakeTypeAnnotation
                                                                        )
                                                                        |> Result.map
                                                                            (\backendEventType ->
                                                                                { modules = typesModules
                                                                                , stateShimRequestType = stateShimRequestType
                                                                                , stateShimResponseResultType = stateShimResponseResultType
                                                                                , backendEventType = backendEventType
                                                                                , backendResponseEventType = backendResponseEventType
                                                                                }
                                                                            )
                                                                )
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
                                                                |> Dict.union (Tuple.second supportingTypes.backendResponseEventType)
                                                                |> Dict.union (Tuple.second supportingTypes.stateShimResponseResultType)
                                                                |> Dict.union (Tuple.second supportingTypes.backendEventType)

                                                        typeToGenerateSerializersFor : List ElmTypeAnnotation
                                                        typeToGenerateSerializersFor =
                                                            stateType.stateTypeAnnotation
                                                                :: Tuple.first supportingTypes.stateShimRequestType
                                                                :: Tuple.first supportingTypes.backendResponseEventType
                                                                :: Tuple.first supportingTypes.stateShimResponseResultType
                                                                :: Tuple.first supportingTypes.backendEventType
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

                                                        backendEventResponseFunctionsNamesInGeneratedModules =
                                                            buildJsonCodingFunctionsForTypeAnnotation (Tuple.first supportingTypes.backendResponseEventType)

                                                        stateShimResponseResultFunctionsNamesInGeneratedModules =
                                                            buildJsonCodingFunctionsForTypeAnnotation (Tuple.first supportingTypes.stateShimResponseResultType)

                                                        backendEventFunctionsNamesInGeneratedModules =
                                                            buildJsonCodingFunctionsForTypeAnnotation (Tuple.first supportingTypes.backendEventType)

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

                                                        stateShimResponseEncodeSerialFunction =
                                                            ( generateSerializersResult.generatedModuleName
                                                            , stateShimResponseResultFunctionsNamesInGeneratedModules.encodeFunction.name
                                                            )

                                                        backendEventResponseEncodeFunction =
                                                            ( generateSerializersResult.generatedModuleName
                                                            , backendEventResponseFunctionsNamesInGeneratedModules.encodeFunction.name
                                                            )

                                                        backendEventDecodeFunction =
                                                            ( generateSerializersResult.generatedModuleName
                                                            , backendEventFunctionsNamesInGeneratedModules.decodeFunction.name
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
                                                                , stateShimResponseEncodeSerialFunction = stateShimResponseEncodeSerialFunction
                                                                , backendEventResponseEncodeFunction = backendEventResponseEncodeFunction
                                                                , backendEventDecodeFunction = backendEventDecodeFunction
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
            [ ( "jsonEncodeWebServerAppState"
              , config.stateEncodeFunction
              )
            , ( "jsonDecodeWebServerAppState"
              , config.stateDecodeFunction
              )
            , ( "jsonDecodeStateShimRequest"
              , config.stateShimRequestDecodeFunction
              )
            , ( "jsonEncodeStateShimResultResponse"
              , config.stateShimResponseEncodeSerialFunction
              )
            , ( "jsonDecodeBackendEvent"
              , config.backendEventDecodeFunction
              )
            , ( "jsonEncodeBackendEventResponse"
              , config.backendEventResponseEncodeFunction
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
    , interfaceToHost_initState
    , interfaceToHost_processEvent
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
import Backend.Generated.StateShim exposing (StateShimConfig, StateShimState)
import Backend.Generated.StateShimTypes exposing (..)
import Backend.Generated.WebServerShimTypes exposing (..)
import Backend.Generated.WebServerShim


type alias WebServerAppState =
    ("""
        ++ buildTypeAnnotationText config.stateTypeAnnotation
        ++ """)


configurationInit =
    """
        ++ mainDeclarationNameQualifiedName
        ++ """.init


configurationSubscriptions =
    """
        ++ mainDeclarationNameQualifiedName
        ++ """.subscriptions


config_exposedFunctions : Dict.Dict String (Backend.Generated.StateShim.ExposedFunctionRecord WebServerAppStateWithTaskFramework)
config_exposedFunctions =
    [ ( "init"
      , { description = { hasAppStateParam = False, resultContainsAppState = True }
        , handler =
            configurationInit
                |> (\\( appState, commands ) ->
                        Backend.Generated.WebServerShim.backendEventResponseFromRuntimeTasksAndSubscriptions
                            configurationSubscriptions
                            commands
                            (initWebServerAppStateWithTaskFramework appState)
                   )
                |> Tuple.mapSecond (jsonEncodeBackendEventResponse >> Json.Encode.encode 0)
                |> Tuple.mapFirst Just
                |> Tuple.mapSecond Just
                |> Ok
                |> always
        }
      )
    , ( "processEvent"
      , { description = { hasAppStateParam = True, resultContainsAppState = True }
        , handler =
            Backend.Generated.StateShim.exposedFunctionExpectingSingleArgumentAndAppState
                jsonDecodeBackendEvent
                (\\backendEvent ->
                    Backend.Generated.WebServerShim.processWebServerEvent configurationSubscriptions backendEvent
                        >> Tuple.mapFirst Just
                        >> Tuple.mapSecond (jsonEncodeBackendEventResponse >> Json.Encode.encode 0 >> Just)
                        >> Ok
                )
        }
      )
    , ( "migrate"
      , { description = { hasAppStateParam = False, resultContainsAppState = True }
        , handler =
            Backend.Generated.StateShim.exposedFunctionExpectingSingleArgument
                Json.Decode.string
                (migrateFromStringPackageWebServerShim
                    >> Result.map
                        (Tuple.mapFirst Just
                            >> Tuple.mapSecond (jsonEncodeBackendEventResponse >> Json.Encode.encode 0 >> Just)
                        )
                )
        }
      )    ]
        |> Dict.fromList


type alias State =
    StateShimState WebServerAppStateWithTaskFramework


type alias WebServerAppStateWithTaskFramework =
    WebServerShimState WebServerAppState


initWebServerAppStateWithTaskFramework : WebServerAppState -> WebServerAppStateWithTaskFramework
initWebServerAppStateWithTaskFramework stateLessFramework =
    { stateLessFramework = stateLessFramework
    , nextTaskIndex = 0
    , posixTimeMilli = 0
    , createVolatileProcessTasks = Dict.empty
    , requestToVolatileProcessTasks = Dict.empty
    , terminateVolatileProcessTasks = Dict.empty
    }


interfaceToHost_initState =
    Backend.Generated.StateShim.init


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    wrapForSerialInterface_processEvent
        { jsonDecodeWebServerAppState = jsonDecodeWebServerAppState |> Json.Decode.map initWebServerAppStateWithTaskFramework
        , jsonEncodeWebServerAppState = .stateLessFramework >> jsonEncodeWebServerAppState
        , exposedFunctions = config_exposedFunctions
        }


-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        { init = always ( interfaceToHost_initState, Cmd.none )
        , update =
            { a = interfaceToHost_processEvent
            }
                |> always ( interfaceToHost_initState, Cmd.none )
                |> always
                |> always
        , subscriptions = always Sub.none
        }


wrapForSerialInterface_processEvent :
    StateShimConfig appState
    -> String
    -> StateShimState appState
    -> ( StateShimState appState, String )
wrapForSerialInterface_processEvent config serializedEvent stateBefore =
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
                        |> Backend.Generated.StateShim.processEvent config hostEvent
                        |> Tuple.mapSecond Ok
    in
    ( state, response |> jsonEncodeStateShimResultResponse |> Json.Encode.encode 0 )


migrateFromString : String -> Result String ( WebServerAppState, Platform.WebServer.Commands WebServerAppState )
migrateFromString =
"""
        ++ indentElmCodeLines 2 config.migrateFromStringExpression
        ++ """


migrateFromStringPackageWebServerShim :
    String
    -> Result String ( WebServerShimState WebServerAppState, Backend.Generated.WebServerShimTypes.BackendEventResponseStruct )
migrateFromStringPackageWebServerShim =
    migrateFromString
        >> Result.map
            (\\( appState, appCommands ) ->
                appState
                    |> initWebServerAppStateWithTaskFramework
                    |> Backend.Generated.WebServerShim.backendEventResponseFromRuntimeTasksAndSubscriptions
                        configurationSubscriptions
                        appCommands
            )


"""
        ++ functionsAliasesText


stateShimModuleText : String
stateShimModuleText =
    String.trimLeft """
module Backend.Generated.StateShim exposing (..)

import Backend.Generated.StateShimTypes exposing (..)
import Dict
import Json.Decode
import Json.Encode


type alias StateShimConfig appState =
    { exposedFunctions : Dict.Dict String (ExposedFunctionRecord appState)
    , jsonEncodeWebServerAppState : appState -> Json.Encode.Value
    , jsonDecodeWebServerAppState : Json.Decode.Decoder appState
    }


type alias StateShimState appState =
    { branches : Dict.Dict String appState }


type alias ExposedFunctionHandler appState =
    ApplyFunctionArguments (Maybe appState) -> Result String ( Maybe appState, Maybe String )


type alias ExposedFunctionRecord appState =
    { description : ExposedFunctionDescription
    , handler : ExposedFunctionHandler appState
    }


init : StateShimState appState
init =
    { branches = Dict.empty }


processEvent :
    StateShimConfig appState
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
                    returnError ("None of the exposed functions matches name '" ++ applyFunction.functionName ++ "'")

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
                                            , ApplyFunctionShimResponse (Ok { resultLessStateJson = maybeFunctionResultOther })
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
                        |> config.jsonEncodeWebServerAppState
                        |> Json.Encode.encode 0
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


setStateOnBranches : List String -> appState -> StateShimState appState -> StateShimState appState
setStateOnBranches branches appState stateBefore =
    { stateBefore
        | branches =
            branches
                |> List.foldl
                    (\\branchName -> Dict.insert branchName appState)
                    stateBefore.branches
    }


resolveStateSource : StateShimConfig appState -> StateShimState appState -> StateSource -> Result String appState
resolveStateSource config shimState stateSource =
    case stateSource of
        SerializedJsonStateSource stateJson ->
            stateJson
                |> Json.Decode.decodeString config.jsonDecodeWebServerAppState
                |> Result.mapError (Json.Decode.errorToString >> (++) "Failed to decode: ")

        BranchStateSource branchName ->
            case Dict.get branchName shimState.branches of
                Nothing ->
                    Err ("Branch named '" ++ branchName ++ "' does not exist")

                Just appState ->
                    Ok appState


exposedFunctionExpectingSingleArgument :
    Json.Decode.Decoder arg
    -> (arg -> Result String ( Maybe appState, Maybe String ))
    -> ExposedFunctionHandler appState
exposedFunctionExpectingSingleArgument argumentDecoder funcAfterDecode genericArguments =
    case genericArguments.serializedArgumentsJson of
        [ singleArgumentJson ] ->
            case Json.Decode.decodeString argumentDecoder singleArgumentJson of
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
    -> (arg -> appState -> Result String ( Maybe appState, Maybe String ))
    -> ExposedFunctionHandler appState
exposedFunctionExpectingSingleArgumentAndAppState argumentDecoder funcAfterDecode genericArguments =
    case genericArguments.stateArgument of
        Nothing ->
            Err "Type mismatch: Missing state argument"

        Just appState ->
            case genericArguments.serializedArgumentsJson of
                [ singleArgumentJson ] ->
                    case Json.Decode.decodeString argumentDecoder singleArgumentJson of
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


type alias ResponseOverSerialInterface =
    Result String StateShimResponse


type StateShimRequest
    = ListExposedFunctionsShimRequest
    | ApplyFunctionShimRequest ApplyFunctionShimRequestStruct
    | SerializeStateShimRequest StateSource
    | SetBranchesStateShimRequest StateSource (List String)


type StateShimResponse
    = ListExposedFunctionsShimResponse (List { functionName : String, functionDescription : ExposedFunctionDescription })
    | ApplyFunctionShimResponse (Result String FunctionApplicationResult)
    | SerializeStateShimResponse (Result String String)
    | SetBranchesStateShimResponse (Result String String)


type alias ApplyFunctionShimRequestStruct =
    { functionName : String
    , arguments : ApplyFunctionArguments (Maybe StateSource)
    , stateDestinationBranches : List String
    }


type alias ApplyFunctionArguments state =
    { stateArgument : state
    , serializedArgumentsJson : List String
    }


type StateSource
    = SerializedJsonStateSource String
    | BranchStateSource String


type alias ExposedFunctionDescription =
    { hasAppStateParam : Bool
    , resultContainsAppState : Bool
    }


type alias FunctionApplicationResult =
    { resultLessStateJson : Maybe String
    }

"""


webServerShimModuleText : String
webServerShimModuleText =
    String.trimLeft """
module Backend.Generated.WebServerShim exposing (..)

import Backend.Generated.WebServerShimTypes exposing (..)
import Dict
import Platform.WebServer


processWebServerEvent :
    (appState -> Platform.WebServer.Subscriptions appState)
    -> Backend.Generated.WebServerShimTypes.BackendEvent
    -> WebServerShimState appState
    -> ( WebServerShimState appState, Backend.Generated.WebServerShimTypes.BackendEventResponseStruct )
processWebServerEvent subscriptions hostEvent stateBefore =
    let
        maybeEventPosixTimeMilli =
            case hostEvent of
                HttpRequestEvent httpRequestEvent ->
                    Just httpRequestEvent.posixTimeMilli

                PosixTimeHasArrivedEvent posixTimeHasArrivedEvent ->
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
    processWebServerEventLessRememberTime subscriptions hostEvent state


processWebServerEventLessRememberTime :
    (appState -> Platform.WebServer.Subscriptions appState)
    -> Backend.Generated.WebServerShimTypes.BackendEvent
    -> WebServerShimState appState
    -> ( WebServerShimState appState, Backend.Generated.WebServerShimTypes.BackendEventResponseStruct )
processWebServerEventLessRememberTime subscriptions hostEvent stateBefore =
    let
        continueWithState newState =
            ( newState
            , backendEventResponseFromSubscriptions (subscriptions newState.stateLessFramework)
            )

        discardEvent =
            continueWithState stateBefore

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
        HttpRequestEvent httpRequestEvent ->
            continueWithUpdateToTasks
                ((subscriptions stateBefore.stateLessFramework).httpRequest httpRequestEvent)
                stateBefore

        PosixTimeHasArrivedEvent posixTimeHasArrivedEvent ->
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

        TaskCompleteEvent taskCompleteEvent ->
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

                CompleteWithoutResult ->
                    continueWithState
                        { stateBefore
                            | terminateVolatileProcessTasks =
                                stateBefore.terminateVolatileProcessTasks |> Dict.remove taskCompleteEvent.taskId
                        }


backendEventResponseFromRuntimeTasksAndSubscriptions :
    (appState -> Platform.WebServer.Subscriptions appState)
    -> List (Platform.WebServer.Command appState)
    -> WebServerShimState appState
    -> ( WebServerShimState appState, BackendEventResponseStruct )
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


backendEventResponseFromRuntimeTask :
    Platform.WebServer.Command appState
    -> WebServerShimState appState
    -> ( WebServerShimState appState, BackendEventResponseStruct )
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


backendEventResponseFromSubscriptions : Platform.WebServer.Subscriptions appState -> BackendEventResponseStruct
backendEventResponseFromSubscriptions subscriptions =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived =
        subscriptions.posixTimeIsPast
            |> Maybe.map (\\posixTimeIsPast -> { minimumPosixTimeMilli = posixTimeIsPast.minimumPosixTimeMilli })
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

"""


webServerShimTypesModuleText : String
webServerShimTypesModuleText =
    String.trimLeft """
module Backend.Generated.WebServerShimTypes exposing (..)

import Dict
import Platform.WebServer


type alias WebServerShimState appState =
    { stateLessFramework : appState
    , nextTaskIndex : Int
    , posixTimeMilli : Int
    , createVolatileProcessTasks :
        Dict.Dict
            TaskId
            (Platform.WebServer.CreateVolatileProcessResult
             -> appState
             -> ( appState, Platform.WebServer.Commands appState )
            )
    , requestToVolatileProcessTasks :
        Dict.Dict
            TaskId
            (Platform.WebServer.RequestToVolatileProcessResult
             -> appState
             -> ( appState, Platform.WebServer.Commands appState )
            )
    , terminateVolatileProcessTasks : Dict.Dict TaskId ()
    }


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
