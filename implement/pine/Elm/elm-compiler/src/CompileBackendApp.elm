module CompileBackendApp exposing (..)

import Common
import CompileElmApp
    exposing
        ( AppFiles
        , CompilationError(..)
        , CompileEntryPointConfig
        , ElmChoiceTypeStruct
        , ElmMakeEntryPointKind(..)
        , ElmTypeAnnotation
        , EntryPointClass
        , LocatedInSourceFiles(..)
        , SourceParsedElmModule
        , buildJsonConverterFunctionsForTypeAnnotation
        , entryPointClassFromSetOfEquallyProcessedFunctionNames
        , filePathFromElmModuleName
        , findModuleByName
        , findSourceDirectories
        , getTextLinesFromRange
        , locatedInSourceFilesFromRange
        , mapLocatedInSourceFiles
        , parseElmFunctionTypeAndDependenciesRecursivelyFromAnnotation
        , parseElmTypeAndDependenciesRecursivelyFromAnnotation
        , syntaxRangeCoveringCompleteModule
        )
import CompileElmAppWithStateShim
    exposing
        ( ExposedFunctionConfig
        , StateShimConfig
        , StateShimConfigJsonConverter
        , StateShimConfigJsonConverterConfig
        , loweredForAppInStateManagementShim
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


type alias MigrationConfig =
    { inputType : ElmTypeAnnotation
    , returnType : ElmTypeAnnotation
    , dependencies : Dict.Dict ( List String, String ) ElmChoiceTypeStruct
    , migrateFunctionModuleName : List String
    , migrateFunctionDeclarationLocalName : String
    , jsonConverterDeclarations : Dict.Dict String StateShimConfigJsonConverter
    , rootModuleSupportingFunctions : List String
    , modulesToImport : List (List String)
    }


type alias ExposeFunctionsConfig =
    { exposedFunctions : Dict.Dict String ExposedFunctionConfig
    , jsonConverterDeclarations : Dict.Dict String StateShimConfigJsonConverter
    , modulesToImport : List (List String)
    }


appStateMigrationInterfaceModuleName : List String
appStateMigrationInterfaceModuleName =
    [ "Backend", "MigrateState" ]


appStateMigrationInterfaceFunctionName : String
appStateMigrationInterfaceFunctionName =
    "migrate"


exposeFunctionsToAdminModuleName : List String
exposeFunctionsToAdminModuleName =
    [ "Backend", "ExposeFunctionsToAdmin" ]


platformModuleNameCandidates : List (List String)
platformModuleNameCandidates =
    [ [ "Platform", "WebService" ]
    , [ "Platform", "WebServer" ]
    ]


entryPoints : List EntryPointClass
entryPoints =
    [ entryPointClassFromSetOfEquallyProcessedFunctionNames
        [ "webServiceMain", "webServerMain", "backendMain" ]
        (\functionDeclaration entryPointConfig sourceFiles ->
            case loweredForBackendApp functionDeclaration entryPointConfig sourceFiles of
                Err err ->
                    Err err

                Ok compiledFiles ->
                    Ok
                        { compiledFiles = compiledFiles
                        , rootModuleEntryPointKind = ClassicMakeEntryPoint
                        }
        )
    ]


loweredForBackendApp :
    Elm.Syntax.Expression.Function
    -> CompileEntryPointConfig
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) AppFiles
loweredForBackendApp appDeclaration config sourceFiles =
    case
        findSourceDirectories
            { compilationRootFilePath = config.compilationRootFilePath
            , sourceFiles = sourceFiles
            }
    of
        Err err ->
            Err
                [ LocatedInSourceFiles
                    { filePath = config.compilationRootFilePath
                    , locationInModuleText = Elm.Syntax.Range.emptyRange
                    }
                    (OtherCompilationError ("Failed to find source directories: " ++ err))
                ]

        Ok sourceDirs ->
            let
                interfaceToHostRootFilePath : List String
                interfaceToHostRootFilePath =
                    filePathFromElmModuleName sourceDirs config.interfaceToHostRootModuleName
            in
            if Common.assocListGet interfaceToHostRootFilePath sourceFiles /= Nothing then
                -- Support integrating applications supplying their own lowered version.
                Ok sourceFiles

            else
                parseAppStateElmTypeAndDependenciesRecursively
                    appDeclaration
                    config.originalSourceModules
                    ( config.compilationRootFilePath, config.compilationRootModule.parsedSyntax )
                    |> Result.mapError (mapLocatedInSourceFiles ((++) "Failed to parse state type: "))
                    |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)
                    |> Result.andThen
                        (\appStateType ->
                            parseMigrationConfig { originalSourceModules = config.originalSourceModules }
                                |> Result.andThen
                                    (\maybeMigrationConfig ->
                                        parseExposeFunctionsToAdminConfig
                                            { originalSourceModules = config.originalSourceModules
                                            , backendStateType = appStateType.stateTypeAnnotation
                                            }
                                            |> Result.mapError (List.map (mapLocatedInSourceFiles OtherCompilationError))
                                            |> Result.andThen
                                                (\maybeExposeFunctionsToAdmin ->
                                                    let
                                                        mainDeclarationName : String
                                                        mainDeclarationName =
                                                            appDeclaration.declaration
                                                                |> Elm.Syntax.Node.value
                                                                |> .name
                                                                |> Elm.Syntax.Node.value

                                                        appRootDeclarationModuleName : String
                                                        appRootDeclarationModuleName =
                                                            config.compilationRootModule.parsedSyntax.moduleDefinition
                                                                |> Elm.Syntax.Node.value
                                                                |> Elm.Syntax.Module.moduleName
                                                                |> String.join "."

                                                        mainDeclarationNameQualifiedName : String
                                                        mainDeclarationNameQualifiedName =
                                                            appRootDeclarationModuleName ++ "." ++ mainDeclarationName

                                                        rootModuleSupportingFunctionsMigrate =
                                                            maybeMigrationConfig
                                                                |> Maybe.map .rootModuleSupportingFunctions
                                                                |> Maybe.withDefault []

                                                        rootModuleSupportingFunctions =
                                                            [ "config_init = "
                                                                ++ mainDeclarationNameQualifiedName
                                                                ++ ".init"
                                                            , "config_subscriptions = "
                                                                ++ mainDeclarationNameQualifiedName
                                                                ++ ".subscriptions"
                                                            ]
                                                                ++ rootModuleSupportingFunctionsMigrate

                                                        jsonConverterDeclarationsConfigs : List ( String, StateShimConfigJsonConverterConfig )
                                                        jsonConverterDeclarationsConfigs =
                                                            [ ( "jsonDecodeBackendEvent"
                                                              , { isDecoder = True
                                                                , moduleName = [ "Backend", "Generated", "WebServiceShimTypes" ]
                                                                , declarationName = "BackendEvent"
                                                                }
                                                              )
                                                            , ( "jsonEncodeBackendEventResponse"
                                                              , { isDecoder = False
                                                                , moduleName = [ "Backend", "Generated", "WebServiceShimTypes" ]
                                                                , declarationName = "BackendEventResponseStruct"
                                                                }
                                                              )
                                                            ]

                                                        jsonConverterDeclarationsMigrate =
                                                            maybeMigrationConfig
                                                                |> Maybe.map .jsonConverterDeclarations
                                                                |> Maybe.withDefault Dict.empty

                                                        jsonConverterDeclarationsExposedFunctionsToAdmin =
                                                            maybeExposeFunctionsToAdmin
                                                                |> Maybe.map .jsonConverterDeclarations
                                                                |> Maybe.withDefault Dict.empty

                                                        jsonConverterDeclarations =
                                                            [ ( "jsonEncodeAppState"
                                                              , { isDecoder = False
                                                                , typeAnnotation = appStateType.stateTypeAnnotation
                                                                , dependencies = appStateType.dependencies
                                                                }
                                                              )
                                                            , ( "jsonDecodeAppState"
                                                              , { isDecoder = True
                                                                , typeAnnotation = appStateType.stateTypeAnnotation
                                                                , dependencies = appStateType.dependencies
                                                                }
                                                              )
                                                            ]
                                                                |> Dict.fromList
                                                                |> Dict.union jsonConverterDeclarationsMigrate
                                                                |> Dict.union jsonConverterDeclarationsExposedFunctionsToAdmin

                                                        modulesToImportMigrate =
                                                            maybeMigrationConfig
                                                                |> Maybe.map .modulesToImport
                                                                |> Maybe.withDefault []

                                                        modulesToImportExposeFunctionsToAdmin =
                                                            maybeExposeFunctionsToAdmin
                                                                |> Maybe.map .modulesToImport
                                                                |> Maybe.withDefault []

                                                        exposedFunctionsMigrate =
                                                            maybeMigrationConfig
                                                                |> Maybe.map exposedFunctionsFromMigrationConfig
                                                                |> Maybe.withDefault Dict.empty

                                                        exposedFunctionsToAdmin =
                                                            maybeExposeFunctionsToAdmin
                                                                |> Maybe.map .exposedFunctions
                                                                |> Maybe.withDefault Dict.empty

                                                        exposedFunctions =
                                                            exposedFunctionsMigrate
                                                                |> Dict.union exposedFunctionsToAdmin
                                                    in
                                                    case
                                                        config.originalSourceModules
                                                            |> Common.listFind
                                                                (\( _, candidate ) ->
                                                                    List.member
                                                                        candidate.moduleName
                                                                        platformModuleNameCandidates
                                                                )
                                                    of
                                                        Nothing ->
                                                            Err
                                                                [ LocatedInSourceFiles
                                                                    { filePath = config.compilationRootFilePath
                                                                    , locationInModuleText = Elm.Syntax.Range.emptyRange
                                                                    }
                                                                    (OtherCompilationError "Did not find platform module")
                                                                ]

                                                        Just ( _, platformModule ) ->
                                                            let
                                                                platformSupportingModules : WebServiceShimVersionModules
                                                                platformSupportingModules =
                                                                    webServiceShimVersionModules platformModule

                                                                stateShimConfig : StateShimConfig
                                                                stateShimConfig =
                                                                    { jsonConverterDeclarationsConfigs = jsonConverterDeclarationsConfigs
                                                                    , jsonConverterDeclarations = jsonConverterDeclarations
                                                                    , appStateType =
                                                                        { typeAnnotation = appStateType.stateTypeAnnotation
                                                                        , dependencies = appStateType.dependencies
                                                                        }
                                                                    , initAppShimStateExpression = ""
                                                                    , appStateLessShimExpression = ""
                                                                    , exposedFunctions = exposedFunctions
                                                                    , supportingModules =
                                                                        [ platformSupportingModules.webServiceShimModuleText
                                                                        , platformSupportingModules.webServiceShimTypesModuleText
                                                                        ]
                                                                    , rootModuleSupportingFunctions = rootModuleSupportingFunctions
                                                                    , modulesToImport = modulesToImportMigrate ++ modulesToImportExposeFunctionsToAdmin
                                                                    , appStateWithPlatformShimTypeAnnotationFromAppStateAnnotation =
                                                                        (++) "Backend.Generated.WebServiceShimTypes.WebServiceShimState "
                                                                    }
                                                            in
                                                            loweredForAppInStateManagementShim
                                                                sourceDirs
                                                                stateShimConfig
                                                                config
                                                                sourceFiles
                                                )
                                    )
                        )


exposedFunctionsFromMigrationConfig : MigrationConfig -> Dict.Dict String ExposedFunctionConfig
exposedFunctionsFromMigrationConfig _ =
    [ ( "migrate"
      , { description =
            { returnType = { sourceCodeText = "", containsAppStateType = True }
            , parameters = []
            }
        , handlerExpression = """
Backend.Generated.StateShim.exposedFunctionExpectingSingleArgument
    Json.Decode.value
    (migrateFromStringPackageWebServiceShim
        >> Result.map
            (Tuple.mapFirst Just
                >> Tuple.mapSecond (jsonEncodeBackendEventResponse >> Just)
            )
    )
"""
        }
      )
    ]
        |> Dict.fromList


parseMigrationConfig :
    { originalSourceModules : List ( List String, SourceParsedElmModule ) }
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
                            , jsonConverterDeclarations =
                                [ ( "jsonDecodeMigratePreviousState"
                                  , { isDecoder = True
                                    , typeAnnotation = inputType
                                    , dependencies = stateTypeDependencies
                                    }
                                  )
                                ]
                                    |> Dict.fromList
                            , rootModuleSupportingFunctions =
                                [ """
migrateFromStringPackageWebServiceShim =
    Json.Decode.decodeValue jsonDecodeMigratePreviousState
        >> Result.mapError Json.Decode.errorToString
        >> Result.map """ ++ String.join "." (appStateMigrationInterfaceModuleName ++ [ appStateMigrationInterfaceFunctionName ]) ++ """
        >> Result.map
            (\\( appState, appCommands ) ->
                appState
                    |> Backend.Generated.WebServiceShim.initWebServiceShimState
                    |> Backend.Generated.WebServiceShim.backendEventResponseFromRuntimeTasksAndSubscriptions
                        config_subscriptions
                        appCommands
            )
"""
                                ]
                            , modulesToImport = [ appStateMigrationInterfaceModuleName ]
                            }
                    )
                |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)


parseAppStateElmTypeAndDependenciesRecursively :
    Elm.Syntax.Expression.Function
    -> List ( List String, SourceParsedElmModule )
    -> ( List String, Elm.Syntax.File.File )
    ->
        Result
            (LocatedInSourceFiles String)
            { stateTypeAnnotation : ElmTypeAnnotation
            , dependencies : Dict.Dict ( List String, String ) ElmChoiceTypeStruct
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
                            let
                                (Elm.Syntax.Node.Node _ instantiatedName) =
                                    stateTypeAnnotation.instantiated
                            in
                            { stateTypeAnnotation = stateType
                            , dependencies = Dict.fromList dependencies
                            , instantiatedConfigTypeName =
                                Tuple.first instantiatedName
                                    ++ [ Tuple.second instantiatedName ]
                            }
                        )
            )


parseAppStateMigrateElmTypeAndDependenciesRecursively :
    List ( List String, SourceParsedElmModule )
    -> ( List String, Elm.Syntax.File.File )
    ->
        Result
            (LocatedInSourceFiles String)
            ( ( ElmTypeAnnotation, ElmTypeAnnotation )
            , Dict.Dict ( List String, String ) ElmChoiceTypeStruct
            )
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
                                        , Dict.fromList
                                            (List.concat
                                                [ originTypeDependencies
                                                , destinationTypeDependencies
                                                ]
                                            )
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

        Just (Elm.Syntax.Node.Node _ signature) ->
            case signature.typeAnnotation of
                Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.Typed instantiated typeArguments) ->
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
    case
        parsedModule.declarations
            |> Common.listMapFind
                (\(Elm.Syntax.Node.Node _ declaration) ->
                    case declaration of
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
    of
        Nothing ->
            Err
                (Elm.Syntax.Node.Node
                    (syntaxRangeCoveringCompleteModule parsedModule)
                    "Did not find function with matching name"
                )

        Just functionDeclaration ->
            (case functionDeclaration.signature of
                Nothing ->
                    Err "Missing function signature"

                Just (Elm.Syntax.Node.Node _ functionSignature) ->
                    case Elm.Syntax.Node.value functionSignature.typeAnnotation of
                        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputType (Elm.Syntax.Node.Node _ returnType) ->
                            case returnType of
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


parseExposeFunctionsToAdminConfig :
    { originalSourceModules : List ( List String, SourceParsedElmModule )
    , backendStateType : ElmTypeAnnotation
    }
    -> Result (List (LocatedInSourceFiles String)) (Maybe ExposeFunctionsConfig)
parseExposeFunctionsToAdminConfig { originalSourceModules, backendStateType } =
    case findModuleByName exposeFunctionsToAdminModuleName originalSourceModules of
        Nothing ->
            Ok Nothing

        Just ( originalInterfaceModuleFilePath, originalInterfaceModule ) ->
            let
                functionDeclarations =
                    originalInterfaceModule.parsedSyntax.declarations
                        |> List.filterMap
                            (\declaration ->
                                case Elm.Syntax.Node.value declaration of
                                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                        Just functionDeclaration

                                    _ ->
                                        Nothing
                            )
            in
            functionDeclarations
                |> Common.resultListMapCombine
                    (\functionDeclaration ->
                        parseExposeFunctionsToAdminConfigFromDeclaration
                            { originalSourceModules = originalSourceModules
                            , interfaceModuleFilePath = originalInterfaceModuleFilePath
                            , interfaceModule = originalInterfaceModule
                            , backendStateType = backendStateType
                            }
                            functionDeclaration
                            |> Result.mapError
                                (mapLocatedInSourceFiles
                                    ((++)
                                        ("Failed exposing function '"
                                            ++ Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                            ++ "': "
                                        )
                                    )
                                )
                    )
                |> Result.mapError List.singleton
                |> Result.map
                    (\parsedDeclarations ->
                        Just
                            { exposedFunctions =
                                parsedDeclarations |> List.foldl (.exposedFunctions >> Dict.union) Dict.empty
                            , jsonConverterDeclarations =
                                parsedDeclarations |> List.foldl (.jsonConverterDeclarations >> Dict.union) Dict.empty
                            , modulesToImport =
                                parsedDeclarations |> List.foldl (.modulesToImport >> (++)) []
                            }
                    )


parseExposeFunctionsToAdminConfigFromDeclaration :
    { originalSourceModules : List ( List String, SourceParsedElmModule )
    , interfaceModuleFilePath : List String
    , interfaceModule : SourceParsedElmModule
    , backendStateType : ElmTypeAnnotation
    }
    -> Elm.Syntax.Expression.Function
    -> Result (LocatedInSourceFiles String) ExposeFunctionsConfig
parseExposeFunctionsToAdminConfigFromDeclaration { originalSourceModules, interfaceModuleFilePath, interfaceModule, backendStateType } functionDeclaration =
    let
        (Elm.Syntax.Node.Node _ declaration) =
            functionDeclaration.declaration

        functionName : String
        functionName =
            Elm.Syntax.Node.value declaration.name

        returnErrorInInterfaceModule : a -> Result (LocatedInSourceFiles a) value
        returnErrorInInterfaceModule error =
            Err
                (LocatedInSourceFiles
                    { filePath = interfaceModuleFilePath
                    , locationInModuleText = Elm.Syntax.Range.emptyRange
                    }
                    error
                )

        parametersSourceCodeTexts : List String
        parametersSourceCodeTexts =
            declaration.arguments
                |> List.map
                    (\argumentNode ->
                        getTextLinesFromRange
                            (Elm.Syntax.Node.range argumentNode)
                            interfaceModule.fileText
                            |> String.join "\n"
                    )
    in
    case Maybe.map Elm.Syntax.Node.value functionDeclaration.signature of
        Nothing ->
            returnErrorInInterfaceModule "Missing function signature"

        Just functionSignature ->
            parseElmFunctionTypeAndDependenciesRecursivelyFromAnnotation
                originalSourceModules
                ( ( interfaceModuleFilePath, interfaceModule.parsedSyntax ), functionSignature.typeAnnotation )
                |> Result.andThen
                    (\exposedFunctionTypeAnnotation ->
                        case List.reverse (Tuple.first exposedFunctionTypeAnnotation) of
                            [] ->
                                returnErrorInInterfaceModule "Zero types in function type annotation?"

                            (Elm.Syntax.Node.Node returnTypeAnnotationRange returnTypeAnnotation) :: functionArgumentsReversed ->
                                let
                                    ( hasAppStateParam, functionArgumentsLessState ) =
                                        if
                                            (functionArgumentsReversed
                                                |> List.head
                                                |> Maybe.map Elm.Syntax.Node.value
                                            )
                                                == Just backendStateType
                                        then
                                            ( True, functionArgumentsReversed |> List.drop 1 |> List.reverse )

                                        else
                                            ( False, List.reverse functionArgumentsReversed )

                                    parameters : List CompileElmAppWithStateShim.ExposedFunctionParameterDescription
                                    parameters =
                                        functionArgumentsReversed
                                            |> List.reverse
                                            |> List.indexedMap
                                                (\parameterIndex (Elm.Syntax.Node.Node paramTypeAnnotationRange paramTypeAnnotation) ->
                                                    { patternSourceCodeText =
                                                        parametersSourceCodeTexts
                                                            |> List.drop parameterIndex
                                                            |> List.head
                                                            |> Maybe.withDefault "unknown"
                                                    , typeSourceCodeText =
                                                        getTextLinesFromRange
                                                            paramTypeAnnotationRange
                                                            interfaceModule.fileText
                                                            |> String.join "\n"
                                                    , typeIsAppStateType =
                                                        paramTypeAnnotation == backendStateType
                                                    }
                                                )

                                    localJsonConverterFunctionFromTypeAnnotation { isDecoder } typeAnnotation =
                                        let
                                            converterFunctions =
                                                buildJsonConverterFunctionsForTypeAnnotation typeAnnotation

                                            nameSuffix =
                                                if isDecoder then
                                                    converterFunctions.decodeFunction

                                                else
                                                    converterFunctions.encodeFunction
                                        in
                                        ( "expose_param_" ++ nameSuffix.name
                                        , ( { isDecoder = isDecoder }, typeAnnotation )
                                        )

                                    argumentsJsonDecoders : List ( String, ( { isDecoder : Bool }, ElmTypeAnnotation ) )
                                    argumentsJsonDecoders =
                                        functionArgumentsLessState
                                            |> List.map
                                                (Elm.Syntax.Node.value
                                                    >> localJsonConverterFunctionFromTypeAnnotation { isDecoder = True }
                                                )

                                    returnTypeJsonEncoders : List ( String, ( { isDecoder : Bool }, ElmTypeAnnotation ) )
                                    returnTypeJsonEncoders =
                                        if returnTypeAnnotation == backendStateType then
                                            []

                                        else
                                            [ localJsonConverterFunctionFromTypeAnnotation
                                                { isDecoder = False }
                                                returnTypeAnnotation
                                            ]

                                    returnTypeEncoderFunction : Maybe String
                                    returnTypeEncoderFunction =
                                        returnTypeJsonEncoders |> List.head |> Maybe.map Tuple.first

                                    jsonConverterDeclarations =
                                        (argumentsJsonDecoders ++ returnTypeJsonEncoders)
                                            |> List.map
                                                (Tuple.mapSecond
                                                    (\( { isDecoder }, typeAnnotation ) ->
                                                        { isDecoder = isDecoder
                                                        , typeAnnotation = typeAnnotation
                                                        , dependencies =
                                                            Dict.fromList (Tuple.second exposedFunctionTypeAnnotation)
                                                        }
                                                    )
                                                )
                                            |> Dict.fromList

                                    exposedFunctionQualifiedName : String
                                    exposedFunctionQualifiedName =
                                        String.join "." (interfaceModule.moduleName ++ [ functionName ])

                                    composeHandler : { expression : String, resultContainsAppState : Bool }
                                    composeHandler =
                                        buildExposedFunctionHandlerExpression
                                            { exposedFunctionQualifiedName = exposedFunctionQualifiedName
                                            , parameterDecoderFunctions = List.map Tuple.first argumentsJsonDecoders
                                            , hasAppStateParam = hasAppStateParam
                                            , appStateType = backendStateType
                                            , returnType = returnTypeAnnotation
                                            , returnTypeEncoderFunction = returnTypeEncoderFunction
                                            }

                                    returnTypeSourceCodeText : String
                                    returnTypeSourceCodeText =
                                        getTextLinesFromRange
                                            returnTypeAnnotationRange
                                            interfaceModule.fileText
                                            |> String.join "\n"
                                in
                                Ok
                                    { exposedFunctions =
                                        Dict.singleton exposedFunctionQualifiedName
                                            { description =
                                                { returnType =
                                                    { sourceCodeText = returnTypeSourceCodeText
                                                    , containsAppStateType = composeHandler.resultContainsAppState
                                                    }
                                                , parameters = parameters
                                                }
                                            , handlerExpression = composeHandler.expression
                                            }
                                    , jsonConverterDeclarations = jsonConverterDeclarations
                                    , modulesToImport = [ interfaceModule.moduleName ]
                                    }
                    )


buildExposedFunctionHandlerExpression :
    { exposedFunctionQualifiedName : String
    , parameterDecoderFunctions : List String
    , hasAppStateParam : Bool
    , appStateType : ElmTypeAnnotation
    , returnType : ElmTypeAnnotation
    , returnTypeEncoderFunction : Maybe String
    }
    -> { expression : String, resultContainsAppState : Bool }
buildExposedFunctionHandlerExpression config =
    case config.parameterDecoderFunctions of
        [ singleParameterDecoderFunction ] ->
            let
                returnValueEncodeExpression : String
                returnValueEncodeExpression =
                    case config.returnTypeEncoderFunction of
                        Nothing ->
                            "always (Nothing, Just \"Serializing response not implemented\")"

                        Just returnTypeEncoderFunction ->
                            returnTypeEncoderFunction ++ " >> Just >> Tuple.pair Nothing"

                ( funcAfterDecode, resultContainsAppState ) =
                    if config.returnType == config.appStateType then
                        ( [ "(\\firstArg shimStateBefore ->"
                          , "{ shimStateBefore"
                          , "| stateLessFramework ="
                          , "    " ++ config.exposedFunctionQualifiedName ++ " firstArg shimStateBefore.stateLessFramework"
                          , "}"
                          , "|> (Just >> Tuple.pair >> (|>) Nothing)"
                          , "|> Ok"
                          , ")"
                          ]
                            |> String.join "\n"
                        , True
                        )

                    else
                        ( [ "(\\firstArg shimStateBefore ->"
                          , config.exposedFunctionQualifiedName ++ " firstArg shimStateBefore.stateLessFramework"
                          , "|> (" ++ returnValueEncodeExpression ++ ")"
                          , "|> Ok"
                          , ")"
                          ]
                            |> String.join "\n"
                        , False
                        )
            in
            { expression =
                [ if config.hasAppStateParam then
                    "Backend.Generated.StateShim.exposedFunctionExpectingSingleArgumentAndAppState"

                  else
                    "Backend.Generated.StateShim.exposedFunctionExpectingSingleArgument"
                , singleParameterDecoderFunction
                , funcAfterDecode
                ]
                    |> String.join "\n"
            , resultContainsAppState = resultContainsAppState
            }

        _ ->
            { expression =
                "always (Err \"Not implemented: Number of parameters: "
                    ++ String.fromInt (List.length config.parameterDecoderFunctions)
                    ++ "\")"
            , resultContainsAppState = False
            }


type alias WebServiceShimVersionModules =
    { webServiceShimModuleText : String
    , webServiceShimTypesModuleText : String
    }


webServiceShimVersionModules : SourceParsedElmModule -> WebServiceShimVersionModules
webServiceShimVersionModules platformModule =
    let
        declarationsTexts : List String
        declarationsTexts =
            platformModule.parsedSyntax.declarations
                |> List.map
                    (\(Elm.Syntax.Node.Node declarationRange _) ->
                        String.join
                            "\n"
                            (getTextLinesFromRange
                                declarationRange
                                platformModule.fileText
                            )
                    )

        declarationsTextsContains : String -> Bool
        declarationsTextsContains string =
            declarationsTexts |> List.any (String.contains string)
    in
    if declarationsTextsContains "CreateVolatileProcessNative" then
        { webServiceShimModuleText =
            webServiceShimModuleText_2023_06
                { platformModuleName = String.join "." platformModule.moduleName }
        , webServiceShimTypesModuleText =
            webServiceShimTypesModuleText_2023_06
                { platformModuleName = String.join "." platformModule.moduleName }
        }

    else
        { webServiceShimModuleText =
            webServiceShimModuleText_2023_05
                { platformModuleName = String.join "." platformModule.moduleName }
        , webServiceShimTypesModuleText =
            webServiceShimTypesModuleText_2023_05
                { platformModuleName = String.join "." platformModule.moduleName }
        }


webServiceShimModuleText_2023_06 : { platformModuleName : String } -> String
webServiceShimModuleText_2023_06 { platformModuleName } =
    String.trimLeft """
module Backend.Generated.WebServiceShim exposing (..)

import Backend.Generated.WebServiceShimTypes exposing (..)
import Dict
import """ ++ platformModuleName ++ """ as PlatformWebService


initWebServiceShimState : appState -> WebServiceShimState appState
initWebServiceShimState stateLessFramework =
    { stateLessFramework = stateLessFramework
    , nextTaskIndex = 0
    , posixTimeMilli = 0
    , readRuntimeIdentifierTasks = Dict.empty
    , createVolatileProcessTasks = Dict.empty
    , requestToVolatileProcessTasks = Dict.empty
    , writeToVolatileProcessNativeStdInTasks = Dict.empty
    , readAllFromVolatileProcessNativeTasks = Dict.empty
    , terminateVolatileProcessTasks = Dict.empty
    }


processWebServiceEvent :
    (appState -> PlatformWebService.Subscriptions appState)
    -> Backend.Generated.WebServiceShimTypes.BackendEvent
    -> WebServiceShimState appState
    -> ( WebServiceShimState appState, Backend.Generated.WebServiceShimTypes.BackendEventResponseStruct )
processWebServiceEvent subscriptions hostEvent stateBefore =
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
    processWebServiceEventLessRememberTime subscriptions hostEvent state


processWebServiceEventLessRememberTime :
    (appState -> PlatformWebService.Subscriptions appState)
    -> Backend.Generated.WebServiceShimTypes.BackendEvent
    -> WebServiceShimState appState
    -> ( WebServiceShimState appState, Backend.Generated.WebServiceShimTypes.BackendEventResponseStruct )
processWebServiceEventLessRememberTime subscriptions hostEvent stateBefore =
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
                ReadRuntimeInformationResponse result ->
                    case Dict.get taskCompleteEvent.taskId stateBefore.readRuntimeIdentifierTasks of
                        Nothing ->
                            discardEvent

                        Just taskEntry ->
                            continueWithUpdateToTasks
                                (taskEntry result)
                                { stateBefore
                                    | readRuntimeIdentifierTasks =
                                        stateBefore.readRuntimeIdentifierTasks |> Dict.remove taskCompleteEvent.taskId
                                }

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

                WriteToVolatileProcessNativeStdInTaskResponse writeResult ->
                    case Dict.get taskCompleteEvent.taskId stateBefore.writeToVolatileProcessNativeStdInTasks of
                        Nothing ->
                            discardEvent

                        Just taskEntry ->
                            continueWithUpdateToTasks
                                (taskEntry writeResult)
                                stateBefore

                ReadAllFromVolatileProcessNativeTaskResponse readResult ->
                    case Dict.get taskCompleteEvent.taskId stateBefore.readAllFromVolatileProcessNativeTasks of
                        Nothing ->
                            discardEvent

                        Just taskEntry ->
                            continueWithUpdateToTasks
                                (taskEntry readResult)
                                stateBefore

                CompleteWithoutResult ->
                    continueWithState
                        { stateBefore
                            | terminateVolatileProcessTasks =
                                stateBefore.terminateVolatileProcessTasks |> Dict.remove taskCompleteEvent.taskId
                        }


backendEventResponseFromRuntimeTasksAndSubscriptions :
    (appState -> PlatformWebService.Subscriptions appState)
    -> List (PlatformWebService.Command appState)
    -> WebServiceShimState appState
    -> ( WebServiceShimState appState, BackendEventResponseStruct )
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
    PlatformWebService.Command appState
    -> WebServiceShimState appState
    -> ( WebServiceShimState appState, BackendEventResponseStruct )
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
        PlatformWebService.RespondToHttpRequest respondToHttpRequest ->
            ( stateBefore
            , passiveBackendEventResponse
                |> withCompleteHttpResponsesAdded [ respondToHttpRequest ]
            )

        PlatformWebService.ReadRuntimeInformationCommand update ->
            let
                ( stateAfterCreateTaskId, taskId ) =
                    createTaskId stateBefore
            in
            ( { stateAfterCreateTaskId
                | readRuntimeIdentifierTasks =
                    stateAfterCreateTaskId.readRuntimeIdentifierTasks
                        |> Dict.insert taskId update
              }
            , passiveBackendEventResponse
                |> withStartTasksAdded
                    [ { taskId = taskId
                      , task = ReadRuntimeInformationTask
                      }
                    ]
            )

        PlatformWebService.CreateVolatileProcess createVolatileProcess ->
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

        PlatformWebService.CreateVolatileProcessNativeCommand createCommand ->
            let
                ( stateAfterCreateTaskId, taskId ) =
                    createTaskId stateBefore
            in
            ( { stateAfterCreateTaskId
                | createVolatileProcessTasks =
                    stateAfterCreateTaskId.createVolatileProcessTasks
                        |> Dict.insert taskId createCommand.update
              }
            , passiveBackendEventResponse
                |> withStartTasksAdded
                    [ { taskId = taskId
                      , task = CreateVolatileProcessNativeTask createCommand.request
                      }
                    ]
            )

        PlatformWebService.RequestToVolatileProcess requestToVolatileProcess ->
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

        PlatformWebService.WriteToVolatileProcessNativeStdInCommand writeCommand ->
            let
                ( stateAfterCreateTaskId, taskId ) =
                    createTaskId stateBefore
            in
            ( { stateAfterCreateTaskId
                | writeToVolatileProcessNativeStdInTasks =
                    stateAfterCreateTaskId.writeToVolatileProcessNativeStdInTasks
                        |> Dict.insert taskId writeCommand.update
              }
            , passiveBackendEventResponse
                |> withStartTasksAdded
                    [ { taskId = taskId
                      , task =
                            WriteToVolatileProcessNativeStdInTask
                                { processId = writeCommand.processId
                                , stdInBase64 = writeCommand.stdInBase64
                                }
                      }
                    ]
            )

        PlatformWebService.ReadAllFromVolatileProcessNativeCommand readCommand ->
            let
                ( stateAfterCreateTaskId, taskId ) =
                    createTaskId stateBefore
            in
            ( { stateAfterCreateTaskId
                | readAllFromVolatileProcessNativeTasks =
                    stateAfterCreateTaskId.readAllFromVolatileProcessNativeTasks
                        |> Dict.insert taskId readCommand.update
              }
            , passiveBackendEventResponse
                |> withStartTasksAdded
                    [ { taskId = taskId
                      , task = ReadAllFromVolatileProcessNativeTask readCommand.processId
                      }
                    ]
            )

        PlatformWebService.TerminateVolatileProcess terminateVolatileProcess ->
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


backendEventResponseFromSubscriptions : PlatformWebService.Subscriptions appState -> BackendEventResponseStruct
backendEventResponseFromSubscriptions subscriptions =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived =
        subscriptions.posixTimeIsPast
            |> Maybe.map (\\posixTimeIsPast -> { minimumPosixTimeMilli = posixTimeIsPast.minimumPosixTimeMilli })
    }


passiveBackendEventResponse : BackendEventResponseStruct
passiveBackendEventResponse =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived = Nothing
    }


withStartTasksAdded : List StartTaskStructure -> BackendEventResponseStruct -> BackendEventResponseStruct
withStartTasksAdded startTasksToAdd responseBefore =
    { responseBefore | startTasks = responseBefore.startTasks ++ startTasksToAdd }


withCompleteHttpResponsesAdded :
    List PlatformWebService.RespondToHttpRequestStruct
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
    }

"""


webServiceShimTypesModuleText_2023_06 : { platformModuleName : String } -> String
webServiceShimTypesModuleText_2023_06 { platformModuleName } =
    String.trimLeft """
module Backend.Generated.WebServiceShimTypes exposing (..)

import Dict
import """ ++ platformModuleName ++ """ as PlatformWebService


type alias WebServiceShimState appState =
    { stateLessFramework : appState
    , nextTaskIndex : Int
    , posixTimeMilli : Int
    , readRuntimeIdentifierTasks :
        Dict.Dict
            TaskId
            (Result String PlatformWebService.RuntimeInformationRecord
             -> appState
             -> ( appState, PlatformWebService.Commands appState )
            )
    , createVolatileProcessTasks :
        Dict.Dict
            TaskId
            (PlatformWebService.CreateVolatileProcessResult
             -> appState
             -> ( appState, PlatformWebService.Commands appState )
            )
    , requestToVolatileProcessTasks :
        Dict.Dict
            TaskId
            (PlatformWebService.RequestToVolatileProcessResult
             -> appState
             -> ( appState, PlatformWebService.Commands appState )
            )
    , writeToVolatileProcessNativeStdInTasks :
        Dict.Dict
            TaskId
            (Result PlatformWebService.RequestToVolatileProcessError ()
             -> appState
             -> ( appState, PlatformWebService.Commands appState )
            )
    , readAllFromVolatileProcessNativeTasks :
        Dict.Dict
            TaskId
            (Result PlatformWebService.RequestToVolatileProcessError PlatformWebService.ReadAllFromVolatileProcessNativeSuccessStruct
             -> appState
             -> ( appState, PlatformWebService.Commands appState )
            )
    , terminateVolatileProcessTasks : Dict.Dict TaskId ()
    }


type alias ExposedFunctionArguments =
    { serializedArguments : List String
    }


type BackendEvent
    = HttpRequestEvent PlatformWebService.HttpRequestEventStruct
    | TaskCompleteEvent TaskCompleteEventStruct
    | PosixTimeHasArrivedEvent { posixTimeMilli : Int }


type alias BackendEventResponseStruct =
    { startTasks : List StartTaskStructure
    , notifyWhenPosixTimeHasArrived : Maybe { minimumPosixTimeMilli : Int }
    , completeHttpResponses : List PlatformWebService.RespondToHttpRequestStruct
    }


type alias TaskCompleteEventStruct =
    { taskId : TaskId
    , taskResult : TaskResultStructure
    }


type TaskResultStructure
    = ReadRuntimeInformationResponse (Result String PlatformWebService.RuntimeInformationRecord)
    | CreateVolatileProcessResponse (Result PlatformWebService.CreateVolatileProcessErrorStruct PlatformWebService.CreateVolatileProcessComplete)
    | RequestToVolatileProcessResponse (Result PlatformWebService.RequestToVolatileProcessError PlatformWebService.RequestToVolatileProcessComplete)
    | WriteToVolatileProcessNativeStdInTaskResponse (Result PlatformWebService.RequestToVolatileProcessError ())
    | ReadAllFromVolatileProcessNativeTaskResponse (Result PlatformWebService.RequestToVolatileProcessError PlatformWebService.ReadAllFromVolatileProcessNativeSuccessStruct)
    | CompleteWithoutResult


type alias StartTaskStructure =
    { taskId : TaskId
    , task : Task
    }


type Task
    = ReadRuntimeInformationTask
    | CreateVolatileProcess CreateVolatileProcessLessUpdateStruct
    | CreateVolatileProcessNativeTask PlatformWebService.CreateVolatileProcessNativeRequestStruct
    | WriteToVolatileProcessNativeStdInTask WriteToVolatileProcessNativeStdInStruct
    | ReadAllFromVolatileProcessNativeTask String
    | RequestToVolatileProcess RequestToVolatileProcessLessUpdateStruct
    | TerminateVolatileProcess PlatformWebService.TerminateVolatileProcessStruct


type alias CreateVolatileProcessLessUpdateStruct =
    { programCode : String
    }


type alias RequestToVolatileProcessLessUpdateStruct =
    { processId : String
    , request : String
    }


type alias WriteToVolatileProcessNativeStdInStruct =
    { processId : String
    , stdInBase64 : String
    }


type alias TaskId =
    String

"""


webServiceShimModuleText_2023_05 : { platformModuleName : String } -> String
webServiceShimModuleText_2023_05 { platformModuleName } =
    String.trimLeft """
module Backend.Generated.WebServiceShim exposing (..)

import Backend.Generated.WebServiceShimTypes exposing (..)
import Dict
import """ ++ platformModuleName ++ """ as PlatformWebService


initWebServiceShimState : appState -> WebServiceShimState appState
initWebServiceShimState stateLessFramework =
    { stateLessFramework = stateLessFramework
    , nextTaskIndex = 0
    , posixTimeMilli = 0
    , createVolatileProcessTasks = Dict.empty
    , requestToVolatileProcessTasks = Dict.empty
    , terminateVolatileProcessTasks = Dict.empty
    }


processWebServiceEvent :
    (appState -> PlatformWebService.Subscriptions appState)
    -> Backend.Generated.WebServiceShimTypes.BackendEvent
    -> WebServiceShimState appState
    -> ( WebServiceShimState appState, Backend.Generated.WebServiceShimTypes.BackendEventResponseStruct )
processWebServiceEvent subscriptions hostEvent stateBefore =
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
    processWebServiceEventLessRememberTime subscriptions hostEvent state


processWebServiceEventLessRememberTime :
    (appState -> PlatformWebService.Subscriptions appState)
    -> Backend.Generated.WebServiceShimTypes.BackendEvent
    -> WebServiceShimState appState
    -> ( WebServiceShimState appState, Backend.Generated.WebServiceShimTypes.BackendEventResponseStruct )
processWebServiceEventLessRememberTime subscriptions hostEvent stateBefore =
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
    (appState -> PlatformWebService.Subscriptions appState)
    -> List (PlatformWebService.Command appState)
    -> WebServiceShimState appState
    -> ( WebServiceShimState appState, BackendEventResponseStruct )
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
    PlatformWebService.Command appState
    -> WebServiceShimState appState
    -> ( WebServiceShimState appState, BackendEventResponseStruct )
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
        PlatformWebService.RespondToHttpRequest respondToHttpRequest ->
            ( stateBefore
            , passiveBackendEventResponse
                |> withCompleteHttpResponsesAdded [ respondToHttpRequest ]
            )

        PlatformWebService.CreateVolatileProcess createVolatileProcess ->
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

        PlatformWebService.RequestToVolatileProcess requestToVolatileProcess ->
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

        PlatformWebService.TerminateVolatileProcess terminateVolatileProcess ->
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


backendEventResponseFromSubscriptions : PlatformWebService.Subscriptions appState -> BackendEventResponseStruct
backendEventResponseFromSubscriptions subscriptions =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived =
        subscriptions.posixTimeIsPast
            |> Maybe.map (\\posixTimeIsPast -> { minimumPosixTimeMilli = posixTimeIsPast.minimumPosixTimeMilli })
    }


passiveBackendEventResponse : BackendEventResponseStruct
passiveBackendEventResponse =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived = Nothing
    }


withStartTasksAdded : List StartTaskStructure -> BackendEventResponseStruct -> BackendEventResponseStruct
withStartTasksAdded startTasksToAdd responseBefore =
    { responseBefore | startTasks = responseBefore.startTasks ++ startTasksToAdd }


withCompleteHttpResponsesAdded :
    List PlatformWebService.RespondToHttpRequestStruct
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
    }

"""


webServiceShimTypesModuleText_2023_05 : { platformModuleName : String } -> String
webServiceShimTypesModuleText_2023_05 { platformModuleName } =
    String.trimLeft """
module Backend.Generated.WebServiceShimTypes exposing (..)

import Dict
import """ ++ platformModuleName ++ """ as PlatformWebService


type alias WebServiceShimState appState =
    { stateLessFramework : appState
    , nextTaskIndex : Int
    , posixTimeMilli : Int
    , createVolatileProcessTasks :
        Dict.Dict
            TaskId
            (PlatformWebService.CreateVolatileProcessResult
             -> appState
             -> ( appState, PlatformWebService.Commands appState )
            )
    , requestToVolatileProcessTasks :
        Dict.Dict
            TaskId
            (PlatformWebService.RequestToVolatileProcessResult
             -> appState
             -> ( appState, PlatformWebService.Commands appState )
            )
    , terminateVolatileProcessTasks : Dict.Dict TaskId ()
    }


type alias ExposedFunctionArguments =
    { serializedArguments : List String
    }


type BackendEvent
    = HttpRequestEvent PlatformWebService.HttpRequestEventStruct
    | TaskCompleteEvent TaskCompleteEventStruct
    | PosixTimeHasArrivedEvent { posixTimeMilli : Int }


type alias BackendEventResponseStruct =
    { startTasks : List StartTaskStructure
    , notifyWhenPosixTimeHasArrived : Maybe { minimumPosixTimeMilli : Int }
    , completeHttpResponses : List PlatformWebService.RespondToHttpRequestStruct
    }


type alias TaskCompleteEventStruct =
    { taskId : TaskId
    , taskResult : TaskResultStructure
    }


type TaskResultStructure
    = CreateVolatileProcessResponse (Result PlatformWebService.CreateVolatileProcessErrorStruct PlatformWebService.CreateVolatileProcessComplete)
    | RequestToVolatileProcessResponse (Result PlatformWebService.RequestToVolatileProcessError PlatformWebService.RequestToVolatileProcessComplete)
    | CompleteWithoutResult


type alias StartTaskStructure =
    { taskId : TaskId
    , task : Task
    }


type Task
    = CreateVolatileProcess CreateVolatileProcessLessUpdateStruct
    | RequestToVolatileProcess RequestToVolatileProcessLessUpdateStruct
    | TerminateVolatileProcess PlatformWebService.TerminateVolatileProcessStruct


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
