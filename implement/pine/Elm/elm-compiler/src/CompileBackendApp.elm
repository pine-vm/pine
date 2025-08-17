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
                case
                    parseAppStateElmTypeAndDependenciesRecursively
                        appDeclaration
                        config.originalSourceModules
                        ( config.compilationRootFilePath, config.compilationRootModule.parsedSyntax )
                of
                    Err locatedErr ->
                        Err
                            [ mapLocatedInSourceFiles
                                (\errLessLocation ->
                                    OtherCompilationError ("Failed to parse state type: " ++ errLessLocation)
                                )
                                locatedErr
                            ]

                    Ok appStateType ->
                        case
                            parseMigrationConfig
                                { originalSourceModules = config.originalSourceModules }
                        of
                            Err err ->
                                Err err

                            Ok maybeMigrationConfig ->
                                case
                                    parseExposeFunctionsToAdminConfig
                                        { originalSourceModules = config.originalSourceModules
                                        , backendStateType = appStateType.stateTypeAnnotation
                                        }
                                of
                                    Err locatedErrs ->
                                        Err
                                            (List.map
                                                (mapLocatedInSourceFiles OtherCompilationError)
                                                locatedErrs
                                            )

                                    Ok maybeExposeFunctionsToAdmin ->
                                        let
                                            (Elm.Syntax.Node.Node _ appDeclarationDeclaration) =
                                                appDeclaration.declaration

                                            (Elm.Syntax.Node.Node _ mainDeclarationName) =
                                                appDeclarationDeclaration.name

                                            (Elm.Syntax.Node.Node _ appRootModuleDefinition) =
                                                config.compilationRootModule.parsedSyntax.moduleDefinition

                                            appRootDeclarationModuleName : String
                                            appRootDeclarationModuleName =
                                                appRootModuleDefinition
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
                                                []

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

                                            exposedFunctionsToAdmin =
                                                maybeExposeFunctionsToAdmin
                                                    |> Maybe.map .exposedFunctions
                                                    |> Maybe.withDefault Dict.empty

                                            exposedFunctions =
                                                exposedFunctionsToAdmin
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
                                                            []
                                                        , rootModuleSupportingFunctions = rootModuleSupportingFunctions
                                                        , modulesToImport =
                                                            modulesToImportMigrate ++ modulesToImportExposeFunctionsToAdmin
                                                        , appStateWithPlatformShimTypeAnnotationFromAppStateAnnotation =
                                                            identity
                                                        }
                                                in
                                                loweredForAppInStateManagementShim
                                                    sourceDirs
                                                    stateShimConfig
                                                    config
                                                    sourceFiles


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
                                []
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
