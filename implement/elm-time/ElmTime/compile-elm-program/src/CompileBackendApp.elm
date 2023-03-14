module CompileBackendApp exposing (..)

import CompileElmAppWithStateShim
    exposing
        ( ExposedFunctionConfig
        , StateShimConfig
        , StateShimConfigJsonConverter
        , StateShimConfigJsonConverterConfig
        , loweredForAppInStateManagementShim
        )
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
        , entryPointClassFromSetOfEquallyProcessedFunctionNames
        , filePathFromElmModuleName
        , findModuleByName
        , locatedInSourceFilesFromRange
        , mapLocatedInSourceFiles
        , parseElmTypeAndDependenciesRecursivelyFromAnnotation
        , syntaxRangeCoveringCompleteModule
        )
import Dict
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.TypeAnnotation
import Set


type alias MigrationConfig =
    { inputType : ElmTypeAnnotation
    , returnType : ElmTypeAnnotation
    , dependencies : Dict.Dict String ElmChoiceTypeStruct
    , migrateFunctionModuleName : List String
    , migrateFunctionDeclarationLocalName : String
    , jsonConverterDeclarations : Dict.Dict String StateShimConfigJsonConverter
    , rootModuleSupportingFunctions : List String
    , modulesToImport : List (List String)
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
                                let
                                    mainDeclarationName =
                                        appDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .name
                                            |> Elm.Syntax.Node.value

                                    appRootDeclarationModuleName =
                                        config.compilationRootModule.parsedSyntax.moduleDefinition
                                            |> Elm.Syntax.Node.value
                                            |> Elm.Syntax.Module.moduleName
                                            |> String.join "."

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

                                    jsonConverterDeclarationsConfigs : Dict.Dict String StateShimConfigJsonConverterConfig
                                    jsonConverterDeclarationsConfigs =
                                        [ ( "jsonDecodeBackendEvent"
                                          , { isDecoder = True
                                            , moduleName = [ "Backend", "Generated", "WebServerShimTypes" ]
                                            , declarationName = "BackendEvent"
                                            }
                                          )
                                        , ( "jsonEncodeBackendEventResponse"
                                          , { isDecoder = False
                                            , moduleName = [ "Backend", "Generated", "WebServerShimTypes" ]
                                            , declarationName = "BackendEventResponseStruct"
                                            }
                                          )
                                        ]
                                            |> Dict.fromList

                                    jsonConverterDeclarationsMigrate =
                                        maybeMigrationConfig
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

                                    modulesToImportMigrate =
                                        maybeMigrationConfig
                                            |> Maybe.map .modulesToImport
                                            |> Maybe.withDefault []

                                    exposedFunctionsGeneral =
                                        [ ( "init"
                                          , { description = { hasAppStateParam = False, resultContainsAppState = True }
                                            , handlerExpression = """
config_init
    |> (\\( appState, commands ) ->
            Backend.Generated.WebServerShim.backendEventResponseFromRuntimeTasksAndSubscriptions
                config_subscriptions
                commands
                (Backend.Generated.WebServerShim.initWebServerShimState appState)
        )
    |> Tuple.mapSecond (jsonEncodeBackendEventResponse >> Json.Encode.encode 0)
    |> Tuple.mapFirst Just
    |> Tuple.mapSecond Just
    |> Ok
    |> always
"""
                                            }
                                          )
                                        , ( "processEvent"
                                          , { description = { hasAppStateParam = True, resultContainsAppState = True }
                                            , handlerExpression = """
Backend.Generated.StateShim.exposedFunctionExpectingSingleArgumentAndAppState
    jsonDecodeBackendEvent
    (\\backendEvent ->
        Backend.Generated.WebServerShim.processWebServerEvent config_subscriptions backendEvent
            >> Tuple.mapFirst Just
            >> Tuple.mapSecond (jsonEncodeBackendEventResponse >> Json.Encode.encode 0 >> Just)
            >> Ok
    )
"""
                                            }
                                          )
                                        ]
                                            |> Dict.fromList

                                    exposedFunctionsMigrate =
                                        maybeMigrationConfig
                                            |> Maybe.map exposedFunctionsFromMigrationConfig
                                            |> Maybe.withDefault Dict.empty

                                    exposedFunctions =
                                        exposedFunctionsGeneral
                                            |> Dict.union exposedFunctionsMigrate

                                    stateShimConfigExpression =
                                        String.trim
                                            """
{ jsonDecodeAppState = jsonDecodeAppState
, jsonEncodeAppState = jsonEncodeAppState
, exposedFunctions = config_exposedFunctions
, initAppShimState = Backend.Generated.WebServerShim.initWebServerShimState
, appStateLessShim = .stateLessFramework
}"""

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
                                            [ webServerShimModuleText
                                            , webServerShimTypesModuleText
                                            ]
                                        , rootModuleSupportingFunctions = rootModuleSupportingFunctions
                                        , modulesToImport = modulesToImportMigrate
                                        , appStateWithPlatformShimTypeAnnotationFromAppStateAnnotation =
                                            (++) "Backend.Generated.WebServerShimTypes.WebServerShimState "
                                        , stateShimConfigExpression = stateShimConfigExpression
                                        }
                                in
                                loweredForAppInStateManagementShim
                                    stateShimConfig
                                    config
                                    sourceFiles
                            )
                )


exposedFunctionsFromMigrationConfig : MigrationConfig -> Dict.Dict String ExposedFunctionConfig
exposedFunctionsFromMigrationConfig _ =
    [ ( "migrate"
      , { description = { hasAppStateParam = False, resultContainsAppState = True }
        , handlerExpression = """
Backend.Generated.StateShim.exposedFunctionExpectingSingleArgument
    Json.Decode.string
    (migrateFromStringPackageWebServerShim
        >> Result.map
            (Tuple.mapFirst Just
                >> Tuple.mapSecond (jsonEncodeBackendEventResponse >> Json.Encode.encode 0 >> Just)
            )
    )
"""
        }
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
migrateFromStringPackageWebServerShim =
    Json.Decode.decodeString jsonDecodeMigratePreviousState
        >> Result.mapError Json.Decode.errorToString
        >> Result.map """ ++ String.join "." (appStateMigrationInterfaceModuleName ++ [ appStateMigrationInterfaceFunctionName ]) ++ """
        >> Result.map
            (\\( appState, appCommands ) ->
                appState
                    |> Backend.Generated.WebServerShim.initWebServerShimState
                    |> Backend.Generated.WebServerShim.backendEventResponseFromRuntimeTasksAndSubscriptions
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


webServerShimModuleText : String
webServerShimModuleText =
    String.trimLeft """
module Backend.Generated.WebServerShim exposing (..)

import Backend.Generated.WebServerShimTypes exposing (..)
import Dict
import Platform.WebServer


initWebServerShimState : appState -> WebServerShimState appState
initWebServerShimState stateLessFramework =
    { stateLessFramework = stateLessFramework
    , nextTaskIndex = 0
    , posixTimeMilli = 0
    , createVolatileProcessTasks = Dict.empty
    , requestToVolatileProcessTasks = Dict.empty
    , terminateVolatileProcessTasks = Dict.empty
    }


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
