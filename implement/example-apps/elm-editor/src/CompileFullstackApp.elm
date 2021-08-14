module CompileFullstackApp exposing
    ( AppFiles
    , CompilationArguments
    , CompilationError(..)
    , DependencyKey(..)
    , ElmCustomTypeStruct
    , ElmMakeOutputType(..)
    , ElmMakeRequestStructure
    , ElmTypeAnnotation(..)
    , InterfaceBlobEncoding(..)
    , LeafElmTypeStruct(..)
    , LocatedCompilationError
    , LocatedInSourceFiles(..)
    , LocationInSourceFiles
    , appendLineAndStringInLogFile
    , asCompletelyLoweredElmApp
    , buildTypeAnnotationText
    , elmModulesDictFromAppFiles
    , filePathFromElmModuleName
    , includeFilePathInElmMakeRequest
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
import JaroWinkler
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

    {-
       TODO: Remove initialStateFunctionName and processSerializedEventFunctionName after migrating apps in production to `backendMainDeclarationName`
    -}
    , backendMainDeclarationName : String
    , serializeStateFunctionName : String
    , deserializeStateFunctionName : String
    }


type InterfaceToHostVersion
    = InterfaceToHostVersion_Before_2021_08
    | InterfaceToHostVersion_2021_08


elmAppInterfaceConvention : ElmAppInterfaceConvention
elmAppInterfaceConvention =
    { initialStateFunctionName = "interfaceToHost_processEvent"
    , processSerializedEventFunctionName = "interfaceToHost_processEvent"
    , backendMainDeclarationName = "backendMain"
    , serializeStateFunctionName = "interfaceToHost_serializeState"
    , deserializeStateFunctionName = "interfaceToHost_deserializeState"
    }


appStateMigrationInterfaceModulesNames : List String
appStateMigrationInterfaceModulesNames =
    [ "Backend.MigrateState", "MigrateBackendState" ]


appStateMigrationRootModuleName : String
appStateMigrationRootModuleName =
    "Backend.MigrateState_Root"


appStateMigrationInterfaceFunctionName : String
appStateMigrationInterfaceFunctionName =
    "migrate"


type alias AppFiles =
    Dict.Dict (List String) Bytes.Bytes


type CompilationError
    = MissingDependencyError DependencyKey
    | OtherCompilationError String


type alias LocationInSourceFiles =
    { filePath : List String
    , locationInModuleText : Elm.Syntax.Range.Range
    }


type LocatedInSourceFiles a
    = LocatedInSourceFiles LocationInSourceFiles a


type alias LocatedCompilationError =
    LocatedInSourceFiles CompilationError


type DependencyKey
    = ElmMakeDependency ElmMakeRequestStructure


type alias ElmMakeRequestStructure =
    { files : AppFiles
    , entryPointFilePath : List String
    , outputType : ElmMakeOutputType
    , enableDebug : Bool
    }


type alias InterfaceElmMakeFunctionConfig =
    { outputType : ElmMakeOutputType
    , enableDebug : Bool
    , encoding : Maybe InterfaceBlobEncoding
    }


type ElmMakeOutputType
    = ElmMakeOutputTypeHtml
    | ElmMakeOutputTypeJs


type InterfaceBlobEncoding
    = Base64Encoding
    | Utf8Encoding


type InterfaceValueConversion
    = FromBase64ToBytes


type alias InterfaceSourceFilesFunctionConfig =
    { encoding : Maybe InterfaceBlobEncoding
    }


{-| This function returns an Err if the needed dependencies for ElmMake are not yet in the arguments.
The integrating software can then perform the ElmMake, insert it into the dependencies dict and retry.
-}
asCompletelyLoweredElmApp : CompilationArguments -> Result (List LocatedCompilationError) AppFiles
asCompletelyLoweredElmApp { sourceFiles, compilationInterfaceElmModuleNamePrefixes, dependencies, rootModuleName, interfaceToHostRootModuleName } =
    let
        sourceModules =
            elmModulesDictFromAppFiles sourceFiles
                |> Dict.map (always (Tuple.mapSecond Tuple.second))
    in
    loweredForSourceFiles compilationInterfaceElmModuleNamePrefixes sourceFiles
        |> Result.andThen (loweredForJsonCoders { originalSourceModules = sourceModules } compilationInterfaceElmModuleNamePrefixes)
        |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)
        |> Result.andThen (loweredForElmMake compilationInterfaceElmModuleNamePrefixes dependencies)
        |> Result.andThen
            (loweredForAppStateSerializer
                { originalSourceModules = sourceModules
                , rootModuleName = rootModuleName
                , interfaceToHostRootModuleName = interfaceToHostRootModuleName
                }
            )
        |> Result.andThen (loweredForAppStateMigration { originalSourceModules = sourceModules })


loweredForSourceFiles : List String -> AppFiles -> Result (LocatedInSourceFiles String) AppFiles
loweredForSourceFiles compilationInterfaceElmModuleNamePrefixes sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    locatedInSourceFilesFromJustFilePath
                    (compilationInterfaceElmModuleNamePrefix ++ ".SourceFiles")
                    mapSourceFilesModuleText
                    files
            )
            (Ok sourceFiles)


loweredForJsonCoders :
    { originalSourceModules : Dict.Dict String ( List String, Elm.Syntax.File.File ) }
    -> List String
    -> AppFiles
    -> Result (LocatedInSourceFiles String) AppFiles
loweredForJsonCoders context compilationInterfaceElmModuleNamePrefixes sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    locatedInSourceFilesFromJustFilePath
                    (compilationInterfaceElmModuleNamePrefix ++ ".GenerateJsonCoders")
                    (mapJsonCodersModuleText context)
                    files
            )
            (Ok sourceFiles)


loweredForElmMake : List String -> List ( DependencyKey, Bytes.Bytes ) -> AppFiles -> Result (List (LocatedInSourceFiles CompilationError)) AppFiles
loweredForElmMake compilationInterfaceElmModuleNamePrefixes dependencies sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    (\context -> OtherCompilationError >> locatedInSourceFilesFromJustFilePath context >> List.singleton)
                    (compilationInterfaceElmModuleNamePrefix ++ ".ElmMake")
                    (mapElmMakeModuleText dependencies)
                    files
            )
            (Ok sourceFiles)


loweredForAppStateSerializer :
    { rootModuleName : List String
    , interfaceToHostRootModuleName : List String
    , originalSourceModules : Dict.Dict String ( List String, Elm.Syntax.File.File )
    }
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) AppFiles
loweredForAppStateSerializer { rootModuleName, interfaceToHostRootModuleName, originalSourceModules } sourceFiles =
    let
        interfaceToHostRootFilePath =
            filePathFromElmModuleName (String.join "." interfaceToHostRootModuleName)
    in
    case Dict.get (String.join "." rootModuleName) originalSourceModules of
        Nothing ->
            -- App contains no backend.
            Ok sourceFiles

        Just backendMainModule ->
            if Dict.get interfaceToHostRootFilePath sourceFiles /= Nothing then
                -- Support integrating applications supplying their own lowered version.
                Ok sourceFiles

            else
                parseAppStateElmTypeAndDependenciesRecursively originalSourceModules backendMainModule
                    |> Result.mapError (mapLocatedInSourceFiles ((++) "Failed to parse state type name: "))
                    |> Result.map
                        (\( interfaceVersion, ( stateTypeAnnotation, stateTypeDependencies ) ) ->
                            let
                                ( appFiles, { generatedModuleName, modulesToImport } ) =
                                    mapAppFilesToSupportJsonCoding
                                        { generatedModuleNamePrefix = interfaceToHostRootModuleName }
                                        [ stateTypeAnnotation ]
                                        stateTypeDependencies
                                        sourceFiles

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

                                rootElmModuleText =
                                    composeAppRootElmModuleText
                                        interfaceVersion
                                        { interfaceToHostRootModuleName = String.join "." interfaceToHostRootModuleName
                                        , rootModuleNameBeforeLowering = String.join "." rootModuleName
                                        , stateTypeAnnotation = stateTypeAnnotation
                                        , modulesToImport = modulesToImport
                                        , encodeFunction = encodeFunction
                                        , decodeFunction = decodeFunction
                                        }
                            in
                            appFiles
                                |> updateFileContentAtPath
                                    (always (fileContentFromString rootElmModuleText))
                                    interfaceToHostRootFilePath
                        )
                    |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)


loweredForAppStateMigration :
    { originalSourceModules : Dict.Dict String ( List String, Elm.Syntax.File.File ) }
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) AppFiles
loweredForAppStateMigration { originalSourceModules } sourceFiles =
    let
        interfaceToHostRootFilePath =
            filePathFromElmModuleName appStateMigrationRootModuleName
    in
    case
        appStateMigrationInterfaceModulesNames
            |> List.filterMap
                (\appStateMigrationInterfaceModuleName ->
                    Dict.get appStateMigrationInterfaceModuleName originalSourceModules
                )
            |> List.head
    of
        Nothing ->
            -- App contains no migrate module.
            Ok sourceFiles

        Just ( originalInterfaceModuleFilePath, originalInterfaceModule ) ->
            if Dict.get interfaceToHostRootFilePath sourceFiles /= Nothing then
                -- Support integrating applications supplying their own lowered version.
                Ok sourceFiles

            else
                parseAppStateMigrateElmTypeAndDependenciesRecursively originalSourceModules ( originalInterfaceModuleFilePath, originalInterfaceModule )
                    |> Result.mapError (mapLocatedInSourceFiles ((++) "Failed to parse state type name: "))
                    |> Result.map
                        (\( ( inputType, returnType ), stateTypeDependencies ) ->
                            let
                                migrateModuleName =
                                    Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value originalInterfaceModule.moduleDefinition)

                                ( appFiles, { generatedModuleName, modulesToImport } ) =
                                    mapAppFilesToSupportJsonCoding
                                        { generatedModuleNamePrefix = String.split "." appStateMigrationRootModuleName }
                                        [ inputType, returnType ]
                                        stateTypeDependencies
                                        sourceFiles

                                inputTypeNamesInGeneratedModules =
                                    buildJsonCodingFunctionsForTypeAnnotation inputType

                                returnTypeNamesInGeneratedModules =
                                    buildJsonCodingFunctionsForTypeAnnotation returnType

                                rootElmModuleText =
                                    composeStateMigrationModuleText
                                        { migrateModuleName = String.join "." migrateModuleName
                                        , generatedModuleName = generatedModuleName
                                        , decodeOrigTypeFunctionName = inputTypeNamesInGeneratedModules.decodeFunction.name
                                        , encodeDestTypeFunctionName = returnTypeNamesInGeneratedModules.encodeFunction.name
                                        , modulesToImport = modulesToImport
                                        }
                            in
                            appFiles
                                |> updateFileContentAtPath
                                    (always (fileContentFromString rootElmModuleText))
                                    interfaceToHostRootFilePath
                        )
                    |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)


parseAppStateElmTypeAndDependenciesRecursively : Dict.Dict String ( List String, Elm.Syntax.File.File ) -> ( List String, Elm.Syntax.File.File ) -> Result (LocatedInSourceFiles String) ( InterfaceToHostVersion, ( ElmTypeAnnotation, Dict.Dict String ElmCustomTypeStruct ) )
parseAppStateElmTypeAndDependenciesRecursively sourceModules ( parsedModuleFilePath, parsedModule ) =
    stateTypeAnnotationFromRootElmModule parsedModule
        |> Result.mapError
            ((++) "Did not find state type annotation: "
                >> LocatedInSourceFiles
                    { filePath = parsedModuleFilePath
                    , locationInModuleText = syntaxRangeCoveringCompleteModule parsedModule
                    }
            )
        |> Result.andThen
            (\( interfaceVersion, stateTypeAnnotation ) ->
                parseElmTypeAndDependenciesRecursivelyFromAnnotation
                    sourceModules
                    ( ( parsedModuleFilePath, parsedModule ), stateTypeAnnotation )
                    |> Result.map (Tuple.pair interfaceVersion)
            )


parseAppStateMigrateElmTypeAndDependenciesRecursively :
    Dict.Dict String ( List String, Elm.Syntax.File.File )
    -> ( List String, Elm.Syntax.File.File )
    -> Result (LocatedInSourceFiles String) ( ( ElmTypeAnnotation, ElmTypeAnnotation ), Dict.Dict String ElmCustomTypeStruct )
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


stateTypeAnnotationFromRootElmModule : Elm.Syntax.File.File -> Result String ( InterfaceToHostVersion, Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation )
stateTypeAnnotationFromRootElmModule parsedModule =
    case stateTypeAnnotationFromRootElmModule_2021_08 parsedModule of
        Ok typeAnnotation ->
            Ok ( InterfaceToHostVersion_2021_08, typeAnnotation )

        Err newError ->
            case stateTypeAnnotationFromRootElmModule_Before_2021_08 parsedModule of
                Err oldError ->
                    Err newError

                Ok typeAnnotation ->
                    Ok ( InterfaceToHostVersion_Before_2021_08, typeAnnotation )


stateTypeAnnotationFromRootElmModule_2021_08 : Elm.Syntax.File.File -> Result String (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
stateTypeAnnotationFromRootElmModule_2021_08 parsedModule =
    parsedModule.declarations
        |> List.filterMap
            (\declaration ->
                case Elm.Syntax.Node.value declaration of
                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                        if
                            Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                == elmAppInterfaceConvention.backendMainDeclarationName
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

                    Just signature ->
                        case Elm.Syntax.Node.value (Elm.Syntax.Node.value signature).typeAnnotation of
                            Elm.Syntax.TypeAnnotation.Typed _ typeArguments ->
                                case typeArguments of
                                    [ singleTypeArgument ] ->
                                        Ok singleTypeArgument

                                    _ ->
                                        Err ("Unexpected number of type arguments: " ++ String.fromInt (List.length typeArguments))

                            _ ->
                                Err "Unexpected type annotation: Not an instance"
            )
        |> Maybe.withDefault (Err "Did not find declaration with matching name")


stateTypeAnnotationFromRootElmModule_Before_2021_08 : Elm.Syntax.File.File -> Result String (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
stateTypeAnnotationFromRootElmModule_Before_2021_08 parsedModule =
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
                                        Ok secondArgument

                                    _ ->
                                        Err "Unexpected type annotation in second argument"

                            _ ->
                                Err "Unexpected type annotation"
            )
        |> Maybe.withDefault (Err "Did not find function with matching name")


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

                                    _ ->
                                        Ok ( inputType, returnType )

                            _ ->
                                Err "Unexpected type annotation"
                )
                    |> Result.mapError (Elm.Syntax.Node.Node (Elm.Syntax.Node.range functionDeclaration.declaration))
            )
        |> Maybe.withDefault
            (Err
                (Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteModule parsedModule) "Did not find function with matching name")
            )


composeAppRootElmModuleText :
    InterfaceToHostVersion
    ->
        { interfaceToHostRootModuleName : String
        , rootModuleNameBeforeLowering : String
        , stateTypeAnnotation : ElmTypeAnnotation
        , modulesToImport : List (List String)
        , encodeFunction : String
        , decodeFunction : String
        }
    -> String
composeAppRootElmModuleText interfaceVersion config =
    case interfaceVersion of
        InterfaceToHostVersion_2021_08 ->
            composeAppRootElmModuleText_2021_08 config

        InterfaceToHostVersion_Before_2021_08 ->
            composeAppRootElmModuleText_Before_2021_08 config


composeAppRootElmModuleText_2021_08 :
    { interfaceToHostRootModuleName : String
    , rootModuleNameBeforeLowering : String
    , stateTypeAnnotation : ElmTypeAnnotation
    , modulesToImport : List (List String)
    , encodeFunction : String
    , decodeFunction : String
    }
    -> String
composeAppRootElmModuleText_2021_08 { interfaceToHostRootModuleName, rootModuleNameBeforeLowering, stateTypeAnnotation, modulesToImport, encodeFunction, decodeFunction } =
    "module " ++ interfaceToHostRootModuleName ++ """ exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import """ ++ rootModuleNameBeforeLowering ++ """
""" ++ (modulesToImport |> List.map (String.join "." >> (++) "import ") |> String.join "\n") ++ """
import Platform
import ElmFullstack exposing (..)


type alias DeserializedState =
    (""" ++ buildTypeAnnotationText stateTypeAnnotation ++ """)


type alias DeserializedStateWithTaskFramework =
    { stateLessFramework : DeserializedState
    , nextTaskIndex : Int
    , posixTimeMilli : Int
    , createVolatileProcessTasks : Dict.Dict TaskId (CreateVolatileProcessResult -> DeserializedState -> ( DeserializedState, BackendCmds DeserializedState ))
    , requestToVolatileProcessTasks : Dict.Dict TaskId (RequestToVolatileProcessResult -> DeserializedState -> ( DeserializedState, BackendCmds DeserializedState ))
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


type ResponseOverSerialInterface
    = DecodeEventError String
    | DecodeEventSuccess BackendEventResponse


type State
    = DeserializeFailed String
    | DeserializeSuccessful DeserializedStateWithTaskFramework


type BackendEvent
    = HttpRequestEvent ElmFullstack.HttpRequestEventStruct
    | TaskCompleteEvent TaskCompleteEventStruct
    | PosixTimeHasArrivedEvent { posixTimeMilli : Int }


type alias BackendEventResponse =
    { startTasks : List StartTaskStructure
    , notifyWhenPosixTimeHasArrived : Maybe { minimumPosixTimeMilli : Int }
    , completeHttpResponses : List RespondToHttpRequestStruct
    }


type alias TaskCompleteEventStruct =
    { taskId : TaskId
    , taskResult : TaskResultStructure
    }


type TaskResultStructure
    = CreateVolatileProcessResponse (Result CreateVolatileProcessErrorStruct CreateVolatileProcessComplete)
    | RequestToVolatileProcessResponse (Result RequestToVolatileProcessError RequestToVolatileProcessComplete)
    | CompleteWithoutResult


type alias StartTaskStructure =
    { taskId : TaskId
    , task : Task
    }


type Task
    = CreateVolatileProcess CreateVolatileProcessLessUpdateStruct
    | RequestToVolatileProcess RequestToVolatileProcessLessUpdateStruct
    | TerminateVolatileProcess TerminateVolatileProcessStruct


type alias CreateVolatileProcessLessUpdateStruct =
    { programCode : String
    }


type alias RequestToVolatileProcessLessUpdateStruct =
    { processId : String
    , request : String
    }


type alias TaskId =
    String


interfaceToHost_initState =
    """ ++ rootModuleNameBeforeLowering ++ """.backendMain.init
        -- TODO: Expand the runtime to consider the tasks from init.
        |> Tuple.first
        |> initDeserializedStateWithTaskFramework
        |> DeserializeSuccessful


interfaceToHost_processEvent hostEvent stateBefore =
    case stateBefore of
        DeserializeFailed _ ->
            ( stateBefore, "[]" )

        DeserializeSuccessful deserializedState ->
            deserializedState
                |> wrapForSerialInterface_processEvent Backend.Main.backendMain.subscriptions hostEvent
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
                { a = interfaceToHost_processEvent
                , b = interfaceToHost_serializeState
                , c = interfaceToHost_deserializeState
                }
                    |> always ( stateBefore, Cmd.none )
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
    (DeserializedState -> BackendSubs DeserializedState)
    -> String
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, String )
wrapForSerialInterface_processEvent subscriptions serializedEvent stateBefore =
    let
        ( state, response ) =
            case serializedEvent |> Json.Decode.decodeString decodeBackendEvent of
                Err error ->
                    ( stateBefore
                    , ("Failed to deserialize event: " ++ (error |> Json.Decode.errorToString))
                        |> DecodeEventError
                    )

                Ok hostEvent ->
                    stateBefore
                        |> processEvent subscriptions hostEvent
                        |> Tuple.mapSecond DecodeEventSuccess
    in
    ( state, response |> encodeResponseOverSerialInterface |> Json.Encode.encode 0 )


processEvent :
    (DeserializedState -> BackendSubs DeserializedState)
    -> BackendEvent
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, BackendEventResponse )
processEvent subscriptions hostEvent stateBefore =
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
    processEventLessRememberTime subscriptions hostEvent state


processEventLessRememberTime :
    (DeserializedState -> BackendSubs DeserializedState)
    -> BackendEvent
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, BackendEventResponse )
processEventLessRememberTime subscriptions hostEvent stateBefore =
    let
        discardEvent =
            ( stateBefore
            , { startTasks = []
              , notifyWhenPosixTimeHasArrived = Nothing
              , completeHttpResponses = []
              }
            )

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
                    ( { stateBefore
                        | terminateVolatileProcessTasks =
                            stateBefore.terminateVolatileProcessTasks |> Dict.remove taskCompleteEvent.taskId
                      }
                    , { startTasks = []
                      , notifyWhenPosixTimeHasArrived = Nothing
                      , completeHttpResponses = []
                      }
                    )


backendEventResponseFromRuntimeTasksAndSubscriptions :
    (DeserializedState -> BackendSubs DeserializedState)
    -> List (BackendCmd DeserializedState)
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, BackendEventResponse )
backendEventResponseFromRuntimeTasksAndSubscriptions subscriptions tasks stateBefore =
    let
        subscriptionsForState =
            subscriptions stateBefore.stateLessFramework
    in
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
            , [ { startTasks = []
                , completeHttpResponses = []
                , notifyWhenPosixTimeHasArrived =
                    subscriptionsForState.posixTimeIsPast
                        |> Maybe.map (\\posixTimeIsPast -> { minimumPosixTimeMilli = posixTimeIsPast.minimumPosixTimeMilli })
                }
              ]
            )
        |> Tuple.mapSecond concatBackendEventResponse


backendEventResponseFromRuntimeTask :
    BackendCmd DeserializedState
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, BackendEventResponse )
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
        RespondToHttpRequest respondToHttpRequest ->
            ( stateBefore
            , passiveBackendEventResponse
                |> withCompleteHttpResponsesAdded [ respondToHttpRequest ]
            )

        ElmFullstack.CreateVolatileProcess createVolatileProcess ->
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

        ElmFullstack.RequestToVolatileProcess requestToVolatileProcess ->
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

        ElmFullstack.TerminateVolatileProcess terminateVolatileProcess ->
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


concatBackendEventResponse : List BackendEventResponse -> BackendEventResponse
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


passiveBackendEventResponse : BackendEventResponse
passiveBackendEventResponse =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived = Nothing
    }


withStartTasksAdded : List StartTaskStructure -> BackendEventResponse -> BackendEventResponse
withStartTasksAdded startTasksToAdd responseBefore =
    { responseBefore | startTasks = responseBefore.startTasks ++ startTasksToAdd }


withCompleteHttpResponsesAdded : List RespondToHttpRequestStruct -> BackendEventResponse -> BackendEventResponse
withCompleteHttpResponsesAdded httpResponsesToAdd responseBefore =
    { responseBefore | completeHttpResponses = responseBefore.completeHttpResponses ++ httpResponsesToAdd }


decodeBackendEvent : Json.Decode.Decoder BackendEvent
decodeBackendEvent =
    Json.Decode.oneOf
        [ Json.Decode.field "ArrivedAtTimeEvent" (Json.Decode.field "posixTimeMilli" Json.Decode.int)
            |> Json.Decode.map (\\posixTimeMilli -> PosixTimeHasArrivedEvent { posixTimeMilli = posixTimeMilli })
        , Json.Decode.field "PosixTimeHasArrivedEvent" (Json.Decode.field "posixTimeMilli" Json.Decode.int)
            |> Json.Decode.map (\\posixTimeMilli -> PosixTimeHasArrivedEvent { posixTimeMilli = posixTimeMilli })
        , Json.Decode.field "TaskCompleteEvent" decodeTaskCompleteEventStructure |> Json.Decode.map TaskCompleteEvent
        , Json.Decode.field "HttpRequestEvent" decodeHttpRequestEventStruct |> Json.Decode.map HttpRequestEvent
        ]


decodeTaskCompleteEventStructure : Json.Decode.Decoder TaskCompleteEventStruct
decodeTaskCompleteEventStructure =
    Json.Decode.map2 TaskCompleteEventStruct
        (Json.Decode.field "taskId" Json.Decode.string)
        (Json.Decode.field "taskResult" decodeTaskResult)


decodeTaskResult : Json.Decode.Decoder TaskResultStructure
decodeTaskResult =
    Json.Decode.oneOf
        [ Json.Decode.field "CreateVolatileProcessResponse" (decodeResult decodeCreateVolatileProcessError decodeCreateVolatileProcessComplete)
            |> Json.Decode.map CreateVolatileProcessResponse
        , Json.Decode.field "RequestToVolatileProcessResponse" (decodeResult decodeRequestToVolatileProcessError decodeRequestToVolatileProcessComplete)
            |> Json.Decode.map RequestToVolatileProcessResponse
        , Json.Decode.field "CompleteWithoutResult" (jsonDecodeSucceedWhenNotNull CompleteWithoutResult)
        ]


decodeCreateVolatileProcessError : Json.Decode.Decoder CreateVolatileProcessErrorStruct
decodeCreateVolatileProcessError =
    Json.Decode.map CreateVolatileProcessErrorStruct
        (Json.Decode.field "exceptionToString" Json.Decode.string)


decodeCreateVolatileProcessComplete : Json.Decode.Decoder CreateVolatileProcessComplete
decodeCreateVolatileProcessComplete =
    Json.Decode.map CreateVolatileProcessComplete
        (Json.Decode.field "processId" Json.Decode.string)


decodeRequestToVolatileProcessComplete : Json.Decode.Decoder RequestToVolatileProcessComplete
decodeRequestToVolatileProcessComplete =
    Json.Decode.map3 RequestToVolatileProcessComplete
        (decodeOptionalField "exceptionToString" Json.Decode.string)
        (decodeOptionalField "returnValueToString" Json.Decode.string)
        (Json.Decode.field "durationInMilliseconds" Json.Decode.int)


decodeRequestToVolatileProcessError : Json.Decode.Decoder RequestToVolatileProcessError
decodeRequestToVolatileProcessError =
    Json.Decode.oneOf
        [ Json.Decode.field "ProcessNotFound" (jsonDecodeSucceedWhenNotNull ProcessNotFound)
        ]


decodeHttpRequestEventStruct : Json.Decode.Decoder HttpRequestEventStruct
decodeHttpRequestEventStruct =
    Json.Decode.map4 HttpRequestEventStruct
        (Json.Decode.field "httpRequestId" Json.Decode.string)
        (Json.Decode.field "posixTimeMilli" Json.Decode.int)
        (Json.Decode.field "requestContext" decodeHttpRequestContext)
        (Json.Decode.field "request" decodeHttpRequest)


decodeHttpRequestContext : Json.Decode.Decoder HttpRequestContext
decodeHttpRequestContext =
    Json.Decode.map HttpRequestContext
        (decodeOptionalField "clientAddress" Json.Decode.string)


decodeHttpRequest : Json.Decode.Decoder HttpRequestProperties
decodeHttpRequest =
    Json.Decode.map4 HttpRequestProperties
        (Json.Decode.field "method" Json.Decode.string)
        (Json.Decode.field "uri" Json.Decode.string)
        (decodeOptionalField "bodyAsBase64" Json.Decode.string)
        (Json.Decode.field "headers" (Json.Decode.list decodeHttpHeader))


decodeHttpHeader : Json.Decode.Decoder HttpHeader
decodeHttpHeader =
    Json.Decode.map2 HttpHeader
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "values" (Json.Decode.list Json.Decode.string))


encodeResponseOverSerialInterface : ResponseOverSerialInterface -> Json.Encode.Value
encodeResponseOverSerialInterface responseOverSerialInterface =
    (case responseOverSerialInterface of
        DecodeEventError error ->
            [ ( "DecodeEventError", error |> Json.Encode.string ) ]

        DecodeEventSuccess response ->
            [ ( "DecodeEventSuccess", response |> encodeBackendEventResponse ) ]
    )
        |> Json.Encode.object


encodeBackendEventResponse : BackendEventResponse -> Json.Encode.Value
encodeBackendEventResponse request =
    [ ( "notifyWhenPosixTimeHasArrived"
      , request.notifyWhenPosixTimeHasArrived
            |> Maybe.map (\\time -> [ ( "minimumPosixTimeMilli", time.minimumPosixTimeMilli |> Json.Encode.int ) ] |> Json.Encode.object)
            |> Maybe.withDefault Json.Encode.null
      )
    , ( "startTasks", request.startTasks |> Json.Encode.list encodeStartTask )
    , ( "completeHttpResponses", request.completeHttpResponses |> Json.Encode.list encodeHttpResponseRequest )
    ]
        |> Json.Encode.object


encodeStartTask : StartTaskStructure -> Json.Encode.Value
encodeStartTask startTaskAfterTime =
    Json.Encode.object
        [ ( "taskId", startTaskAfterTime.taskId |> encodeTaskId )
        , ( "task", startTaskAfterTime.task |> encodeTask )
        ]


encodeTaskId : TaskId -> Json.Encode.Value
encodeTaskId =
    Json.Encode.string


encodeTask : Task -> Json.Encode.Value
encodeTask task =
    case task of
        CreateVolatileProcess createVolatileProcess ->
            Json.Encode.object
                [ ( "CreateVolatileProcess"
                  , Json.Encode.object [ ( "programCode", createVolatileProcess.programCode |> Json.Encode.string ) ]
                  )
                ]

        RequestToVolatileProcess requestToVolatileProcess ->
            Json.Encode.object
                [ ( "RequestToVolatileProcess"
                  , Json.Encode.object
                        [ ( "processId", requestToVolatileProcess.processId |> Json.Encode.string )
                        , ( "request", requestToVolatileProcess.request |> Json.Encode.string )
                        ]
                  )
                ]

        TerminateVolatileProcess terminateVolatileProcess ->
            Json.Encode.object
                [ ( "TerminateVolatileProcess"
                  , Json.Encode.object
                        [ ( "processId", terminateVolatileProcess.processId |> Json.Encode.string )
                        ]
                  )
                ]


encodeHttpResponseRequest : RespondToHttpRequestStruct -> Json.Encode.Value
encodeHttpResponseRequest httpResponseRequest =
    Json.Encode.object
        [ ( "httpRequestId", httpResponseRequest.httpRequestId |> Json.Encode.string )
        , ( "response", httpResponseRequest.response |> encodeHttpResponse )
        ]


encodeHttpResponse : HttpResponse -> Json.Encode.Value
encodeHttpResponse httpResponse =
    [ ( "statusCode", httpResponse.statusCode |> Json.Encode.int )
    , ( "headersToAdd", httpResponse.headersToAdd |> Json.Encode.list encodeHttpHeader )
    , ( "bodyAsBase64", httpResponse.bodyAsBase64 |> Maybe.map Json.Encode.string |> Maybe.withDefault Json.Encode.null )
    ]
        |> Json.Encode.object


encodeHttpHeader : HttpHeader -> Json.Encode.Value
encodeHttpHeader httpHeader =
    [ ( "name", httpHeader.name |> Json.Encode.string )
    , ( "values", httpHeader.values |> Json.Encode.list Json.Encode.string )
    ]
        |> Json.Encode.object


""" ++ encodeFunction ++ "\n\n" ++ decodeFunction ++ """

decodeResult : Json.Decode.Decoder error -> Json.Decode.Decoder ok -> Json.Decode.Decoder (Result error ok)
decodeResult errorDecoder okDecoder =
    Json.Decode.oneOf
        [ Json.Decode.field "Err" errorDecoder |> Json.Decode.map Err
        , Json.Decode.field "Ok" okDecoder |> Json.Decode.map Ok
        ]


jsonDecodeSucceedWhenNotNull : a -> Json.Decode.Decoder a
jsonDecodeSucceedWhenNotNull valueIfNotNull =
    Json.Decode.value
        |> Json.Decode.andThen
            (\\asValue ->
                if asValue == Json.Encode.null then
                    Json.Decode.fail "Is null."

                else
                    Json.Decode.succeed valueIfNotNull
            )


decodeOptionalField : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
decodeOptionalField fieldName decoder =
    let
        finishDecoding json =
            case Json.Decode.decodeValue (Json.Decode.field fieldName Json.Decode.value) json of
                Ok _ ->
                    -- The field is present, so run the decoder on it.
                    Json.Decode.map Just (Json.Decode.field fieldName decoder)

                Err _ ->
                    -- The field was missing, which is fine!
                    Json.Decode.succeed Nothing
    in
    Json.Decode.value
        |> Json.Decode.andThen finishDecoding
"""


composeAppRootElmModuleText_Before_2021_08 :
    { interfaceToHostRootModuleName : String
    , rootModuleNameBeforeLowering : String
    , stateTypeAnnotation : ElmTypeAnnotation
    , modulesToImport : List (List String)
    , encodeFunction : String
    , decodeFunction : String
    }
    -> String
composeAppRootElmModuleText_Before_2021_08 { interfaceToHostRootModuleName, rootModuleNameBeforeLowering, stateTypeAnnotation, modulesToImport, encodeFunction, decodeFunction } =
    "module " ++ interfaceToHostRootModuleName ++ """ exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import """ ++ rootModuleNameBeforeLowering ++ """
""" ++ (modulesToImport |> List.map (String.join "." >> (++) "import ") |> String.join "\n") ++ """
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

""" ++ encodeFunction ++ "\n\n" ++ decodeFunction


composeStateMigrationModuleText :
    { migrateModuleName : String
    , generatedModuleName : String
    , decodeOrigTypeFunctionName : String
    , encodeDestTypeFunctionName : String
    , modulesToImport : List (List String)
    }
    -> String
composeStateMigrationModuleText { migrateModuleName, generatedModuleName, decodeOrigTypeFunctionName, encodeDestTypeFunctionName, modulesToImport } =
    String.trimLeft """
module """ ++ appStateMigrationRootModuleName ++ """ exposing (decodeMigrateAndEncodeAndSerializeResult, main)

import """ ++ migrateModuleName ++ """
import """ ++ generatedModuleName ++ """
""" ++ (modulesToImport |> List.map (String.join "." >> (++) "import ") |> List.sort |> String.join "\n") ++ """
import Json.Decode
import Json.Encode


decodeMigrateAndEncode : String -> Result String String
decodeMigrateAndEncode =
    Json.Decode.decodeString """ ++ generatedModuleName ++ "." ++ decodeOrigTypeFunctionName ++ """
        >> Result.map (""" ++ migrateModuleName ++ "." ++ appStateMigrationInterfaceFunctionName ++ """ >>  """ ++ generatedModuleName ++ "." ++ encodeDestTypeFunctionName ++ """ >> Json.Encode.encode 0)
        >> Result.mapError Json.Decode.errorToString


decodeMigrateAndEncodeAndSerializeResult : String -> String
decodeMigrateAndEncodeAndSerializeResult =
    decodeMigrateAndEncode
        >> jsonEncodeResult Json.Encode.string Json.Encode.string
        >> Json.Encode.encode 0


jsonEncodeResult : (err -> Json.Encode.Value) -> (ok -> Json.Encode.Value) -> Result err ok -> Json.Encode.Value
jsonEncodeResult encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( "Err", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( "Ok", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object


main : Program Int {} String
main =
    Platform.worker
        { init = \\_ -> ( {}, Cmd.none )
        , update =
            \\_ _ ->
                ( decodeMigrateAndEncodeAndSerializeResult |> always {}, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }
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
    { originalSourceModules : Dict.Dict String ( List String, Elm.Syntax.File.File ) }
    -> ( AppFiles, List String, String )
    -> Result (LocatedInSourceFiles String) ( AppFiles, String )
mapJsonCodersModuleText { originalSourceModules } ( sourceFiles, moduleFilePath, moduleText ) =
    parseElmModuleText moduleText
        |> Result.mapError
            (parserDeadEndsToString moduleText
                >> (++) "Failed to parse Elm module text: "
                >> Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteString moduleText)
                >> locatedInSourceFilesFromRange moduleFilePath
            )
        |> Result.andThen
            (\parsedModule ->
                let
                    interfaceModuleName =
                        Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedModule.moduleDefinition)
                in
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
                                    Err
                                        (LocatedInSourceFiles
                                            { filePath = moduleFilePath
                                            , locationInModuleText = Elm.Syntax.Node.range functionDeclaration.declaration
                                            }
                                            "Missing function signature"
                                        )

                                Just functionSignature ->
                                    let
                                        functionName =
                                            Elm.Syntax.Node.value (Elm.Syntax.Node.value functionSignature).name
                                    in
                                    case parseJsonCodingFunctionType (Elm.Syntax.Node.value functionSignature) of
                                        Err error ->
                                            Err
                                                (LocatedInSourceFiles
                                                    { filePath = moduleFilePath
                                                    , locationInModuleText = Elm.Syntax.Node.range functionSignature
                                                    }
                                                    ("Failed parsing json coding function type: " ++ error)
                                                )

                                        Ok functionType ->
                                            parseElmTypeAndDependenciesRecursivelyFromAnnotation
                                                originalSourceModules
                                                ( ( moduleFilePath, parsedModule ), functionType.typeAnnotation )
                                                |> Result.mapError
                                                    (mapLocatedInSourceFiles ((++) "Failed to parse type annotation: "))
                                                |> Result.map
                                                    (\( parsedTypeAnnotation, dependencies ) ->
                                                        { declarationRange = Elm.Syntax.Node.range functionDeclaration.declaration
                                                        , functionName = functionName
                                                        , functionType = functionType
                                                        , parsedTypeAnnotation = parsedTypeAnnotation
                                                        , dependencies = dependencies
                                                        }
                                                    )
                                                |> Result.mapError (mapLocatedInSourceFiles ((++) ("Failed to prepare mapping '" ++ functionName ++ "': ")))
                        )
                    |> Result.Extra.combine
                    |> Result.andThen
                        (\functionsToReplace ->
                            let
                                ( appFiles, { generatedModuleName, modulesToImport } ) =
                                    mapAppFilesToSupportJsonCoding
                                        { generatedModuleNamePrefix = interfaceModuleName }
                                        (functionsToReplace |> List.map .parsedTypeAnnotation)
                                        (functionsToReplace |> List.map .dependencies |> List.foldl Dict.union Dict.empty)
                                        sourceFiles
                            in
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
                                            |> Result.mapError (Elm.Syntax.Node.Node functionToReplace.declarationRange)
                                    )
                                    (addImportsInElmModuleText
                                        modulesToImport
                                        moduleText
                                        |> Result.mapError (Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteString moduleText))
                                    )
                                |> Result.map (Tuple.pair appFiles)
                                |> Result.mapError (locatedInSourceFilesFromRange moduleFilePath)
                        )
            )


mapAppFilesToSupportJsonCoding :
    { generatedModuleNamePrefix : List String }
    -> List ElmTypeAnnotation
    -> Dict.Dict String ElmCustomTypeStruct
    -> AppFiles
    -> ( AppFiles, { generatedModuleName : String, modulesToImport : List (List String) } )
mapAppFilesToSupportJsonCoding { generatedModuleNamePrefix } typeAnnotationsBeforeDeduplicating customTypes appFilesBefore =
    let
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
            [ [ modulesToImport |> List.map (String.join "." >> (++) "import ") |> List.sort |> String.join "\n" ]
            , functionsForGeneratedModule |> List.map .functionText
            ]
                |> List.concat
                |> List.map String.trim
                |> String.join "\n\n\n"

        generatedModuleHash =
            generatedModuleTextWithoutModuleDeclaration
                |> SHA256.fromString
                |> SHA256.toHex

        generatedModuleName =
            (generatedModuleNamePrefix |> String.join ".")
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
    ( appFiles
    , { generatedModuleName = generatedModuleName
      , modulesToImport = String.split "." generatedModuleName :: modulesToImport
      }
    )


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


mapSourceFilesModuleText : ( AppFiles, List String, String ) -> Result (LocatedInSourceFiles String) ( AppFiles, String )
mapSourceFilesModuleText ( sourceFiles, moduleFilePath, moduleText ) =
    let
        mapErrorStringForFunctionDeclaration functionDeclaration =
            let
                functionName =
                    Elm.Syntax.Node.value
                        (Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration).declaration).name
            in
            (++) ("Failed to replace function '" ++ functionName ++ "': ")
    in
    parseElmModuleText moduleText
        |> Result.mapError
            (parserDeadEndsToString moduleText
                >> (++) "Failed to parse Elm module text: "
                >> Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteString moduleText)
            )
        |> Result.andThen
            (\parsedModule ->
                parsedModule.declarations
                    -- TODO: Also share the 'map all functions' part with `mapJsonCodersModuleText`
                    -- Remember: The module to interface with git services will probably use similar functionality.
                    |> List.filterMap declarationWithRangeAsFunctionDeclaration
                    |> List.map
                        (\functionDeclaration ->
                            prepareReplaceFunctionInSourceFilesModuleText
                                sourceFiles
                                functionDeclaration
                                |> Result.mapError (mapErrorStringForFunctionDeclaration functionDeclaration)
                                |> Result.map (Tuple.pair functionDeclaration)
                                |> Result.mapError (Elm.Syntax.Node.Node (Elm.Syntax.Node.range functionDeclaration))
                        )
                    |> Result.Extra.combine
                    |> Result.andThen
                        (\preparedFunctions ->
                            let
                                interfaceModuleName =
                                    Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedModule.moduleDefinition)

                                generatedModuleTextWithoutModuleDeclaration =
                                    preparedFunctions
                                        |> List.map (Tuple.second >> .valueFunctionText)
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
                            preparedFunctions
                                |> listFoldlToAggregateResult
                                    (\( functionDeclaration, replaceFunction ) previousAggregate ->
                                        replaceFunction.updateInterfaceModuleText { generatedModuleName = generatedModuleName } previousAggregate
                                            |> Result.mapError (mapErrorStringForFunctionDeclaration functionDeclaration)
                                            |> Result.mapError (Elm.Syntax.Node.Node (Elm.Syntax.Node.range functionDeclaration))
                                    )
                                    (addImportsInElmModuleText [ [ "Base64" ], String.split "." generatedModuleName ] moduleText
                                        |> Result.mapError (Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteString moduleText))
                                    )
                                |> Result.map (Tuple.pair appFiles)
                        )
            )
        |> Result.mapError (locatedInSourceFilesFromRange moduleFilePath)


mapElmMakeModuleText :
    List ( DependencyKey, Bytes.Bytes )
    -> ( AppFiles, List String, String )
    -> Result (List LocatedCompilationError) ( AppFiles, String )
mapElmMakeModuleText dependencies ( sourceFiles, moduleFilePath, moduleText ) =
    parseElmModuleText moduleText
        |> Result.mapError
            (parserDeadEndsToString moduleText
                >> (++) "Failed to parse Elm module text: "
                >> OtherCompilationError
                >> Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteString moduleText)
                >> List.singleton
            )
        |> Result.andThen
            (\parsedModule ->
                parsedModule.declarations
                    -- TODO: Also share the 'map all functions' part with `mapJsonCodersModuleText`
                    |> List.filterMap declarationWithRangeAsFunctionDeclaration
                    |> List.map
                        (\declaration ->
                            prepareReplaceFunctionInElmMakeModuleText dependencies sourceFiles (Elm.Syntax.Node.value declaration)
                                |> Result.mapError (Elm.Syntax.Node.Node (Elm.Syntax.Node.range declaration))
                                |> Result.map (Tuple.pair (Elm.Syntax.Node.range declaration))
                        )
                    |> resultCombineConcatenatingErrors
                    |> Result.andThen
                        (\functionsToReplaceFunction ->
                            let
                                interfaceModuleName =
                                    Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedModule.moduleDefinition)

                                generatedModuleTextWithoutModuleDeclaration =
                                    functionsToReplaceFunction
                                        |> List.map (Tuple.second >> .valueFunctionText)
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
                                |> listFoldlToAggregateResult
                                    (\( declarationRange, replaceFunction ) previousAggregate ->
                                        replaceFunction.updateInterfaceModuleText { generatedModuleName = generatedModuleName } previousAggregate
                                            |> Result.mapError (Elm.Syntax.Node.Node declarationRange)
                                    )
                                    (addImportsInElmModuleText [ [ "Base64" ], String.split "." generatedModuleName ] moduleText
                                        |> Result.mapError (Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteString moduleText))
                                    )
                                |> Result.mapError (Elm.Syntax.Node.map OtherCompilationError >> List.singleton)
                                |> Result.map (Tuple.pair appFiles)
                        )
            )
        |> Result.mapError (List.map (locatedInSourceFilesFromRange moduleFilePath))


locatedInSourceFilesFromRange : List String -> Elm.Syntax.Node.Node a -> LocatedInSourceFiles a
locatedInSourceFilesFromRange filePath node =
    case node of
        Elm.Syntax.Node.Node range a ->
            LocatedInSourceFiles { filePath = filePath, locationInModuleText = range } a


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
    Dict.Dict String ( List String, Elm.Syntax.File.File )
    -> ( ( List String, Elm.Syntax.File.File ), Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Result (LocatedInSourceFiles String) ( ElmTypeAnnotation, Dict.Dict String ElmCustomTypeStruct )
parseElmTypeAndDependenciesRecursivelyFromAnnotation modules ( ( currentModuleFilePath, currentModule ), typeAnnotationNode ) =
    parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal
        { typesToIgnore = Set.empty }
        (Dict.map (always Tuple.second) modules)
        ( currentModule, Elm.Syntax.Node.value typeAnnotationNode )
        |> Result.mapError
            (LocatedInSourceFiles
                { filePath = currentModuleFilePath, locationInModuleText = Elm.Syntax.Node.range typeAnnotationNode }
            )


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


elmModulesDictFromAppFiles : AppFiles -> Dict.Dict String ( List String, ( String, Elm.Syntax.File.File ) )
elmModulesDictFromAppFiles =
    Dict.toList
        >> List.filter
            (Tuple.first
                >> List.reverse
                >> List.head
                >> Maybe.map (String.toLower >> String.endsWith ".elm")
                >> Maybe.withDefault False
            )
        >> List.filterMap
            (\( filePath, fileContent ) ->
                stringFromFileContent fileContent
                    |> Maybe.andThen
                        (\fileContentAsString ->
                            case parseElmModuleText fileContentAsString of
                                Err _ ->
                                    Nothing

                                Ok elmFile ->
                                    Just
                                        ( String.join "." (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value elmFile.moduleDefinition))
                                        , ( filePath
                                          , ( fileContentAsString, elmFile )
                                          )
                                        )
                        )
            )
        >> Dict.fromList


parseJsonCodingFunctionType :
    Elm.Syntax.Signature.Signature
    -> Result String { isDecoder : Bool, typeAnnotation : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation }
parseJsonCodingFunctionType signature =
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
                            { isDecoder = True
                            , typeAnnotation = singleTypeArgument
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
                            { isDecoder = False
                            , typeAnnotation = leftType
                            }

                _ ->
                    errorValue "right side of function type"

        _ ->
            errorValue ""


prepareReplaceFunctionInSourceFilesModuleText :
    AppFiles
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Function
    -> Result String { valueFunctionText : String, updateInterfaceModuleText : { generatedModuleName : String } -> String -> Result String String }
prepareReplaceFunctionInSourceFilesModuleText sourceFiles originalFunctionDeclaration =
    let
        functionName =
            Elm.Syntax.Node.value
                (Elm.Syntax.Node.value (Elm.Syntax.Node.value originalFunctionDeclaration).declaration).name
    in
    case parseSourceFileFunctionName functionName of
        Err error ->
            Err ("Failed to parse function name: " ++ error)

        Ok ( filePathRepresentation, config ) ->
            case findFileWithPathMatchingRepresentationInFunctionName sourceFiles filePathRepresentation of
                Err error ->
                    Err ("Failed to identify file for '" ++ filePathRepresentation ++ "': " ++ error)

                Ok ( _, fileContent ) ->
                    baseStringExpressionAndConversion config.encoding fileContent
                        |> Result.map
                            (\( expression, conversion ) ->
                                let
                                    valueFunctionName =
                                        [ "file_as"
                                        , expression.encodingName
                                        , SHA256.toHex (SHA256.fromBytes fileContent)
                                        ]
                                            |> String.join "_"

                                    valueFunctionText =
                                        valueFunctionName
                                            ++ " =\n"
                                            ++ indentElmCodeLines 1 expression.expression
                                in
                                { valueFunctionText = valueFunctionText
                                , updateInterfaceModuleText =
                                    \{ generatedModuleName } moduleText ->
                                        let
                                            fileExpression =
                                                buildElmExpressionFromStringExpression
                                                    conversion
                                                    (generatedModuleName ++ "." ++ valueFunctionName)

                                            buildNewFunctionLines previousFunctionLines =
                                                List.take 2 previousFunctionLines
                                                    ++ [ indentElmCodeLines 1 fileExpression ]
                                        in
                                        addOrUpdateFunctionInElmModuleText
                                            { functionName = functionName
                                            , mapFunctionLines = Maybe.withDefault [] >> buildNewFunctionLines
                                            }
                                            moduleText
                                }
                            )


prepareReplaceFunctionInElmMakeModuleText :
    List ( DependencyKey, Bytes.Bytes )
    -> AppFiles
    -> Elm.Syntax.Expression.Function
    -> Result CompilationError { valueFunctionText : String, updateInterfaceModuleText : { generatedModuleName : String } -> String -> Result String String }
prepareReplaceFunctionInElmMakeModuleText dependencies sourceFiles originalFunctionDeclaration =
    let
        functionName =
            Elm.Syntax.Node.value
                (Elm.Syntax.Node.value originalFunctionDeclaration.declaration).name
    in
    case parseElmMakeModuleFunctionName functionName of
        Err error ->
            Err (OtherCompilationError ("Failed to parse function name: " ++ error))

        Ok ( filePathRepresentation, { encoding, outputType, enableDebug } ) ->
            case findFileWithPathMatchingRepresentationInFunctionName sourceFiles filePathRepresentation of
                Err error ->
                    Err (OtherCompilationError ("Failed to identify file for '" ++ filePathRepresentation ++ "': " ++ error))

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
                            Err (MissingDependencyError dependencyKey)

                        Just ( _, dependencyValue ) ->
                            baseStringExpressionAndConversion encoding dependencyValue
                                |> Result.map
                                    (\( expression, conversion ) ->
                                        let
                                            valueFunctionName =
                                                [ "file_as"
                                                , expression.encodingName
                                                , SHA256.toHex (SHA256.fromBytes dependencyValue)
                                                ]
                                                    |> String.join "_"

                                            valueFunctionText =
                                                valueFunctionName
                                                    ++ " =\n"
                                                    ++ indentElmCodeLines 1 expression.expression
                                        in
                                        { valueFunctionText = valueFunctionText
                                        , updateInterfaceModuleText =
                                            \{ generatedModuleName } moduleText ->
                                                let
                                                    fileExpression =
                                                        buildElmExpressionFromStringExpression
                                                            conversion
                                                            (generatedModuleName ++ "." ++ valueFunctionName)

                                                    buildNewFunctionLines previousFunctionLines =
                                                        List.take 2 previousFunctionLines
                                                            ++ [ indentElmCodeLines 1 fileExpression ]
                                                in
                                                addOrUpdateFunctionInElmModuleText
                                                    { functionName = functionName, mapFunctionLines = Maybe.withDefault [] >> buildNewFunctionLines }
                                                    moduleText
                                        }
                                    )
                                |> Result.mapError OtherCompilationError


baseStringExpressionAndConversion :
    Maybe InterfaceBlobEncoding
    -> Bytes.Bytes
    -> Result String ( { expression : String, encodingName : String }, Maybe InterfaceValueConversion )
baseStringExpressionAndConversion encoding blob =
    let
        continueWithBase64AndConversion conversion =
            buildBase64ElmExpression blob
                |> Result.map (\expr -> ( { expression = expr, encodingName = "base64" }, conversion ))
    in
    case encoding of
        Nothing ->
            continueWithBase64AndConversion (Just FromBase64ToBytes)

        Just Base64Encoding ->
            continueWithBase64AndConversion Nothing

        Just Utf8Encoding ->
            case Bytes.Decode.decode (Bytes.Decode.string (Bytes.width blob)) blob of
                Nothing ->
                    Err "Failed to decode blob as UTF8"

                Just asUtf8 ->
                    Ok ( { expression = stringExpressionFromString asUtf8, encodingName = "utf8" }, Nothing )


buildBase64ElmExpression : Bytes.Bytes -> Result String String
buildBase64ElmExpression bytes =
    case Base64.fromBytes bytes of
        Nothing ->
            Err "Error encoding to base64"

        Just asBase64 ->
            Ok (stringExpressionFromString asBase64)


stringExpressionFromString : String -> String
stringExpressionFromString string =
    "\""
        ++ (string
                |> String.replace "\\" "\\\\"
                |> String.replace "\n" "\\n"
                |> String.replace "\u{000D}" "\\r"
                |> String.replace "\"" "\\\""
           )
        ++ "\""


buildElmExpressionFromStringExpression : Maybe InterfaceValueConversion -> String -> String
buildElmExpressionFromStringExpression conversion stringExpression =
    case conversion of
        Nothing ->
            stringExpression

        Just FromBase64ToBytes ->
            [ stringExpression
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


declarationWithRangeAsFunctionDeclaration : Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.Expression.Function)
declarationWithRangeAsFunctionDeclaration declaration =
    case Elm.Syntax.Node.value declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            Just (Elm.Syntax.Node.Node (Elm.Syntax.Node.range declaration) functionDeclaration)

        _ ->
            Nothing


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
        filesWithRepresentations =
            sourceFiles
                |> Dict.toList
                |> List.map (\( filePath, file ) -> ( filePathRepresentationInFunctionName filePath, ( filePath, file ) ))

        filesGroupedByRepresentation : Dict.Dict String (List ( List String, Bytes.Bytes ))
        filesGroupedByRepresentation =
            filesWithRepresentations
                |> List.map Tuple.first
                |> Set.fromList
                |> Set.toList
                |> List.map
                    (\representation ->
                        ( representation
                        , filesWithRepresentations |> List.filter (Tuple.first >> (==) representation) |> List.map Tuple.second
                        )
                    )
                |> Dict.fromList
    in
    case Maybe.withDefault [] (Dict.get pathPattern filesGroupedByRepresentation) of
        [ matchingFile ] ->
            Ok matchingFile

        [] ->
            let
                filesWithSimilarity =
                    filesGroupedByRepresentation
                        |> Dict.keys
                        |> List.map
                            (\pathRepresentation ->
                                ( pathRepresentation
                                , JaroWinkler.similarity pathRepresentation pathPattern
                                )
                            )
                        |> List.sortBy (Tuple.second >> negate)

                pointOutSimilarNamesLines =
                    case filesWithSimilarity |> List.filter (Tuple.second >> (<=) 0.5) of
                        ( mostSimilarRepresentation, _ ) :: _ ->
                            [ "Did you mean '" ++ mostSimilarRepresentation ++ "'?" ]

                        _ ->
                            []

                examplesListItems =
                    Dict.keys filesGroupedByRepresentation

                examplesListItemsForDisplay =
                    List.take 8 examplesListItems

                examplesListItemsDisplayItems =
                    examplesListItemsForDisplay
                        ++ (if examplesListItemsForDisplay == examplesListItems then
                                []

                            else
                                [ "..." ]
                           )
            in
            [ [ "Did not find any source file with a path matching the representation '"
                    ++ pathPattern
                    ++ "'."
              ]
            , pointOutSimilarNamesLines
            , [ "There are "
                    ++ String.fromInt (Dict.size sourceFiles)
                    ++ " files available in this compilation: "
                    ++ (examplesListItemsDisplayItems |> String.join ", ")
              ]
            ]
                |> List.concat
                |> String.join "\n"
                |> Err

        matchingFiles ->
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
mapElmModuleWithNameIfExists : ({ filePath : List String } -> String -> err) -> String -> (( AppFiles, List String, String ) -> Result err ( AppFiles, String )) -> AppFiles -> Result err AppFiles
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
                    Err (errFromString { filePath = elmModuleFilePath } "Failed to decode file content as string")

                Just moduleText ->
                    tryMapModuleText ( appCode, elmModuleFilePath, moduleText )
                        |> Result.map
                            (\( newAppCode, newModuleText ) ->
                                newAppCode
                                    |> Dict.insert elmModuleFilePath (Bytes.Encode.encode (Bytes.Encode.string newModuleText))
                            )


parseSourceFileFunctionName : String -> Result String ( String, InterfaceSourceFilesFunctionConfig )
parseSourceFileFunctionName functionName =
    parseFlagsAndPathPatternFromFunctionName sourceFileFunctionNameStart functionName
        |> Result.andThen
            (\( flags, filePathRepresentation ) ->
                flags
                    |> parseInterfaceFunctionFlags parseSourceFileFunctionFlag { encoding = Nothing }
                    |> Result.map (Tuple.pair filePathRepresentation)
            )


parseSourceFileFunctionFlag : String -> InterfaceSourceFilesFunctionConfig -> Result String InterfaceSourceFilesFunctionConfig
parseSourceFileFunctionFlag flag config =
    case String.toLower flag of
        "base64" ->
            Ok { config | encoding = Just Base64Encoding }

        "utf8" ->
            Ok { config | encoding = Just Utf8Encoding }

        _ ->
            Err "Unknown flag"


parseElmMakeModuleFunctionName : String -> Result String ( String, InterfaceElmMakeFunctionConfig )
parseElmMakeModuleFunctionName functionName =
    parseFlagsAndPathPatternFromFunctionName elmMakeFunctionNameStart functionName
        |> Result.andThen
            (\( flags, filePathRepresentation ) ->
                flags
                    |> parseInterfaceFunctionFlags parseElmMakeFunctionFlag
                        { outputType = ElmMakeOutputTypeHtml, enableDebug = False, encoding = Nothing }
                    |> Result.map (Tuple.pair filePathRepresentation)
            )


parseElmMakeFunctionFlag : String -> InterfaceElmMakeFunctionConfig -> Result String InterfaceElmMakeFunctionConfig
parseElmMakeFunctionFlag flag config =
    case String.toLower flag of
        "base64" ->
            Ok { config | encoding = Just Base64Encoding }

        "utf8" ->
            Ok { config | encoding = Just Utf8Encoding }

        "javascript" ->
            Ok { config | outputType = ElmMakeOutputTypeJs }

        "debug" ->
            Ok { config | enableDebug = True }

        _ ->
            Err "Unknown flag"


parseInterfaceFunctionFlags : (String -> aggregate -> Result String aggregate) -> aggregate -> List String -> Result String aggregate
parseInterfaceFunctionFlags parseFlag =
    Ok
        >> listFoldlToAggregateResult
            (\flag ->
                parseFlag flag
                    >> Result.mapError ((++) ("Error parsing flag '" ++ flag ++ "': "))
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


mapLocatedInSourceFiles : (a -> b) -> LocatedInSourceFiles a -> LocatedInSourceFiles b
mapLocatedInSourceFiles mapLocated orig =
    case orig of
        LocatedInSourceFiles location located ->
            LocatedInSourceFiles location (mapLocated located)


locatedInSourceFilesFromJustFilePath : { filePath : List String } -> err -> LocatedInSourceFiles err
locatedInSourceFilesFromJustFilePath { filePath } err =
    LocatedInSourceFiles { filePath = filePath, locationInModuleText = Elm.Syntax.Range.emptyRange } err


syntaxRangeCoveringCompleteString : String -> Elm.Syntax.Range.Range
syntaxRangeCoveringCompleteString string =
    let
        lines =
            String.lines string
    in
    { start = { row = 1, column = 1 }
    , end =
        { row = List.length lines + 1
        , column =
            case lines |> List.reverse |> List.head of
                Nothing ->
                    1

                Just lastLine ->
                    String.length lastLine + 1
        }
    }


syntaxRangeCoveringCompleteModule : Elm.Syntax.File.File -> Elm.Syntax.Range.Range
syntaxRangeCoveringCompleteModule file =
    [ [ Elm.Syntax.Node.range file.moduleDefinition ]
    , List.map Elm.Syntax.Node.range file.comments
    , List.map Elm.Syntax.Node.range file.imports
    , List.map Elm.Syntax.Node.range file.declarations
    ]
        |> List.concat
        |> Elm.Syntax.Range.combine


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


resultCombineConcatenatingErrors : List (Result err ok) -> Result (List err) (List ok)
resultCombineConcatenatingErrors =
    List.foldl
        (\result previousAggregate ->
            case previousAggregate of
                Err previousErrors ->
                    Err (previousErrors ++ (result |> Result.Extra.error |> Maybe.map List.singleton |> Maybe.withDefault []))

                Ok previousList ->
                    case result of
                        Err error ->
                            Err [ error ]

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
