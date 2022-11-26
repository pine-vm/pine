module CompileFullstackApp exposing
    ( AppFiles
    , CompilationArguments
    , CompilationError(..)
    , DependencyKey(..)
    , ElmChoiceTypeStruct
    , ElmMakeOutputType(..)
    , ElmMakeRequestStructure
    , ElmTypeAnnotation(..)
    , InterfaceBlobSingleEncoding(..)
    , InterfaceSourceFilesFunctionVariant(..)
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
    , jsonCodingFunctionFromChoiceType
    , parseAppStateElmTypeAndDependenciesRecursively
    , parseElmMakeModuleFunctionName
    , parseElmModuleText
    , parseElmTypeAndDependenciesRecursivelyFromAnnotation
    , parseInterfaceRecordTree
    , parseSourceFileFunction
    , parseSourceFileFunctionName
    , parserDeadEndToString
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
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.Signature
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import FileTree
import Flate
import JaroWinkler
import Json.Encode
import List
import List.Extra
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
    { backendMainDeclarationName : String
    , serializeStateFunctionName : String
    , deserializeStateFunctionName : String
    }


type alias MigrationConfig =
    { inputType : ElmTypeAnnotation
    , returnType : ElmTypeAnnotation
    , dependencies : Dict.Dict String ElmChoiceTypeStruct
    , migrateFunctionModuleName : List String
    , migrateFunctionDeclarationLocalName : String
    }


type alias BackendRootModuleConfig =
    { interfaceToHostRootModuleName : String
    , rootModuleNameBeforeLowering : String
    , stateTypeAnnotation : ElmTypeAnnotation
    , modulesToImport : List (List String)
    , stateEncodeFunction : String
    , stateDecodeFunction : String
    , stateFromStringExpression : String
    , migrateFromStringExpression : String
    }


elmAppInterfaceConvention : ElmAppInterfaceConvention
elmAppInterfaceConvention =
    { backendMainDeclarationName = "backendMain"
    , serializeStateFunctionName = "interfaceToHost_serializeState"
    , deserializeStateFunctionName = "interfaceToHost_deserializeState"
    }


appStateMigrationInterfaceModuleName : String
appStateMigrationInterfaceModuleName =
    "Backend.MigrateState"


appStateMigrationInterfaceFunctionName : String
appStateMigrationInterfaceFunctionName =
    "migrate"


encodingModuleImportBytes : ( List String, Maybe String )
encodingModuleImportBytes =
    ( [ "CompilerGenerated", "EncodeBytes" ], Just "EncodeBytes" )


encodingModuleImportBase64 : ( List String, Maybe String )
encodingModuleImportBase64 =
    ( [ "CompilerGenerated", "Base64" ], Just "Base64" )


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


type CompilationInterfaceRecordTreeNode leaf
    = RecordTreeLeaf leaf
    | RecordTreeBranch (List ( String, CompilationInterfaceRecordTreeNode leaf ))


type alias InterfaceElmMakeFunctionConfig =
    CompilationInterfaceRecordTreeNode InterfaceElmMakeFunctionLeafConfig


type alias InterfaceElmMakeFunctionLeafConfig =
    { elmMakeConfig : InterfaceElmMakeConfig
    , emitBlob : RecordTreeEmit
    }


type alias InterfaceElmMakeConfig =
    { outputType : ElmMakeOutputType
    , enableDebug : Bool
    }


type alias ElmMakeRecordTreeLeafEmit =
    { blob : Bytes.Bytes
    , emitBlob : RecordTreeEmit
    , valueFunctionName : String
    }


type ElmMakeOutputType
    = ElmMakeOutputTypeHtml
    | ElmMakeOutputTypeJs


type InterfaceBlobSingleEncoding
    = Base64Encoding
    | Utf8Encoding
    | BytesEncoding
    | GZipEncoding


type alias InterfaceSourceFilesFunctionConfig =
    { variant : InterfaceSourceFilesFunctionVariant
    , encoding : CompilationInterfaceRecordTreeNode InterfaceBlobSingleEncoding
    }


type InterfaceSourceFilesFunctionVariant
    = SourceFile
    | SourceFileTree


{-| Generic enough to be used in source files trees: We can use the `valueModule` function for each blob contained in the tree.
The signature of the `valueModule` function allows for encoding (e.g. `utf8`) to fail
-}
type alias RecordTreeEmit =
    { interfaceModule : RecordTreeEmitInterfaceModule
    , valueModule : RecordTreeEmitValueModule
    }


type alias RecordTreeEmitElmMake =
    { interfaceModule : RecordTreeEmitInterfaceModule
    , valueModule : { expression : String }
    }


type alias RecordTreeEmitInterfaceModule =
    CompilationInterfaceRecordTreeNode ({ sourceExpression : String } -> String)


type alias RecordTreeEmitValueModule =
    Bytes.Bytes -> Result String (Dict.Dict String { expression : String })


type alias RecordTreeEmitBlobIntermediateResult =
    { interfaceModule : { sourceExpression : String } -> String
    , valueModule : { fieldName : String, buildExpression : Bytes.Bytes -> Result String String }
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

        compilationInterfaceModuleDependencies : Dict.Dict String (List String)
        compilationInterfaceModuleDependencies =
            [ ( "SourceFiles", modulesToAddForBytesCoding )
            , ( "ElmMake", modulesToAddForBytesCoding )
            , ( "GenerateJsonCoders", modulesToAddForBase64Coding )
            ]
                |> Dict.fromList

        usedCompilationInterfaceModules : Set.Set String
        usedCompilationInterfaceModules =
            sourceModules
                |> Dict.keys
                |> List.concatMap
                    (\moduleName ->
                        compilationInterfaceElmModuleNamePrefixes
                            |> List.concatMap
                                (\compilationInterfacePrefix ->
                                    if String.startsWith (compilationInterfacePrefix ++ ".") moduleName then
                                        [ String.dropLeft (String.length compilationInterfacePrefix + 1) moduleName ]

                                    else
                                        []
                                )
                    )
                |> Set.fromList

        containsBackend =
            Dict.get (String.join "." rootModuleName) sourceModules /= Nothing

        modulesToAddForBackendDepdendencies =
            if containsBackend then
                Set.fromList modulesToAddForBase64Coding

            else
                Set.empty

        modulesToAdd =
            usedCompilationInterfaceModules
                |> Set.toList
                |> List.filterMap (\moduleName -> Dict.get moduleName compilationInterfaceModuleDependencies)
                |> List.concat
                |> Set.fromList
                |> Set.union modulesToAddForBackendDepdendencies
                |> Set.toList
    in
    modulesToAdd
        |> List.foldl
            (\moduleToAdd prevFiles ->
                moduleToAdd
                    |> parseElmModuleText
                    |> Result.map
                        (\moduleToAddSyntax ->
                            let
                                filePath =
                                    filePathFromElmModuleName
                                        (String.join "." (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value moduleToAddSyntax.moduleDefinition)))
                            in
                            prevFiles
                                |> Dict.insert filePath (fileContentFromString moduleToAdd)
                        )
                    |> Result.withDefault prevFiles
            )
            sourceFiles
        |> loweredForSourceFiles compilationInterfaceElmModuleNamePrefixes
        |> Result.andThen (loweredForJsonCoders { originalSourceModules = sourceModules } compilationInterfaceElmModuleNamePrefixes)
        |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)
        |> Result.andThen (loweredForElmMake compilationInterfaceElmModuleNamePrefixes dependencies)
        |> Result.andThen
            (loweredForBackendApp
                { originalSourceModules = sourceModules
                , rootModuleName = rootModuleName
                , interfaceToHostRootModuleName = interfaceToHostRootModuleName
                }
            )


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


loweredForBackendApp :
    { rootModuleName : List String
    , interfaceToHostRootModuleName : List String
    , originalSourceModules : Dict.Dict String ( List String, Elm.Syntax.File.File )
    }
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) AppFiles
loweredForBackendApp { rootModuleName, interfaceToHostRootModuleName, originalSourceModules } sourceFiles =
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
                    |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)
                    |> Result.andThen
                        (\( stateTypeAnnotation, stateTypeDependencies ) ->
                            parseMigrationConfig { originalSourceModules = originalSourceModules }
                                |> Result.map
                                    (\maybeMigrationConfig ->
                                        let
                                            stateAndMigrationTypeDependencies =
                                                stateTypeDependencies
                                                    |> Dict.union
                                                        (maybeMigrationConfig
                                                            |> Maybe.map .dependencies
                                                            |> Maybe.withDefault Dict.empty
                                                        )

                                            typeToGenerateSerializersFor =
                                                stateTypeAnnotation :: typeToGenerateSerializersForMigration

                                            ( appFiles, generateSerializersResult ) =
                                                mapAppFilesToSupportJsonCoding
                                                    { generatedModuleNamePrefix = interfaceToHostRootModuleName }
                                                    typeToGenerateSerializersFor
                                                    stateAndMigrationTypeDependencies
                                                    sourceFiles

                                            modulesToImport =
                                                generateSerializersResult.modulesToImport
                                                    ++ (maybeMigrationConfig
                                                            |> Maybe.map (.migrateFunctionModuleName >> List.singleton)
                                                            |> Maybe.withDefault []
                                                       )

                                            functionsNamesInGeneratedModules =
                                                buildJsonCodingFunctionsForTypeAnnotation stateTypeAnnotation

                                            encodeFunction =
                                                "jsonEncodeDeserializedState =\n"
                                                    ++ indentElmCodeLines 1
                                                        (generateSerializersResult.generatedModuleName ++ "." ++ functionsNamesInGeneratedModules.encodeFunction.name)

                                            decodeFunction =
                                                "jsonDecodeDeserializedState =\n"
                                                    ++ indentElmCodeLines 1
                                                        (generateSerializersResult.generatedModuleName ++ "." ++ functionsNamesInGeneratedModules.decodeFunction.name)

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
                                                            [ "Json.Decode.decodeString " ++ (generatedModuleName ++ "." ++ inputTypeFunctionNames.decodeFunction.name)
                                                            , ">> Result.mapError Json.Decode.errorToString"
                                                            , ">> Result.map " ++ String.join "." (migrationConfig.migrateFunctionModuleName ++ [ migrationConfig.migrateFunctionDeclarationLocalName ])
                                                            ]
                                                                |> String.join "\n"
                                                        , [ migrationConfig.inputType, migrationConfig.returnType ]
                                                        )

                                            stateFromStringExpression =
                                                [ "Json.Decode.decodeString " ++ (generateSerializersResult.generatedModuleName ++ "." ++ functionsNamesInGeneratedModules.decodeFunction.name)
                                                , ">> Result.mapError Json.Decode.errorToString"
                                                ]
                                                    |> String.join "\n"

                                            rootElmModuleText =
                                                composeBackendRootElmModuleText
                                                    { interfaceToHostRootModuleName = String.join "." interfaceToHostRootModuleName
                                                    , rootModuleNameBeforeLowering = String.join "." rootModuleName
                                                    , stateTypeAnnotation = stateTypeAnnotation
                                                    , modulesToImport = modulesToImport
                                                    , stateEncodeFunction = encodeFunction
                                                    , stateDecodeFunction = decodeFunction
                                                    , stateFromStringExpression = stateFromStringExpression
                                                    , migrateFromStringExpression = migrateFromStringExpressionFromGenerateModuleName generateSerializersResult.generatedModuleName
                                                    }
                                        in
                                        appFiles
                                            |> updateFileContentAtPath
                                                (always (fileContentFromString rootElmModuleText))
                                                interfaceToHostRootFilePath
                                    )
                        )


parseMigrationConfig :
    { originalSourceModules : Dict.Dict String ( List String, Elm.Syntax.File.File ) }
    -> Result (List (LocatedInSourceFiles CompilationError)) (Maybe MigrationConfig)
parseMigrationConfig { originalSourceModules } =
    case Dict.get appStateMigrationInterfaceModuleName originalSourceModules of
        Nothing ->
            Ok Nothing

        Just ( originalInterfaceModuleFilePath, originalInterfaceModule ) ->
            parseAppStateMigrateElmTypeAndDependenciesRecursively originalSourceModules ( originalInterfaceModuleFilePath, originalInterfaceModule )
                |> Result.mapError (mapLocatedInSourceFiles ((++) "Failed to parse migration state type name: "))
                |> Result.map
                    (\( ( inputType, returnType ), stateTypeDependencies ) ->
                        Just
                            { inputType = inputType
                            , returnType = returnType
                            , dependencies = stateTypeDependencies
                            , migrateFunctionModuleName = String.split "." appStateMigrationInterfaceModuleName
                            , migrateFunctionDeclarationLocalName = appStateMigrationInterfaceFunctionName
                            }
                    )
                |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)


parseAppStateElmTypeAndDependenciesRecursively :
    Dict.Dict String ( List String, Elm.Syntax.File.File )
    -> ( List String, Elm.Syntax.File.File )
    -> Result (LocatedInSourceFiles String) ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
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
            (\stateTypeAnnotation ->
                parseElmTypeAndDependenciesRecursivelyFromAnnotation
                    sourceModules
                    ( ( parsedModuleFilePath, parsedModule ), stateTypeAnnotation )
            )


parseAppStateMigrateElmTypeAndDependenciesRecursively :
    Dict.Dict String ( List String, Elm.Syntax.File.File )
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


stateTypeAnnotationFromRootElmModule : Elm.Syntax.File.File -> Result String (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
stateTypeAnnotationFromRootElmModule parsedModule =
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
        |> Maybe.withDefault (Err ("Did not find declaration with name '" ++ elmAppInterfaceConvention.backendMainDeclarationName ++ "'"))


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


composeBackendRootElmModuleText : BackendRootModuleConfig -> String
composeBackendRootElmModuleText config =
    "module " ++ config.interfaceToHostRootModuleName ++ """ exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import """ ++ config.rootModuleNameBeforeLowering ++ """
""" ++ (config.modulesToImport |> List.map (Tuple.pair >> (|>) Nothing >> importSyntaxTextFromModuleNameAndAlias) |> String.join "\n") ++ """
import Platform
import ElmFullstack exposing (..)


type alias DeserializedState =
    (""" ++ buildTypeAnnotationText config.stateTypeAnnotation ++ """)


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
    | InitStateEvent
    | SetStateEvent String
    | MigrateStateEvent String


type alias BackendEventResponse =
    { startTasks : List StartTaskStructure
    , notifyWhenPosixTimeHasArrived : Maybe { minimumPosixTimeMilli : Int }
    , completeHttpResponses : List RespondToHttpRequestStruct
    , migrateResult : Maybe (Result String ())
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
    """ ++ config.rootModuleNameBeforeLowering ++ """.backendMain.init
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

        InitStateEvent ->
            continueWithUpdateToTasks (always Backend.Main.backendMain.init) stateBefore

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


setStateFromString : String -> Result String DeserializedState
setStateFromString =
""" ++ indentElmCodeLines 1 config.stateFromStringExpression ++ """

migrateFromString : String -> Result String ( DeserializedState, BackendCmds DeserializedState )
migrateFromString =
""" ++ indentElmCodeLines 1 config.migrateFromStringExpression ++ """


backendEventResponseFromRuntimeTasksAndSubscriptions :
    (DeserializedState -> BackendSubs DeserializedState)
    -> List (BackendCmd DeserializedState)
    -> DeserializedStateWithTaskFramework
    -> ( DeserializedStateWithTaskFramework, BackendEventResponse )
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


backendEventResponseFromSubscriptions : BackendSubs DeserializedState -> BackendEventResponse
backendEventResponseFromSubscriptions subscriptions =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived =
        subscriptions.posixTimeIsPast
            |> Maybe.map (\\posixTimeIsPast -> { minimumPosixTimeMilli = posixTimeIsPast.minimumPosixTimeMilli })
    , migrateResult = Nothing
    }


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
    , migrateResult = Nothing
    }


passiveBackendEventResponse : BackendEventResponse
passiveBackendEventResponse =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived = Nothing
    , migrateResult = Nothing
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
        , Json.Decode.field "InitStateEvent" (jsonDecodeSucceedWhenNotNull InitStateEvent)
        , Json.Decode.field "SetStateEvent" (Json.Decode.map SetStateEvent Json.Decode.string)
        , Json.Decode.field "MigrateStateEvent" (Json.Decode.map MigrateStateEvent Json.Decode.string)
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
encodeBackendEventResponse response =
    [ ( "notifyWhenPosixTimeHasArrived"
      , response.notifyWhenPosixTimeHasArrived
            |> Maybe.map (\\time -> [ ( "minimumPosixTimeMilli", time.minimumPosixTimeMilli |> Json.Encode.int ) ] |> Json.Encode.object)
            |> Maybe.withDefault Json.Encode.null
      )
    , ( "startTasks", response.startTasks |> Json.Encode.list encodeStartTask )
    , ( "completeHttpResponses", response.completeHttpResponses |> Json.Encode.list encodeHttpResponseRequest )
    , ( "migrateResult", response.migrateResult |> jsonEncode__generic_Maybe (jsonEncode__generic_Result Json.Encode.string (always (Json.Encode.object []))) )
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


""" ++ config.stateEncodeFunction ++ "\n\n" ++ config.stateDecodeFunction ++ """


jsonEncode__generic_Result : (a -> Json.Encode.Value) -> (b -> Json.Encode.Value) -> Result a b -> Json.Encode.Value
jsonEncode__generic_Result encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( "Err", encodeErr valueToEncodeError ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( "Ok", encodeOk valueToEncodeOk ) ] |> Json.Encode.object


jsonEncode__generic_Maybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
jsonEncode__generic_Maybe encodeJust valueToEncode =
    case valueToEncode of
        Nothing ->
            [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object

        Just just ->
            [ ( "Just", encodeJust just ) ] |> Json.Encode.object


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


sourceFileFunctionNameStart : String
sourceFileFunctionNameStart =
    "file"


sourceFileTreeFunctionNameStart : String
sourceFileTreeFunctionNameStart =
    "file_tree"


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
                                        (List.map (Tuple.pair >> (|>) Nothing) modulesToImport)
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
    -> Dict.Dict String ElmChoiceTypeStruct
    -> AppFiles
    -> ( AppFiles, { generatedModuleName : String, modulesToImport : List (List String) } )
mapAppFilesToSupportJsonCoding { generatedModuleNamePrefix } typeAnnotationsBeforeDeduplicating choiceTypes appFilesBefore =
    let
        modulesToImportForChoiceTypes =
            choiceTypes
                |> Dict.keys
                |> List.map moduleNameFromTypeName
                |> Set.fromList
                |> Set.toList
                |> List.map (String.split ".")

        modulesToImport =
            [ [ "Dict" ]
            , [ "Set" ]
            , [ "Json", "Decode" ]
            , [ "Json", "Encode" ]
            , [ "Bytes" ]
            , [ "Bytes", "Decode" ]
            , [ "Bytes", "Encode" ]
            ]
                ++ modulesToImportForChoiceTypes

        generatedModuleModulesToImport =
            encodingModuleImportBase64 :: List.map (Tuple.pair >> (|>) Nothing) modulesToImport

        appFilesAfterExposingChoiceTypesInModules =
            modulesToImportForChoiceTypes
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
            choiceTypes
                |> Dict.toList
                |> List.map
                    (\( choiceTypeName, choiceType ) ->
                        jsonCodingFunctionFromChoiceType
                            { choiceTypeName = choiceTypeName
                            , encodeValueExpression = jsonEncodeParamName
                            , typeArgLocalName = "type_arg"
                            }
                            choiceType
                    )
                |> List.concatMap
                    (\functionsForType ->
                        [ functionsForType.encodeFunction, functionsForType.decodeFunction ]
                    )
                |> List.map (\function -> { functionName = function.name, functionText = function.text })

        functionsForGeneratedModule =
            typeAnnotationsFunctionsForGeneratedModule ++ dependenciesFunctions ++ generalSupportingFunctionsTexts

        generatedModuleTextWithoutModuleDeclaration =
            [ [ generatedModuleModulesToImport
                    |> List.map importSyntaxTextFromModuleNameAndAlias
                    |> List.sort
                    |> String.join "\n"
              ]
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
            appFilesAfterExposingChoiceTypesInModules
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

        sourceFilesInterfaceModuleAddedFunctionsNames =
            List.map .functionName sourceFilesInterfaceModuleAddedFunctions
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
                    |> List.filter
                        (Elm.Syntax.Node.value
                            >> .declaration
                            >> Elm.Syntax.Node.value
                            >> .name
                            >> Elm.Syntax.Node.value
                            >> (\functionName -> not (List.member functionName sourceFilesInterfaceModuleAddedFunctionsNames))
                        )
                    |> List.map
                        (\functionDeclaration ->
                            prepareReplaceFunctionInSourceFilesModuleText
                                sourceFiles
                                ( moduleFilePath, parsedModule )
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

                                generatedModuleTypeDeclarations : List String
                                generatedModuleTypeDeclarations =
                                    [ """
type FileTreeNode blobStructure
    = BlobNode blobStructure
    | TreeNode (List ( String, FileTreeNode blobStructure ))
"""
                                    ]

                                generatedModuleDeclarationsBeforeRemovingDuplicates =
                                    generatedModuleTypeDeclarations
                                        ++ List.map (Tuple.second >> .valueFunctionText) preparedFunctions

                                generatedModuleDeclarations =
                                    Set.fromList generatedModuleDeclarationsBeforeRemovingDuplicates

                                generatedModuleTextWithoutModuleDeclaration =
                                    String.join "\n\n" (Set.toList generatedModuleDeclarations)

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

                                interfaceModuleDeclaresTypeFileTreeNode =
                                    parsedModule.declarations
                                        |> List.any
                                            (\declaration ->
                                                case Elm.Syntax.Node.value declaration of
                                                    Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                                        Elm.Syntax.Node.value choiceTypeDeclaration.name == "FileTreeNode"

                                                    _ ->
                                                        False
                                            )

                                addMappingFunctionIfTypeIsPresent =
                                    if interfaceModuleDeclaresTypeFileTreeNode then
                                        \moduleTextBefore ->
                                            List.foldl
                                                (\addedFunction previousResult ->
                                                    previousResult
                                                        |> Result.andThen
                                                            (addOrUpdateFunctionInElmModuleText
                                                                { functionName = addedFunction.functionName
                                                                , mapFunctionLines = addedFunction.mapFunctionLines { generatedModuleName = generatedModuleName }
                                                                }
                                                            )
                                                )
                                                (Ok moduleTextBefore)
                                                sourceFilesInterfaceModuleAddedFunctions

                                    else
                                        Ok
                            in
                            preparedFunctions
                                |> listFoldlToAggregateResult
                                    (\( functionDeclaration, replaceFunction ) previousAggregate ->
                                        replaceFunction.updateInterfaceModuleText { generatedModuleName = generatedModuleName } previousAggregate
                                            |> Result.mapError (mapErrorStringForFunctionDeclaration functionDeclaration)
                                            |> Result.mapError (Elm.Syntax.Node.Node (Elm.Syntax.Node.range functionDeclaration))
                                    )
                                    (addImportsInElmModuleText
                                        [ encodingModuleImportBytes
                                        , ( String.split "." generatedModuleName, Nothing )
                                        ]
                                        moduleText
                                        |> Result.andThen addMappingFunctionIfTypeIsPresent
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
                            prepareReplaceFunctionInElmMakeModuleText dependencies
                                sourceFiles
                                ( moduleFilePath, parsedModule )
                                (Elm.Syntax.Node.value declaration)
                                |> Result.mapError (List.map (Elm.Syntax.Node.Node (Elm.Syntax.Node.range declaration)))
                                |> Result.map (Tuple.pair (Elm.Syntax.Node.range declaration))
                        )
                    |> resultCombineConcatenatingErrors
                    |> Result.mapError List.concat
                    |> Result.andThen
                        (\functionsToReplaceFunction ->
                            let
                                interfaceModuleName =
                                    Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedModule.moduleDefinition)

                                generatedModuleTextWithoutModuleDeclaration =
                                    functionsToReplaceFunction
                                        |> List.concatMap (Tuple.second >> .valueFunctionsTexts)
                                        |> Set.fromList
                                        |> Set.toList
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
                                    (addImportsInElmModuleText
                                        [ encodingModuleImportBytes
                                        , ( String.split "." generatedModuleName, Nothing )
                                        ]
                                        moduleText
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
    = ChoiceElmType String
    | RecordElmType { fields : List ( String, ElmTypeAnnotation ) }
    | InstanceElmType { instantiated : ElmTypeAnnotation, arguments : List ElmTypeAnnotation }
    | TupleElmType (List ElmTypeAnnotation)
    | LeafElmType LeafElmTypeStruct
    | GenericType String
    | UnitType


type alias ElmChoiceTypeStruct =
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
    -> Result (LocatedInSourceFiles String) ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
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
    -> Result String ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal stack modules ( currentModule, typeAnnotation ) =
    case typeAnnotation of
        Elm.Syntax.TypeAnnotation.Typed instantiatedNode argumentsNodes ->
            parseElmTypeAndDependenciesRecursivelyFromAnnotationInternalTyped
                stack
                modules
                ( currentModule, ( instantiatedNode, argumentsNodes ) )

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


parseElmTypeAndDependenciesRecursivelyFromAnnotationInternalTyped :
    { typesToIgnore : Set.Set String }
    -> Dict.Dict String Elm.Syntax.File.File
    -> ( Elm.Syntax.File.File, ( Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ), List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation) ) )
    -> Result String ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
parseElmTypeAndDependenciesRecursivelyFromAnnotationInternalTyped stack modules ( currentModule, ( instantiatedNode, argumentsNodes ) ) =
    let
        ( instantiatedModuleAlias, instantiatedLocalName ) =
            Elm.Syntax.Node.value instantiatedNode

        instantiatedResult : Result String ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct, List String )
        instantiatedResult =
            case
                parseElmTypeLeavesNames
                    |> Dict.get (instantiatedModuleAlias ++ [ instantiatedLocalName ] |> String.join ".")
            of
                Just leaf ->
                    Ok ( LeafElmType leaf, Dict.empty, [] )

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

                                            Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                                if Elm.Syntax.Node.value choiceTypeDeclaration.name /= instantiatedLocalName then
                                                    Nothing

                                                else
                                                    Just (ChoiceTypeDeclaration choiceTypeDeclaration)

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
                                                        Ok
                                                            ( aliasedType
                                                            , aliasedTypeDeps
                                                            , aliasDeclaration.generics |> List.map Elm.Syntax.Node.value
                                                            )

                                            ChoiceTypeDeclaration choiceTypeDeclaration ->
                                                let
                                                    typeName =
                                                        Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value instantiatedModule.moduleDefinition)
                                                            ++ [ Elm.Syntax.Node.value choiceTypeDeclaration.name ]
                                                            |> String.join "."

                                                    genericsNames =
                                                        choiceTypeDeclaration.generics
                                                            |> List.map Elm.Syntax.Node.value
                                                in
                                                if stack.typesToIgnore |> Set.member typeName then
                                                    Ok ( ChoiceElmType typeName, Dict.empty, genericsNames )

                                                else
                                                    choiceTypeDeclaration.constructors
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
                                                                ( ChoiceElmType typeName
                                                                , constructorsDeps
                                                                    |> Dict.insert
                                                                        typeName
                                                                        { parameters = genericsNames
                                                                        , tags = constructors |> Dict.fromList
                                                                        }
                                                                , genericsNames
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
            (\( instantiatedType, instantiatedDependencies, genericsNames ) ->
                argumentsNodes
                    |> List.map (Elm.Syntax.Node.value >> Tuple.pair currentModule)
                    |> List.map (parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal stack modules)
                    |> Result.Extra.combine
                    |> Result.map listTupleSecondDictUnion
                    |> Result.andThen
                        (\( instanceArguments, instanceArgumentsDeps ) ->
                            if instanceArguments == [] then
                                Ok ( instantiatedType, instantiatedDependencies )

                            else
                                case instantiatedType of
                                    RecordElmType recordType ->
                                        (if List.length genericsNames /= List.length instanceArguments then
                                            Err "Mismatch between lengths of genericsNames and instanceArguments"

                                         else
                                            tryConcretizeRecordInstance
                                                (Dict.fromList (List.Extra.zip genericsNames instanceArguments))
                                                recordType
                                                |> Result.map
                                                    (\concretizedRecord ->
                                                        ( RecordElmType concretizedRecord
                                                        , instanceArgumentsDeps |> Dict.union instantiatedDependencies
                                                        )
                                                    )
                                        )
                                            |> Result.mapError ((++) "Failed to concretize record instance: ")

                                    _ ->
                                        Ok
                                            ( InstanceElmType
                                                { instantiated = instantiatedType
                                                , arguments = instanceArguments
                                                }
                                            , instanceArgumentsDeps |> Dict.union instantiatedDependencies
                                            )
                        )
            )


tryConcretizeRecordInstance :
    Dict.Dict String ElmTypeAnnotation
    -> { fields : List ( String, ElmTypeAnnotation ) }
    -> Result String { fields : List ( String, ElmTypeAnnotation ) }
tryConcretizeRecordInstance typeArguments recordType =
    let
        tryConcretizeFieldType fieldType =
            case fieldType of
                GenericType genericName ->
                    case Dict.get genericName typeArguments of
                        Nothing ->
                            Err ("Did not find type for '" ++ genericName ++ "'")

                        Just typeArgument ->
                            Ok typeArgument

                _ ->
                    Ok fieldType
    in
    recordType.fields
        |> List.foldl
            (\( fieldName, fieldType ) prevAggregate ->
                prevAggregate
                    |> Result.andThen
                        (\prevFields ->
                            tryConcretizeFieldType fieldType
                                |> Result.map
                                    (\concretizedFieldType ->
                                        ( fieldName, concretizedFieldType ) :: prevFields
                                    )
                        )
            )
            (Ok [])
        |> Result.map (\fields -> { fields = fields })


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
        ChoiceElmType choice ->
            let
                typeNameRepresentation =
                    jsonCodingFunctionNameCommonPartFromTypeName choice
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


jsonCodingFunctionFromChoiceType :
    { choiceTypeName : String, encodeValueExpression : String, typeArgLocalName : String }
    -> ElmChoiceTypeStruct
    -> { encodeFunction : { name : String, text : String }, decodeFunction : { name : String, text : String } }
jsonCodingFunctionFromChoiceType { choiceTypeName, encodeValueExpression, typeArgLocalName } choiceType =
    let
        encodeParametersText =
            choiceType.parameters
                |> List.map (jsonCodingFunctionNameFromTypeParameterName >> .encodeName)
                |> String.join " "

        decodeParametersText =
            choiceType.parameters
                |> List.map (jsonCodingFunctionNameFromTypeParameterName >> .decodeName)
                |> String.join " "

        moduleName =
            moduleNameFromTypeName choiceTypeName

        typeNameRepresentation =
            jsonCodingFunctionNameCommonPartFromTypeName choiceTypeName

        tagsExpressions =
            choiceType.tags
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
        ChoiceElmType choice ->
            choice

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
    | ChoiceTypeDeclaration Elm.Syntax.Type.Type


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
    -> ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Function
    -> Result String { valueFunctionText : String, updateInterfaceModuleText : { generatedModuleName : String } -> String -> Result String String }
prepareReplaceFunctionInSourceFilesModuleText sourceFiles currentModule originalFunctionDeclaration =
    let
        functionName =
            Elm.Syntax.Node.value
                (Elm.Syntax.Node.value (Elm.Syntax.Node.value originalFunctionDeclaration).declaration).name
    in
    case parseSourceFileFunction currentModule (Elm.Syntax.Node.value originalFunctionDeclaration) of
        Err error ->
            Err ("Failed to parse function: " ++ error)

        Ok ( filePathRepresentation, config ) ->
            case findFileTreeNodeWithPathMatchingRepresentationInFunctionName sourceFiles filePathRepresentation of
                Err error ->
                    Err ("Failed to identify file for '" ++ filePathRepresentation ++ "': " ++ error)

                Ok ( matchingPath, fileTreeContent ) ->
                    prepareRecordTreeEmitForTreeOrBlobUnderPath [] config.encoding
                        |> Result.andThen
                            (\prepareOk ->
                                let
                                    expressionFromFileContent : Bytes.Bytes -> Result String String
                                    expressionFromFileContent =
                                        valueModuleRecordExpressionFromEncodings prepareOk.valueModule

                                    expressionFromFileTreeNode fileTreeNode =
                                        case fileTreeNode of
                                            FileTree.BlobNode blob ->
                                                expressionFromFileContent blob
                                                    |> Result.map (\expression -> "BlobNode (" ++ expression ++ ")")

                                            FileTree.TreeNode tree ->
                                                let
                                                    buildTreeEntryExpression ( entryName, entryNode ) =
                                                        expressionFromFileTreeNode entryNode
                                                            |> Result.map
                                                                (\entryNodeExpr ->
                                                                    [ "( \"" ++ entryName ++ "\""
                                                                    , ", " ++ entryNodeExpr
                                                                    , ")"
                                                                    ]
                                                                        |> String.join "\n"
                                                                )
                                                in
                                                tree
                                                    |> List.map buildTreeEntryExpression
                                                    |> Result.Extra.combine
                                                    |> Result.map
                                                        (\entriesExpressions ->
                                                            "TreeNode\n"
                                                                ++ indentElmCodeLines 1
                                                                    ([ "["
                                                                        ++ String.join "\n," entriesExpressions
                                                                        ++ "]"
                                                                     ]
                                                                        |> String.join "\n"
                                                                    )
                                                        )

                                    expressionResult =
                                        case config.variant of
                                            SourceFile ->
                                                case fileTreeContent of
                                                    FileTree.TreeNode _ ->
                                                        Err ("This pattern matches path '" ++ String.join "/" matchingPath ++ "' but the node here is a tree, not a file")

                                                    FileTree.BlobNode fileContent ->
                                                        expressionFromFileContent fileContent
                                                            |> Result.map
                                                                (\expression ->
                                                                    ( [ "file_"
                                                                      , filePathRepresentation
                                                                      ]
                                                                    , expression
                                                                    )
                                                                )

                                            SourceFileTree ->
                                                expressionFromFileTreeNode fileTreeContent
                                                    |> Result.map
                                                        (Tuple.pair
                                                            [ "file_tree_node"
                                                            , filePathRepresentation
                                                            ]
                                                        )

                                    fileEncodingTreeExpression : String -> String
                                    fileEncodingTreeExpression sourceExpression =
                                        interfaceModuleRecordExpression
                                            prepareOk.interfaceModule
                                            { sourceExpression = sourceExpression }
                                in
                                expressionResult
                                    |> Result.map
                                        (\( fileNameComponents, expression ) ->
                                            let
                                                valueFunctionName =
                                                    String.join "_" fileNameComponents

                                                valueFunctionText =
                                                    valueFunctionName
                                                        ++ " =\n"
                                                        ++ indentElmCodeLines 1 expression
                                            in
                                            { valueFunctionText = valueFunctionText
                                            , updateInterfaceModuleText =
                                                \{ generatedModuleName } moduleText ->
                                                    let
                                                        fileExpression =
                                                            case config.variant of
                                                                SourceFile ->
                                                                    fileEncodingTreeExpression
                                                                        (generatedModuleName ++ "." ++ valueFunctionName)

                                                                SourceFileTree ->
                                                                    [ generatedModuleName ++ "." ++ valueFunctionName
                                                                    , sourceFilesInterfaceModuleAddedFunctionMapNode.functionName
                                                                    , sourceFilesInterfaceModuleAddedFunctionMapBlobs.functionName
                                                                        ++ """(\\blobValue -> """
                                                                        ++ fileEncodingTreeExpression "blobValue"
                                                                        ++ ")"
                                                                    ]
                                                                        |> String.join "\n|>"

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
                            )


sourceFilesInterfaceModuleAddedFunctions : List { functionName : String, mapFunctionLines : { generatedModuleName : String } -> Maybe (List String) -> List String }
sourceFilesInterfaceModuleAddedFunctions =
    [ sourceFilesInterfaceModuleAddedFunctionMapNode
    , sourceFilesInterfaceModuleAddedFunctionMapBlobs
    ]


sourceFilesInterfaceModuleAddedFunctionMapNode : { functionName : String, mapFunctionLines : { generatedModuleName : String } -> Maybe (List String) -> List String }
sourceFilesInterfaceModuleAddedFunctionMapNode =
    { functionName = "mapFileTreeNodeFromGenerated"
    , mapFunctionLines = \{ generatedModuleName } -> always (String.split "\n" ("""
mapFileTreeNodeFromGenerated : """ ++ generatedModuleName ++ """.FileTreeNode a -> FileTreeNode a
mapFileTreeNodeFromGenerated node =
    case node of
        """ ++ generatedModuleName ++ """.BlobNode blob ->
            BlobNode blob

        """ ++ generatedModuleName ++ """.TreeNode tree ->
            tree |> List.map (Tuple.mapSecond mapFileTreeNodeFromGenerated) |> TreeNode
"""))
    }


sourceFilesInterfaceModuleAddedFunctionMapBlobs : { functionName : String, mapFunctionLines : { generatedModuleName : String } -> Maybe (List String) -> List String }
sourceFilesInterfaceModuleAddedFunctionMapBlobs =
    { functionName = "mapBlobs"
    , mapFunctionLines = always (always (String.split "\n" """
mapBlobs : (a -> b) -> FileTreeNode a -> FileTreeNode b
mapBlobs mapBlob node =
    case node of
        TreeNode tree ->
            TreeNode (tree |> List.map (Tuple.mapSecond (mapBlobs mapBlob)))

        BlobNode blob ->
            BlobNode (mapBlob blob)
"""))
    }


prepareReplaceFunctionInElmMakeModuleText :
    List ( DependencyKey, Bytes.Bytes )
    -> AppFiles
    -> ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Expression.Function
    ->
        Result
            (List CompilationError)
            { valueFunctionsTexts : List String
            , updateInterfaceModuleText : { generatedModuleName : String } -> String -> Result String String
            }
prepareReplaceFunctionInElmMakeModuleText dependencies sourceFiles currentModule originalFunctionDeclaration =
    let
        functionName =
            Elm.Syntax.Node.value
                (Elm.Syntax.Node.value originalFunctionDeclaration.declaration).name
    in
    case parseElmMakeModuleFunction currentModule originalFunctionDeclaration of
        Err error ->
            Err [ OtherCompilationError ("Failed to parse Elm make function: " ++ error) ]

        Ok ( filePathRepresentation, elmMakeTree ) ->
            let
                continueMapResult : ElmMakeRecordTreeLeafEmit -> Result String { emitBlob : RecordTreeEmitElmMake, valueFunctionName : String }
                continueMapResult leafBeforeApplyBytes =
                    recordTreeEmitElmMake leafBeforeApplyBytes.emitBlob leafBeforeApplyBytes.blob
                        |> Result.map
                            (\emitBlob ->
                                { emitBlob = emitBlob
                                , valueFunctionName = leafBeforeApplyBytes.valueFunctionName
                                }
                            )

                mapTreeLeaf =
                    prepareElmMakeFunctionForEmit sourceFiles dependencies { filePathRepresentation = filePathRepresentation }
                        >> Result.andThen (continueMapResult >> Result.mapError OtherCompilationError)

                mappedTreeResult : Result (List CompilationError) (CompilationInterfaceRecordTreeNode { emitBlob : RecordTreeEmitElmMake, valueFunctionName : String })
                mappedTreeResult =
                    attemptMapRecordTreeLeaves
                        []
                        (always mapTreeLeaf)
                        elmMakeTree
                        |> Result.mapError (List.map Tuple.second)
            in
            mappedTreeResult
                |> Result.andThen
                    (\mappedTree ->
                        let
                            leaves : List { emitBlob : RecordTreeEmitElmMake, valueFunctionName : String }
                            leaves =
                                mappedTree
                                    |> enumerateLeavesFromRecordTree
                                    |> List.map Tuple.second

                            valueFunctions : List { functionName : String, functionText : String }
                            valueFunctions =
                                leaves
                                    |> List.map
                                        (\leaf ->
                                            { functionName = leaf.valueFunctionName
                                            , functionText =
                                                leaf.valueFunctionName
                                                    ++ " =\n"
                                                    ++ indentElmCodeLines 1 leaf.emitBlob.valueModule.expression
                                            }
                                        )

                            updateInterfaceModuleText =
                                \{ generatedModuleName } moduleText ->
                                    let
                                        fileExpression : String
                                        fileExpression =
                                            emitRecordExpressionFromRecordTree
                                                (\leaf ->
                                                    interfaceModuleRecordExpression
                                                        leaf.emitBlob.interfaceModule
                                                        { sourceExpression = generatedModuleName ++ "." ++ leaf.valueFunctionName }
                                                )
                                                mappedTree

                                        buildNewFunctionLines previousFunctionLines =
                                            List.take 2 previousFunctionLines
                                                ++ [ indentElmCodeLines 1 fileExpression ]
                                    in
                                    addOrUpdateFunctionInElmModuleText
                                        { functionName = functionName, mapFunctionLines = Maybe.withDefault [] >> buildNewFunctionLines }
                                        moduleText
                        in
                        Ok
                            { valueFunctionsTexts = List.map .functionText valueFunctions
                            , updateInterfaceModuleText = updateInterfaceModuleText
                            }
                    )


prepareElmMakeFunctionForEmit :
    AppFiles
    -> List ( DependencyKey, Bytes.Bytes )
    -> { filePathRepresentation : String }
    -> InterfaceElmMakeFunctionLeafConfig
    -> Result CompilationError ElmMakeRecordTreeLeafEmit
prepareElmMakeFunctionForEmit sourceFiles dependencies { filePathRepresentation } config =
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
                    , outputType = config.elmMakeConfig.outputType
                    , enableDebug = config.elmMakeConfig.enableDebug
                    }

                dependencyKey =
                    ElmMakeDependency elmMakeRequest

                getNameComponentsFromLeafConfig leafConfig =
                    [ if leafConfig.enableDebug then
                        [ "debug" ]

                      else
                        []
                    , case leafConfig.outputType of
                        ElmMakeOutputTypeHtml ->
                            [ "html" ]

                        ElmMakeOutputTypeJs ->
                            [ "javascript" ]
                    ]
                        |> List.concat
            in
            case dependencies |> List.filter (Tuple.first >> dependencyKeysAreEqual dependencyKey) |> List.head of
                Nothing ->
                    Err (MissingDependencyError dependencyKey)

                Just ( _, dependencyValue ) ->
                    let
                        variantName =
                            getNameComponentsFromLeafConfig config.elmMakeConfig
                                |> List.sort
                                |> String.join "_"

                        valueFunctionName =
                            [ "elm_make_output"
                            , filePathRepresentation
                            , variantName
                            ]
                                |> String.join "_"
                    in
                    Ok
                        { valueFunctionName = valueFunctionName
                        , emitBlob = config.emitBlob
                        , blob = dependencyValue
                        }


interfaceModuleRecordExpression : RecordTreeEmitInterfaceModule -> { sourceExpression : String } -> String
interfaceModuleRecordExpression interfaceModuleTree context =
    interfaceModuleTree
        |> emitRecordExpressionFromRecordTree (\leafMap -> leafMap context)


valueModuleRecordExpressionFromEncodings : RecordTreeEmitValueModule -> Bytes.Bytes -> Result String String
valueModuleRecordExpressionFromEncodings encodings blob =
    encodings blob
        |> Result.map
            (\fieldsDict ->
                "{ "
                    ++ (fieldsDict
                            |> Dict.toList
                            |> List.map
                                (\( fieldName, field ) ->
                                    fieldName ++ " = " ++ field.expression
                                )
                            |> String.join "\n, "
                       )
                    ++ " }"
            )


recordTreeEmitElmMake : RecordTreeEmit -> Bytes.Bytes -> Result String RecordTreeEmitElmMake
recordTreeEmitElmMake recordTree bytes =
    valueModuleRecordExpressionFromEncodings recordTree.valueModule bytes
        |> Result.map
            (\expression ->
                { interfaceModule = recordTree.interfaceModule
                , valueModule = { expression = expression }
                }
            )


buildBase64ElmExpression : Bytes.Bytes -> Result String String
buildBase64ElmExpression bytes =
    case Base64.fromBytes bytes of
        Nothing ->
            Err "Error encoding to base64"

        Just asBase64 ->
            Ok (stringExpressionFromString asBase64)


buildUtf8ElmExpression : Bytes.Bytes -> Result String String
buildUtf8ElmExpression bytes =
    case Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes of
        Nothing ->
            Err "Failed to decode bytes as UTF8"

        Just asUtf8 ->
            Ok (stringExpressionFromString asUtf8)


buildGZipBase64ElmExpression : Bytes.Bytes -> Result String String
buildGZipBase64ElmExpression bytes =
    buildBase64ElmExpression (Flate.deflateGZip bytes)


buildUint32Uint8ElmExpression : Bytes.Bytes -> Result String String
buildUint32Uint8ElmExpression bytes =
    let
        uint32_count =
            Bytes.width bytes // 4

        uint8_offset =
            uint32_count * 4

        uint8_count =
            Bytes.width bytes - uint8_offset

        uint32_decoder =
            if uint32_count == 0 then
                Bytes.Decode.succeed []

            else
                bytes_decode_list uint32_count (Bytes.Decode.unsignedInt32 Bytes.BE)

        uint8_decoder =
            if uint8_count == 0 then
                Bytes.Decode.succeed []

            else
                bytes_decode_withOffset uint8_offset (bytes_decode_list uint8_count (Bytes.Decode.unsignedInt32 Bytes.BE))

        expressionFromListInt list =
            "[ " ++ String.join ", " (List.map String.fromInt list) ++ " ]"

        recordExpression fields =
            "{ "
                ++ (fields
                        |> List.map (\( fieldName, fieldValueExpression ) -> fieldName ++ " = " ++ fieldValueExpression)
                        |> String.join ", "
                   )
                ++ " }"

        decoder =
            uint32_decoder
                |> Bytes.Decode.andThen
                    (\uint32 ->
                        uint8_decoder |> Bytes.Decode.map (\uint8 -> { uint32 = uint32, uint8 = uint8 })
                    )
    in
    case Bytes.Decode.decode decoder bytes of
        Nothing ->
            Err "Failed decoding bytes to integers"

        Just { uint32, uint8 } ->
            Ok
                (recordExpression
                    [ ( "uint32", expressionFromListInt uint32 )
                    , ( "uint8", expressionFromListInt uint8 )
                    ]
                )


bytes_decode_list : Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder (List a)
bytes_decode_list length aDecoder =
    if length == 0 then
        Bytes.Decode.succeed []

    else
        Bytes.Decode.loop ( length, [] ) (bytes_decode_listStep aDecoder)


bytes_decode_listStep : Bytes.Decode.Decoder a -> ( Int, List a ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List a ) (List a))
bytes_decode_listStep elementDecoder ( n, elements ) =
    if n <= 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse elements))

    else
        Bytes.Decode.map (\element -> Bytes.Decode.Loop ( n - 1, element :: elements )) elementDecoder


bytes_decode_withOffset : Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder a
bytes_decode_withOffset offset decoder =
    Bytes.Decode.bytes (max 0 offset) |> Bytes.Decode.andThen (\_ -> decoder)


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
    findFileTreeNodeWithPathMatchingRepresentationInFunctionName sourceFiles pathPattern
        |> Result.andThen
            (\( matchPath, matchNode ) ->
                case matchNode of
                    FileTree.BlobNode blob ->
                        Ok ( matchPath, blob )

                    FileTree.TreeNode _ ->
                        Err ("This pattern matches path '" ++ pathPattern ++ "' but the node here is a tree, not a file")
            )


findFileTreeNodeWithPathMatchingRepresentationInFunctionName : AppFiles -> String -> Result String ( List String, FileTree.FileTreeNode Bytes.Bytes )
findFileTreeNodeWithPathMatchingRepresentationInFunctionName sourceFiles pathPattern =
    let
        fileTree =
            sourceFiles
                |> Dict.toList
                |> List.map (Tuple.mapSecond FileTree.BlobNode)
                |> List.foldl FileTree.setNodeAtPathInSortedFileTree (FileTree.TreeNode [])

        nodesWithRepresentations =
            fileTree
                |> FileTree.listNodesWithPath
                |> List.map (\( nodePath, node ) -> ( filePathRepresentationInFunctionName nodePath, ( nodePath, node ) ))

        nodesGroupedByRepresentation : Dict.Dict String (List ( List String, FileTree.FileTreeNode Bytes.Bytes ))
        nodesGroupedByRepresentation =
            nodesWithRepresentations
                |> List.map Tuple.first
                |> Set.fromList
                |> Set.toList
                |> List.map
                    (\representation ->
                        ( representation
                        , nodesWithRepresentations |> List.filter (Tuple.first >> (==) representation) |> List.map Tuple.second
                        )
                    )
                |> Dict.fromList
    in
    case Maybe.withDefault [] (Dict.get pathPattern nodesGroupedByRepresentation) of
        [ matchingNode ] ->
            Ok matchingNode

        [] ->
            let
                filesWithSimilarity =
                    nodesGroupedByRepresentation
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
                    Dict.keys nodesGroupedByRepresentation

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
            [ [ "Did not find any source file node with a path matching the representation '"
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
                    ++ " of the source files nodes: "
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


addImportsInElmModuleText : List ( List String, Maybe String ) -> String -> Result String String
addImportsInElmModuleText imports moduleText =
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
                        imports |> List.map importSyntaxTextFromModuleNameAndAlias
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


parseSourceFileFunction :
    ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Expression.Function
    -> Result String ( String, InterfaceSourceFilesFunctionConfig )
parseSourceFileFunction currentModule functionDeclaration =
    case parseSourceFileFunctionEncodingFromDeclaration currentModule functionDeclaration of
        Err error ->
            Err ("Failed to parse encoding: " ++ error)

        Ok encodingFromDeclaration ->
            let
                functionName =
                    Elm.Syntax.Node.value
                        (Elm.Syntax.Node.value functionDeclaration.declaration).name
            in
            parseSourceFileFunctionName functionName
                |> Result.map
                    (Tuple.mapSecond
                        (\variant ->
                            { encoding = encodingFromDeclaration.encoding
                            , variant = variant
                            }
                        )
                    )


parseSourceFileFunctionName : String -> Result String ( String, InterfaceSourceFilesFunctionVariant )
parseSourceFileFunctionName functionName =
    parseFlagsAndPathPatternFromFunctionName
        ([ ( sourceFileFunctionNameStart, SourceFile )
         , ( sourceFileTreeFunctionNameStart, SourceFileTree )
         ]
            |> Dict.fromList
        )
        functionName
        |> Result.andThen
            (\( variant, flags, filePathRepresentation ) ->
                if flags /= [] then
                    Err "Flags are not supported in SourceFiles declarations"

                else
                    Ok ( filePathRepresentation, variant )
            )


encodingFromSourceFileFieldName : Dict.Dict String InterfaceBlobSingleEncoding
encodingFromSourceFileFieldName =
    [ ( "base64", Base64Encoding )
    , ( "utf8", Utf8Encoding )
    , ( "bytes", BytesEncoding )
    , ( "gzip", GZipEncoding )
    ]
        |> Dict.fromList


parseElmMakeModuleFunction :
    ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Expression.Function
    -> Result String ( String, InterfaceElmMakeFunctionConfig )
parseElmMakeModuleFunction currentModule functionDeclaration =
    let
        functionName =
            Elm.Syntax.Node.value
                (Elm.Syntax.Node.value functionDeclaration.declaration).name
    in
    parseElmMakeModuleFunctionName functionName
        |> Result.andThen
            (\referencedName ->
                (case functionDeclaration.signature of
                    Nothing ->
                        Err "Missing function signature"

                    Just signature ->
                        parseElmMakeFunctionConfigFromTypeAnnotation
                            currentModule
                            (Elm.Syntax.Node.value signature).typeAnnotation
                            |> Result.mapError ((++) "Failed to parse config: ")
                )
                    |> Result.map (Tuple.pair referencedName)
            )


parseElmMakeModuleFunctionName : String -> Result String String
parseElmMakeModuleFunctionName functionName =
    parseFlagsAndPathPatternFromFunctionName
        (Dict.fromList [ ( elmMakeFunctionNameStart, () ) ])
        functionName
        |> Result.andThen
            (\( _, flags, filePathRepresentation ) ->
                if flags /= [] then
                    Err "Flags are not supported in ElmMake declarations"

                else
                    Ok filePathRepresentation
            )


parseSourceFileFunctionEncodingFromDeclaration :
    ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Expression.Function
    -> Result String { isTree : Bool, encoding : CompilationInterfaceRecordTreeNode InterfaceBlobSingleEncoding }
parseSourceFileFunctionEncodingFromDeclaration currentModule functionDeclaration =
    case functionDeclaration.signature of
        Nothing ->
            Err "Missing signature"

        Just signature ->
            parseSourceFileFunctionEncodingFromElmTypeAnnotation currentModule (Elm.Syntax.Node.value signature).typeAnnotation


parseElmMakeFunctionConfigFromTypeAnnotation :
    ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String InterfaceElmMakeFunctionConfig
parseElmMakeFunctionConfigFromTypeAnnotation currentModule typeAnnotationNode =
    case parseElmTypeAndDependenciesRecursivelyFromAnnotation Dict.empty ( currentModule, typeAnnotationNode ) of
        Err (LocatedInSourceFiles _ error) ->
            Err ("Failed to parse type annotation: " ++ error)

        Ok ( typeAnnotation, _ ) ->
            parseInterfaceRecordTree
                identity
                (always Ok)
                typeAnnotation
                ()
                |> Result.mapError (\( path, error ) -> "Failed at path " ++ String.join "." path ++ ": " ++ error)
                |> Result.andThen parseElmMakeFunctionConfigFromRecordTree


parseElmMakeFunctionConfigFromRecordTree : CompilationInterfaceRecordTreeNode a -> Result String InterfaceElmMakeFunctionConfig
parseElmMakeFunctionConfigFromRecordTree =
    parseElmMakeFunctionConfigFromRecordTreeInternal { enableDebug = False, outputType = ElmMakeOutputTypeHtml }


parseElmMakeFunctionConfigFromRecordTreeInternal :
    InterfaceElmMakeConfig
    -> CompilationInterfaceRecordTreeNode a
    -> Result String InterfaceElmMakeFunctionConfig
parseElmMakeFunctionConfigFromRecordTreeInternal elmMakeConfig recordTree =
    let
        leafFromTree pathPrefix =
            prepareRecordTreeEmitForTreeOrBlobUnderPath pathPrefix
                >> Result.map (\emitBlob -> RecordTreeLeaf { elmMakeConfig = elmMakeConfig, emitBlob = emitBlob })
    in
    case recordTree of
        RecordTreeLeaf leaf ->
            leafFromTree [] (RecordTreeLeaf ())

        RecordTreeBranch branch ->
            branch
                |> List.map
                    (\( branchName, branchValue ) ->
                        (case integrateElmMakeFunctionRecordFieldName branchName elmMakeConfig of
                            Err _ ->
                                leafFromTree [ branchName ] branchValue

                            Ok newElmMakeConfig ->
                                parseElmMakeFunctionConfigFromRecordTreeInternal newElmMakeConfig branchValue
                        )
                            |> Result.map (Tuple.pair branchName)
                    )
                |> Result.Extra.combine
                |> Result.map RecordTreeBranch


prepareRecordTreeEmitForTreeOrBlobUnderPath : List String -> CompilationInterfaceRecordTreeNode a -> Result String RecordTreeEmit
prepareRecordTreeEmitForTreeOrBlobUnderPath pathPrefix tree =
    let
        mappingBase64 =
            { fieldName = "base64"
            , valueModuleBuildExpression = buildBase64ElmExpression
            }

        mappingUtf8 =
            { fieldName = "utf8"
            , valueModuleBuildExpression = buildUtf8ElmExpression
            }

        mappingGZipBase64 =
            { fieldName = "gzipBase64"
            , valueModuleBuildExpression = buildGZipBase64ElmExpression
            }

        mappingUint32Uint8 =
            { fieldName = "uint32_uint8"
            , valueModuleBuildExpression = buildUint32Uint8ElmExpression
            }

        fromUint32Uint8ToBytes =
            [ "|> EncodeBytes.bytes_encoder_from_uint32_uint8"
            , "|> Bytes.Encode.encode"
            ]
                |> String.join " "

        mappingBytes =
            ( mappingUint32Uint8, Just fromUint32Uint8ToBytes )

        mapToValueDict : Dict.Dict (List String) ( { fieldName : String, valueModuleBuildExpression : Bytes.Bytes -> Result String String }, Maybe String )
        mapToValueDict =
            [ ( [ "base64" ], ( mappingBase64, Nothing ) )
            , ( [ "bytes" ], mappingBytes )
            , ( [], mappingBytes )
            , ( [ "utf8" ], ( mappingUtf8, Nothing ) )
            , ( [ "gzip", "base64" ], ( mappingGZipBase64, Nothing ) )
            ]
                |> Dict.fromList

        attemptMapLeaf : List String -> a -> Result String RecordTreeEmitBlobIntermediateResult
        attemptMapLeaf leafPath _ =
            let
                path =
                    pathPrefix ++ leafPath
            in
            case Dict.get path mapToValueDict of
                Nothing ->
                    Err ("Found no mapping for path " ++ String.join "." path)

                Just ( valueModule, maybeMappingInInterfaceModule ) ->
                    Ok
                        { interfaceModule =
                            \{ sourceExpression } ->
                                let
                                    beforeMapping =
                                        sourceExpression ++ "." ++ valueModule.fieldName
                                in
                                case maybeMappingInInterfaceModule of
                                    Nothing ->
                                        beforeMapping

                                    Just mappingInInterfaceModule ->
                                        "(" ++ beforeMapping ++ ") " ++ mappingInInterfaceModule
                        , valueModule =
                            { fieldName = valueModule.fieldName
                            , buildExpression = valueModule.valueModuleBuildExpression
                            }
                        }
    in
    tree
        |> attemptMapRecordTreeLeaves [] attemptMapLeaf
        |> Result.mapError
            (List.map (\( errorPath, error ) -> "Error at path " ++ String.join "." errorPath ++ ": " ++ error) >> String.join ", ")
        |> Result.map
            (\mappedTree ->
                { interfaceModule = mappedTree |> mapRecordTreeLeaves .interfaceModule
                , valueModule =
                    \valueBytes ->
                        mappedTree
                            |> enumerateLeavesFromRecordTree
                            |> List.map
                                (\( _, leaf ) ->
                                    leaf.valueModule.buildExpression valueBytes
                                        |> Result.mapError ((++) ("Failed to build expression for field '" ++ leaf.valueModule.fieldName ++ "': "))
                                        |> Result.map
                                            (\expression -> ( leaf.valueModule.fieldName, { expression = expression } ))
                                )
                            |> Result.Extra.combine
                            |> Result.map Dict.fromList
                }
            )


integrateElmMakeFunctionRecordFieldName : String -> InterfaceElmMakeConfig -> Result String InterfaceElmMakeConfig
integrateElmMakeFunctionRecordFieldName fieldName configBefore =
    case fieldName of
        "debug" ->
            Ok { configBefore | enableDebug = True }

        "html" ->
            Ok { configBefore | outputType = ElmMakeOutputTypeHtml }

        "javascript" ->
            Ok { configBefore | outputType = ElmMakeOutputTypeJs }

        _ ->
            Err ("Unsupported field name: " ++ fieldName)


parseSourceFileFunctionEncodingFromElmTypeAnnotation :
    ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String { isTree : Bool, encoding : CompilationInterfaceRecordTreeNode InterfaceBlobSingleEncoding }
parseSourceFileFunctionEncodingFromElmTypeAnnotation currentModule typeAnnotationNode =
    case
        parseElmTypeAndDependenciesRecursivelyFromAnnotation
            Dict.empty
            ( currentModule, typeAnnotationNode )
    of
        Err (LocatedInSourceFiles _ error) ->
            Err ("Failed to parse type annotation: " ++ error)

        Ok ( typeAnnotation, _ ) ->
            parseSourceFileFunctionFromTypeAnnotation typeAnnotation


parseSourceFileFunctionFromTypeAnnotation :
    ElmTypeAnnotation
    -> Result String { isTree : Bool, encoding : CompilationInterfaceRecordTreeNode InterfaceBlobSingleEncoding }
parseSourceFileFunctionFromTypeAnnotation typeAnnotation =
    case typeAnnotation of
        RecordElmType _ ->
            typeAnnotation
                |> parseSourceFileFunctionEncodingFromTypeAnnotation
                |> Result.map (\encoding -> { isTree = False, encoding = encoding })

        InstanceElmType instance ->
            let
                continueWithErrorUnexpectedInst detail =
                    Err ("Unexpected shape of instantiation: " ++ detail)
            in
            case instance.instantiated of
                ChoiceElmType instantiatedTypeName ->
                    if not (String.endsWith ".FileTreeNode" instantiatedTypeName) then
                        continueWithErrorUnexpectedInst ("instatiatedTypeName: " ++ instantiatedTypeName)

                    else
                        case instance.arguments of
                            [ singleArgument ] ->
                                singleArgument
                                    |> parseSourceFileFunctionEncodingFromTypeAnnotation
                                    |> Result.mapError ((++) "Failed to parse argument: ")
                                    |> Result.map (\encoding -> { isTree = True, encoding = encoding })

                            _ ->
                                continueWithErrorUnexpectedInst ("Unexpected number of arguments: " ++ String.fromInt (List.length instance.arguments))

                _ ->
                    continueWithErrorUnexpectedInst "Instantiated is not a choice type"

        _ ->
            Ok { isTree = False, encoding = RecordTreeLeaf BytesEncoding }


parseSourceFileFunctionEncodingFromTypeAnnotation :
    ElmTypeAnnotation
    -> Result String (CompilationInterfaceRecordTreeNode InterfaceBlobSingleEncoding)
parseSourceFileFunctionEncodingFromTypeAnnotation typeAnnotation =
    parseInterfaceRecordTree
        identity
        integrateSourceFilesFunctionRecordFieldName
        typeAnnotation
        BytesEncoding
        |> Result.mapError (\( path, error ) -> "Failed at path " ++ String.join "." path ++ ": " ++ error)


integrateSourceFilesFunctionRecordFieldName :
    String
    -> InterfaceBlobSingleEncoding
    -> Result String InterfaceBlobSingleEncoding
integrateSourceFilesFunctionRecordFieldName fieldName configBefore =
    -- TODO: Aggregate different encodings
    case Dict.get fieldName encodingFromSourceFileFieldName of
        Just encoding ->
            Ok encoding

        Nothing ->
            Err ("Unsupported field name: " ++ fieldName)


parseFlagsAndPathPatternFromFunctionName : Dict.Dict String prefix -> String -> Result String ( prefix, List String, String )
parseFlagsAndPathPatternFromFunctionName prefixes functionName =
    case String.split functionNameFlagsSeparator functionName of
        [] ->
            Err "String.split returned empty list"

        [ _ ] ->
            Err "Did not find separator between flags and path"

        prefixAndFlags :: rest ->
            let
                path =
                    String.join functionNameFlagsSeparator rest

                continueWithPrefixStringAndFlags prefixString flags =
                    case Dict.get prefixString prefixes of
                        Nothing ->
                            Err ("Unknown prefix: '" ++ prefixString ++ "'")

                        Just prefix ->
                            Ok
                                ( prefix
                                , flags
                                , path
                                )
            in
            case String.split "__" prefixAndFlags of
                [] ->
                    Err "String.split returned empty list"

                [ prefix ] ->
                    continueWithPrefixStringAndFlags prefix []

                prefix :: flags ->
                    continueWithPrefixStringAndFlags prefix flags


emitRecordExpressionFromRecordTree : (a -> String) -> CompilationInterfaceRecordTreeNode a -> String
emitRecordExpressionFromRecordTree expressionFromLeafValue tree =
    case tree of
        RecordTreeLeaf leaf ->
            expressionFromLeafValue leaf

        RecordTreeBranch fields ->
            let
                fieldsExpressions =
                    fields
                        |> List.map
                            (\( fieldName, encodingExpression ) ->
                                fieldName
                                    ++ " = "
                                    ++ emitRecordExpressionFromRecordTree
                                        expressionFromLeafValue
                                        encodingExpression
                            )
            in
            "{ " ++ String.join "\n, " fieldsExpressions ++ "\n}"


attemptMapRecordTreeLeaves : List String -> (List String -> a -> Result e b) -> CompilationInterfaceRecordTreeNode a -> Result (List ( List String, e )) (CompilationInterfaceRecordTreeNode b)
attemptMapRecordTreeLeaves pathPrefix attemptMapLeaf tree =
    case tree of
        RecordTreeLeaf leaf ->
            attemptMapLeaf pathPrefix leaf
                |> Result.mapError (Tuple.pair [] >> List.singleton)
                |> Result.map RecordTreeLeaf

        RecordTreeBranch fields ->
            let
                ( successes, errors ) =
                    fields
                        |> List.map
                            (\( fieldName, fieldNode ) ->
                                attemptMapRecordTreeLeaves (pathPrefix ++ [ fieldName ]) attemptMapLeaf fieldNode
                                    |> Result.map (Tuple.pair fieldName)
                                    |> Result.mapError (List.map (Tuple.mapFirst ((::) fieldName)))
                            )
                        |> Result.Extra.partition
                        |> Tuple.mapSecond List.concat
            in
            if errors == [] then
                Ok (RecordTreeBranch successes)

            else
                Err errors


mapRecordTreeLeaves : (a -> b) -> CompilationInterfaceRecordTreeNode a -> CompilationInterfaceRecordTreeNode b
mapRecordTreeLeaves mapLeaf tree =
    case tree of
        RecordTreeLeaf leaf ->
            RecordTreeLeaf (mapLeaf leaf)

        RecordTreeBranch fields ->
            fields
                |> List.map
                    (\( fieldName, fieldNode ) ->
                        mapRecordTreeLeaves mapLeaf fieldNode
                            |> Tuple.pair fieldName
                    )
                |> RecordTreeBranch


parseInterfaceRecordTree : (String -> e) -> (String -> leaf -> Result e leaf) -> ElmTypeAnnotation -> leaf -> Result ( List String, e ) (CompilationInterfaceRecordTreeNode leaf)
parseInterfaceRecordTree errorFromString integrateFieldName typeAnnotation seed =
    let
        errorUnsupportedType typeText =
            Err ( [], errorFromString ("Unsupported type: " ++ typeText) )
    in
    case typeAnnotation of
        RecordElmType record ->
            record.fields
                |> List.map
                    (\( fieldName, fieldType ) ->
                        integrateFieldName fieldName seed
                            |> Result.mapError (Tuple.pair [ fieldName ])
                            |> Result.andThen
                                (\withFieldNameIntegrated ->
                                    withFieldNameIntegrated
                                        |> parseInterfaceRecordTree errorFromString integrateFieldName fieldType
                                        |> Result.map (Tuple.pair fieldName)
                                        |> Result.mapError (Tuple.mapFirst ((::) fieldName))
                                )
                    )
                |> Result.Extra.combine
                |> Result.map RecordTreeBranch

        ChoiceElmType _ ->
            errorUnsupportedType "choice"

        InstanceElmType _ ->
            errorUnsupportedType "instance"

        TupleElmType _ ->
            errorUnsupportedType "tuple"

        LeafElmType _ ->
            Ok (RecordTreeLeaf seed)

        GenericType _ ->
            errorUnsupportedType "generic"

        UnitType ->
            Ok (RecordTreeBranch [])


enumerateLeavesFromRecordTree : CompilationInterfaceRecordTreeNode a -> List ( List String, a )
enumerateLeavesFromRecordTree node =
    case node of
        RecordTreeLeaf leaf ->
            [ ( [], leaf ) ]

        RecordTreeBranch children ->
            children
                |> List.map
                    (\( fieldName, child ) ->
                        child |> enumerateLeavesFromRecordTree |> List.map (Tuple.mapFirst ((::) fieldName))
                    )
                |> List.concat


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


importSyntaxTextFromModuleNameAndAlias : ( List String, Maybe String ) -> String
importSyntaxTextFromModuleNameAndAlias ( moduleName, maybeAlias ) =
    "import "
        ++ String.join "." moduleName
        ++ (maybeAlias |> Maybe.map ((++) " as ") |> Maybe.withDefault "")


modulesToAddForBytesCoding : List String
modulesToAddForBytesCoding =
    [ String.trimLeft """
module CompilerGenerated.EncodeBytes exposing (..)

import Bytes
import Bytes.Encode


bytes_encoder_from_uint32_uint8 : { uint32 : List Int, uint8 : List Int } -> Bytes.Encode.Encoder
bytes_encoder_from_uint32_uint8 { uint32, uint8 } =
    [ uint32 |> List.map (Bytes.Encode.unsignedInt32 Bytes.BE)
    , uint8 |> List.map Bytes.Encode.unsignedInt8
    ]
        |> List.concat
        |> Bytes.Encode.sequence

"""
    ]


modulesToAddForBase64Coding : List String
modulesToAddForBase64Coding =
    [ -- https://github.com/danfishgold/base64-bytes/blob/ee966331d3819f56244145ed485ab13b0dc4f45a/src/Base64.elm
      String.trimLeft
        """
module CompilerGenerated.Base64 exposing
    ( fromBytes, fromString, toBytes, toString
    , encoder, decoder
    )

{-| This package can convert
[bytes](https://package.elm-lang.org/packages/elm/bytes/latest/)
to Base64 strings and vice versa.


# Conversion

@docs fromBytes, fromString, toBytes, toString


# Bytes Encoder and Decoder

Slightly lower level functions.

[`fromBytes`](#fromBytes) and [`toBytes`](#toBytes) functions
are pretty much wrappers around these functions.

@docs encoder, decoder

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import CompilerGenerated.Decode as Decode
import CompilerGenerated.Encode as Encode


{-| Convert bytes to a Base64 string.
If you want more control over the process, you should use [`decoder`](#decoder).

This function should never return `Nothing`, but it uses
[`Bytes.Decode.decode`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes-Decode#decode),
which returns a `Maybe String`.

-}
fromBytes : Bytes -> Maybe String
fromBytes =
    Decode.fromBytes


{-| Encode a string into a Base64 string.
This function is a wrapper around [`fromBytes`](#fromBytes).

Similarly, it should never return `Nothing`, but alas, [`Bytes.Decode.decode`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes-Decode#decode),
which [`fromBytes`](#fromBytes) uses, returns a `Maybe String`.

-}
fromString : String -> Maybe String
fromString string =
    string
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> fromBytes


{-| Convert a Base64 string to bytes.
If you want more control over the process, you should use [`encoder`](#encoder).

This function fails (returns `Nothing`) if you give it an invalid Base64 sequence.

-}
toBytes : String -> Maybe Bytes
toBytes =
    Encode.toBytes


{-| Decode a Base64 string into a string.
This function is a wrapper around [`toBytes`](#toBytes).

It will fail (return `Nothing`) if you give it an invalid Base64 sequence.

-}
toString : String -> Maybe String
toString b64String =
    case toBytes b64String of
        Nothing ->
            Nothing

        Just b64Bytes ->
            Bytes.Decode.decode
                (Bytes.Decode.string (Bytes.width b64Bytes))
                b64Bytes


{-| `decoder width` is a bytes decoder that will convert `width` bytes into a
Base64 string.

It's used in [`fromBytes`](#fromBytes):

    fromBytes : Bytes -> Maybe String
    fromBytes bytes =
        Bytes.Decode.decode (decoder (Bytes.width bytes)) bytes

-}
decoder : Int -> Bytes.Decode.Decoder String
decoder =
    Decode.decoder


{-| `encoder` returns a bytes encoder. It fails if the string that is passed
to it is not a valid Base64 sequence.

It's used in [`toBytes`](#toBytes):

    toBytes : String -> Maybe Bytes
    toBytes string =
        Maybe.map Bytes.Encode.encode (encoder string)

-}
encoder : String -> Maybe Bytes.Encode.Encoder
encoder =
    Encode.encoder
"""
    , String.trimLeft """
module CompilerGenerated.Encode exposing (encoder, toBytes)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)


toBytes : String -> Maybe Bytes
toBytes string =
    Maybe.map Encode.encode (encoder string)


encoder : String -> Maybe Encode.Encoder
encoder string =
    encodeChunks string []
        |> Maybe.map (List.reverse >> Encode.sequence)


{-| Big picture:

  - read 4 base64 characters
  - convert them to 3 bytes (24 bits)
  - encode these bytes

-}
encodeChunks : String -> List Encoder -> Maybe (List Encoder)
encodeChunks input accum =
    {- Performance Note

       slice and toList is just as fast as (possibly a little faster than) repeated `String.uncons`,
       but this code is much more readable
    -}
    case String.toList (String.left 4 input) of
        [] ->
            Just accum

        [ a, b, c, d ] ->
            case encodeCharacters a b c d of
                Just enc ->
                    encodeChunks (String.dropLeft 4 input) (enc :: accum)

                Nothing ->
                    Nothing

        [ a, b, c ] ->
            case encodeCharacters a b c '=' of
                Nothing ->
                    Nothing

                Just enc ->
                    Just (enc :: accum)

        [ a, b ] ->
            case encodeCharacters a b '=' '=' of
                Nothing ->
                    Nothing

                Just enc ->
                    Just (enc :: accum)

        _ ->
            Nothing


{-| Convert 4 characters to 24 bits (as an Encoder)
-}
encodeCharacters : Char -> Char -> Char -> Char -> Maybe Encoder
encodeCharacters a b c d =
    {- Performance notes

       We use bitshifts and other bitwise operators here. They are much faster than the alternatives.
       This may not normally matter but this function is called a lot so even small increases
       in efficiency are noticable

       Secondly, we combine two `uint8` into one `uint16 BE`. This has no direct speed benefit
       (elm/bytes uses a DataView, which only natively supports adding/reading uint8)
       but having fewer list items decreases # of encoding steps and allocation,
       and is therefore faster.
    -}
    if isValidChar a && isValidChar b then
        let
            n1 =
                unsafeConvertChar a

            n2 =
                unsafeConvertChar b
        in
        -- `=` is the padding character, and must be special-cased
        -- only the `c` and `d` char are allowed to be padding
        case d of
            '=' ->
                case c of
                    '=' ->
                        let
                            n =
                                Bitwise.or
                                    (Bitwise.shiftLeftBy 18 n1)
                                    (Bitwise.shiftLeftBy 12 n2)

                            b1 =
                                -- masking higher bits is not needed, Encode.unsignedInt8 ignores higher bits
                                Bitwise.shiftRightBy 16 n
                        in
                        Just (Encode.unsignedInt8 b1)

                    _ ->
                        if isValidChar c then
                            let
                                n3 =
                                    unsafeConvertChar c

                                n =
                                    Bitwise.or
                                        (Bitwise.or (Bitwise.shiftLeftBy 18 n1) (Bitwise.shiftLeftBy 12 n2))
                                        (Bitwise.shiftLeftBy 6 n3)

                                combined =
                                    Bitwise.shiftRightBy 8 n
                            in
                            Just (Encode.unsignedInt16 BE combined)

                        else
                            Nothing

            _ ->
                if isValidChar c && isValidChar d then
                    let
                        n3 =
                            unsafeConvertChar c

                        n4 =
                            unsafeConvertChar d

                        n =
                            Bitwise.or
                                (Bitwise.or (Bitwise.shiftLeftBy 18 n1) (Bitwise.shiftLeftBy 12 n2))
                                (Bitwise.or (Bitwise.shiftLeftBy 6 n3) n4)

                        b3 =
                            -- Masking the higher bits is not needed: Encode.unsignedInt8 ignores higher bits
                            n

                        combined =
                            Bitwise.shiftRightBy 8 n
                    in
                    Just
                        (Encode.sequence
                            [ Encode.unsignedInt16 BE combined
                            , Encode.unsignedInt8 b3
                            ]
                        )

                else
                    Nothing

    else
        Nothing


{-| is the character a base64 digit?

The base16 digits are: A-Z, a-z, 0-1, '+' and '/'

-}
isValidChar : Char -> Bool
isValidChar c =
    if Char.isAlphaNum c then
        True

    else
        case c of
            '+' ->
                True

            '/' ->
                True

            _ ->
                False


{-| Convert a base64 character/digit to its index

See also [Wikipedia](https://en.wikipedia.org/wiki/Base64#Base64_table)

-}
unsafeConvertChar : Char -> Int
unsafeConvertChar char =
    {- Performance Note

       Working with the key directly is faster than using e.g. `Char.isAlpha` and `Char.isUpper`
    -}
    let
        key =
            Char.toCode char
    in
    if key >= 65 && key <= 90 then
        -- A-Z
        key - 65

    else if key >= 97 && key <= 122 then
        -- a-z
        (key - 97) + 26

    else if key >= 48 && key <= 57 then
        -- 0-9
        (key - 48) + 26 + 26

    else
        case char of
            '+' ->
                62

            '/' ->
                63

            _ ->
                -1
"""
    , String.trimLeft """
module CompilerGenerated.Decode exposing (decoder, fromBytes)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode


fromBytes : Bytes -> Maybe String
fromBytes bytes =
    Decode.decode (decoder (Bytes.width bytes)) bytes


decoder : Int -> Decode.Decoder String
decoder width =
    Decode.loop { remaining = width, string = "" } loopHelp



-- INTERNALS


{-| Base64 uses 6 bits per digit (because 2^6 == 64)
and can nicely store 4 digits in 24 bits, which are 3 bytes.

The decoding process is thus roughly:

  - read a 3-byte chunk
  - extract the 4 6-bit segments
  - convert those segments into characters

But the input does not need to have a multiple of 4 characters,
so at the end of the string some characters can be omitted.
This means there may be 2 or 1 bytes remaining at the end. We have to cover those cases!

-}
loopHelp : { remaining : Int, string : String } -> Decode.Decoder (Decode.Step { remaining : Int, string : String } String)
loopHelp { remaining, string } =
    if remaining >= 18 then
        -- Note: this case is heavily optimized.
        -- For the general idea of what this function does, the `remaining >= 3` case is more illustrative.
        decode18Bytes
            |> Decode.map
                (\\result ->
                    Decode.Loop
                        { remaining = remaining - 18
                        , string = string ++ result
                        }
                )

    else if remaining >= 3 then
        let
            helper a b c =
                let
                    combined =
                        Bitwise.or (Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.shiftLeftBy 8 b)) c
                in
                Decode.Loop
                    { remaining = remaining - 3
                    , string = string ++ bitsToChars combined 0
                    }
        in
        Decode.map3 helper
            Decode.unsignedInt8
            Decode.unsignedInt8
            Decode.unsignedInt8

    else if remaining == 0 then
        Decode.succeed (Decode.Done string)

    else if remaining == 2 then
        let
            helper a b =
                let
                    combined =
                        Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.shiftLeftBy 8 b)
                in
                Decode.Done (string ++ bitsToChars combined 1)
        in
        Decode.map2 helper
            Decode.unsignedInt8
            Decode.unsignedInt8

    else
        -- remaining == 1
        Decode.map (\\a -> Decode.Done (string ++ bitsToChars (Bitwise.shiftLeftBy 16 a) 2))
            Decode.unsignedInt8


{-| Mask that can be used to get the lowest 6 bits of a binary number
-}
lowest6BitsMask : Int
lowest6BitsMask =
    63


{-| Turn the decoded bits (at most 24, can be fewer because of padding) into 4 base64 characters.

(- - - - - - - -)(- - - - - - - -)(- - - - - - - -)
(- - - - - -|- - - - - -|- - - - - -|- - - - - -)

-}
bitsToChars : Int -> Int -> String
bitsToChars bits missing =
    {- Performance Notes

       `String.cons` proved to be the fastest way of combining characters into a string
       see also https://github.com/danfishgold/base64-bytes/pull/3#discussion_r342321940

       The input is 24 bits, which we have to partition into 4 6-bit segments. We achieve this by
       shifting to the right by (a multiple of) 6 to remove unwanted bits on the right, then `Bitwise.and`
       with `0b111111` (which is 2^6 - 1 or 63) (so, 6 1s) to remove unwanted bits on the left.

    -}
    let
        -- any 6-bit number is a valid base64 digit, so this is actually safe
        p =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits)

        q =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits) lowest6BitsMask)

        r =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits) lowest6BitsMask)

        s =
            unsafeToChar (Bitwise.and bits lowest6BitsMask)
    in
    case missing of
        -- case `0` is the most common, so put it first.
        0 ->
            String.cons p (String.cons q (String.cons r (String.fromChar s)))

        1 ->
            String.cons p (String.cons q (String.cons r "="))

        2 ->
            String.cons p (String.cons q "==")

        _ ->
            ""


{-| Base64 index to character/digit
-}
unsafeToChar : Int -> Char
unsafeToChar n =
    if n <= 25 then
        -- uppercase characters
        Char.fromCode (65 + n)

    else if n <= 51 then
        -- lowercase characters
        Char.fromCode (97 + (n - 26))

    else if n <= 61 then
        -- digit characters
        Char.fromCode (48 + (n - 52))

    else
        -- special cases
        case n of
            62 ->
                '+'

            63 ->
                '/'

            _ ->
                '\\u{0000}'



-- OPTIMIZED VERSION


u32BE : Decode.Decoder Int
u32BE =
    Decode.unsignedInt32 Bytes.BE


u16BE : Decode.Decoder Int
u16BE =
    Decode.unsignedInt16 Bytes.BE


{-| A specialized version reading 18 bytes at once
To get a better understanding of what this code does, read the `remainder >= 3` case above.

This tries to take the biggest step possible within a `Decode.loop` iteration.
The idea is similar to loop-unrolling in languages like c: avoiding jumps gives better performance

But `Decode.loop` also requires that the accumulator is wrapped in a `Step a b`, i.e. it allocates a `Decode.Loop _`
for every iteration. Allocation is expensive in tight loops like this one. So there is a double reason to limit the number
of iterations: avoiding jumps and avoiding allocation.

Given that `Decode.map5` is the highest one defined by `elm/bytes` and we need a multiple of 3 bytes,
`4 * 4 + 2 = 18` is the best we can do.

-}
decode18Bytes : Decode.Decoder String
decode18Bytes =
    Decode.map5 decode18Help
        u32BE
        u32BE
        u32BE
        u32BE
        u16BE


{-| Get 18 bytes (4 times 32-bit, one 16-bit) and split them into 3-byte chunks.

Then convert the 3-byte chunks to characters and produce a string.

-}
decode18Help : Int -> Int -> Int -> Int -> Int -> String
decode18Help a b c d e =
    let
        combined1 =
            Bitwise.shiftRightZfBy 8 a

        combined2 =
            Bitwise.or
                (Bitwise.and 0xFF a |> Bitwise.shiftLeftBy 16)
                (Bitwise.shiftRightZfBy 16 b)

        combined3 =
            Bitwise.or
                (Bitwise.and 0xFFFF b |> Bitwise.shiftLeftBy 8)
                (Bitwise.shiftRightZfBy 24 c)

        combined4 =
            Bitwise.and 0x00FFFFFF c

        combined5 =
            Bitwise.shiftRightZfBy 8 d

        combined6 =
            Bitwise.or
                (Bitwise.and 0xFF d |> Bitwise.shiftLeftBy 16)
                e
    in
    -- the order is counter-intuitive because `String.cons` is used in bitsToCharSpecialized.
    ""
        |> bitsToCharSpecialized combined6 combined5 combined4
        |> bitsToCharSpecialized combined3 combined2 combined1


{-| A specialized version of bitsToChar that handles 3 24-bit integers at once.

This was done to limit the number of function calls. When doing bitwise manipulations (which are very efficient), the
overhead of function calls -- something we normally don't really think about -- starts to matter.

-}
bitsToCharSpecialized : Int -> Int -> Int -> String -> String
bitsToCharSpecialized bits1 bits2 bits3 accum =
    let
        -- BITS 1
        p =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits1)

        q =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits1) lowest6BitsMask)

        r =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits1) lowest6BitsMask)

        s =
            unsafeToChar (Bitwise.and bits1 lowest6BitsMask)

        -- BITS 2
        a =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits2)

        b =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits2) lowest6BitsMask)

        c =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits2) lowest6BitsMask)

        d =
            unsafeToChar (Bitwise.and bits2 lowest6BitsMask)

        -- BITS 3
        x =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits3)

        y =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits3) lowest6BitsMask)

        z =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits3) lowest6BitsMask)

        w =
            unsafeToChar (Bitwise.and bits3 lowest6BitsMask)
    in
    -- Performance: This is the fastest way to create a string from characters.
    -- see also https://github.com/danfishgold/base64-bytes/pull/3#discussion_r342321940
    -- cons adds on the left, so characters are added in reverse order.
    accum
        |> String.cons s
        |> String.cons r
        |> String.cons q
        |> String.cons p
        |> String.cons d
        |> String.cons c
        |> String.cons b
        |> String.cons a
        |> String.cons w
        |> String.cons z
        |> String.cons y
        |> String.cons x
"""
    ]
