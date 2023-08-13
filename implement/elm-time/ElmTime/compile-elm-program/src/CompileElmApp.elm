module CompileElmApp exposing (..)

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
import Json.Decode
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
    , compilationRootFilePath : List String
    , interfaceToHostRootModuleName : List String
    }


type alias SourceDirectories =
    { mainSourceDirectoryPath : List String
    , elmJsonDirectoryPath : List String
    }


type alias ElmJson =
    { sourceDirectories : List String
    }


encodingModuleImportBytes : ( List String, Maybe String )
encodingModuleImportBytes =
    ( [ "CompilerGenerated", "EncodeBytes" ], Just "EncodeBytes" )


encodingModuleImportBase64 : ( List String, Maybe String )
encodingModuleImportBase64 =
    ( [ "CompilerGenerated", "Base64" ], Just "Base64" )


type alias CompilationIterationSuccess =
    { compiledFiles : AppFiles
    , rootModuleEntryPointKind : Result String ElmMakeEntryPointKind
    }


type ElmMakeEntryPointKind
    = ClassicMakeEntryPoint ElmMakeEntryPointStruct
    | BlobMakeEntryPoint ElmMakeEntryPointStruct


type alias ElmMakeEntryPointStruct =
    { elmMakeJavaScriptFunctionName : String
    }


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


type alias SourceParsedElmModule =
    { fileText : String
    , parsedSyntax : Elm.Syntax.File.File
    , moduleName : List String
    }


type alias EntryPointClass =
    SourceParsedElmModule -> Result { supportedDeclarationNames : Set.Set String } ProcessEntryPoint


type alias ProcessEntryPoint =
    CompileEntryPointConfig
    -> AppFiles
    ->
        Result
            (List (LocatedInSourceFiles CompilationError))
            { compiledFiles : AppFiles, rootModuleEntryPointKind : ElmMakeEntryPointKind }


type alias CompileEntryPointConfig =
    { compilationRootFilePath : List String
    , compilationRootModule : SourceParsedElmModule
    , interfaceToHostRootModuleName : List String
    , originalSourceModules : Dict.Dict (List String) SourceParsedElmModule
    }


defaultEntryPoints : List EntryPointClass
defaultEntryPoints =
    [ entryPointClassFromSetOfEquallyProcessedFunctionNames
        (Set.singleton "blobMain")
        (\_ entryPointConfig ->
            loweredForBlobEntryPoint entryPointConfig
                >> Result.map
                    (\( compiledFiles, entryPoint ) ->
                        { compiledFiles = compiledFiles
                        , rootModuleEntryPointKind = BlobMakeEntryPoint entryPoint
                        }
                    )
        )
    , entryPointClassFromSetOfEquallyProcessedFunctionNames
        (Set.singleton "main")
        (\_ entryPointConfig compiledFiles ->
            Ok
                { compiledFiles = compiledFiles
                , rootModuleEntryPointKind =
                    ClassicMakeEntryPoint
                        { elmMakeJavaScriptFunctionName =
                            ((entryPointConfig.compilationRootModule.parsedSyntax.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> Elm.Syntax.Module.moduleName
                             )
                                ++ [ "main" ]
                            )
                                |> String.join "."
                        }
                }
        )
    ]


entryPointClassFromSetOfEquallyProcessedFunctionNames :
    Set.Set String
    -> (Elm.Syntax.Expression.Function -> ProcessEntryPoint)
    -> EntryPointClass
entryPointClassFromSetOfEquallyProcessedFunctionNames supportedDeclarationNames processEntryPoint sourceModule =
    entryPointClassFromSetOfEquallyProcessedNames
        supportedDeclarationNames
        (\declaration ->
            case declaration of
                Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                    processEntryPoint functionDeclaration

                _ ->
                    \config _ ->
                        Err
                            [ LocatedInSourceFiles
                                { filePath = config.compilationRootFilePath
                                , locationInModuleText = Elm.Syntax.Range.emptyRange
                                }
                                (OtherCompilationError "Is not a function declaration")
                            ]
        )
        sourceModule


entryPointClassFromSetOfEquallyProcessedNames :
    Set.Set String
    -> (Elm.Syntax.Declaration.Declaration -> ProcessEntryPoint)
    -> EntryPointClass
entryPointClassFromSetOfEquallyProcessedNames supportedDeclarationNames processEntryPoint sourceModule =
    let
        declarationsNames =
            sourceModule.parsedSyntax.declarations
                |> List.map Elm.Syntax.Node.value
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                Just
                                    ( Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                    , declaration
                                    )

                            _ ->
                                Nothing
                    )
    in
    case
        declarationsNames
            |> List.filter (Tuple.first >> Set.member >> (|>) supportedDeclarationNames)
            |> List.head
    of
        Nothing ->
            Err { supportedDeclarationNames = supportedDeclarationNames }

        Just ( _, declaration ) ->
            Ok (processEntryPoint declaration)


{-| This function returns an Err if the needed dependencies for ElmMake are not yet in the arguments.
The integrating software can then perform the ElmMake, insert it into the dependencies dict and retry.
-}
asCompletelyLoweredElmApp :
    List EntryPointClass
    -> CompilationArguments
    -> Result (List LocatedCompilationError) CompilationIterationSuccess
asCompletelyLoweredElmApp entryPointClasses arguments =
    case findSourceDirectories arguments of
        Err err ->
            Err
                [ LocatedInSourceFiles
                    { filePath = arguments.compilationRootFilePath
                    , locationInModuleText = Elm.Syntax.Range.emptyRange
                    }
                    (OtherCompilationError ("Failed to find source directories: " ++ err))
                ]

        Ok sourceDirs ->
            let
                sourceModules =
                    arguments.sourceFiles
                        |> elmModulesDictFromAppFiles
                        |> Dict.toList
                        |> List.filterMap
                            (\( filePath, moduleResult ) ->
                                moduleResult
                                    |> Result.toMaybe
                                    |> Maybe.map (Tuple.pair filePath)
                            )
                        |> Dict.fromList

                compilationInterfaceModuleDependencies : Dict.Dict String (List String)
                compilationInterfaceModuleDependencies =
                    [ ( "SourceFiles", modulesToAddForBytesCoding )
                    , ( "ElmMake", modulesToAddForBytesCoding )
                    , ( "GenerateJsonCoders", modulesToAddForBase64Coding )
                    , ( "GenerateJsonConverters", modulesToAddForBase64Coding )
                    ]
                        |> Dict.fromList

                usedCompilationInterfaceModules : Set.Set String
                usedCompilationInterfaceModules =
                    sourceModules
                        |> Dict.values
                        |> List.map .moduleName
                        |> List.concatMap
                            (\moduleName ->
                                arguments.compilationInterfaceElmModuleNamePrefixes
                                    |> List.concatMap
                                        (\compilationInterfacePrefix ->
                                            case moduleName of
                                                [] ->
                                                    []

                                                firstDirectory :: others ->
                                                    if firstDirectory == compilationInterfacePrefix then
                                                        [ String.join "." others ]

                                                    else
                                                        []
                                        )
                            )
                        |> Set.fromList

                modulesToAdd =
                    usedCompilationInterfaceModules
                        |> Set.toList
                        |> List.filterMap (\moduleName -> Dict.get moduleName compilationInterfaceModuleDependencies)
                        |> List.concat
            in
            arguments.sourceFiles
                |> addModulesFromTextToAppFiles sourceDirs modulesToAdd
                |> loweredForSourceFiles sourceDirs arguments.compilationInterfaceElmModuleNamePrefixes
                |> Result.andThen (loweredForJsonConverters { originalSourceModules = sourceModules, sourceDirs = sourceDirs } arguments.compilationInterfaceElmModuleNamePrefixes)
                |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)
                |> Result.andThen (loweredForElmMake sourceDirs arguments.compilationInterfaceElmModuleNamePrefixes arguments.dependencies)
                |> Result.andThen
                    (loweredForCompilationRoot
                        entryPointClasses
                        { originalSourceModules = sourceModules
                        , compilationRootFilePath = arguments.compilationRootFilePath
                        , interfaceToHostRootModuleName = arguments.interfaceToHostRootModuleName
                        }
                    )


findSourceDirectories :
    { a | compilationRootFilePath : List String, sourceFiles : AppFiles }
    -> Result String SourceDirectories
findSourceDirectories arguments =
    let
        searchRecursive : List String -> Result String SourceDirectories
        searchRecursive currentDirectory =
            case Dict.get (currentDirectory ++ [ "elm.json" ]) arguments.sourceFiles of
                Nothing ->
                    if currentDirectory == [] then
                        Err "Did not find elm.json"

                    else
                        searchRecursive (currentDirectory |> List.reverse |> List.drop 1 |> List.reverse)

                Just elmJsonFile ->
                    case stringFromFileContent elmJsonFile of
                        Nothing ->
                            Err "Failed to decode file content as string"

                        Just elmJsonString ->
                            case Json.Decode.decodeString decodeElmJson elmJsonString of
                                Err err ->
                                    Err ("Failed to decode elm.json: " ++ Json.Decode.errorToString err)

                                Ok elmJson ->
                                    case
                                        elmJson.sourceDirectories
                                            |> List.head
                                            |> Maybe.map (String.split "/")
                                    of
                                        Nothing ->
                                            Err "Did not find a matching directory in source-directories"

                                        Just matchingSourceDirectory ->
                                            Ok
                                                { mainSourceDirectoryPath =
                                                    currentDirectory ++ matchingSourceDirectory
                                                , elmJsonDirectoryPath = currentDirectory
                                                }
    in
    searchRecursive arguments.compilationRootFilePath


decodeElmJson : Json.Decode.Decoder ElmJson
decodeElmJson =
    Json.Decode.map ElmJson
        (Json.Decode.field "source-directories" (Json.Decode.list Json.Decode.string))


loweredForSourceFiles : SourceDirectories -> List String -> AppFiles -> Result (LocatedInSourceFiles String) AppFiles
loweredForSourceFiles sourceDirs compilationInterfaceElmModuleNamePrefixes sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    sourceDirs
                    locatedInSourceFilesFromJustFilePath
                    (compilationInterfaceElmModuleNamePrefix ++ ".SourceFiles")
                    (mapSourceFilesModuleText sourceDirs)
                    files
            )
            (Ok sourceFiles)


loweredForJsonConverters :
    { originalSourceModules : Dict.Dict (List String) SourceParsedElmModule
    , sourceDirs : SourceDirectories
    }
    -> List String
    -> AppFiles
    -> Result (LocatedInSourceFiles String) AppFiles
loweredForJsonConverters context compilationInterfaceElmModuleNamePrefixes sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    context.sourceDirs
                    locatedInSourceFilesFromJustFilePath
                    (compilationInterfaceElmModuleNamePrefix ++ ".GenerateJsonCoders")
                    (mapJsonConvertersModuleText context)
                    files
                    |> Result.andThen
                        (mapElmModuleWithNameIfExists
                            context.sourceDirs
                            locatedInSourceFilesFromJustFilePath
                            (compilationInterfaceElmModuleNamePrefix ++ ".GenerateJsonConverters")
                            (mapJsonConvertersModuleText context)
                        )
            )
            (Ok sourceFiles)


loweredForElmMake :
    SourceDirectories
    -> List String
    -> List ( DependencyKey, Bytes.Bytes )
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) AppFiles
loweredForElmMake sourceDirs compilationInterfaceElmModuleNamePrefixes dependencies sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    sourceDirs
                    (\context -> OtherCompilationError >> locatedInSourceFilesFromJustFilePath context >> List.singleton)
                    (compilationInterfaceElmModuleNamePrefix ++ ".ElmMake")
                    (mapElmMakeModuleText sourceDirs dependencies)
                    files
            )
            (Ok sourceFiles)


loweredForCompilationRoot :
    List EntryPointClass
    ->
        { compilationRootFilePath : List String
        , interfaceToHostRootModuleName : List String
        , originalSourceModules : Dict.Dict (List String) SourceParsedElmModule
        }
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) CompilationIterationSuccess
loweredForCompilationRoot entryPointClasses config sourceFiles =
    case Dict.get config.compilationRootFilePath config.originalSourceModules of
        Nothing ->
            Err
                [ LocatedInSourceFiles
                    { filePath = config.compilationRootFilePath
                    , locationInModuleText = Elm.Syntax.Range.emptyRange
                    }
                    (OtherCompilationError ("Did not find file " ++ String.join "/" config.compilationRootFilePath))
                ]

        Just compilationRootModule ->
            let
                entryPointMatchesResults =
                    entryPointClasses
                        |> List.map ((|>) compilationRootModule)
            in
            case entryPointMatchesResults |> List.filterMap Result.toMaybe |> List.head of
                Nothing ->
                    let
                        allSupportedDeclarationNames =
                            entryPointMatchesResults
                                |> List.map (Result.Extra.unpack .supportedDeclarationNames (always Set.empty))
                                |> List.foldl Set.union Set.empty
                    in
                    Ok
                        { compiledFiles = sourceFiles
                        , rootModuleEntryPointKind =
                            Err
                                ("Found no declaration of an entry point. I only support the following "
                                    ++ String.fromInt (Set.size allSupportedDeclarationNames)
                                    ++ " names for entry points declarations: "
                                    ++ String.join ", " (Set.toList allSupportedDeclarationNames)
                                )
                        }

                Just buildEntryPoint ->
                    buildEntryPoint
                        { compilationRootFilePath = config.compilationRootFilePath
                        , compilationRootModule = compilationRootModule
                        , interfaceToHostRootModuleName = config.interfaceToHostRootModuleName
                        , originalSourceModules = config.originalSourceModules
                        }
                        sourceFiles
                        |> Result.map
                            (\buildEntryPointOk ->
                                { compiledFiles = buildEntryPointOk.compiledFiles
                                , rootModuleEntryPointKind = Ok buildEntryPointOk.rootModuleEntryPointKind
                                }
                            )


loweredForBlobEntryPoint :
    CompileEntryPointConfig
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) ( AppFiles, ElmMakeEntryPointStruct )
loweredForBlobEntryPoint { compilationRootFilePath, compilationRootModule } sourceFiles =
    ([ compilationRootModule.fileText
     , String.trim """
blob_main_as_base64 : String
blob_main_as_base64 =
    blobMain
        |> Base64.fromBytes
        |> Maybe.withDefault "Failed to encode as Base64"


{-| Support function-level dead code elimination (<https://elm-lang.org/blog/small-assets-without-the-headache>) Elm code needed to inform the Elm compiler about our entry points.
-}
main : Program Int {} String
main =
    Platform.worker
        { init = always ( {}, Cmd.none )
        , update = always (always ( blob_main_as_base64 |> always {}, Cmd.none ))
        , subscriptions = always Sub.none
        }
"""
     ]
        |> String.join "\n\n"
        |> addImportsInElmModuleText [ ( [ "Base64" ], Nothing ) ]
        |> Result.mapError
            (\err ->
                [ LocatedInSourceFiles
                    { filePath = compilationRootFilePath
                    , locationInModuleText = Elm.Syntax.Range.emptyRange
                    }
                    (OtherCompilationError ("Failed to add import: " ++ err))
                ]
            )
    )
        |> Result.map
            (\rootModuleText ->
                ( sourceFiles
                    |> updateFileContentAtPath (always (fileContentFromString rootModuleText)) compilationRootFilePath
                , { elmMakeJavaScriptFunctionName =
                        ((compilationRootModule.parsedSyntax.moduleDefinition
                            |> Elm.Syntax.Node.value
                            |> Elm.Syntax.Module.moduleName
                         )
                            ++ [ "blob_main_as_base64" ]
                        )
                            |> String.join "."
                  }
                )
            )


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


mapJsonConvertersModuleText :
    { originalSourceModules : Dict.Dict (List String) SourceParsedElmModule
    , sourceDirs : SourceDirectories
    }
    -> ( AppFiles, List String, String )
    -> Result (LocatedInSourceFiles String) ( AppFiles, String )
mapJsonConvertersModuleText { originalSourceModules, sourceDirs } ( sourceFiles, moduleFilePath, moduleText ) =
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
                                    case parseJsonConverterDeclarationType (Elm.Syntax.Node.value functionSignature) of
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
                                    mapAppFilesToSupportJsonConverters
                                        { generatedModuleNamePrefix = interfaceModuleName
                                        , sourceDirs = sourceDirs
                                        }
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
                                                buildJsonConverterFunctionsForTypeAnnotation
                                                    functionToReplace.parsedTypeAnnotation

                                            newFunction =
                                                functionName
                                                    ++ " =\n    "
                                                    ++ String.join "."
                                                        (generatedModuleName
                                                            ++ [ if functionToReplace.functionType.isDecoder then
                                                                    functionsNamesInGeneratedModules.decodeFunction.name

                                                                 else
                                                                    functionsNamesInGeneratedModules.encodeFunction.name
                                                               ]
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
                                        (modulesToImport |> Set.toList |> List.map (Tuple.pair >> (|>) Nothing))
                                        moduleText
                                        |> Result.mapError (Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteString moduleText))
                                    )
                                |> Result.map (Tuple.pair appFiles)
                                |> Result.mapError (locatedInSourceFilesFromRange moduleFilePath)
                        )
            )


{-| Combines two transformations of the app files to support JSON converters:

  - Exposes the choice type tags from declaring modules where necessary to implement JSON decoders.
  - Adds a module containing generated JSON conversion functions for the given list of type annotations and choice types.

-}
mapAppFilesToSupportJsonConverters :
    { generatedModuleNamePrefix : List String
    , sourceDirs : SourceDirectories
    }
    -> List ElmTypeAnnotation
    -> Dict.Dict String ElmChoiceTypeStruct
    -> AppFiles
    -> ( AppFiles, { generatedModuleName : List String, modulesToImport : Set.Set (List String) } )
mapAppFilesToSupportJsonConverters { generatedModuleNamePrefix, sourceDirs } typeAnnotationsBeforeDeduplicating choiceTypes appFilesBefore =
    let
        generatedFunctions =
            buildJsonConverterFunctionsForMultipleTypes typeAnnotationsBeforeDeduplicating choiceTypes

        modulesToImport =
            generatedFunctions.generatedFunctions
                |> List.map .modulesToImport
                |> List.foldl Set.union Set.empty
                |> Set.union generatedFunctions.modulesToImportForChoiceTypes

        generatedModuleModulesToImport =
            encodingModuleImportBase64
                :: (modulesToImport
                        |> Set.toList
                        |> List.map (Tuple.pair >> (|>) Nothing)
                   )

        appFilesAfterExposingChoiceTypesInModules =
            generatedFunctions.modulesToImportForChoiceTypes
                |> Set.toList
                |> List.foldl (exposeAllInElmModuleInAppFiles sourceDirs) appFilesBefore

        generatedModuleTextWithoutModuleDeclaration =
            [ [ generatedModuleModulesToImport
                    |> List.map importSyntaxTextFromModuleNameAndAlias
                    |> List.sort
                    |> String.join "\n"
              ]
            , generatedFunctions.generatedFunctions |> List.map .functionText
            ]
                |> List.concat
                |> List.map String.trim
                |> String.join "\n\n\n"

        generatedModuleHash =
            generatedModuleTextWithoutModuleDeclaration
                |> SHA256.fromString
                |> SHA256.toHex

        generatedModuleName =
            generatedModuleNamePrefix ++ [ "Generated_" ++ String.left 8 generatedModuleHash ]

        generatedModulePath =
            filePathFromElmModuleName sourceDirs generatedModuleName

        generatedModuleText =
            [ "module " ++ String.join "." generatedModuleName ++ " exposing (..)"
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
      , modulesToImport = Set.insert generatedModuleName modulesToImport
      }
    )


type alias GenerateFunctionsFromTypesConfig =
    { generateFromTypeAnnotation : ElmTypeAnnotation -> List GenerateFunctionFromTypeResult
    , generateFromChoiceType : ( String, ElmChoiceTypeStruct ) -> List GenerateFunctionFromTypeResult
    }


type alias GenerateFunctionFromTypeResult =
    { functionName : String
    , functionText : String
    , modulesToImport : Set.Set (List String)
    }


buildJsonConverterFunctionsForMultipleTypes :
    List ElmTypeAnnotation
    -> Dict.Dict String ElmChoiceTypeStruct
    ->
        { generatedFunctions : List GenerateFunctionFromTypeResult
        , modulesToImportForChoiceTypes : Set.Set (List String)
        }
buildJsonConverterFunctionsForMultipleTypes typeAnnotations choiceTypes =
    let
        defaultModulesToImportForFunction =
            [ [ "Dict" ]
            , [ "Set" ]
            , [ "Array" ]
            , [ "Json", "Decode" ]
            , [ "Json", "Encode" ]
            , [ "Bytes" ]
            , [ "Bytes", "Decode" ]
            , [ "Bytes", "Encode" ]
            ]
                |> Set.fromList

        generatedFunctionsForTypes =
            generateFunctionsForMultipleTypes
                { generateFromTypeAnnotation =
                    buildJsonConverterFunctionsForTypeAnnotation
                        >> (\functionsForType ->
                                [ functionsForType.encodeFunction, functionsForType.decodeFunction ]
                                    |> List.map
                                        (\function ->
                                            { functionName = function.name
                                            , functionText = function.text
                                            , modulesToImport = defaultModulesToImportForFunction
                                            }
                                        )
                           )
                , generateFromChoiceType =
                    (\( choiceTypeName, choiceType ) ->
                        jsonConverterFunctionFromChoiceType
                            { choiceTypeName = choiceTypeName
                            , encodeValueExpression = jsonEncodeParamName
                            , typeArgLocalName = "type_arg"
                            }
                            choiceType
                    )
                        >> (\functionsForType ->
                                [ functionsForType.encodeFunction, functionsForType.decodeFunction ]
                                    |> List.map
                                        (\function ->
                                            { functionName = function.name
                                            , functionText = function.text
                                            , modulesToImport = defaultModulesToImportForFunction
                                            }
                                        )
                           )
                }
                typeAnnotations
                choiceTypes

        generatedFunctions =
            generatedFunctionsForTypes.generatedFunctions
                ++ jsonConverterSupportingFunctionsTexts
    in
    { generatedFunctions = generatedFunctions
    , modulesToImportForChoiceTypes = generatedFunctionsForTypes.modulesToImportForChoiceTypes
    }


buildEstimateJsonEncodeLengthFunctionsForMultipleTypes :
    List ElmTypeAnnotation
    -> Dict.Dict String ElmChoiceTypeStruct
    ->
        { generatedFunctions : List GenerateFunctionFromTypeResult
        , modulesToImportForChoiceTypes : Set.Set (List String)
        }
buildEstimateJsonEncodeLengthFunctionsForMultipleTypes typeAnnotations choiceTypes =
    let
        defaultModulesToImportForFunction =
            [ [ "Dict" ]
            , [ "Set" ]
            , [ "Array" ]
            , [ "Bytes" ]
            ]
                |> Set.fromList

        generatedFunctionsForTypes =
            generateFunctionsForMultipleTypes
                { generateFromTypeAnnotation =
                    buildEstimateJsonEncodeLengthFunctionForTypeAnnotation
                        >> (\functionForType ->
                                { functionName = functionForType.name
                                , functionText = functionForType.text
                                , modulesToImport = defaultModulesToImportForFunction
                                }
                           )
                        >> List.singleton
                , generateFromChoiceType =
                    (\( choiceTypeName, choiceType ) ->
                        estimateSerializedSizeFunctionFromChoiceType
                            { choiceTypeName = choiceTypeName
                            , encodeValueExpression = estimateJsonEncodeLengthParamName
                            , typeArgLocalName = "type_arg"
                            }
                            choiceType
                    )
                        >> (\functionForType ->
                                { functionName = functionForType.name
                                , functionText = functionForType.text
                                , modulesToImport = defaultModulesToImportForFunction
                                }
                           )
                        >> List.singleton
                }
                typeAnnotations
                choiceTypes

        generatedFunctions =
            generatedFunctionsForTypes.generatedFunctions
                ++ estimateJsonEncodeLengthSupportingFunctionsTexts
    in
    { generatedFunctionsForTypes
        | generatedFunctions = generatedFunctions
    }


generateFunctionsForMultipleTypes :
    GenerateFunctionsFromTypesConfig
    -> List ElmTypeAnnotation
    -> Dict.Dict String ElmChoiceTypeStruct
    ->
        { generatedFunctions : List GenerateFunctionFromTypeResult
        , modulesToImportForChoiceTypes : Set.Set (List String)
        }
generateFunctionsForMultipleTypes config typeAnnotationsBeforeDeduplicating choiceTypes =
    let
        modulesToImportForChoiceTypes =
            choiceTypes
                |> Dict.keys
                |> List.map moduleNameFromTypeName
                |> Set.fromList
                |> Set.toList
                |> List.map (String.split ".")
                |> Set.fromList

        generatedFunctionsFromTypeAnnotations =
            typeAnnotationsBeforeDeduplicating
                |> List.Extra.unique
                |> List.concatMap config.generateFromTypeAnnotation

        generatedFunctionsFromChoiceTypes =
            choiceTypes
                |> Dict.toList
                |> List.concatMap config.generateFromChoiceType

        generatedFunctions =
            generatedFunctionsFromTypeAnnotations ++ generatedFunctionsFromChoiceTypes
    in
    { generatedFunctions = generatedFunctions
    , modulesToImportForChoiceTypes = modulesToImportForChoiceTypes
    }


buildJsonConverterFunctionsForTypeAnnotation :
    ElmTypeAnnotation
    -> { encodeFunction : { name : String, text : String }, decodeFunction : { name : String, text : String } }
buildJsonConverterFunctionsForTypeAnnotation typeAnnotation =
    let
        jsonConverterExpressions =
            jsonConverterExpressionFromType
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
                ++ indentElmCodeLines 1 jsonConverterExpressions.encodeExpression

        decodeFunctionText =
            decodeFunctionName
                ++ " =\n"
                ++ indentElmCodeLines 1 jsonConverterExpressions.decodeExpression
    in
    { encodeFunction = { name = encodeFunctionName, text = encodeFunctionText }
    , decodeFunction = { name = decodeFunctionName, text = decodeFunctionText }
    }


buildEstimateJsonEncodeLengthFunctionForTypeAnnotation : ElmTypeAnnotation -> { name : String, text : String }
buildEstimateJsonEncodeLengthFunctionForTypeAnnotation typeAnnotation =
    let
        jsonConverterExpressions =
            estimateJsonEncodeLengthExpressionFromType
                { encodeValueExpression = estimateJsonEncodeLengthParamName
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
            estimateJsonEncodeLengthFunctionNamePrefix ++ nameCommonPart

        estimateFunctionText =
            encodeFunctionName
                ++ " "
                ++ estimateJsonEncodeLengthParamName
                ++ " =\n"
                ++ indentElmCodeLines 1 jsonConverterExpressions.estimateExpression
    in
    { name = encodeFunctionName, text = estimateFunctionText }


mapSourceFilesModuleText :
    SourceDirectories
    -> ( AppFiles, List String, String )
    -> Result (LocatedInSourceFiles String) ( AppFiles, String )
mapSourceFilesModuleText sourceDirs ( sourceFiles, moduleFilePath, moduleText ) =
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
                    -- TODO: Also share the 'map all functions' part with `mapJsonConvertersModuleText`
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
                                sourceDirs
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
                                    interfaceModuleName ++ [ "Generated_" ++ String.left 8 generatedModuleHash ]

                                generatedModulePath =
                                    filePathFromElmModuleName sourceDirs generatedModuleName

                                generatedModuleText =
                                    [ "module " ++ String.join "." generatedModuleName ++ " exposing (..)"
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
                                        , ( generatedModuleName, Nothing )
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
    SourceDirectories
    -> List ( DependencyKey, Bytes.Bytes )
    -> ( AppFiles, List String, String )
    -> Result (List LocatedCompilationError) ( AppFiles, String )
mapElmMakeModuleText sourceDirs dependencies ( sourceFiles, moduleFilePath, moduleText ) =
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
                    -- TODO: Also share the 'map all functions' part with `mapJsonConvertersModuleText`
                    |> List.filterMap declarationWithRangeAsFunctionDeclaration
                    |> List.map
                        (\declaration ->
                            prepareReplaceFunctionInElmMakeModuleText dependencies
                                sourceDirs
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
                                    interfaceModuleName ++ [ "Generated_" ++ String.left 8 generatedModuleHash ]

                                generatedModulePath =
                                    filePathFromElmModuleName sourceDirs generatedModuleName

                                generatedModuleText =
                                    [ "module " ++ String.join "." generatedModuleName ++ " exposing (..)"
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
                                        , ( generatedModuleName, Nothing )
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


exposeAllInElmModuleInAppFiles : SourceDirectories -> List String -> AppFiles -> AppFiles
exposeAllInElmModuleInAppFiles sourceDirs moduleName appFiles =
    let
        moduleFilePath =
            filePathFromElmModuleName sourceDirs moduleName
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
    | ArrayLeaf
    | SetLeaf
    | MaybeLeaf
    | ResultLeaf
    | DictLeaf
    | JsonEncodeValueLeaf


parseElmFunctionTypeAndDependenciesRecursivelyFromAnnotation :
    Dict.Dict (List String) SourceParsedElmModule
    -> ( ( List String, Elm.Syntax.File.File ), Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Result (LocatedInSourceFiles String) ( List (Elm.Syntax.Node.Node ElmTypeAnnotation), Dict.Dict String ElmChoiceTypeStruct )
parseElmFunctionTypeAndDependenciesRecursivelyFromAnnotation modules ( ( currentModuleFilePath, currentModule ), typeAnnotationNode ) =
    case Elm.Syntax.Node.value typeAnnotationNode of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode returnNode ->
            parseElmFunctionTypeAndDependenciesRecursivelyFromAnnotation
                modules
                ( ( currentModuleFilePath, currentModule ), inputNode )
                |> Result.andThen
                    (\( parsedInput, parsedInputDeps ) ->
                        parseElmFunctionTypeAndDependenciesRecursivelyFromAnnotation
                            modules
                            ( ( currentModuleFilePath, currentModule ), returnNode )
                            |> Result.map
                                (\( parsedReturn, parsedReturnDeps ) ->
                                    ( parsedInput ++ parsedReturn
                                    , Dict.union parsedInputDeps parsedReturnDeps
                                    )
                                )
                    )

        _ ->
            parseElmTypeAndDependenciesRecursivelyFromAnnotation
                modules
                ( ( currentModuleFilePath, currentModule ), typeAnnotationNode )
                |> Result.map
                    (Tuple.mapFirst
                        (Elm.Syntax.Node.Node (Elm.Syntax.Node.range typeAnnotationNode) >> List.singleton)
                    )


parseElmTypeAndDependenciesRecursivelyFromAnnotation :
    Dict.Dict (List String) SourceParsedElmModule
    -> ( ( List String, Elm.Syntax.File.File ), Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Result (LocatedInSourceFiles String) ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
parseElmTypeAndDependenciesRecursivelyFromAnnotation modules ( ( currentModuleFilePath, currentModule ), typeAnnotationNode ) =
    parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal
        { typesToIgnore = Set.empty }
        modules
        ( currentModule, Elm.Syntax.Node.value typeAnnotationNode )
        |> Result.mapError
            (LocatedInSourceFiles
                { filePath = currentModuleFilePath, locationInModuleText = Elm.Syntax.Node.range typeAnnotationNode }
            )


parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal :
    { typesToIgnore : Set.Set String }
    -> Dict.Dict (List String) SourceParsedElmModule
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
    -> Dict.Dict (List String) SourceParsedElmModule
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
                                                        modules
                                                            |> findModuleByName (Elm.Syntax.Node.value moduleImport.moduleName)
                                                            |> Maybe.map (Tuple.second >> .parsedSyntax)

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
                                            modules
                                                |> findModuleByName (Elm.Syntax.Node.value matchingImport.moduleName)
                                                |> Maybe.map (Tuple.second >> .parsedSyntax)
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

                InstanceElmType instanceElmType ->
                    instanceElmType.instantiated
                        |> tryConcretizeFieldType
                        |> Result.mapError ((++) "Failed to concretize instantiated: ")
                        |> Result.andThen
                            (\concreteInstantiated ->
                                instanceElmType.arguments
                                    |> List.indexedMap
                                        (\argIndex ->
                                            tryConcretizeFieldType
                                                >> Result.mapError
                                                    ((++)
                                                        ("Failed to concretize instance argument "
                                                            ++ String.fromInt argIndex
                                                            ++ ": "
                                                        )
                                                    )
                                        )
                                    |> Result.Extra.combine
                                    |> Result.map
                                        (\concreteArguments ->
                                            InstanceElmType
                                                { instantiated = concreteInstantiated
                                                , arguments = concreteArguments
                                                }
                                        )
                            )

                TupleElmType tupleElmType ->
                    tupleElmType
                        |> List.indexedMap
                            (\argIndex ->
                                tryConcretizeFieldType
                                    >> Result.mapError
                                        ((++)
                                            ("Failed to concretize tuple element "
                                                ++ String.fromInt argIndex
                                                ++ ": "
                                            )
                                        )
                            )
                        |> Result.Extra.combine
                        |> Result.map TupleElmType

                RecordElmType recordElmType ->
                    recordElmType.fields
                        |> List.map
                            (\( fieldName, innerFieldType ) ->
                                innerFieldType
                                    |> tryConcretizeFieldType
                                    |> Result.mapError
                                        ((++)
                                            ("Failed to concretize field "
                                                ++ fieldName
                                                ++ ": "
                                            )
                                        )
                                    |> Result.map (Tuple.pair fieldName)
                            )
                        |> Result.Extra.combine
                        |> Result.map (\fields -> RecordElmType { fields = fields })

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


jsonConverterExpressionFromType :
    { encodeValueExpression : String, typeArgLocalName : String }
    -> ( ElmTypeAnnotation, List ElmTypeAnnotation )
    -> { encodeExpression : String, decodeExpression : String }
jsonConverterExpressionFromType { encodeValueExpression, typeArgLocalName } ( typeAnnotation, typeArguments ) =
    let
        typeArgumentsExpressions =
            typeArguments
                |> List.map
                    (\typeArgument ->
                        let
                            typeArgumentExpressions =
                                jsonConverterExpressionFromType
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
                    jsonConverterFunctionNameCommonPartFromTypeName choice
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
                                            jsonConverterExpressionFromType
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
            jsonConverterExpressionFromType
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
                                        jsonConverterExpressionFromType
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
                    jsonConverterFunctionNameFromTypeParameterName name
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
                    continueWithLocalNameAndCommonPrefix jsonConvertListFunctionNameCommonPart

                ArrayLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertArrayFunctionNameCommonPart

                SetLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertSetFunctionNameCommonPart

                ResultLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertResultFunctionNameCommonPart

                MaybeLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertMaybeFunctionNameCommonPart

                DictLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertDictFunctionNameCommonPart

                JsonEncodeValueLeaf ->
                    { encodeExpression = encodeValueExpression
                    , decodeExpression = "Json.Decode.value"
                    }


jsonConverterFunctionFromChoiceType :
    { choiceTypeName : String, encodeValueExpression : String, typeArgLocalName : String }
    -> ElmChoiceTypeStruct
    -> { encodeFunction : { name : String, text : String }, decodeFunction : { name : String, text : String } }
jsonConverterFunctionFromChoiceType { choiceTypeName, encodeValueExpression, typeArgLocalName } choiceType =
    let
        encodeParametersText =
            choiceType.parameters
                |> List.map (jsonConverterFunctionNameFromTypeParameterName >> .encodeName)
                |> String.join " "

        decodeParametersText =
            choiceType.parameters
                |> List.map (jsonConverterFunctionNameFromTypeParameterName >> .decodeName)
                |> String.join " "

        moduleName =
            moduleNameFromTypeName choiceTypeName

        typeNameRepresentation =
            jsonConverterFunctionNameCommonPartFromTypeName choiceTypeName

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
                                                    jsonConverterExpressionFromType
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


estimateJsonEncodeLengthExpressionFromType :
    { encodeValueExpression : String, typeArgLocalName : String }
    -> ( ElmTypeAnnotation, List ElmTypeAnnotation )
    -> { estimateExpression : String }
estimateJsonEncodeLengthExpressionFromType { encodeValueExpression, typeArgLocalName } ( typeAnnotation, typeArguments ) =
    let
        typeArgumentsExpressions =
            typeArguments
                |> List.map
                    (\typeArgument ->
                        let
                            typeArgumentExpressions =
                                estimateJsonEncodeLengthExpressionFromType
                                    { encodeValueExpression = typeArgLocalName
                                    , typeArgLocalName = typeArgLocalName ++ "_"
                                    }
                                    ( typeArgument, [] )
                        in
                        { encode =
                            "(\\"
                                ++ typeArgLocalName
                                ++ " -> "
                                ++ typeArgumentExpressions.estimateExpression
                                ++ ")"
                        }
                    )

        typeArgumentsEncodeExpressionsText =
            typeArgumentsExpressions
                |> List.map .encode
                |> String.join " "

        continueWithLocalNameAndCommonPrefix localName =
            { estimateExpression =
                [ estimateJsonEncodeLengthFunctionNamePrefix ++ localName
                , typeArgumentsEncodeExpressionsText
                , encodeValueExpression
                ]
                    |> List.filter (String.isEmpty >> not)
                    |> String.join " "
            }
    in
    case typeAnnotation of
        ChoiceElmType choice ->
            let
                typeNameRepresentation =
                    jsonConverterFunctionNameCommonPartFromTypeName choice
            in
            { estimateExpression =
                [ estimateJsonEncodeLengthFunctionNamePrefix ++ typeNameRepresentation
                , typeArgumentsEncodeExpressionsText
                , encodeValueExpression
                ]
                    |> List.filter (String.isEmpty >> not)
                    |> String.join " "
            }

        RecordElmType record ->
            let
                fieldsExpressions =
                    record.fields
                        |> List.map
                            (\( fieldName, fieldType ) ->
                                let
                                    fieldExpression =
                                        estimateJsonEncodeLengthExpressionFromType
                                            { encodeValueExpression = encodeValueExpression ++ "." ++ fieldName
                                            , typeArgLocalName = typeArgLocalName
                                            }
                                            ( fieldType, [] )
                                in
                                { fieldName = fieldName
                                , encode =
                                    "( "
                                        ++ String.fromInt (String.length fieldName + 3)
                                        ++ "\n + "
                                        ++ String.trimLeft (indentElmCodeLines 1 fieldExpression.estimateExpression)
                                        ++ "\n  )"
                                }
                            )

                estimateListExpression =
                    "[ "
                        ++ (fieldsExpressions |> List.map .encode |> String.join "\n, ")
                        ++ "\n]"
            in
            { estimateExpression =
                [ "List.sum"
                , indentElmCodeLines 1 estimateListExpression
                , " + 3"
                ]
                    |> String.join "\n"
            }

        InstanceElmType instance ->
            estimateJsonEncodeLengthExpressionFromType
                { encodeValueExpression = encodeValueExpression, typeArgLocalName = typeArgLocalName }
                ( instance.instantiated, instance.arguments )

        TupleElmType tuple ->
            let
                itemsNames =
                    List.range 0 (List.length tuple - 1)
                        |> List.map (String.fromInt >> (++) "item_")

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
                                        estimateJsonEncodeLengthExpressionFromType
                                            { encodeValueExpression =
                                                "("
                                                    ++ getItemFunction
                                                    ++ " "
                                                    ++ encodeValueExpression
                                                    ++ ")"
                                            , typeArgLocalName = typeArgLocalName
                                            }
                                            ( itemType, [] )
                                in
                                { localName = localName
                                , encode = itemExpression.estimateExpression
                                }
                            )

                estimateListExpression =
                    "[ "
                        ++ (itemsExpressions |> List.map .encode |> String.join "\n, ")
                        ++ "\n]"
            in
            { estimateExpression =
                [ "List.sum"
                , indentElmCodeLines 1 estimateListExpression
                ]
                    |> String.join "\n"
            }

        GenericType name ->
            let
                functionsNames =
                    jsonConverterFunctionNameFromTypeParameterName name
            in
            { estimateExpression = functionsNames.encodeName ++ " " ++ encodeValueExpression
            }

        UnitType ->
            { estimateExpression = "3"
            }

        LeafElmType leaf ->
            case leaf of
                StringLeaf ->
                    { estimateExpression = "String.length " ++ encodeValueExpression
                    }

                IntLeaf ->
                    { estimateExpression = "estimateJsonEncodeLength_int " ++ encodeValueExpression
                    }

                BoolLeaf ->
                    { estimateExpression = "2"
                    }

                FloatLeaf ->
                    { estimateExpression = "17"
                    }

                BytesLeaf ->
                    { estimateExpression = "Bytes.width " ++ encodeValueExpression
                    }

                ListLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertListFunctionNameCommonPart

                ArrayLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertArrayFunctionNameCommonPart

                SetLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertSetFunctionNameCommonPart

                ResultLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertResultFunctionNameCommonPart

                MaybeLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertMaybeFunctionNameCommonPart

                DictLeaf ->
                    continueWithLocalNameAndCommonPrefix jsonConvertDictFunctionNameCommonPart

                JsonEncodeValueLeaf ->
                    { estimateExpression = "String.length (Json.Encode.encode 0 " ++ encodeValueExpression ++ ")"
                    }


estimateSerializedSizeFunctionFromChoiceType :
    { choiceTypeName : String, encodeValueExpression : String, typeArgLocalName : String }
    -> ElmChoiceTypeStruct
    -> { name : String, text : String }
estimateSerializedSizeFunctionFromChoiceType { choiceTypeName, encodeValueExpression, typeArgLocalName } choiceType =
    let
        encodeParametersText =
            choiceType.parameters
                |> List.map (jsonConverterFunctionNameFromTypeParameterName >> .encodeName)
                |> String.join " "

        moduleName =
            moduleNameFromTypeName choiceTypeName

        typeNameRepresentation =
            jsonConverterFunctionNameCommonPartFromTypeName choiceTypeName

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
                                                    estimateJsonEncodeLengthExpressionFromType
                                                        { encodeValueExpression = argumentLocalName
                                                        , typeArgLocalName = typeArgLocalName
                                                        }
                                                        ( tagParamType, [] )
                                            in
                                            { localName = argumentLocalName
                                            , encode = tagParamExpr.estimateExpression
                                            }
                                        )

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
                                "( "
                                    ++ String.fromInt (String.length tagName + 3)
                                    ++ " + List.sum ["
                                    ++ (if tagParametersExpressions == [] then
                                            ""

                                        else
                                            " " ++ (tagParametersExpressions |> List.map .encode |> String.join ", ") ++ " "
                                       )
                                    ++ "] )"
                        in
                        { encode =
                            [ encodeFirstLine
                            , indentElmCodeLines 1 encodeSecondLine
                            ]
                                |> String.join "\n"
                        }
                    )

        estimateSizeListExpression =
            tagsExpressions |> List.map .encode |> String.join "\n"

        estimateSizeExpression =
            [ "case " ++ encodeValueExpression ++ " of"
            , indentElmCodeLines 1 estimateSizeListExpression
            ]
                |> String.join "\n"

        estimateSizeFunctionName =
            estimateJsonEncodeLengthFunctionNamePrefix ++ typeNameRepresentation
    in
    { name = estimateSizeFunctionName
    , text =
        [ [ estimateSizeFunctionName
          , encodeParametersText
          , encodeValueExpression
          , "="
          ]
            |> List.filter (String.isEmpty >> not)
            |> String.join " "
        , indentElmCodeLines 1 estimateSizeExpression
        ]
            |> String.join "\n"
    }


jsonConverterFunctionNameFromTypeParameterName : String -> { encodeName : String, decodeName : String }
jsonConverterFunctionNameFromTypeParameterName paramName =
    { encodeName = jsonEncodeFunctionNamePrefix ++ "type_parameter_" ++ paramName
    , decodeName = jsonDecodeFunctionNamePrefix ++ "type_parameter_" ++ paramName
    }


jsonConverterFunctionNameCommonPartFromTypeName : String -> String
jsonConverterFunctionNameCommonPartFromTypeName =
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

                ArrayLeaf ->
                    "Array.Array"

                SetLeaf ->
                    "Set.Set"

                ResultLeaf ->
                    "Result"

                MaybeLeaf ->
                    "Maybe"

                DictLeaf ->
                    "Dict.Dict"

                JsonEncodeValueLeaf ->
                    "Json.Encode.Value"


parseElmTypeLeavesNames : Dict.Dict String LeafElmTypeStruct
parseElmTypeLeavesNames =
    [ ( "String", StringLeaf )
    , ( "Int", IntLeaf )
    , ( "Bool", BoolLeaf )
    , ( "Float", FloatLeaf )
    , ( "Bytes.Bytes", BytesLeaf )
    , ( "List", ListLeaf )
    , ( "Array.Array", ArrayLeaf )
    , ( "Set.Set", SetLeaf )
    , ( "Result", ResultLeaf )
    , ( "Maybe", MaybeLeaf )
    , ( "Dict.Dict", DictLeaf )
    , ( "Json.Encode.Value", JsonEncodeValueLeaf )
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


jsonConverterSupportingFunctionsTexts : List GenerateFunctionFromTypeResult
jsonConverterSupportingFunctionsTexts =
    (jsonConverterSupportingFunctionsTextsWithCommonNamePattern
        |> List.concatMap
            (\supportingFunction ->
                [ { name =
                        jsonEncodeFunctionNamePrefix
                            ++ supportingFunction.functionNameCommonPart
                  , afterName = supportingFunction.encodeSyntax
                  }
                , { name =
                        jsonDecodeFunctionNamePrefix
                            ++ supportingFunction.functionNameCommonPart
                  , afterName = supportingFunction.decodeSyntax
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
        |> List.map
            (\function ->
                { functionName = function.functionName
                , functionText = function.functionText
                , modulesToImport = Set.empty
                }
            )


estimateJsonEncodeLengthSupportingFunctionsTexts : List GenerateFunctionFromTypeResult
estimateJsonEncodeLengthSupportingFunctionsTexts =
    ((estimateJsonEncodeLengthSupportingFunctionsTextsWithCommonNamePattern
        |> List.concatMap
            (\supportingFunction ->
                [ { name =
                        estimateJsonEncodeLengthFunctionNamePrefix
                            ++ supportingFunction.functionNameCommonPart
                  , afterName = supportingFunction.encodeSyntax
                  }
                ]
            )
     )
        ++ [ { name = "estimateJsonEncodeLength_int"
             , afterName = """ integer =
    if 1000 * 1000 * 1000 < abs integer
    then 12
    else if 1000 * 1000 < abs integer
    then 9
    else if 1000 < abs integer
    then 6
    else 3
                """
             }
           ]
        |> List.map
            (\function ->
                { functionName = function.name
                , functionText = function.name ++ " " ++ function.afterName
                }
            )
    )
        |> List.map
            (\function ->
                { functionName = function.functionName
                , functionText = function.functionText
                , modulesToImport = Set.empty
                }
            )


jsonConverterSupportingFunctionsTextsWithCommonNamePattern :
    List
        { functionNameCommonPart : String
        , encodeSyntax : String
        , decodeSyntax : String
        }
jsonConverterSupportingFunctionsTextsWithCommonNamePattern =
    [ { functionNameCommonPart = jsonConvertMaybeFunctionNameCommonPart
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
    , { functionNameCommonPart = jsonConvertListFunctionNameCommonPart
      , encodeSyntax = """ = Json.Encode.list"""
      , decodeSyntax = """ = Json.Decode.list"""
      }
    , { functionNameCommonPart = jsonConvertArrayFunctionNameCommonPart
      , encodeSyntax = """ = Json.Encode.array"""
      , decodeSyntax = """ = Json.Decode.array"""
      }
    , { functionNameCommonPart = jsonConvertSetFunctionNameCommonPart
      , encodeSyntax = """encoder =
    Set.toList >> Json.Encode.list encoder"""
      , decodeSyntax = """decoder =
    Json.Decode.list decoder |> Json.Decode.map Set.fromList"""
      }
    , { functionNameCommonPart = jsonConvertDictFunctionNameCommonPart
      , encodeSyntax = """encodeKey encodeValue =
    Dict.toList >> Json.Encode.list (""" ++ jsonEncodeFunctionNamePrefix ++ jsonConvertTupleFunctionNameCommonPart ++ "2 encodeKey encodeValue)"
      , decodeSyntax = """decodeKey decodeValue =
        (Json.Decode.list (""" ++ jsonDecodeFunctionNamePrefix ++ jsonConvertTupleFunctionNameCommonPart ++ """2 decodeKey decodeValue))
            |> Json.Decode.map Dict.fromList"""
      }
    , { functionNameCommonPart = jsonConvertResultFunctionNameCommonPart
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
    , { functionNameCommonPart = jsonConvertTupleFunctionNameCommonPart ++ "2"
      , encodeSyntax = """encodeA encodeB ( a, b ) =
    [ a |> encodeA, b |> encodeB ]
        |> Json.Encode.list identity"""
      , decodeSyntax = """decodeA decodeB =
    Json.Decode.map2 (\\a b -> ( a, b ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)"""
      }
    , { functionNameCommonPart = jsonConvertTupleFunctionNameCommonPart ++ "3"
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


estimateJsonEncodeLengthSupportingFunctionsTextsWithCommonNamePattern :
    List
        { functionNameCommonPart : String
        , encodeSyntax : String
        }
estimateJsonEncodeLengthSupportingFunctionsTextsWithCommonNamePattern =
    [ { functionNameCommonPart = jsonConvertMaybeFunctionNameCommonPart
      , encodeSyntax = """encodeJust valueToEncode =
    case valueToEncode of
        Nothing ->
            3

        Just just ->
            encodeJust just + 3
"""
      }
    , { functionNameCommonPart = jsonConvertListFunctionNameCommonPart
      , encodeSyntax = """ estimateItem =
        List.map estimateItem >> List.sum """
      }
    , { functionNameCommonPart = jsonConvertArrayFunctionNameCommonPart
      , encodeSyntax = """ estimateItem =
        Array.toList >> List.map estimateItem >> List.sum """
      }
    , { functionNameCommonPart = jsonConvertSetFunctionNameCommonPart
      , encodeSyntax = """ estimateItem =
        Set.toList >> List.map estimateItem >> List.sum """
      }
    , { functionNameCommonPart = jsonConvertDictFunctionNameCommonPart
      , encodeSyntax =
            """estimateKey estimateValue =
        Dict.toList >> List.map (\\( key, value ) -> estimateKey key + estimateValue value) >> List.sum """
      }
    , { functionNameCommonPart = jsonConvertResultFunctionNameCommonPart
      , encodeSyntax = """estimateErr estimateOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            estimateErr valueToEncodeError

        Ok valueToEncodeOk ->
            estimateOk valueToEncodeOk"""
      }
    , { functionNameCommonPart = jsonConvertTupleFunctionNameCommonPart ++ "2"
      , encodeSyntax = """estimateA estimateB ( a, b ) =
    estimateA a + estimateB b
    """
      }
    , { functionNameCommonPart = jsonConvertTupleFunctionNameCommonPart ++ "3"
      , encodeSyntax = """estimateA estimateB estimateC ( a, b, c ) =
    estimateA a + estimateB b + estimateC c
    """
      }
    ]


jsonConvertMaybeFunctionNameCommonPart : String
jsonConvertMaybeFunctionNameCommonPart =
    "_generic_Maybe"


jsonConvertListFunctionNameCommonPart : String
jsonConvertListFunctionNameCommonPart =
    "_generic_List"


jsonConvertArrayFunctionNameCommonPart : String
jsonConvertArrayFunctionNameCommonPart =
    "_generic_Array"


jsonConvertSetFunctionNameCommonPart : String
jsonConvertSetFunctionNameCommonPart =
    "_generic_Set"


jsonConvertDictFunctionNameCommonPart : String
jsonConvertDictFunctionNameCommonPart =
    "_generic_Dict"


jsonConvertResultFunctionNameCommonPart : String
jsonConvertResultFunctionNameCommonPart =
    "_generic_Result"


jsonConvertTupleFunctionNameCommonPart : String
jsonConvertTupleFunctionNameCommonPart =
    "_tuple_"


jsonEncodeParamName : String
jsonEncodeParamName =
    "valueToEncode"


estimateJsonEncodeLengthParamName : String
estimateJsonEncodeLengthParamName =
    "valueToEncode"


jsonEncodeFunctionNamePrefix : String
jsonEncodeFunctionNamePrefix =
    "jsonEncode_"


jsonDecodeFunctionNamePrefix : String
jsonDecodeFunctionNamePrefix =
    "jsonDecode_"


estimateJsonEncodeLengthFunctionNamePrefix : String
estimateJsonEncodeLengthFunctionNamePrefix =
    "estimateJsonEncodeLength_"


findModuleByName : List String -> Dict.Dict (List String) SourceParsedElmModule -> Maybe ( List String, SourceParsedElmModule )
findModuleByName moduleName =
    Dict.toList >> List.filter (Tuple.second >> .moduleName >> (==) moduleName) >> List.head


elmModulesDictFromAppFiles : AppFiles -> Dict.Dict (List String) (Result String SourceParsedElmModule)
elmModulesDictFromAppFiles =
    Dict.filter
        (List.reverse
            >> List.head
            >> Maybe.map (String.toLower >> String.endsWith ".elm")
            >> Maybe.withDefault False
            >> always
        )
        >> Dict.map
            (\_ ->
                stringFromFileContent
                    >> Maybe.map Ok
                    >> Maybe.withDefault (Err "Failed to decode file content as string")
                    >> Result.andThen
                        (\fileContentAsString ->
                            parseElmModuleText fileContentAsString
                                |> Result.mapError Parser.deadEndsToString
                                |> Result.map
                                    (\parsedSyntax ->
                                        { fileText = fileContentAsString
                                        , parsedSyntax = parsedSyntax
                                        , moduleName = Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedSyntax.moduleDefinition)
                                        }
                                    )
                        )
            )


elmModulesDictFromModuleTexts : (List String -> List String) -> List String -> Result String (Dict.Dict (List String) SourceParsedElmModule)
elmModulesDictFromModuleTexts filePathFromModuleName =
    List.map
        (\moduleText ->
            parseElmModuleText moduleText
                |> Result.map (Tuple.pair moduleText)
                |> Result.mapError (parserDeadEndsToString moduleText)
        )
        >> Result.Extra.combine
        >> Result.map
            (List.map
                (\( moduleText, moduleSyntax ) ->
                    ( filePathFromModuleName (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value moduleSyntax.moduleDefinition))
                    , { fileText = moduleText
                      , parsedSyntax = moduleSyntax
                      , moduleName = Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value moduleSyntax.moduleDefinition)
                      }
                    )
                )
                >> Dict.fromList
            )


parseJsonConverterDeclarationType :
    Elm.Syntax.Signature.Signature
    -> Result String { isDecoder : Bool, typeAnnotation : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation }
parseJsonConverterDeclarationType signature =
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
    SourceDirectories
    -> AppFiles
    -> ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Function
    -> Result String { valueFunctionText : String, updateInterfaceModuleText : { generatedModuleName : List String } -> String -> Result String String }
prepareReplaceFunctionInSourceFilesModuleText sourceDirs sourceFiles currentModule originalFunctionDeclaration =
    let
        functionName =
            Elm.Syntax.Node.value
                (Elm.Syntax.Node.value (Elm.Syntax.Node.value originalFunctionDeclaration).declaration).name
    in
    case parseSourceFileFunction currentModule (Elm.Syntax.Node.value originalFunctionDeclaration) of
        Err error ->
            Err ("Failed to parse function: " ++ error)

        Ok ( filePathRepresentation, config ) ->
            case findFileTreeNodeWithPathMatchingRepresentationInFunctionName sourceDirs sourceFiles filePathRepresentation of
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
                                                                        (String.join "." generatedModuleName ++ "." ++ valueFunctionName)

                                                                SourceFileTree ->
                                                                    [ String.join "." generatedModuleName ++ "." ++ valueFunctionName
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


sourceFilesInterfaceModuleAddedFunctions : List { functionName : String, mapFunctionLines : { generatedModuleName : List String } -> Maybe (List String) -> List String }
sourceFilesInterfaceModuleAddedFunctions =
    [ sourceFilesInterfaceModuleAddedFunctionMapNode
    , sourceFilesInterfaceModuleAddedFunctionMapBlobs
    ]


sourceFilesInterfaceModuleAddedFunctionMapNode :
    { functionName : String
    , mapFunctionLines : { generatedModuleName : List String } -> Maybe (List String) -> List String
    }
sourceFilesInterfaceModuleAddedFunctionMapNode =
    { functionName = "mapFileTreeNodeFromGenerated"
    , mapFunctionLines = \{ generatedModuleName } -> always (String.split "\n" ("""
mapFileTreeNodeFromGenerated : """ ++ String.join "." generatedModuleName ++ """.FileTreeNode a -> FileTreeNode a
mapFileTreeNodeFromGenerated node =
    case node of
        """ ++ String.join "." generatedModuleName ++ """.BlobNode blob ->
            BlobNode blob

        """ ++ String.join "." generatedModuleName ++ """.TreeNode tree ->
            tree |> List.map (Tuple.mapSecond mapFileTreeNodeFromGenerated) |> TreeNode
"""))
    }


sourceFilesInterfaceModuleAddedFunctionMapBlobs :
    { functionName : String
    , mapFunctionLines : { generatedModuleName : List String } -> Maybe (List String) -> List String
    }
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
    -> SourceDirectories
    -> AppFiles
    -> ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Expression.Function
    ->
        Result
            (List CompilationError)
            { valueFunctionsTexts : List String
            , updateInterfaceModuleText : { generatedModuleName : List String } -> String -> Result String String
            }
prepareReplaceFunctionInElmMakeModuleText dependencies sourceDirs sourceFiles currentModule originalFunctionDeclaration =
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
                    prepareElmMakeFunctionForEmit sourceDirs sourceFiles dependencies { filePathRepresentation = filePathRepresentation }
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
                                                        { sourceExpression = String.join "." generatedModuleName ++ "." ++ leaf.valueFunctionName }
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
    SourceDirectories
    -> AppFiles
    -> List ( DependencyKey, Bytes.Bytes )
    -> { filePathRepresentation : String }
    -> InterfaceElmMakeFunctionLeafConfig
    -> Result CompilationError ElmMakeRecordTreeLeafEmit
prepareElmMakeFunctionForEmit sourceDirs sourceFiles dependencies { filePathRepresentation } config =
    case findFileWithPathMatchingRepresentationInFunctionName sourceDirs sourceFiles filePathRepresentation of
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


findFileWithPathMatchingRepresentationInFunctionName :
    SourceDirectories
    -> AppFiles
    -> String
    -> Result String ( List String, Bytes.Bytes )
findFileWithPathMatchingRepresentationInFunctionName sourceDirs sourceFiles pathPattern =
    findFileTreeNodeWithPathMatchingRepresentationInFunctionName sourceDirs sourceFiles pathPattern
        |> Result.andThen
            (\( matchPath, matchNode ) ->
                case matchNode of
                    FileTree.BlobNode blob ->
                        Ok ( matchPath, blob )

                    FileTree.TreeNode _ ->
                        Err ("This pattern matches path '" ++ pathPattern ++ "' but the node here is a tree, not a file")
            )


findFileTreeNodeWithPathMatchingRepresentationInFunctionName :
    SourceDirectories
    -> AppFiles
    -> String
    -> Result String ( List String, FileTree.FileTreeNode Bytes.Bytes )
findFileTreeNodeWithPathMatchingRepresentationInFunctionName sourceDirs sourceFiles pathPattern =
    let
        fileTree =
            sourceFiles
                |> Dict.toList
                |> List.map (Tuple.mapSecond FileTree.BlobNode)
                |> List.foldl FileTree.setNodeAtPathInSortedFileTree (FileTree.TreeNode [])

        nodesWithRepresentations =
            fileTree
                |> FileTree.getNodeAtPathFromFileTree sourceDirs.elmJsonDirectoryPath
                |> Maybe.withDefault (FileTree.TreeNode [])
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
mapElmModuleWithNameIfExists :
    SourceDirectories
    -> ({ filePath : List String } -> String -> err)
    -> String
    -> (( AppFiles, List String, String ) -> Result err ( AppFiles, String ))
    -> AppFiles
    -> Result err AppFiles
mapElmModuleWithNameIfExists sourceDirs errFromString elmModuleName tryMapModuleText appCode =
    let
        elmModuleFilePath =
            filePathFromElmModuleName sourceDirs (String.split "." elmModuleName)
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


addModulesFromTextToAppFiles : SourceDirectories -> List String -> AppFiles -> AppFiles
addModulesFromTextToAppFiles sourceDirs modulesToAdd sourceFiles =
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
                                        sourceDirs
                                        (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value moduleToAddSyntax.moduleDefinition))
                            in
                            prevFiles
                                |> Dict.insert filePath (fileContentFromString moduleToAdd)
                        )
                    |> Result.withDefault prevFiles
            )
            sourceFiles


filePathFromElmModuleName : SourceDirectories -> List String -> List String
filePathFromElmModuleName sourceDirs elmModuleName =
    case elmModuleName |> List.reverse of
        [] ->
            []

        moduleLastName :: reversedDirectoryNames ->
            sourceDirs.mainSourceDirectoryPath ++ List.reverse ((moduleLastName ++ ".elm") :: reversedDirectoryNames)


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
