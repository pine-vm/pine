module CompileElmApp exposing (..)

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Common
import CompileElmAppListExtra
import Dict
import Elm.Parser
import Elm.Syntax.Declaration
import Elm.Syntax.Encode.TypeAnnotation
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
import FNV1a
import FileTree
import Json.Decode
import Json.Encode
import List
import List.Extra
import Parser
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
    , secondarySourceDirectories : List (List String)
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
    List ( List String, Bytes.Bytes )


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
    , enableOptimize : Bool
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
    SourceParsedElmModule -> Result { supportedDeclarationNames : List String } ProcessEntryPoint


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
    , originalSourceModules : List ( List String, SourceParsedElmModule )
    }


type Range
    = Range ( Int, Int ) ( Int, Int )


defaultEntryPoints : List EntryPointClass
defaultEntryPoints =
    [ entryPointClassFromSetOfEquallyProcessedFunctionNames
        [ "blobMain" ]
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
        [ "main" ]
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
    List String
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
    List String
    -> (Elm.Syntax.Declaration.Declaration -> ProcessEntryPoint)
    -> EntryPointClass
entryPointClassFromSetOfEquallyProcessedNames supportedDeclarationNames processEntryPoint sourceModule =
    let
        declarationsNames : List ( String, Elm.Syntax.Declaration.Declaration )
        declarationsNames =
            sourceModule.parsedSyntax.declarations
                |> List.filterMap
                    (\(Elm.Syntax.Node.Node _ declaration) ->
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
            |> Common.listFind
                (\( name, _ ) ->
                    List.member name supportedDeclarationNames
                )
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
                sourceModules : List ( List String, SourceParsedElmModule )
                sourceModules =
                    arguments.sourceFiles
                        |> elmModulesDictFromAppFiles
                        |> List.filterMap
                            (\( filePath, moduleResult ) ->
                                case moduleResult of
                                    Err _ ->
                                        Nothing

                                    Ok moduleParsed ->
                                        Just ( filePath, moduleParsed )
                            )

                usedCompilationInterfaceModules : List String
                usedCompilationInterfaceModules =
                    sourceModules
                        |> List.concatMap
                            (\( _, sourceModule ) ->
                                List.concatMap
                                    (\compilationInterfacePrefix ->
                                        case sourceModule.moduleName of
                                            [] ->
                                                []

                                            firstDirectory :: afterCommonPrefix ->
                                                if firstDirectory /= compilationInterfacePrefix then
                                                    []

                                                else
                                                    case List.reverse afterCommonPrefix of
                                                        lastPathComponent :: _ ->
                                                            [ lastPathComponent
                                                            , String.join "." afterCommonPrefix
                                                            ]

                                                        [] ->
                                                            []
                                    )
                                    arguments.compilationInterfaceElmModuleNamePrefixes
                            )
                        |> Common.listUnique

                modulesToAdd : List String
                modulesToAdd =
                    usedCompilationInterfaceModules
                        |> List.concatMap
                            (\moduleName ->
                                case Common.assocListGet moduleName compilationInterfaceModuleDependencies of
                                    Nothing ->
                                        []

                                    Just moduleDependencies ->
                                        moduleDependencies
                            )
            in
            {-
               TODO: Reorder lowering so that one lowering stage can reuse results from another one.
               This will require building a dependency graph of lowering stages.
            -}
            arguments.sourceFiles
                |> addModulesFromTextToAppFiles sourceDirs modulesToAdd
                |> applyLoweringUnderPrefixes
                    (mapSourceFilesModuleText sourceDirs)
                    { prefixes = List.map (String.split ".") arguments.compilationInterfaceElmModuleNamePrefixes
                    , moduleNameEnd = [ "SourceFiles" ]
                    }
                    locatedInSourceFilesFromJustFilePath
                    sourceDirs
                |> Result.andThen
                    (applyLoweringUnderPrefixes
                        (mapJsonConvertersModuleText { originalSourceModules = sourceModules, sourceDirs = sourceDirs })
                        { prefixes = List.map (String.split ".") arguments.compilationInterfaceElmModuleNamePrefixes
                        , moduleNameEnd = [ "GenerateJsonConverters" ]
                        }
                        locatedInSourceFilesFromJustFilePath
                        sourceDirs
                    )
                |> Result.mapError (mapLocatedInSourceFiles OtherCompilationError >> List.singleton)
                |> Result.andThen
                    (applyLoweringUnderPrefixes
                        (mapElmMakeModuleText sourceDirs arguments.dependencies)
                        { prefixes = List.map (String.split ".") arguments.compilationInterfaceElmModuleNamePrefixes
                        , moduleNameEnd = [ "ElmMake" ]
                        }
                        (\context ->
                            OtherCompilationError
                                >> locatedInSourceFilesFromJustFilePath context
                                >> List.singleton
                        )
                        sourceDirs
                    )
                |> Result.andThen
                    (loweredForCompilationRoot
                        entryPointClasses
                        { originalSourceModules = sourceModules
                        , compilationRootFilePath = arguments.compilationRootFilePath
                        , interfaceToHostRootModuleName = arguments.interfaceToHostRootModuleName
                        }
                    )


compilationInterfaceModuleDependencies : List ( String, List String )
compilationInterfaceModuleDependencies =
    [ ( "SourceFiles", modulesToAddForBytesCoding )
    , ( "ElmMake", modulesToAddForBytesCoding )
    , ( "GenerateJsonConverters", modulesToAddForBase64Coding )
    ]


findSourceDirectories :
    { a | compilationRootFilePath : List String, sourceFiles : AppFiles }
    -> Result String SourceDirectories
findSourceDirectories arguments =
    let
        searchRecursive : List String -> Result String SourceDirectories
        searchRecursive currentDirectory =
            case Common.assocListGet (currentDirectory ++ [ "elm.json" ]) arguments.sourceFiles of
                Nothing ->
                    if currentDirectory == [] then
                        Err "Did not find elm.json"

                    else
                        searchRecursive
                            (List.take (List.length currentDirectory - 1) currentDirectory)

                Just elmJsonFile ->
                    case stringFromFileContent elmJsonFile of
                        Nothing ->
                            Err "Failed to decode file content as string"

                        Just elmJsonString ->
                            case
                                Json.Decode.decodeString
                                    decodeElmJson
                                    (String.trim elmJsonString)
                            of
                                Err err ->
                                    Err ("Failed to decode elm.json: " ++ Json.Decode.errorToString err)

                                Ok elmJson ->
                                    let
                                        parsedSourceDirs =
                                            List.map parseElmJsonSourceDirectoryPath elmJson.sourceDirectories

                                        compilationRootFilePathFromElmJson =
                                            List.drop
                                                (List.length currentDirectory)
                                                arguments.compilationRootFilePath
                                    in
                                    case
                                        parsedSourceDirs
                                            |> Common.listFind
                                                (\c ->
                                                    {- TODO:
                                                       Technically, the one containing compilationRootFilePath could also be one with parentLevel > 0.
                                                    -}
                                                    (c.parentLevel == 0)
                                                        && CompileElmAppListExtra.isPrefixOf c.subdirectories compilationRootFilePathFromElmJson
                                                )
                                    of
                                        Nothing ->
                                            Err
                                                (String.join " "
                                                    [ "Did not find a matching directory in source-directories"
                                                    , "(currentDirectory: " ++ String.join "/" currentDirectory ++ ")"
                                                    ]
                                                )

                                        Just matchingSourceDirectory ->
                                            let
                                                secondarySourceDirectories =
                                                    parsedSourceDirs
                                                        |> CompileElmAppListExtra.remove matchingSourceDirectory
                                                        |> List.map
                                                            (\parsedDir ->
                                                                List.take (List.length currentDirectory - parsedDir.parentLevel) currentDirectory
                                                                    ++ parsedDir.subdirectories
                                                            )
                                            in
                                            Ok
                                                { mainSourceDirectoryPath =
                                                    currentDirectory ++ matchingSourceDirectory.subdirectories
                                                , elmJsonDirectoryPath = currentDirectory
                                                , secondarySourceDirectories = secondarySourceDirectories
                                                }
    in
    searchRecursive arguments.compilationRootFilePath


parseElmJsonSourceDirectoryPath : String -> { parentLevel : Int, subdirectories : List String }
parseElmJsonSourceDirectoryPath pathInJson =
    if String.contains "\\" pathInJson then
        parseElmJsonSourceDirectoryPath (String.replace "\\" "/" pathInJson)

    else
        let
            pathItems =
                String.split "/" pathInJson
                    |> List.Extra.dropWhile String.isEmpty
        in
        List.foldl
            (\nextSegment { parentLevel, subdirectories } ->
                case nextSegment of
                    ".." ->
                        if 0 < List.length subdirectories then
                            { parentLevel = parentLevel
                            , subdirectories = List.take (List.length subdirectories - 1) subdirectories
                            }

                        else
                            { parentLevel = parentLevel + 1
                            , subdirectories = subdirectories
                            }

                    "." ->
                        { parentLevel = parentLevel, subdirectories = subdirectories }

                    _ ->
                        { parentLevel = parentLevel
                        , subdirectories = subdirectories ++ [ nextSegment ]
                        }
            )
            { parentLevel = 0, subdirectories = [] }
            pathItems


decodeElmJson : Json.Decode.Decoder ElmJson
decodeElmJson =
    Json.Decode.map ElmJson
        (Json.Decode.field "source-directories" (Json.Decode.list Json.Decode.string))


applyLoweringUnderPrefixes :
    (( AppFiles, List String, String ) -> Result err ( AppFiles, String ))
    -> { prefixes : List (List String), moduleNameEnd : List String }
    -> ({ filePath : List String } -> String -> err)
    -> SourceDirectories
    -> AppFiles
    -> Result err AppFiles
applyLoweringUnderPrefixes lowerModule { prefixes, moduleNameEnd } errFromString sourceDirs sourceFiles =
    listFoldlToAggregateResult
        (\prefix ->
            applyLoweringUnderPrefix
                lowerModule
                { prefix = prefix, moduleNameEnd = moduleNameEnd }
                errFromString
                sourceDirs
        )
        (Ok sourceFiles)
        prefixes


{-| Consider restricting the interface of `applyLoweringUnderPrefix` to not support arbitrary changes to app code but only addition of expose syntax and new modules.
-}
applyLoweringUnderPrefix :
    (( AppFiles, List String, String ) -> Result err ( AppFiles, String ))
    -> { prefix : List String, moduleNameEnd : List String }
    -> ({ filePath : List String } -> String -> err)
    -> SourceDirectories
    -> AppFiles
    -> Result err AppFiles
applyLoweringUnderPrefix lowerModule { prefix, moduleNameEnd } errFromString sourceDirs sourceFiles =
    listFoldlToAggregateResult
        (\filePath intermediateFiles ->
            case elmModuleNameFromFilePath sourceDirs filePath of
                Nothing ->
                    Ok intermediateFiles

                Just moduleName ->
                    if not (CompileElmAppListExtra.isPrefixOf prefix moduleName) then
                        Ok intermediateFiles

                    else if not (CompileElmAppListExtra.isSuffixOf moduleNameEnd moduleName) then
                        Ok intermediateFiles

                    else
                        case Common.assocListGet filePath intermediateFiles of
                            Nothing ->
                                Ok intermediateFiles

                            Just elmModuleFile ->
                                case stringFromFileContent elmModuleFile of
                                    Nothing ->
                                        Err (errFromString { filePath = filePath } "Failed to decode file content as string")

                                    Just moduleText ->
                                        case lowerModule ( intermediateFiles, filePath, moduleText ) of
                                            Err err ->
                                                Err err

                                            Ok ( newAppCode, newModuleText ) ->
                                                Ok
                                                    (Common.assocListInsert
                                                        filePath
                                                        (Bytes.Encode.encode (Bytes.Encode.string newModuleText))
                                                        newAppCode
                                                    )
        )
        (Ok sourceFiles)
        (List.map Tuple.first sourceFiles)


loweredForCompilationRoot :
    List EntryPointClass
    ->
        { compilationRootFilePath : List String
        , interfaceToHostRootModuleName : List String
        , originalSourceModules : List ( List String, SourceParsedElmModule )
        }
    -> AppFiles
    -> Result (List (LocatedInSourceFiles CompilationError)) CompilationIterationSuccess
loweredForCompilationRoot entryPointClasses config sourceFiles =
    case Common.assocListGet config.compilationRootFilePath config.originalSourceModules of
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
                entryPointMatchesResults : List (Result { supportedDeclarationNames : List String } ProcessEntryPoint)
                entryPointMatchesResults =
                    entryPointClasses
                        |> List.map ((|>) compilationRootModule)
            in
            case
                entryPointMatchesResults
                    |> Common.listMapFind Result.toMaybe
            of
                Nothing ->
                    let
                        allSupportedDeclarationNames : List String
                        allSupportedDeclarationNames =
                            entryPointMatchesResults
                                |> List.foldl
                                    (\entryPointMatchesResult aggregate ->
                                        case entryPointMatchesResult of
                                            Err { supportedDeclarationNames } ->
                                                List.concat [ supportedDeclarationNames, aggregate ]

                                            Ok _ ->
                                                aggregate
                                    )
                                    []
                                |> Common.listUnique
                    in
                    Ok
                        { compiledFiles = sourceFiles
                        , rootModuleEntryPointKind =
                            Err
                                ("Found no declaration of an entry point. I only support the following "
                                    ++ String.fromInt (List.length allSupportedDeclarationNames)
                                    ++ " names for entry points declarations: "
                                    ++ String.join ", " allSupportedDeclarationNames
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
    Ok
        ( sourceFiles
        , { elmMakeJavaScriptFunctionName =
                ((compilationRootModule.parsedSyntax.moduleDefinition
                    |> Elm.Syntax.Node.value
                    |> Elm.Syntax.Module.moduleName
                 )
                    ++ [ "blobMain" ]
                )
                    |> String.join "."
          }
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
    { originalSourceModules : List ( List String, SourceParsedElmModule )
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
                    (Elm.Syntax.Node.Node _ moduleDefinition) =
                        parsedModule.moduleDefinition

                    interfaceModuleName : Elm.Syntax.ModuleName.ModuleName
                    interfaceModuleName =
                        Elm.Syntax.Module.moduleName moduleDefinition
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
                    |> Common.resultListMapCombine
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

                                Just (Elm.Syntax.Node.Node functionSignatureRange functionSignature) ->
                                    let
                                        functionName : String
                                        functionName =
                                            Elm.Syntax.Node.value functionSignature.name
                                    in
                                    case parseJsonConverterDeclarationType functionSignature of
                                        Err error ->
                                            Err
                                                (LocatedInSourceFiles
                                                    { filePath = moduleFilePath
                                                    , locationInModuleText = functionSignatureRange
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
                                            functionName : String
                                            functionName =
                                                functionToReplace.functionName

                                            functionsNamesInGeneratedModules : { encodeFunction : { name : String, text : String }, decodeFunction : { name : String, text : String } }
                                            functionsNamesInGeneratedModules =
                                                buildJsonConverterFunctionsForTypeAnnotation
                                                    functionToReplace.parsedTypeAnnotation

                                            newFunction : String
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

                                            mapFunctionDeclarationLines : List String -> List String
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
                                        (modulesToImport
                                            |> Common.listUnique
                                            |> List.map (Tuple.pair >> (|>) Nothing)
                                        )
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
    -> ( AppFiles, { generatedModuleName : List String, modulesToImport : List (List String) } )
mapAppFilesToSupportJsonConverters { generatedModuleNamePrefix, sourceDirs } typeAnnotationsBeforeDeduplicating choiceTypes appFilesBefore =
    let
        generatedFunctions : { generatedFunctions : List GenerateFunctionFromTypeResult, modulesToImportForChoiceTypes : List (List String) }
        generatedFunctions =
            buildJsonConverterFunctionsForMultipleTypes
                typeAnnotationsBeforeDeduplicating
                choiceTypes

        modulesToImport : List (List String)
        modulesToImport =
            List.concat
                [ List.concatMap .modulesToImport generatedFunctions.generatedFunctions
                , generatedFunctions.modulesToImportForChoiceTypes
                ]
                |> Common.listUnique

        generatedModuleModulesToImport : List ( List String, Maybe String )
        generatedModuleModulesToImport =
            encodingModuleImportBase64
                :: (modulesToImport
                        |> List.map (\moduleName -> ( moduleName, Nothing ))
                   )

        appFilesAfterExposingChoiceTypesInModules : AppFiles
        appFilesAfterExposingChoiceTypesInModules =
            generatedFunctions.modulesToImportForChoiceTypes
                |> List.foldl (exposeAllInElmModuleInAppFiles sourceDirs) appFilesBefore

        generatedModuleTextWithoutModuleDeclaration : String
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

        generatedModuleName : List String
        generatedModuleName =
            List.concat [ generatedModuleNamePrefix, [ "Generated_JsonConverters" ] ]

        generatedModulePath : List String
        generatedModulePath =
            filePathFromElmModuleName sourceDirs generatedModuleName

        generatedModuleText : String
        generatedModuleText =
            [ "module " ++ String.join "." generatedModuleName ++ " exposing (..)"
            , generatedModuleTextWithoutModuleDeclaration
            ]
                |> String.join "\n\n"

        appFiles : AppFiles
        appFiles =
            appFilesAfterExposingChoiceTypesInModules
                |> updateFileContentAtPath
                    (always (fileContentFromString generatedModuleText))
                    generatedModulePath
    in
    ( appFiles
    , { generatedModuleName = generatedModuleName
      , modulesToImport = generatedModuleName :: modulesToImport
      }
    )


type alias GenerateFunctionsFromTypesConfig =
    { generateFromTypeAnnotation : ElmTypeAnnotation -> List GenerateFunctionFromTypeResult
    , generateFromChoiceType : ( String, ElmChoiceTypeStruct ) -> List GenerateFunctionFromTypeResult
    }


type alias GenerateFunctionFromTypeResult =
    { functionName : String
    , functionText : String
    , modulesToImport : List (List String)
    }


buildJsonConverterFunctionsForMultipleTypes :
    List ElmTypeAnnotation
    -> Dict.Dict String ElmChoiceTypeStruct
    ->
        { generatedFunctions : List GenerateFunctionFromTypeResult
        , modulesToImportForChoiceTypes : List (List String)
        }
buildJsonConverterFunctionsForMultipleTypes typeAnnotations choiceTypes =
    let
        defaultModulesToImportForFunction : List (List String)
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

        generatedFunctionsForTypes :
            { generatedFunctions : List GenerateFunctionFromTypeResult
            , modulesToImportForChoiceTypes : List (List String)
            }
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

        generatedFunctions : List GenerateFunctionFromTypeResult
        generatedFunctions =
            List.concat
                [ generatedFunctionsForTypes.generatedFunctions
                , jsonConverterSupportingFunctionsTexts
                ]
    in
    { generatedFunctions = generatedFunctions
    , modulesToImportForChoiceTypes = generatedFunctionsForTypes.modulesToImportForChoiceTypes
    }


buildEstimateJsonEncodeLengthFunctionsForMultipleTypes :
    List ElmTypeAnnotation
    -> Dict.Dict String ElmChoiceTypeStruct
    ->
        { generatedFunctions : List GenerateFunctionFromTypeResult
        , modulesToImportForChoiceTypes : List (List String)
        }
buildEstimateJsonEncodeLengthFunctionsForMultipleTypes typeAnnotations choiceTypes =
    let
        defaultModulesToImportForFunction : List (List String)
        defaultModulesToImportForFunction =
            [ [ "Dict" ]
            , [ "Set" ]
            , [ "Array" ]
            , [ "Bytes" ]
            ]

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

        generatedFunctions : List GenerateFunctionFromTypeResult
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
        , modulesToImportForChoiceTypes : List (List String)
        }
generateFunctionsForMultipleTypes config typeAnnotationsBeforeDeduplicating choiceTypes =
    let
        modulesToImportForChoiceTypes : List (List String)
        modulesToImportForChoiceTypes =
            choiceTypes
                |> Dict.keys
                |> List.map moduleNameFromTypeName
                |> Set.fromList
                |> Set.toList
                |> List.map (String.split ".")

        generatedFunctionsFromTypeAnnotations : List GenerateFunctionFromTypeResult
        generatedFunctionsFromTypeAnnotations =
            typeAnnotationsBeforeDeduplicating
                |> Common.listUnique
                |> List.concatMap config.generateFromTypeAnnotation

        generatedFunctionsFromChoiceTypes : List GenerateFunctionFromTypeResult
        generatedFunctionsFromChoiceTypes =
            choiceTypes
                |> Dict.toList
                |> List.concatMap config.generateFromChoiceType

        generatedFunctions : List GenerateFunctionFromTypeResult
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
        jsonConverterExpressions : { encodeExpression : String, decodeExpression : String }
        jsonConverterExpressions =
            jsonConverterExpressionFromType
                { encodeValueExpression = jsonEncodeParamName
                , typeArgLocalName = "type_arg"
                }
                ( typeAnnotation, [] )

        typeAnnotationText : String
        typeAnnotationText =
            buildTypeAnnotationText typeAnnotation

        nameCommonPart : String
        nameCommonPart =
            typeAnnotationText
                |> FNV1a.hash
                |> String.fromInt
                |> String.left 10

        encodeFunctionName : String
        encodeFunctionName =
            jsonEncodeFunctionNamePrefix ++ nameCommonPart

        decodeFunctionName : String
        decodeFunctionName =
            jsonDecodeFunctionNamePrefix ++ nameCommonPart

        encodeFunctionText : String
        encodeFunctionText =
            encodeFunctionName
                ++ " "
                ++ jsonEncodeParamName
                ++ " =\n"
                ++ indentElmCodeLines 1 jsonConverterExpressions.encodeExpression

        decodeFunctionText : String
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
        jsonConverterExpressions : { estimateExpression : String }
        jsonConverterExpressions =
            estimateJsonEncodeLengthExpressionFromType
                { encodeValueExpression = estimateJsonEncodeLengthParamName
                , typeArgLocalName = "type_arg"
                }
                ( typeAnnotation, [] )

        typeAnnotationText : String
        typeAnnotationText =
            buildTypeAnnotationText typeAnnotation

        nameCommonPart : String
        nameCommonPart =
            typeAnnotationText
                |> FNV1a.hash
                |> String.fromInt
                |> String.left 10

        encodeFunctionName : String
        encodeFunctionName =
            estimateJsonEncodeLengthFunctionNamePrefix ++ nameCommonPart

        estimateFunctionText : String
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
        mapErrorStringForFunctionDeclaration (Elm.Syntax.Node.Node _ functionDeclaration) =
            let
                (Elm.Syntax.Node.Node _ functionName) =
                    (Elm.Syntax.Node.value functionDeclaration.declaration).name
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
                    |> Common.resultListMapCombine
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

                                generatedModuleTextWithoutModuleDeclaration : String
                                generatedModuleTextWithoutModuleDeclaration =
                                    String.join "\n\n" (Set.toList generatedModuleDeclarations)

                                generatedModuleName : Elm.Syntax.ModuleName.ModuleName
                                generatedModuleName =
                                    List.concat
                                        [ interfaceModuleName
                                        , [ "Generated_SourceFiles" ]
                                        ]

                                generatedModulePath : List String
                                generatedModulePath =
                                    filePathFromElmModuleName sourceDirs generatedModuleName

                                generatedModuleText : String
                                generatedModuleText =
                                    [ "module " ++ String.join "." generatedModuleName ++ " exposing (..)"
                                    , generatedModuleTextWithoutModuleDeclaration
                                    ]
                                        |> String.join "\n\n"

                                appFiles : AppFiles
                                appFiles =
                                    sourceFiles
                                        |> updateFileContentAtPath
                                            (always (fileContentFromString generatedModuleText))
                                            generatedModulePath

                                interfaceModuleDeclaresTypeFileTreeNode : Bool
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

                                addMappingFunctionIfTypeIsPresent : String -> Result String String
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
    case parseElmModuleText moduleText of
        Err err ->
            Err
                [ locatedInSourceFilesFromRange moduleFilePath
                    (Elm.Syntax.Node.Node (syntaxRangeCoveringCompleteString moduleText)
                        (OtherCompilationError
                            ("Failed to parse Elm module text: " ++ parserDeadEndsToString moduleText err)
                        )
                    )
                ]

        Ok parsedModule ->
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
                            interfaceModuleName : Elm.Syntax.ModuleName.ModuleName
                            interfaceModuleName =
                                Elm.Syntax.Module.moduleName
                                    (Elm.Syntax.Node.value parsedModule.moduleDefinition)

                            generatedModuleTextWithoutModuleDeclaration : String
                            generatedModuleTextWithoutModuleDeclaration =
                                functionsToReplaceFunction
                                    |> List.concatMap (Tuple.second >> .valueFunctionsTexts)
                                    |> Set.fromList
                                    |> Set.toList
                                    |> String.join "\n\n"

                            generatedModuleName : Elm.Syntax.ModuleName.ModuleName
                            generatedModuleName =
                                interfaceModuleName ++ [ "Generated_ElmMake" ]

                            generatedModulePath : List String
                            generatedModulePath =
                                filePathFromElmModuleName sourceDirs generatedModuleName

                            generatedModuleText : String
                            generatedModuleText =
                                [ "module " ++ String.join "." generatedModuleName ++ " exposing (..)"
                                , generatedModuleTextWithoutModuleDeclaration
                                ]
                                    |> String.join "\n\n"

                            appFiles : AppFiles
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
                |> Result.mapError (List.map (locatedInSourceFilesFromRange moduleFilePath))


locatedInSourceFilesFromRange : List String -> Elm.Syntax.Node.Node a -> LocatedInSourceFiles a
locatedInSourceFilesFromRange filePath (Elm.Syntax.Node.Node range a) =
    LocatedInSourceFiles
        { filePath = filePath
        , locationInModuleText = range
        }
        a


exposeAllInElmModuleInAppFiles : SourceDirectories -> List String -> AppFiles -> AppFiles
exposeAllInElmModuleInAppFiles sourceDirs moduleName appFiles =
    let
        moduleFilePath : List String
        moduleFilePath =
            filePathFromElmModuleName sourceDirs moduleName
    in
    case
        Common.assocListGet moduleFilePath appFiles
    of
        Nothing ->
            appFiles

        Just fileContent ->
            case stringFromFileContent fileContent of
                Nothing ->
                    appFiles

                Just originalModuleText ->
                    let
                        moduleText =
                            exposeAllInElmModule originalModuleText
                    in
                    appFiles
                        |> Common.assocListInsert
                            moduleFilePath
                            (fileContentFromString moduleText)


exposeAllInElmModule : String -> String
exposeAllInElmModule moduleText =
    case parseElmModuleText moduleText of
        Err _ ->
            moduleText

        Ok parsedModule ->
            let
                (Elm.Syntax.Node.Node _ moduleDefinition) =
                    parsedModule.moduleDefinition

                exposingListNode : Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
                exposingListNode =
                    case moduleDefinition of
                        Elm.Syntax.Module.NormalModule normalModule ->
                            normalModule.exposingList

                        Elm.Syntax.Module.PortModule portModule ->
                            portModule.exposingList

                        Elm.Syntax.Module.EffectModule effectModule ->
                            effectModule.exposingList

                (Elm.Syntax.Node.Node exposingListRange exposingList) =
                    exposingListNode
            in
            case exposingList of
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
        fileContent : Bytes.Bytes
        fileContent =
            appFiles
                |> Common.assocListGet filePath
                |> updateFileContent
    in
    appFiles
        |> Common.assocListInsert filePath fileContent


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
    , tags : List ( String, List ElmTypeAnnotation )
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
    List ( List String, SourceParsedElmModule )
    -> ( ( List String, Elm.Syntax.File.File ), Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Result (LocatedInSourceFiles String) ( List (Elm.Syntax.Node.Node ElmTypeAnnotation), Dict.Dict String ElmChoiceTypeStruct )
parseElmFunctionTypeAndDependenciesRecursivelyFromAnnotation modules ( ( currentModuleFilePath, currentModule ), (Elm.Syntax.Node.Node typeAnnotationRange typeAnnotation) as typeAnnotationNode ) =
    case typeAnnotation of
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
                        (Elm.Syntax.Node.Node typeAnnotationRange >> List.singleton)
                    )


parseElmTypeAndDependenciesRecursivelyFromAnnotation :
    List ( List String, SourceParsedElmModule )
    -> ( ( List String, Elm.Syntax.File.File ), Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation )
    -> Result (LocatedInSourceFiles String) ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
parseElmTypeAndDependenciesRecursivelyFromAnnotation modules ( ( currentModuleFilePath, currentModule ), Elm.Syntax.Node.Node typeAnnotationRange typeAnnotation ) =
    case
        parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal
            { typesToIgnore = Set.empty }
            modules
            ( currentModule, typeAnnotation )
    of
        Ok ok ->
            Ok ok

        Err err ->
            Err
                (LocatedInSourceFiles
                    { filePath = currentModuleFilePath
                    , locationInModuleText = typeAnnotationRange
                    }
                    err
                )


parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal :
    { typesToIgnore : Set.Set String }
    -> List ( List String, SourceParsedElmModule )
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
                |> Common.resultListMapCombine
                    (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldAnnotation )) ->
                        case
                            parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal
                                stack
                                modules
                                ( currentModule, Elm.Syntax.Node.value fieldAnnotation )
                        of
                            Err error ->
                                Err
                                    ("Failed to parse annotation of field '"
                                        ++ fieldName
                                        ++ "': "
                                        ++ error
                                    )

                            Ok ( fieldType, fieldTypeDeps ) ->
                                Ok
                                    ( ( fieldName, fieldType )
                                    , fieldTypeDeps
                                    )
                    )
                |> Result.map listTupleSecondDictUnion
                |> Result.map
                    (\( fields, fieldsDependencies ) -> ( RecordElmType { fields = fields }, fieldsDependencies ))

        Elm.Syntax.TypeAnnotation.Tupled tupled ->
            tupled
                |> List.map (Elm.Syntax.Node.value >> Tuple.pair currentModule)
                |> Common.resultListMapCombine
                    (parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal stack modules)
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
    -> List ( List String, SourceParsedElmModule )
    -> ( Elm.Syntax.File.File, ( Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ), List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation) ) )
    -> Result String ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct )
parseElmTypeAndDependenciesRecursivelyFromAnnotationInternalTyped stack modules ( currentModule, ( Elm.Syntax.Node.Node _ instantiated, argumentsNodes ) ) =
    let
        ( instantiatedModuleAlias, instantiatedLocalName ) =
            instantiated

        instantiatedResult : Result String ( ElmTypeAnnotation, Dict.Dict String ElmChoiceTypeStruct, List String )
        instantiatedResult =
            case
                parseElmTypeLeavesNames
                    |> Common.assocListGet
                        (instantiatedModuleAlias ++ [ instantiatedLocalName ] |> String.join ".")
            of
                Just leaf ->
                    Ok ( LeafElmType leaf, Dict.empty, [] )

                Nothing ->
                    let
                        maybeInstantiatedModule : Maybe Elm.Syntax.File.File
                        maybeInstantiatedModule =
                            if instantiatedModuleAlias == [] then
                                currentModule.imports
                                    |> Common.listMapFind
                                        (\(Elm.Syntax.Node.Node _ moduleImport) ->
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
                                    |> Maybe.withDefault currentModule
                                    |> Just

                            else
                                currentModule.imports
                                    |> Common.listFind
                                        (\(Elm.Syntax.Node.Node _ moduleImport) ->
                                            moduleImport.moduleAlias
                                                |> Maybe.withDefault moduleImport.moduleName
                                                |> Elm.Syntax.Node.value
                                                |> (==) instantiatedModuleAlias
                                        )
                                    |> Maybe.andThen
                                        (\(Elm.Syntax.Node.Node _ matchingImport) ->
                                            modules
                                                |> findModuleByName (Elm.Syntax.Node.value matchingImport.moduleName)
                                                |> Maybe.map (Tuple.second >> .parsedSyntax)
                                        )
                    in
                    case maybeInstantiatedModule of
                        Nothing ->
                            Err
                                ("Did not find referenced module '"
                                    ++ String.join "." instantiatedModuleAlias
                                    ++ "'"
                                    ++ "("
                                    ++ String.fromInt (List.length currentModule.imports)
                                    ++ " imports: "
                                    ++ String.join ", "
                                        (List.map
                                            (\(Elm.Syntax.Node.Node _ imp) ->
                                                let
                                                    (Elm.Syntax.Node.Node _ impModuleName) =
                                                        imp.moduleName
                                                in
                                                String.join "." impModuleName
                                            )
                                            currentModule.imports
                                        )
                                    ++ ", "
                                    ++ String.fromInt (List.length modules)
                                    ++ " available modules: "
                                    ++ String.join ", "
                                        (List.map
                                            (\( availableModuleName, _ ) ->
                                                String.join "." availableModuleName
                                            )
                                            modules
                                        )
                                    ++ ")"
                                )

                        Just instantiatedModule ->
                            instantiatedModule.declarations
                                |> Common.listMapFind
                                    (\(Elm.Syntax.Node.Node _ declaration) ->
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
                                                    typeName : String
                                                    typeName =
                                                        Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value instantiatedModule.moduleDefinition)
                                                            ++ [ Elm.Syntax.Node.value choiceTypeDeclaration.name ]
                                                            |> String.join "."

                                                    genericsNames : List String
                                                    genericsNames =
                                                        choiceTypeDeclaration.generics
                                                            |> List.map Elm.Syntax.Node.value
                                                in
                                                if stack.typesToIgnore |> Set.member typeName then
                                                    Ok
                                                        ( ChoiceElmType typeName
                                                        , Dict.empty
                                                        , genericsNames
                                                        )

                                                else
                                                    choiceTypeDeclaration.constructors
                                                        |> Common.resultListMapCombine
                                                            (\(Elm.Syntax.Node.Node _ constructor) ->
                                                                constructor.arguments
                                                                    |> Common.resultListMapCombine
                                                                        (\constructorArgument ->
                                                                            parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal
                                                                                { typesToIgnore = stack.typesToIgnore |> Set.insert typeName }
                                                                                modules
                                                                                ( instantiatedModule, Elm.Syntax.Node.value constructorArgument )
                                                                                |> Result.mapError ((++) "Failed to parse argument: ")
                                                                        )
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
                                                        |> Result.map listTupleSecondDictUnion
                                                        |> Result.map
                                                            (\( constructors, constructorsDeps ) ->
                                                                ( ChoiceElmType typeName
                                                                , constructorsDeps
                                                                    |> Dict.insert
                                                                        typeName
                                                                        { parameters = genericsNames
                                                                        , tags = constructors
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
                    |> Common.resultListMapCombine
                        (parseElmTypeAndDependenciesRecursivelyFromAnnotationInternal stack modules)
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
                                                (CompileElmAppListExtra.zip genericsNames instanceArguments)
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
    List ( String, ElmTypeAnnotation )
    -> { fields : List ( String, ElmTypeAnnotation ) }
    -> Result String { fields : List ( String, ElmTypeAnnotation ) }
tryConcretizeRecordInstance typeArguments recordType =
    let
        tryConcretizeFieldType : ElmTypeAnnotation -> Result String ElmTypeAnnotation
        tryConcretizeFieldType fieldType =
            case fieldType of
                GenericType genericName ->
                    case Common.assocListGet genericName typeArguments of
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
                                    |> Common.resultListIndexedMapCombine
                                        (\( argIndex, argVal ) ->
                                            case tryConcretizeFieldType argVal of
                                                Ok ok ->
                                                    Ok ok

                                                Err err ->
                                                    Err
                                                        ("Failed to concretize instance argument "
                                                            ++ String.fromInt argIndex
                                                            ++ ": "
                                                            ++ err
                                                        )
                                        )
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
                        |> Common.resultListIndexedMapCombine
                            (\( argIndex, argVal ) ->
                                tryConcretizeFieldType argVal
                                    |> Result.mapError
                                        ((++)
                                            ("Failed to concretize tuple element "
                                                ++ String.fromInt argIndex
                                                ++ ": "
                                            )
                                        )
                            )
                        |> Result.map TupleElmType

                RecordElmType recordElmType ->
                    case
                        recordElmType.fields
                            |> Common.resultListMapCombine
                                (\( fieldName, innerFieldType ) ->
                                    case tryConcretizeFieldType innerFieldType of
                                        Err err ->
                                            Err ("Failed to concretize field " ++ fieldName ++ ": " ++ err)

                                        Ok concretizedFieldType ->
                                            Ok ( fieldName, concretizedFieldType )
                                )
                    of
                        Err err ->
                            Err err

                        Ok fields ->
                            Ok (RecordElmType { fields = fields })

                _ ->
                    Ok fieldType
    in
    case
        recordType.fields
            |> Common.resultListMapCombine
                (\( fieldName, fieldType ) ->
                    case tryConcretizeFieldType fieldType of
                        Err err ->
                            Err err

                        Ok concretizedFieldType ->
                            Ok ( fieldName, concretizedFieldType )
                )
    of
        Err err ->
            Err err

        Ok fields ->
            Ok { fields = fields }


jsonConverterExpressionFromType :
    { encodeValueExpression : String, typeArgLocalName : String }
    -> ( ElmTypeAnnotation, List ElmTypeAnnotation )
    -> { encodeExpression : String, decodeExpression : String }
jsonConverterExpressionFromType { encodeValueExpression, typeArgLocalName } ( typeAnnotation, typeArguments ) =
    let
        typeArgumentsExpressions : List { encode : String, decode : String }
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

        typeArgumentsDecodeExpressionsText : String
        typeArgumentsDecodeExpressionsText =
            typeArgumentsExpressions
                |> List.map .decode
                |> String.join " "

        continueWithAtomInJsonCore : String -> { encodeExpression : String, decodeExpression : String }
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
                    fieldsExpressions : List { fieldName : String, encode : String, decode : String }
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

                    decodeMap : String
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

                                        alternateFieldNames =
                                            [ firstCharToUpperCase field.fieldName ]

                                        fieldMapLines =
                                            [ "( jsonDecode_field_withAlternateNames "
                                                ++ stringExpressionFromString field.fieldName
                                            , [ "["
                                              , alternateFieldNames
                                                    |> List.map stringExpressionFromString
                                                    |> String.join ", "
                                              , "]"
                                              ]
                                                |> String.join " "
                                                |> indentElmCodeLines 1
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
                itemsNames : List String
                itemsNames =
                    List.range 0 (List.length tuple - 1)
                        |> List.map (String.fromInt >> (++) "item_")

                decodeMap : String
                decodeMap =
                    "(\\" ++ (itemsNames |> String.join " ") ++ " -> ( " ++ (itemsNames |> String.join ", ") ++ " ))"

                getItemFunctionFromIndex : Int -> String
                getItemFunctionFromIndex itemIndex =
                    "(\\( " ++ (itemsNames |> String.join ", ") ++ " ) -> item_" ++ String.fromInt itemIndex ++ ")"

                itemsExpressions : List { localName : String, encode : String, decode : String }
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


firstCharToUpperCase : String -> String
firstCharToUpperCase string =
    case String.uncons string of
        Nothing ->
            ""

        Just ( firstChar, rest ) ->
            String.fromChar (Char.toUpper firstChar) ++ rest


jsonConverterFunctionFromChoiceType :
    { choiceTypeName : String, encodeValueExpression : String, typeArgLocalName : String }
    -> ElmChoiceTypeStruct
    -> { encodeFunction : { name : String, text : String }, decodeFunction : { name : String, text : String } }
jsonConverterFunctionFromChoiceType { choiceTypeName, encodeValueExpression, typeArgLocalName } choiceType =
    let
        encodeParametersText : String
        encodeParametersText =
            choiceType.parameters
                |> List.map (jsonConverterFunctionNameFromTypeParameterName >> .encodeName)
                |> String.join " "

        decodeParametersText : String
        decodeParametersText =
            choiceType.parameters
                |> List.map (jsonConverterFunctionNameFromTypeParameterName >> .decodeName)
                |> String.join " "

        moduleName : String
        moduleName =
            moduleNameFromTypeName choiceTypeName

        typeNameRepresentation : String
        typeNameRepresentation =
            jsonConverterFunctionNameCommonPartFromTypeName choiceTypeName

        tagsExpressions : List { encode : String, decode : String }
        tagsExpressions =
            choiceType.tags
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

                            decodeInField : String
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

                            encodeArguments : String
                            encodeArguments =
                                tagParametersExpressions
                                    |> List.map .localName
                                    |> String.join " "

                            encodeFirstLine : String
                            encodeFirstLine =
                                [ moduleName ++ "." ++ tagName
                                , encodeArguments
                                , "->"
                                ]
                                    |> List.filter (String.isEmpty >> not)
                                    |> String.join " "

                            encodeSecondLine : String
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

        encodeListExpression : String
        encodeListExpression =
            tagsExpressions |> List.map .encode |> String.join "\n"

        encodeExpression : String
        encodeExpression =
            [ "case " ++ encodeValueExpression ++ " of"
            , indentElmCodeLines 1 encodeListExpression
            ]
                |> String.join "\n"

        decodeListExpression : String
        decodeListExpression =
            "[ "
                ++ (tagsExpressions |> List.map .decode |> String.join "\n, ")
                ++ "\n]"

        decodeExpression : String
        decodeExpression =
            [ "Json.Decode.oneOf"
            , indentElmCodeLines 1 decodeListExpression
            ]
                |> String.join "\n"

        encodeFunctionName : String
        encodeFunctionName =
            jsonEncodeFunctionNamePrefix ++ typeNameRepresentation

        decodeFunctionName : String
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
        typeArgumentsExpressions : List { encode : String }
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

        typeArgumentsEncodeExpressionsText : String
        typeArgumentsEncodeExpressionsText =
            typeArgumentsExpressions
                |> List.map .encode
                |> String.join " "

        continueWithLocalNameAndCommonPrefix : String -> { estimateExpression : String }
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
        encodeParametersText : String
        encodeParametersText =
            choiceType.parameters
                |> List.map (jsonConverterFunctionNameFromTypeParameterName >> .encodeName)
                |> String.join " "

        moduleName : String
        moduleName =
            moduleNameFromTypeName choiceTypeName

        typeNameRepresentation : String
        typeNameRepresentation =
            jsonConverterFunctionNameCommonPartFromTypeName choiceTypeName

        tagsExpressions : List { encode : String }
        tagsExpressions =
            choiceType.tags
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
    String.map
        (\char ->
            if Char.isAlphaNum char then
                char

            else
                '_'
        )


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


parseElmTypeLeavesNames : List ( String, LeafElmTypeStruct )
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
           , { functionName = "jsonDecode_field_withAlternateNames"
             , functionText = """
jsonDecode_field_withAlternateNames : String -> List String -> Json.Decode.Decoder a -> Json.Decode.Decoder a
jsonDecode_field_withAlternateNames fieldName alternateNames decoder =
    Json.Decode.oneOf
        ((fieldName :: alternateNames)
            |> List.map (\\name -> Json.Decode.field name decoder)
        )"""
             }
           ]
        |> List.map
            (\function ->
                { functionName = function.functionName
                , functionText = function.functionText
                , modulesToImport = []
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
                , modulesToImport = []
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


findModuleByName :
    List String
    -> List ( List String, SourceParsedElmModule )
    -> Maybe ( List String, SourceParsedElmModule )
findModuleByName moduleName =
    Common.listFind (\( _, parsedModule ) -> parsedModule.moduleName == moduleName)


elmModulesDictFromAppFiles : AppFiles -> List ( List String, Result String SourceParsedElmModule )
elmModulesDictFromAppFiles appFiles =
    appFiles
        |> List.concatMap
            (\( filePath, fileContent ) ->
                case filePath |> List.reverse |> List.head of
                    Nothing ->
                        []

                    Just fileName ->
                        if String.endsWith ".elm" (String.toLower fileName) then
                            let
                                fileEntry : Result String SourceParsedElmModule
                                fileEntry =
                                    case stringFromFileContent fileContent of
                                        Nothing ->
                                            Err "Failed to decode file content as string"

                                        Just fileContentAsString ->
                                            case parseElmModuleText fileContentAsString of
                                                Err err ->
                                                    Err ("Failed parsing module text: " ++ Parser.deadEndsToString err)

                                                Ok parsedSyntax ->
                                                    Ok
                                                        { fileText = fileContentAsString
                                                        , parsedSyntax = parsedSyntax
                                                        , moduleName = Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedSyntax.moduleDefinition)
                                                        }
                            in
                            [ ( filePath
                              , fileEntry
                              )
                            ]

                        else
                            []
            )


elmModulesDictFromModuleTexts : (List String -> List String) -> List String -> Result String (List ( List String, SourceParsedElmModule ))
elmModulesDictFromModuleTexts filePathFromModuleName =
    Common.resultListMapCombine
        (\moduleText ->
            parseElmModuleText moduleText
                |> Result.map (Tuple.pair moduleText)
                |> Result.mapError (parserDeadEndsToString moduleText)
        )
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
            )


parseJsonConverterDeclarationType :
    Elm.Syntax.Signature.Signature
    -> Result String { isDecoder : Bool, typeAnnotation : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation }
parseJsonConverterDeclarationType signature =
    let
        (Elm.Syntax.Node.Node _ typeAnnotation) =
            signature.typeAnnotation

        errorValue detail =
            Err
                ("Unexpected type signature ("
                    ++ detail
                    ++ "): "
                    ++ (typeAnnotation
                            |> Elm.Syntax.Encode.TypeAnnotation.encode
                            |> Json.Encode.encode 0
                       )
                )
    in
    case typeAnnotation of
        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( _, genericTypeLocalName )) typeArguments ->
            if genericTypeLocalName /= "Decoder" then
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

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation leftTypeNode (Elm.Syntax.Node.Node _ rightType) ->
            case rightType of
                Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( _, rightNameLocal )) _ ->
                    if rightNameLocal /= "Value" then
                        errorValue "right side of function type is not 'Value'"

                    else
                        Ok
                            { isDecoder = False
                            , typeAnnotation = leftTypeNode
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
        functionName : String
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

                Ok ( pathMatchInfo, fileTreeContent ) ->
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
                                                    |> Common.resultListMapCombine buildTreeEntryExpression
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
                                                        Err
                                                            ("This pattern matches path '"
                                                                ++ String.join "/" pathMatchInfo.absolutePath
                                                                ++ "' but the node here is a tree, not a file"
                                                            )

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

                                                        buildNewFunctionLines : List String -> List String
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
        functionName : String
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

                mapTreeLeaf :
                    InterfaceElmMakeFunctionLeafConfig
                    -> Result CompilationError { emitBlob : RecordTreeEmitElmMake, valueFunctionName : String }
                mapTreeLeaf leafConfig =
                    case
                        prepareElmMakeFunctionForEmit
                            sourceDirs
                            sourceFiles
                            dependencies
                            { filePathRepresentation = filePathRepresentation }
                            leafConfig
                    of
                        Err err ->
                            Err err

                        Ok leafOk ->
                            case continueMapResult leafOk of
                                Err err ->
                                    Err (OtherCompilationError err)

                                Ok ok ->
                                    Ok ok

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

        Ok ( entryPointFileMatch, _ ) ->
            let
                sharedLevels =
                    Common.commonPrefixLength
                        entryPointFileMatch.absolutePath
                        sourceDirs.elmJsonDirectoryPath

                entryPointFilePath =
                    List.repeat (List.length sourceDirs.elmJsonDirectoryPath - sharedLevels) ".."
                        ++ List.drop sharedLevels entryPointFileMatch.absolutePath

                sourceFilesForElmMake =
                    List.filter
                        (\( filePath, _ ) -> includeFilePathInElmMakeRequest filePath)
                        sourceFiles

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
                    , if leafConfig.enableOptimize then
                        [ "optimize" ]

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
            case
                dependencies
                    |> Common.listFind (\( candidateKey, _ ) -> dependencyKeysAreEqual dependencyKey candidateKey)
            of
                Nothing ->
                    Err (MissingDependencyError dependencyKey)

                Just ( _, dependencyValue ) ->
                    let
                        variantName : String
                        variantName =
                            getNameComponentsFromLeafConfig config.elmMakeConfig
                                |> List.sort
                                |> String.join "_"

                        valueFunctionName : String
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
    emitRecordExpressionFromRecordTree
        (\leafMap -> leafMap context)
        interfaceModuleTree


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
    case valueModuleRecordExpressionFromEncodings recordTree.valueModule bytes of
        Err error ->
            Err error

        Ok expression ->
            Ok
                { interfaceModule = recordTree.interfaceModule
                , valueModule = { expression = expression }
                }


buildBase64ElmExpression : Bytes.Bytes -> Result String String
buildBase64ElmExpression bytes =
    case Base64.fromBytes bytes of
        Nothing ->
            Err "Error encoding to base64"

        Just asBase64 ->
            Ok ("\"" ++ asBase64 ++ "\"")


buildUtf8ElmExpression : Bytes.Bytes -> Result String String
buildUtf8ElmExpression bytes =
    case Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes of
        Nothing ->
            Err "Failed to decode bytes as UTF8"

        Just asUtf8 ->
            Ok (stringExpressionFromString asUtf8)


buildUint32Uint8ElmExpression : Bytes.Bytes -> Result String String
buildUint32Uint8ElmExpression bytes =
    let
        uint32_count : Int
        uint32_count =
            Bytes.width bytes // 4

        uint8_offset : Int
        uint8_offset =
            uint32_count * 4

        uint8_count : Int
        uint8_count =
            Bytes.width bytes - uint8_offset

        uint32_decoder : Bytes.Decode.Decoder (List Int)
        uint32_decoder =
            if uint32_count == 0 then
                Bytes.Decode.succeed []

            else
                bytes_decode_list uint32_count (Bytes.Decode.unsignedInt32 Bytes.BE)

        uint8_decoder : Bytes.Decode.Decoder (List Int)
        uint8_decoder =
            if uint8_count == 0 then
                Bytes.Decode.succeed []

            else
                bytes_decode_withOffset uint8_offset (bytes_decode_list uint8_count (Bytes.Decode.unsignedInt32 Bytes.BE))

        expressionFromListInt : List Int -> String
        expressionFromListInt list =
            "[ " ++ String.join ", " (List.map String.fromInt list) ++ " ]"

        recordExpression : List ( String, String ) -> String
        recordExpression fields =
            "{ "
                ++ (fields
                        |> List.map (\( fieldName, fieldValueExpression ) -> fieldName ++ " = " ++ fieldValueExpression)
                        |> String.join ", "
                   )
                ++ " }"

        decoder : Bytes.Decode.Decoder { uint32 : List Int, uint8 : List Int }
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
    let
        escapedChars : List (List Char)
        escapedChars =
            List.map
                escapeCharForStringLiteral
                (String.toList string)
    in
    String.fromList
        (List.concat
            [ [ '"' ]
            , List.concat escapedChars
            , [ '"' ]
            ]
        )


escapeCharForStringLiteral : Char -> List Char
escapeCharForStringLiteral char =
    case char of
        '\\' ->
            [ '\\', '\\' ]

        '\n' ->
            [ '\\', 'n' ]

        '\u{000D}' ->
            [ '\\', 'r' ]

        '"' ->
            [ '\\', '"' ]

        _ ->
            [ char ]


includeFilePathInElmMakeRequest : List String -> Bool
includeFilePathInElmMakeRequest path =
    case List.head (List.reverse path) of
        Nothing ->
            False

        Just fileName ->
            (fileName == "elm.json")
                || String.endsWith ".elm" fileName


declarationWithRangeAsFunctionDeclaration :
    Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.Expression.Function)
declarationWithRangeAsFunctionDeclaration (Elm.Syntax.Node.Node declarationRange declaration) =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            Just (Elm.Syntax.Node.Node declarationRange functionDeclaration)

        _ ->
            Nothing


getTextLinesFromRange : Elm.Syntax.Range.Range -> String -> List String
getTextLinesFromRange rangeRecord text =
    let
        lines : List String
        lines =
            String.lines text

        rangeRecordStart : Elm.Syntax.Range.Location
        rangeRecordStart =
            rangeRecord.start

        rangeRecordEnd : Elm.Syntax.Range.Location
        rangeRecordEnd =
            rangeRecord.end
    in
    sliceRangeFromTextLines
        lines
        (Range
            ( rangeRecordStart.row, rangeRecordStart.column )
            ( rangeRecordEnd.row, rangeRecordEnd.column )
        )


sliceRangeFromTextLines : List String -> Range -> List String
sliceRangeFromTextLines textLines (Range ( startRow, startColumn ) ( endRow, endColumn )) =
    let
        startRowInt : Int
        startRowInt =
            startRow - 1

        endRowInt : Int
        endRowInt =
            endRow - 1

        startColumnInt : Int
        startColumnInt =
            startColumn - 1

        endColumnInt : Int
        endColumnInt =
            endColumn - 1

        rangeRowCount : Int
        rangeRowCount =
            endRowInt - startRowInt

        linesFromStart : List String
        linesFromStart =
            List.drop startRowInt textLines
    in
    if rangeRowCount == 0 then
        case linesFromStart of
            [] ->
                []

            line :: _ ->
                [ String.slice startColumnInt endColumnInt line ]

    else
        let
            firstLine : String
            firstLine =
                case linesFromStart of
                    [] ->
                        ""

                    line :: _ ->
                        String.dropLeft startColumnInt line

            lastLine : String
            lastLine =
                case List.drop rangeRowCount linesFromStart of
                    [] ->
                        ""

                    line :: _ ->
                        String.left endColumnInt line

            middleLines : List String
            middleLines =
                List.take
                    (rangeRowCount - 1)
                    (List.drop 1 linesFromStart)
        in
        List.concat
            [ [ firstLine ]
            , middleLines
            , [ lastLine ]
            ]


indentElmCodeLines : Int -> String -> String
indentElmCodeLines level string =
    let
        indentString : String
        indentString =
            String.repeat level "    "
    in
    String.join
        "\n"
        (String.lines string
            |> List.map (\line -> String.concat [ indentString, line ])
        )


type alias DeclarationFileMatch =
    { relativePath : List String
    , absolutePath : List String
    }


findFileWithPathMatchingRepresentationInFunctionName :
    SourceDirectories
    -> AppFiles
    -> String
    -> Result String ( DeclarationFileMatch, Bytes.Bytes )
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
    -> Result String ( DeclarationFileMatch, FileTree.FileTreeNode Bytes.Bytes )
findFileTreeNodeWithPathMatchingRepresentationInFunctionName sourceDirs sourceFiles pathPattern =
    let
        fileTree : FileTree.FileTreeNode Bytes.Bytes
        fileTree =
            List.foldl
                (\( filePath, fileContent ) ->
                    FileTree.setNodeAtPathInSortedFileTree
                        ( filePath, FileTree.BlobNode fileContent )
                )
                (FileTree.TreeNode [])
                sourceFiles

        sourceDirectoryPaths : List (List String)
        sourceDirectoryPaths =
            sourceDirs.elmJsonDirectoryPath
                :: (sourceDirs.mainSourceDirectoryPath :: sourceDirs.secondarySourceDirectories)

        nodesWithRepresentations : List ( String, ( DeclarationFileMatch, FileTree.FileTreeNode Bytes.Bytes ) )
        nodesWithRepresentations =
            sourceDirectoryPaths
                |> List.concatMap
                    (\sourceDirPath ->
                        case FileTree.getNodeAtPathFromFileTree sourceDirPath fileTree of
                            Nothing ->
                                []

                            Just sourceDirNode ->
                                sourceDirNode
                                    |> FileTree.listNodesWithPath
                                    |> List.map
                                        (\( nodePathRelative, node ) ->
                                            ( filePathRepresentationInFunctionName nodePathRelative
                                            , ( { relativePath = nodePathRelative
                                                , absolutePath = List.concat [ sourceDirPath, nodePathRelative ]
                                                }
                                              , node
                                              )
                                            )
                                        )
                    )

        nodesGroupedByRepresentation : Dict.Dict String (List ( DeclarationFileMatch, FileTree.FileTreeNode Bytes.Bytes ))
        nodesGroupedByRepresentation =
            nodesWithRepresentations
                |> List.map Tuple.first
                |> Set.fromList
                |> Set.toList
                |> List.map
                    (\representation ->
                        ( representation
                        , nodesWithRepresentations
                            |> List.filter (Tuple.first >> (==) representation)
                            |> List.map Tuple.second
                            |> CompileElmAppListExtra.uniqueBy (Tuple.first >> .absolutePath)
                        )
                    )
                |> Dict.fromList
    in
    case Maybe.withDefault [] (Dict.get pathPattern nodesGroupedByRepresentation) of
        [ matchingNode ] ->
            Ok matchingNode

        [] ->
            let
                examplesListItems : List String
                examplesListItems =
                    Dict.keys nodesGroupedByRepresentation

                examplesListItemsForDisplay : List String
                examplesListItemsForDisplay =
                    List.take 8 examplesListItems

                examplesListItemsDisplayItems : List String
                examplesListItemsDisplayItems =
                    List.concat
                        [ examplesListItemsForDisplay
                        , if examplesListItemsForDisplay == examplesListItems then
                            []

                          else
                            [ "..." ]
                        ]
            in
            [ "Did not find any file or directory with a path matching the representation '"
                ++ pathPattern
                ++ "'."
            , "There are "
                ++ String.fromInt (List.length sourceFiles)
                ++ " files and directories available in this compilation: "
                ++ (examplesListItemsDisplayItems |> String.join ", ")
            ]
                |> String.join "\n"
                |> Err

        matchingFiles ->
            Err
                ("The file path representation '"
                    ++ pathPattern
                    ++ "' is not unique because it matches "
                    ++ String.fromInt (List.length matchingFiles)
                    ++ " of the source files nodes: "
                    ++ String.join ", " (List.map (Tuple.first >> .absolutePath >> String.join "/") matchingFiles)
                )


addOrUpdateFunctionInElmModuleText : { functionName : String, mapFunctionLines : Maybe (List String) -> List String } -> String -> Result String String
addOrUpdateFunctionInElmModuleText { functionName, mapFunctionLines } moduleText =
    moduleText
        |> parseAndMapElmModuleText
            (\parsedModule ->
                case
                    parsedModule.declarations
                        |> Common.listFind
                            (\(Elm.Syntax.Node.Node _ declaration) ->
                                case declaration of
                                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                        let
                                            (Elm.Syntax.Node.Node _ functionDeclarationValue) =
                                                functionDeclaration.declaration

                                            (Elm.Syntax.Node.Node _ candidateFunctionName) =
                                                functionDeclarationValue.name
                                        in
                                        functionName == candidateFunctionName

                                    _ ->
                                        False
                            )
                of
                    Nothing ->
                        Ok (String.join "\n\n\n" [ moduleText, String.join "\n" (mapFunctionLines Nothing) ])

                    Just (Elm.Syntax.Node.Node declarationRange _) ->
                        let
                            originalFunctionLines : List String
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
        originalTextLines : List String
        originalTextLines =
            String.lines originalText

        startLines : List String
        startLines =
            originalTextLines |> List.take (range.start.row - 1)

        lineStart : String
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
                    moduleTextLines : List String
                    moduleTextLines =
                        String.lines moduleText

                    insertionRow : Int
                    insertionRow =
                        parsedModule.imports
                            |> List.map (Elm.Syntax.Node.range >> .end >> .row)
                            |> List.maximum
                            |> Maybe.withDefault (Elm.Syntax.Node.range parsedModule.moduleDefinition).end.row

                    importStatementsLines : List String
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


{-| Consider restricting the interface of `mapElmModuleWithNameIfExists` to not support arbitrary changes to app code but only addition of expose syntax and new modules.
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
        elmModuleFilePath : List String
        elmModuleFilePath =
            filePathFromElmModuleName sourceDirs (String.split "." elmModuleName)
    in
    case Common.assocListGet elmModuleFilePath appCode of
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
                                    |> Common.assocListInsert
                                        elmModuleFilePath
                                        (Bytes.Encode.encode (Bytes.Encode.string newModuleText))
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
                (Elm.Syntax.Node.Node _ declaration) =
                    functionDeclaration.declaration

                (Elm.Syntax.Node.Node _ functionName) =
                    declaration.name
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
    ]
        |> Dict.fromList


parseElmMakeModuleFunction :
    ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Expression.Function
    -> Result String ( String, InterfaceElmMakeFunctionConfig )
parseElmMakeModuleFunction currentModule functionDeclaration =
    let
        functionName : String
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
            parseSourceFileFunctionEncodingFromElmTypeAnnotation
                currentModule
                (Elm.Syntax.Node.value signature).typeAnnotation


parseElmMakeFunctionConfigFromTypeAnnotation :
    ( List String, Elm.Syntax.File.File )
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String InterfaceElmMakeFunctionConfig
parseElmMakeFunctionConfigFromTypeAnnotation currentModule typeAnnotationNode =
    case parseElmTypeAndDependenciesRecursivelyFromAnnotation [] ( currentModule, typeAnnotationNode ) of
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
    parseElmMakeFunctionConfigFromRecordTreeInternal
        { enableDebug = False
        , enableOptimize = False
        , outputType = ElmMakeOutputTypeHtml
        }


parseElmMakeFunctionConfigFromRecordTreeInternal :
    InterfaceElmMakeConfig
    -> CompilationInterfaceRecordTreeNode a
    -> Result String InterfaceElmMakeFunctionConfig
parseElmMakeFunctionConfigFromRecordTreeInternal elmMakeConfig recordTree =
    let
        leafFromTree pathPrefix treeNode =
            case prepareRecordTreeEmitForTreeOrBlobUnderPath pathPrefix treeNode of
                Err err ->
                    Err err

                Ok emitBlob ->
                    Ok (RecordTreeLeaf { elmMakeConfig = elmMakeConfig, emitBlob = emitBlob })
    in
    case recordTree of
        RecordTreeLeaf leaf ->
            leafFromTree [] (RecordTreeLeaf ())

        RecordTreeBranch branch ->
            case
                Common.resultListMapCombine
                    (\( branchName, branchValue ) ->
                        (case integrateElmMakeFunctionRecordFieldName branchName elmMakeConfig of
                            Err _ ->
                                leafFromTree [ branchName ] branchValue

                            Ok newElmMakeConfig ->
                                parseElmMakeFunctionConfigFromRecordTreeInternal newElmMakeConfig branchValue
                        )
                            |> Result.map (Tuple.pair branchName)
                    )
                    branch
            of
                Err err ->
                    Err err

                Ok ok ->
                    Ok (RecordTreeBranch ok)


prepareRecordTreeEmitForTreeOrBlobUnderPath :
    List String
    -> CompilationInterfaceRecordTreeNode a
    -> Result String RecordTreeEmit
prepareRecordTreeEmitForTreeOrBlobUnderPath pathPrefix tree =
    let
        mappingBase64 : { fieldName : String, valueModuleBuildExpression : Bytes.Bytes -> Result String String }
        mappingBase64 =
            { fieldName = "base64"
            , valueModuleBuildExpression = buildBase64ElmExpression
            }

        mappingUtf8 : { fieldName : String, valueModuleBuildExpression : Bytes.Bytes -> Result String String }
        mappingUtf8 =
            { fieldName = "utf8"
            , valueModuleBuildExpression = buildUtf8ElmExpression
            }

        mappingUint32Uint8 : { fieldName : String, valueModuleBuildExpression : Bytes.Bytes -> Result String String }
        mappingUint32Uint8 =
            { fieldName = "uint32_uint8"
            , valueModuleBuildExpression = buildUint32Uint8ElmExpression
            }

        fromUint32Uint8ToBytes : String
        fromUint32Uint8ToBytes =
            [ "|> EncodeBytes.bytes_encoder_from_uint32_uint8"
            , "|> Bytes.Encode.encode"
            ]
                |> String.join " "

        mappingBytes : ( { fieldName : String, valueModuleBuildExpression : Bytes.Bytes -> Result String String }, Maybe String )
        mappingBytes =
            ( mappingUint32Uint8, Just fromUint32Uint8ToBytes )

        mapToValueDict : Dict.Dict (List String) ( { fieldName : String, valueModuleBuildExpression : Bytes.Bytes -> Result String String }, Maybe String )
        mapToValueDict =
            [ ( [ "base64" ], ( mappingBase64, Nothing ) )
            , ( [ "bytes" ], mappingBytes )
            , ( [], mappingBytes )
            , ( [ "utf8" ], ( mappingUtf8, Nothing ) )

            -- , ( [ "gzip", "base64" ], ( mappingGZipBase64, Nothing ) )
            ]
                |> Dict.fromList

        attemptMapLeaf : List String -> a -> Result String RecordTreeEmitBlobIntermediateResult
        attemptMapLeaf leafPath _ =
            let
                path : List String
                path =
                    List.concat [ pathPrefix, leafPath ]
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
    case attemptMapRecordTreeLeaves [] attemptMapLeaf tree of
        Err errorList ->
            Err
                ((List.map
                    (\( errorPath, error ) ->
                        "Error at path " ++ String.join "." errorPath ++ ": " ++ error
                    )
                    >> String.join ", "
                 )
                    errorList
                )

        Ok mappedTree ->
            Ok
                { interfaceModule = mappedTree |> mapRecordTreeLeaves .interfaceModule
                , valueModule =
                    \valueBytes ->
                        mappedTree
                            |> enumerateLeavesFromRecordTree
                            |> Common.resultListMapCombine
                                (\( _, leaf ) ->
                                    case leaf.valueModule.buildExpression valueBytes of
                                        Err err ->
                                            Err
                                                ("Failed to build expression for field '"
                                                    ++ leaf.valueModule.fieldName
                                                    ++ "': "
                                                    ++ err
                                                )

                                        Ok expression ->
                                            Ok ( leaf.valueModule.fieldName, { expression = expression } )
                                )
                            |> Result.map Dict.fromList
                }


integrateElmMakeFunctionRecordFieldName : String -> InterfaceElmMakeConfig -> Result String InterfaceElmMakeConfig
integrateElmMakeFunctionRecordFieldName fieldName configBefore =
    case fieldName of
        "debug" ->
            Ok { configBefore | enableDebug = True }

        "optimize" ->
            Ok { configBefore | enableOptimize = True }

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
            []
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
                continueWithErrorUnexpectedInst : String -> Result String value
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
                                case parseSourceFileFunctionEncodingFromTypeAnnotation singleArgument of
                                    Err err ->
                                        Err ("Failed to parse argument: " ++ err)

                                    Ok encoding ->
                                        Ok { isTree = True, encoding = encoding }

                            _ ->
                                continueWithErrorUnexpectedInst
                                    ("Unexpected number of arguments: "
                                        ++ String.fromInt (List.length instance.arguments)
                                    )

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
                fieldsExpressions : List String
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


attemptMapRecordTreeLeaves :
    List String
    -> (List String -> a -> Result e b)
    -> CompilationInterfaceRecordTreeNode a
    -> Result (List ( List String, e )) (CompilationInterfaceRecordTreeNode b)
attemptMapRecordTreeLeaves pathPrefix attemptMapLeaf tree =
    case tree of
        RecordTreeLeaf leaf ->
            case attemptMapLeaf pathPrefix leaf of
                Err error ->
                    Err [ ( [], error ) ]

                Ok ok ->
                    Ok (RecordTreeLeaf ok)

        RecordTreeBranch fields ->
            let
                ( errors, successes ) =
                    fields
                        |> List.map
                            (\( fieldName, fieldNode ) ->
                                attemptMapRecordTreeLeaves (pathPrefix ++ [ fieldName ]) attemptMapLeaf fieldNode
                                    |> Result.map (Tuple.pair fieldName)
                                    |> Result.mapError (List.map (Tuple.mapFirst ((::) fieldName)))
                            )
                        |> listResultPartition
                        |> Tuple.mapFirst List.concat
            in
            if errors == [] then
                Ok (RecordTreeBranch successes)

            else
                Err errors


listResultPartition : List (Result e a) -> ( List e, List a )
listResultPartition results =
    List.foldr
        (\r ( err, succ ) ->
            case r of
                Ok v ->
                    ( err, v :: succ )

                Err e ->
                    ( e :: err, succ )
        )
        ( [], [] )
        results


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


parseInterfaceRecordTree :
    (String -> e)
    -> (String -> leaf -> Result e leaf)
    -> ElmTypeAnnotation
    -> leaf
    -> Result ( List String, e ) (CompilationInterfaceRecordTreeNode leaf)
parseInterfaceRecordTree errorFromString integrateFieldName typeAnnotation seed =
    let
        errorUnsupportedType : String -> Result ( List a, e ) value
        errorUnsupportedType typeText =
            Err ( [], errorFromString ("Unsupported type: " ++ typeText) )
    in
    case typeAnnotation of
        RecordElmType record ->
            record.fields
                |> Common.resultListMapCombine
                    (\( fieldName, fieldType ) ->
                        integrateFieldName fieldName seed
                            |> Result.mapError (Tuple.pair [ fieldName ])
                            |> Result.andThen
                                (\withFieldNameIntegrated ->
                                    case
                                        parseInterfaceRecordTree errorFromString
                                            integrateFieldName
                                            fieldType
                                            withFieldNameIntegrated
                                    of
                                        Err ( errFst, errSnd ) ->
                                            Err ( fieldName :: errFst, errSnd )

                                        Ok ok ->
                                            Ok ( fieldName, ok )
                                )
                    )
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
            List.concatMap
                (\( fieldName, child ) ->
                    child |> enumerateLeavesFromRecordTree |> List.map (Tuple.mapFirst ((::) fieldName))
                )
                children


filePathRepresentationInFunctionName : List String -> String
filePathRepresentationInFunctionName pathItems =
    String.map
        (\char ->
            if Char.isAlphaNum char then
                char

            else
                '_'
        )
        (String.join "_" pathItems)


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
                                |> Common.assocListInsert
                                    filePath
                                    (fileContentFromString moduleToAdd)
                        )
                    |> Result.withDefault prevFiles
            )
            sourceFiles


elmModuleNameFromFilePath : SourceDirectories -> List String -> Maybe (List String)
elmModuleNameFromFilePath sourceDirs filePath =
    case List.reverse filePath of
        [] ->
            Nothing

        fileName :: directoryNameReversed ->
            if not (String.endsWith ".elm" (String.toLower fileName)) then
                Nothing

            else
                let
                    moduleNameLastItem =
                        String.dropRight 4 fileName

                    directoryName =
                        List.reverse directoryNameReversed

                    sourceDirectoryPaths =
                        sourceDirs.mainSourceDirectoryPath
                            :: sourceDirs.secondarySourceDirectories
                in
                Common.listMapFind
                    (\sourceDir ->
                        if CompileElmAppListExtra.isPrefixOf sourceDir directoryName then
                            Just (List.drop (List.length sourceDir) directoryName ++ [ moduleNameLastItem ])

                        else
                            Nothing
                    )
                    sourceDirectoryPaths


filePathFromElmModuleName : SourceDirectories -> List String -> List String
filePathFromElmModuleName sourceDirs elmModuleName =
    case elmModuleName |> List.reverse of
        [] ->
            []

        moduleLastName :: reversedDirectoryNames ->
            sourceDirs.mainSourceDirectoryPath ++ List.reverse ((moduleLastName ++ ".elm") :: reversedDirectoryNames)


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    Elm.Parser.parseToFile


stringFromFileContent : Bytes.Bytes -> Maybe String
stringFromFileContent bytes =
    Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes


fileContentFromString : String -> Bytes.Bytes
fileContentFromString string =
    Bytes.Encode.encode
        (Bytes.Encode.string string)


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
    (a == b) && areAppFilesContentsEqual a.files b.files


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
        lines : List String
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


areAppFilesContentsEqual : AppFiles -> AppFiles -> Bool
areAppFilesContentsEqual a b =
    {- Avoid bug in Elm core library as reported at https://github.com/elm/bytes/issues/15 :
       Convert to other representation before comparing.
       TODO: Remove conversion after switching to platorm that supports bytes comparison.
    -}
    let
        representationForComparison =
            List.map
                (\( _, fileContent ) ->
                    Base64.fromBytes fileContent
                )
    in
    representationForComparison a == representationForComparison b


resultCombineConcatenatingErrors : List (Result err ok) -> Result (List err) (List ok)
resultCombineConcatenatingErrors =
    List.foldl
        (\result previousAggregate ->
            case previousAggregate of
                Err previousErrors ->
                    Err
                        (case result of
                            Err error ->
                                error :: previousErrors

                            Ok _ ->
                                previousErrors
                        )

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
    let
        moduleNameFlat : String
        moduleNameFlat =
            String.join "." moduleName
    in
    case maybeAlias of
        Nothing ->
            "import " ++ moduleNameFlat

        Just aliasString ->
            "import " ++ moduleNameFlat ++ " as " ++ aliasString


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
