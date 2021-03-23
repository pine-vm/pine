module CompileFullstackApp exposing (AppFiles, CompilationError(..), loweredForSourceFiles)

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Node
import Elm.Syntax.Range
import List
import Parser
import String


type alias AppFiles =
    Dict.Dict (List String) Bytes.Bytes


type CompilationError
    = MissingDependencyError DependencyKey
    | OtherCompilationError String


type DependencyKey
    = ElmMakeDependency ElmMakeRequestStructure


type alias ElmMakeRequestStructure =
    { files : AppFiles
    , entryPointFilePath : List String
    , outputType : ElmMakeOutputType
    , enableDebug : Bool
    }


type ElmMakeOutputType
    = ElmMakeOutputTypeHtml
    | ElmMakeOutputTypeJs


{-| TODO: Add dependencies dictionary for `asCompletelyLoweredElmApp`.
This function returns an Err if the needed dependencies for ElmMake are not yet in the dictionary.
The integrating software can then perform the ElmMake, insert it into the dependencies dict and retry.
-}
asCompletelyLoweredElmApp : List ( DependencyKey, Bytes.Bytes ) -> AppFiles -> Result (List CompilationError) AppFiles
asCompletelyLoweredElmApp dependencies sourceFiles =
    Ok sourceFiles


loweredForSourceFiles : List String -> AppFiles -> Result String AppFiles
loweredForSourceFiles compilationInterfaceElmModuleNamePrefixes sourceFiles =
    compilationInterfaceElmModuleNamePrefixes
        |> listFoldlToAggregateResult
            (\compilationInterfaceElmModuleNamePrefix files ->
                mapElmModuleWithNameIfExists
                    (compilationInterfaceElmModuleNamePrefix ++ ".SourceFiles")
                    (mapSourceFilesModuleText sourceFiles)
                    files
            )
            (Ok sourceFiles)


sourceFileFunctionNameStart : String
sourceFileFunctionNameStart =
    "file"


functionNameFlagsSeparator : String
functionNameFlagsSeparator =
    "____"


mapSourceFilesModuleText : AppFiles -> String -> Result String String
mapSourceFilesModuleText sourceFiles moduleText =
    case parseElmModuleText moduleText of
        Err error ->
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString error)

        Ok parsedModule ->
            parsedModule.declarations
                |> List.filterMap
                    (\declaration ->
                        case Elm.Syntax.Node.value declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                Just
                                    (Elm.Syntax.Node.value
                                        (Elm.Syntax.Node.value functionDeclaration.declaration).name
                                    )

                            _ ->
                                Nothing
                    )
                |> listFoldlToAggregateResult
                    (\functionName previousAggregate ->
                        replaceFunctionInSourceFilesModuleText
                            sourceFiles
                            { moduleText = previousAggregate
                            , functionName = functionName
                            }
                            |> Result.mapError
                                (\replaceFunctionError ->
                                    "Failed to replace function '"
                                        ++ functionName
                                        ++ "': "
                                        ++ replaceFunctionError
                                )
                    )
                    (addImportInElmModuleText [ "Base64" ] moduleText)


replaceFunctionInSourceFilesModuleText : AppFiles -> { functionName : String, moduleText : String } -> Result String String
replaceFunctionInSourceFilesModuleText sourceFiles { functionName, moduleText } =
    getDeclarationFromElmModuleTextAndFunctionName
        { functionName = functionName, moduleText = moduleText }
        |> Result.andThen (Maybe.map Ok >> Maybe.withDefault (Err ("Did not find the function '" ++ functionName ++ "'")))
        |> Result.andThen
            (\functionDeclaration ->
                case parseSourceFileFunctionName functionName of
                    Err error ->
                        Err ("Failed to parse function name: " ++ error)

                    Ok { filePathRepresentation, base64 } ->
                        case findFileWithPathMatchingRepresentationInFunctionName sourceFiles filePathRepresentation of
                            Err error ->
                                Err ("Failed to identify file for '" ++ filePathRepresentation ++ "': " ++ error)

                            Ok ( _, fileContent ) ->
                                let
                                    functionLines =
                                        moduleText |> getTextLinesFromRange (Elm.Syntax.Node.range functionDeclaration)

                                    fileContentAsBase64 =
                                        fileContent |> Base64.fromBytes |> Maybe.withDefault "Error encoding to base64"

                                    base64Expression =
                                        "\"" ++ fileContentAsBase64 ++ "\""

                                    fileExpression =
                                        if base64 then
                                            base64Expression

                                        else
                                            [ base64Expression
                                            , "|> Base64.toBytes"
                                            , "|> Maybe.withDefault (\"Failed to convert from base64\" |> Bytes.Encode.string |> Bytes.Encode.encode)"
                                            ]
                                                |> String.join "\n"

                                    newFunctionLines =
                                        List.take 2 functionLines ++ [ indentElmCodeLines 1 fileExpression ]
                                in
                                replaceFunctionInElmModuleText
                                    { functionName = functionName, newFunctionLines = newFunctionLines }
                                    moduleText
            )


getDeclarationFromElmModuleTextAndFunctionName : { moduleText : String, functionName : String } -> Result String (Maybe (Elm.Syntax.Node.Node Elm.Syntax.Expression.Function))
getDeclarationFromElmModuleTextAndFunctionName { moduleText, functionName } =
    moduleText
        |> parseAndMapElmModuleText
            (\parsedModule ->
                parsedModule.declarations
                    |> List.filterMap
                        (\declaration ->
                            case Elm.Syntax.Node.value declaration of
                                Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                    Just (Elm.Syntax.Node.Node (Elm.Syntax.Node.range declaration) functionDeclaration)

                                _ ->
                                    Nothing
                        )
                    |> List.filter
                        (Elm.Syntax.Node.value
                            >> .declaration
                            >> Elm.Syntax.Node.value
                            >> .name
                            >> Elm.Syntax.Node.value
                            >> (==) functionName
                        )
                    |> List.head
                    |> Ok
            )


getTextLinesFromRange : Elm.Syntax.Range.Range -> String -> List String
getTextLinesFromRange range text =
    let
        lines =
            String.lines text
    in
    lines
        |> List.take range.end.row
        |> List.drop (range.start.row - 1)


indentElmCodeLines : Int -> String -> String
indentElmCodeLines level =
    String.lines
        >> List.map ((++) (String.repeat level "    "))
        >> String.join "\n"


findFileWithPathMatchingRepresentationInFunctionName : AppFiles -> String -> Result String ( List String, Bytes.Bytes )
findFileWithPathMatchingRepresentationInFunctionName sourceFiles pathPattern =
    let
        matchingFiles =
            sourceFiles
                |> Dict.toList
                |> List.filter (Tuple.first >> pathMatchesPatternFromFunctionName pathPattern)
    in
    case matchingFiles of
        [ matchingFile ] ->
            Ok matchingFile

        [] ->
            Err
                ("Did not find any source file with a path matching the representation '"
                    ++ pathPattern
                    ++ "'. Here is a list of the available files: "
                    ++ String.join ", " (List.map (String.join "/") (Dict.keys sourceFiles))
                )

        _ ->
            Err
                ("The file path representation '"
                    ++ pathPattern
                    ++ "' is not unique because it matches "
                    ++ String.fromInt (List.length matchingFiles)
                    ++ " of the source files: "
                    ++ String.join ", " (List.map (Tuple.first >> String.join "/") matchingFiles)
                )


replaceFunctionInElmModuleText : { functionName : String, newFunctionLines : List String } -> String -> Result String String
replaceFunctionInElmModuleText { functionName, newFunctionLines } moduleText =
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
                        Err ("Did not find function '" ++ functionName ++ "' in the module text")

                    Just declaration ->
                        let
                            moduleTextOriginalLines =
                                String.lines moduleText

                            declarationRange =
                                Elm.Syntax.Node.range declaration

                            moduleTextLines =
                                (moduleTextOriginalLines |> List.take (declarationRange.start.row - 1))
                                    ++ newFunctionLines
                                    ++ (moduleTextOriginalLines |> List.drop declarationRange.end.row)
                        in
                        Ok (String.join "\n" moduleTextLines)
            )


addImportInElmModuleText : List String -> String -> Result String String
addImportInElmModuleText importModuleName moduleText =
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

                    importStatement =
                        "import " ++ String.join "." importModuleName
                in
                (List.take insertionRow moduleTextLines
                    ++ [ importStatement ]
                    ++ List.drop insertionRow moduleTextLines
                )
                    |> String.join "\n"
                    |> Ok
            )


parseAndMapElmModuleText : (Elm.Syntax.File.File -> Result String ok) -> String -> Result String ok
parseAndMapElmModuleText mapDependingOnParsing moduleText =
    case parseElmModuleText moduleText of
        Err error ->
            Err ("Failed to parse Elm module text: " ++ parserDeadEndsToString error)

        Ok parsedModule ->
            mapDependingOnParsing parsedModule


mapElmModuleWithNameIfExists : String -> (String -> Result String String) -> AppFiles -> Result String AppFiles
mapElmModuleWithNameIfExists elmModuleName tryMapModuleText appCode =
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
                    Err "Failed to decode file content as string"

                Just moduleText ->
                    case tryMapModuleText moduleText of
                        Err mapModuleTextError ->
                            Err ("Failed to map module text: " ++ mapModuleTextError)

                        Ok newModuleText ->
                            appCode
                                |> Dict.insert elmModuleFilePath (Bytes.Encode.encode (Bytes.Encode.string newModuleText))
                                |> Ok


parseSourceFileFunctionName : String -> Result String { filePathRepresentation : String, base64 : Bool }
parseSourceFileFunctionName functionName =
    parseFlagsAndPathPatternFromFunctionName sourceFileFunctionNameStart functionName
        |> Result.map
            (\( flags, filePathRepresentation ) ->
                { filePathRepresentation = filePathRepresentation
                , base64 = flags |> List.member "base64"
                }
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


pathMatchesPatternFromFunctionName : String -> List String -> Bool
pathMatchesPatternFromFunctionName pathPattern path =
    filePathRepresentationInFunctionName path == pathPattern


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


parserDeadEndsToString : List Parser.DeadEnd -> String
parserDeadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map parserDeadEndToString deadEnds))


parserDeadEndToString : Parser.DeadEnd -> String
parserDeadEndToString deadend =
    parserProblemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


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
