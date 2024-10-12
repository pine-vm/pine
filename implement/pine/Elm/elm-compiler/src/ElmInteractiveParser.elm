module ElmInteractiveParser exposing (..)

import Common
import Elm.Parser
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Node
import ElmCompiler
    exposing
        ( InteractiveSubmission(..)
        , ProjectParsedElmFile
        )
import ElmInteractive
    exposing
        ( ElmCoreModulesExtent(..)
        , InteractiveContext(..)
        , SubmissionResponse
        , parsedElmFileRecordFromSeparatelyParsedSyntax
        , submissionResponseFromResponsePineValue
        )
import ElmInteractiveCoreModules
import ElmInteractiveKernelModules
import EncodeElmSyntaxAsPineValue
import Json.Encode
import Parser
import Pine
import Result.Extra


compileEvalContextForElmInteractive : InteractiveContext -> Result String Pine.EvalEnvironment
compileEvalContextForElmInteractive context =
    let
        contextModulesTextsBatches : List (List String)
        contextModulesTextsBatches =
            case context of
                DefaultContext ->
                    [ ElmInteractiveCoreModules.elmCoreModulesTexts
                        ++ ElmInteractiveKernelModules.elmKernelModulesTexts
                    ]

                CustomModulesContext { includeCoreModules, modulesTexts } ->
                    [ case includeCoreModules of
                        Nothing ->
                            []

                        Just OnlyCoreModules ->
                            ElmInteractiveCoreModules.elmCoreModulesTexts

                        Just CoreAndOtherKernelModules ->
                            ElmInteractiveCoreModules.elmCoreModulesTexts
                                ++ ElmInteractiveKernelModules.elmKernelModulesTexts
                    , modulesTexts
                    ]
    in
    contextModulesTextsBatches
        |> List.foldl
            (\batchModuleTexts ->
                Result.andThen
                    (\prevEnvValue ->
                        expandElmInteractiveEnvironmentWithModuleTexts batchModuleTexts
                            |> Result.andThen
                                (\( _, addModules ) ->
                                    Result.map .environment (addModules prevEnvValue)
                                )
                    )
            )
            (Ok (Pine.ListValue []))
        |> Result.map Pine.EvalEnvironment


expandElmInteractiveEnvironmentWithModuleTexts :
    List String
    ->
        Result
            String
            ( List ProjectParsedElmFile
            , Pine.Value -> Result String { addedModules : List ( List String, Pine.Value ), environment : Pine.Value }
            )
expandElmInteractiveEnvironmentWithModuleTexts contextModulesTexts =
    contextModulesTexts
        |> Common.resultListMapCombine parsedElmFileFromOnlyFileText
        |> Result.map
            (\parsedModules ->
                ( parsedModules
                , \environmentBefore ->
                    ElmCompiler.expandElmInteractiveEnvironmentWithModules environmentBefore parsedModules
                )
            )


submissionInInteractive : InteractiveContext -> List String -> String -> Result String SubmissionResponse
submissionInInteractive context previousSubmissions submission =
    case compileEvalContextForElmInteractive context of
        Err error ->
            Err ("Failed to prepare the initial context: " ++ error)

        Ok initialContext ->
            submissionWithHistoryInInteractive initialContext previousSubmissions submission


submissionWithHistoryInInteractive : Pine.EvalEnvironment -> List String -> String -> Result String SubmissionResponse
submissionWithHistoryInInteractive initialContext previousSubmissions submission =
    case previousSubmissions of
        [] ->
            submissionInInteractiveInPineContext initialContext submission
                |> Result.map Tuple.second

        firstSubmission :: remainingPreviousSubmissions ->
            case submissionInInteractiveInPineContext initialContext firstSubmission of
                Err _ ->
                    submissionWithHistoryInInteractive initialContext remainingPreviousSubmissions submission

                Ok ( expressionContext, _ ) ->
                    submissionWithHistoryInInteractive expressionContext remainingPreviousSubmissions submission


submissionInInteractiveInPineContext : Pine.EvalEnvironment -> String -> Result String ( Pine.EvalEnvironment, SubmissionResponse )
submissionInInteractiveInPineContext (Pine.EvalEnvironment envValue) submission =
    compileInteractiveSubmission envValue submission
        |> Result.andThen
            (\pineExpression ->
                case Pine.evaluateExpression (Pine.EvalEnvironment envValue) pineExpression of
                    Err error ->
                        Err ("Failed to evaluate expression:\n" ++ Pine.displayStringFromPineError error)

                    Ok (Pine.BlobValue _) ->
                        Err "Type mismatch: Pine expression evaluated to a blob"

                    Ok (Pine.ListValue [ newState, responseValue ]) ->
                        submissionResponseFromResponsePineValue responseValue
                            |> Result.map (Tuple.pair (Pine.EvalEnvironment newState))

                    Ok (Pine.ListValue resultList) ->
                        Err
                            ("Type mismatch: Pine expression evaluated to a list with unexpected number of elements: "
                                ++ String.fromInt (List.length resultList)
                                ++ " instead of 2"
                            )
            )


{-| The expression evaluates to a list with two elements:
The first element contains the new interactive session state for the possible next submission.
The second element contains the response, the value to display to the user.
-}
compileInteractiveSubmission : Pine.Value -> String -> Result String Pine.Expression
compileInteractiveSubmission environment submission =
    case parseInteractiveSubmissionFromString submission of
        Err error ->
            Ok
                (ElmCompiler.buildExpressionForNewStateAndResponse
                    { newStateExpression = Pine.environmentExpr
                    , responseExpression =
                        ElmCompiler.responseExpressionFromString ("Failed to parse submission: " ++ error)
                    }
                )

        Ok parseOk ->
            ElmCompiler.compileParsedInteractiveSubmission environment parseOk


parsedElmFileFromOnlyFileText : String -> Result String ProjectParsedElmFile
parsedElmFileFromOnlyFileText fileText =
    case parseElmModuleText fileText of
        Err parseError ->
            [ [ "Failed to parse the module text with " ++ String.fromInt (List.length parseError) ++ " errors:" ]
            , parseError
                |> List.map parserDeadEndToString
            , [ "Module text was as follows:"
              , fileText
              ]
            ]
                |> List.concat
                |> String.join "\n"
                |> Err

        Ok parsedModule ->
            Ok
                (parsedElmFileRecordFromSeparatelyParsedSyntax
                    ( fileText, parsedModule )
                )


parseElmModuleTextToJson : String -> String
parseElmModuleTextToJson elmModule =
    let
        jsonValue =
            case parseElmModuleText elmModule of
                Err _ ->
                    Json.Encode.object
                        [ ( "Err"
                          , Json.Encode.string "Failed to parse this as module text"
                          )
                        ]

                Ok file ->
                    Json.Encode.object
                        [ ( "Ok"
                          , Elm.Syntax.File.encode file
                          )
                        ]
    in
    Json.Encode.encode 0 jsonValue


parseElmModuleTextToPineValue : String -> Result String Pine.Value
parseElmModuleTextToPineValue elmModule =
    case parseElmModuleText elmModule of
        Err _ ->
            Err "Failed to parse this as module text"

        Ok file ->
            case EncodeElmSyntaxAsPineValue.encodeElmSyntaxFileAsPineValue file of
                Err error ->
                    Err ("Failed to encode the parsed module as Pine value: " ++ error)

                Ok pineValue ->
                    Ok pineValue


parseInteractiveSubmissionFromString : String -> Result String InteractiveSubmission
parseInteractiveSubmissionFromString submission =
    let
        looksLikeDeclaration =
            case String.split "=" submission of
                leftOfEquals :: betweenFirstAndSecondEquals :: _ ->
                    if betweenFirstAndSecondEquals == "" then
                        -- Is operator '=='
                        False

                    else
                        case String.toList (String.reverse (String.trim leftOfEquals)) of
                            [] ->
                                False

                            lastCharBeforeEquals :: _ ->
                                (Char.isAlphaNum lastCharBeforeEquals || lastCharBeforeEquals == '_')
                                    && not (String.startsWith "let " (String.trim (String.replace "\n" " " leftOfEquals)))
                                    && not (String.contains "{" leftOfEquals)

                _ ->
                    False
    in
    if looksLikeDeclaration then
        parseDeclarationFromString submission
            |> Result.mapError parserDeadEndsToString
            |> Result.Extra.join
            |> Result.map DeclarationSubmission
            |> Result.mapError ((++) "Failed to parse as declaration: ")

    else
        parseExpressionFromString submission
            |> Result.mapError parserDeadEndsToString
            |> Result.Extra.join
            |> Result.map ExpressionSubmission
            |> Result.mapError ((++) "Failed to parse as expression: ")


parseExpressionFromString : String -> Result (List Parser.DeadEnd) (Result String Elm.Syntax.Expression.Expression)
parseExpressionFromString expressionCode =
    -- https://github.com/stil4m/elm-syntax/issues/34
    let
        indentAmount =
            4

        indentedExpressionCode =
            expressionCode
                |> String.lines
                |> List.map ((++) (String.repeat indentAmount (String.fromChar ' ')))
                |> String.join "\n"

        declarationTextBeforeExpression =
            "wrapping_expression_in_function = \n"
    in
    parseDeclarationFromString (declarationTextBeforeExpression ++ indentedExpressionCode)
        |> Result.mapError (List.map (mapLocationForPrefixText declarationTextBeforeExpression >> mapLocationForIndentAmount indentAmount))
        |> Result.map
            (Result.andThen
                (\declaration ->
                    case declaration of
                        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                            functionDeclaration
                                |> .declaration
                                |> Elm.Syntax.Node.value
                                |> .expression
                                |> Elm.Syntax.Node.value
                                |> Ok

                        _ ->
                            Err "Failed to extract the wrapping function."
                )
            )


parseDeclarationFromString : String -> Result (List Parser.DeadEnd) (Result String Elm.Syntax.Declaration.Declaration)
parseDeclarationFromString declarationCode =
    -- https://github.com/stil4m/elm-syntax/issues/34
    let
        moduleTextBeforeDeclaration =
            """
module Main exposing (..)


"""

        moduleText =
            [ moduleTextBeforeDeclaration
            , String.trim declarationCode
            , ""
            ]
                |> String.join "\n"
    in
    parseElmModuleText moduleText
        |> Result.mapError (List.map (mapLocationForPrefixText moduleTextBeforeDeclaration))
        |> Result.map
            (.declarations
                >> List.map Elm.Syntax.Node.value
                >> List.head
                >> Result.fromMaybe "Failed to extract the declaration from the parsed module."
            )


mapLocationForPrefixText : String -> Parser.DeadEnd -> Parser.DeadEnd
mapLocationForPrefixText prefixText =
    let
        prefixLines =
            String.lines prefixText
    in
    mapLocation
        { row = 1 - List.length prefixLines
        , col = -(prefixLines |> List.reverse |> List.head |> Maybe.withDefault "" |> String.length)
        }


mapLocationForIndentAmount : Int -> Parser.DeadEnd -> Parser.DeadEnd
mapLocationForIndentAmount indentAmount =
    mapLocation { row = 0, col = -indentAmount }


mapLocation : { row : Int, col : Int } -> Parser.DeadEnd -> Parser.DeadEnd
mapLocation offset deadEnd =
    { deadEnd | row = deadEnd.row + offset.row, col = deadEnd.col + offset.col }


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    Elm.Parser.parseToFile


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
