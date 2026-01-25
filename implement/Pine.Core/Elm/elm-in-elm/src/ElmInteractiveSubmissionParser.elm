module ElmInteractiveSubmissionParser exposing (..)

import Elm.Parser
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Node
import ElmCompiler exposing (InteractiveSubmission(..))
import Parser


parseInteractiveSubmissionFromString : String -> Result String InteractiveSubmission
parseInteractiveSubmissionFromString submission =
    let
        charCouldBePartOfOperator : Char -> Bool
        charCouldBePartOfOperator char =
            case char of
                '=' ->
                    True

                '/' ->
                    True

                '&' ->
                    True

                '*' ->
                    True

                '+' ->
                    True

                '.' ->
                    True

                '<' ->
                    True

                '>' ->
                    True

                '?' ->
                    True

                '^' ->
                    True

                '|' ->
                    True

                '-' ->
                    True

                _ ->
                    False

        computeCurlyBraceBalance : List Char -> Int
        computeCurlyBraceBalance chars =
            case chars of
                [] ->
                    0

                first :: rest ->
                    case first of
                        '{' ->
                            1 + computeCurlyBraceBalance rest

                        '}' ->
                            -1 + computeCurlyBraceBalance rest

                        _ ->
                            computeCurlyBraceBalance rest

        looksLikeDeclaration =
            case String.split "=" (String.trim submission) of
                beforeEquals :: afterEquals :: _ ->
                    case String.uncons beforeEquals of
                        Nothing ->
                            False

                        Just ( firstCharBeforeEquals, _ ) ->
                            {-
                               Note: The part before the equals sign can contain spaces, at least if the declaration is a function.
                            -}
                            case String.uncons afterEquals of
                                Just ( charAfterEqualsSign, _ ) ->
                                    Char.isLower firstCharBeforeEquals
                                        && not (String.startsWith "let" beforeEquals)
                                        {-
                                           Account for expression containing equals sign as part of a record expression.
                                           Example:
                                           getAlfa { alfa = 31, beta = 37 }
                                        -}
                                        && (computeCurlyBraceBalance (String.toList beforeEquals) == 0)
                                        && not (charCouldBePartOfOperator charAfterEqualsSign)

                                Nothing ->
                                    False

                _ ->
                    False
    in
    if looksLikeDeclaration then
        case parseDeclarationFromString submission of
            Err error ->
                Err ("Failed to parse as declaration: " ++ parserDeadEndsToString error)

            Ok (Err error) ->
                Err ("Failed to parse as declaration: " ++ error)

            Ok (Ok declaration) ->
                Ok (DeclarationSubmission declaration)

    else
        case parseExpressionFromString submission of
            Err error ->
                Err ("Failed to parse as expression: " ++ parserDeadEndsToString error)

            Ok (Err error) ->
                Err ("Failed to parse as expression: " ++ error)

            Ok (Ok expression) ->
                Ok (ExpressionSubmission expression)


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
