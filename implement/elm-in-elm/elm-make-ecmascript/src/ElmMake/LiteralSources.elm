module ElmMake.LiteralSources exposing (fromFile)

import Char
import Dict exposing (Dict)
import ElmMake.JavaScriptLiterals exposing (unicode)
import ElmMake.Model exposing (SourceOrigin(..))
import ElmSyntax.Concrete.Declaration as Declaration
import ElmSyntax.Concrete.Expression as Expr exposing (Expression(..))
import ElmSyntax.Concrete.File exposing (File)
import ElmSyntax.Concrete.Node as Node
import ElmSyntax.Concrete.SeparatedSyntaxList as Separated


{-| A parallel origin tree preserves spelling without changing the shared abstract
syntax model. Its shape follows normalized expression children, not source ranges,
so declaration reordering cannot associate a literal with the wrong occurrence.
-}
fromFile : File -> Dict String SourceOrigin
fromFile file =
    file.declarations
        |> List.filterMap
            (\declarationNode ->
                case Node.value declarationNode of
                    Declaration.FunctionDeclaration function ->
                        let
                            implementation =
                                Node.value function.declaration
                        in
                        Just ( Node.value implementation.name, expression implementation.expression )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


expression : Node.Node Expression -> SourceOrigin
expression node =
    let
        children values =
            SourceOrigin Nothing (List.map expression values) (Just (Node.range node))

        fields setters =
            setters |> separated |> List.sortBy (.fieldName >> Node.value) |> List.map .valueExpr |> children
    in
    case Node.value node of
        StringLiteral _ (Just source) ->
            SourceOrigin (Just ("'" ++ stringContent source ++ "'")) [] (Just (Node.range node))

        MultilineStringLiteral _ (Just lines) ->
            SourceOrigin (Just ("'" ++ stringContent (String.join "\n" lines) ++ "'")) [] (Just (Node.range node))

        FloatLiteral text ->
            SourceOrigin (Just (String.replace "E" "e" text)) [] (Just (Node.range node))

        Parenthesized value ->
            expression value

        Negation value ->
            children [ value ]

        Application function arguments ->
            children (function :: arguments)

        OperatorApplication _ _ left right ->
            children [ left, right ]

        IfBlock _ condition _ yes _ no ->
            children [ condition, yes, no ]

        ListExpr entries ->
            children (separated entries)

        TupledExpression entries ->
            children (separated entries)

        RecordExpr entries ->
            fields entries

        RecordUpdateExpression _ _ entries ->
            fields entries

        RecordAccess target _ ->
            children [ target ]

        LambdaExpression lambda ->
            children [ lambda.expression ]

        CaseExpression case_ ->
            children (case_.expression :: List.map .expression case_.cases)

        LetExpression let_ ->
            let_.declarations
                |> List.map
                    (\declarationNode ->
                        case Node.value declarationNode of
                            Expr.LetFunction function ->
                                (Node.value function.declaration).expression

                            Expr.LetDestructuring _ _ value ->
                                value
                    )
                |> (\definitions -> children (definitions ++ [ let_.expression ]))

        _ ->
            SourceOrigin Nothing [] (Just (Node.range node))


separated : Separated.SeparatedSyntaxList a -> List a
separated values =
    case values of
        Separated.Empty ->
            []

        Separated.NonEmpty first rest ->
            first :: List.map Tuple.second rest


stringContent : String -> String
stringContent source =
    case String.uncons source of
        Nothing ->
            ""

        Just ( '\\', rest ) ->
            if String.startsWith "u{" rest then
                case String.indexes "}" rest |> List.head of
                    Just end ->
                        let
                            digits =
                                String.slice 2 end rest

                            code =
                                String.foldl (\char value -> value * 16 + hexValue char) 0 digits

                            escaped =
                                if code < 0x10000 then
                                    unicode code

                                else
                                    unicode (0xD800 + ((code - 0x10000) // 0x0400)) ++ unicode (0xDC00 + modBy 0x0400 (code - 0x10000))
                        in
                        escaped ++ stringContent (String.dropLeft (end + 1) rest)

                    Nothing ->
                        "\\u" ++ stringContent (String.dropLeft 1 rest)

            else
                case String.uncons rest of
                    Just ( escaped, remaining ) ->
                        "\\" ++ String.fromChar escaped ++ stringContent remaining

                    Nothing ->
                        "\\\\"

        Just ( '\'', rest ) ->
            "\\'" ++ stringContent rest

        Just ( '\n', rest ) ->
            "\\n" ++ stringContent rest

        Just ( '\u{000D}', rest ) ->
            "\\r" ++ stringContent rest

        Just ( char, rest ) ->
            String.fromChar char ++ stringContent rest


hexValue : Char -> Int
hexValue char =
    if Char.isDigit char then
        Char.toCode char - Char.toCode '0'

    else
        Char.toCode (Char.toUpper char) - Char.toCode 'A' + 10
