module ElmCompilerConstruction2024 exposing (..)

import BigInt
import FirCompiler2024
import Pine2024


{-| This function returns an expression that evaluates to the encoding of the input expression. In other words, it is an inversion of `Pine2024.evaluateExpression emptyEnv >> Pine2024.parseExpressionFromValue`
Most expressions have multiple valid encoded representations, and the one produced here supports building templates to bind the parent environment.
One typical use case for these templates is wrapping a function to support the partial application of the wrapped function.
-}
generateTemplateEvaluatingToExpression : Pine2024.Expression -> Pine2024.Expression
generateTemplateEvaluatingToExpression expression =
    let
        buildFromTagAndArguments tagName arguments =
            [ Pine2024.LiteralExpression (Pine2024.valueFromString tagName)
            , arguments
            ]
                |> Pine2024.ListExpression

        buildRecordExpression fields =
            fields
                |> List.map
                    (\( name, value ) ->
                        [ Pine2024.LiteralExpression (Pine2024.valueFromString name)
                        , value
                        ]
                            |> Pine2024.ListExpression
                    )
                |> Pine2024.ListExpression
    in
    case expression of
        Pine2024.ListExpression list ->
            buildFromTagAndArguments
                "List"
                (Pine2024.ListExpression
                    [ Pine2024.ListExpression (List.map generateTemplateEvaluatingToExpression list)
                    ]
                )

        Pine2024.LiteralExpression literal ->
            buildFromTagAndArguments
                "Literal"
                (Pine2024.ListExpression
                    [ Pine2024.LiteralExpression literal
                    ]
                )

        Pine2024.ParseAndEvalExpression encodedExpr envExpr ->
            buildFromTagAndArguments
                "ParseAndEval"
                (Pine2024.ListExpression
                    [ generateTemplateEvaluatingToExpression encodedExpr
                    , generateTemplateEvaluatingToExpression envExpr
                    ]
                )

        Pine2024.KernelApplicationExpression functionName input ->
            buildFromTagAndArguments
                "KernelApplication"
                (Pine2024.ListExpression
                    [ Pine2024.LiteralExpression (Pine2024.valueFromString functionName)
                    , generateTemplateEvaluatingToExpression input
                    ]
                )

        Pine2024.ConditionalExpression condition falseBranch trueBranch ->
            buildFromTagAndArguments
                "Conditional"
                (Pine2024.ListExpression
                    [ generateTemplateEvaluatingToExpression condition
                    , generateTemplateEvaluatingToExpression falseBranch
                    , generateTemplateEvaluatingToExpression trueBranch
                    ]
                )

        Pine2024.EnvironmentExpression ->
            buildFromTagAndArguments
                "Environment"
                (Pine2024.ListExpression [])

        Pine2024.StringTagExpression tag tagged ->
            buildFromTagAndArguments
                "StringTag"
                (Pine2024.ListExpression
                    [ Pine2024.LiteralExpression (Pine2024.valueFromString tag)
                    , generateTemplateEvaluatingToExpression tagged
                    ]
                )


buildPineExpressionSyntax : { attemptEncodeExpression : Bool } -> Pine2024.Expression -> List String
buildPineExpressionSyntax config expression =
    let
        maybeAsEncodedExpression =
            if not config.attemptEncodeExpression then
                Nothing

            else if FirCompiler2024.pineExpressionIsIndependent expression then
                case Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment expression of
                    Err _ ->
                        Nothing

                    Ok expressionValue ->
                        case Pine2024.parseExpressionFromValue expressionValue of
                            Err _ ->
                                Nothing

                            Ok decodedExpression ->
                                String.join "\n" (buildPineExpressionSyntax config decodedExpression)
                                    :: List.map indentString
                                        [ "|> Pine2024.encodeExpressionAsValue"
                                        , "|> Pine2024.LiteralExpression"
                                        ]
                                    |> Just

            else
                Nothing

        buildRecordSyntax : List ( String, String ) -> List String
        buildRecordSyntax fields =
            let
                fieldSyntax =
                    fields
                        |> List.map
                            (\( fieldName, fieldValue ) -> fieldName ++ " = " ++ fieldValue)
            in
            case fieldSyntax of
                [] ->
                    [ "{}" ]

                firstField :: otherFields ->
                    ("{ " ++ firstField)
                        :: List.map ((++) ", ") otherFields
                        ++ [ "}" ]

        indentString : String -> String
        indentString =
            String.lines
                >> List.map ((++) "    ")
                >> String.join "\n"

        wrapArgumentInParentheses : String -> String
        wrapArgumentInParentheses argument =
            let
                argumentTrimmed =
                    String.trim argument
            in
            if not (String.contains " " argumentTrimmed) then
                argument

            else if String.startsWith "[" argumentTrimmed && String.endsWith "]" argumentTrimmed then
                argument

            else
                "(" ++ argument ++ ")"

        buildFromTagNameAndArguments : String -> List String -> List String
        buildFromTagNameAndArguments tagName arguments =
            tagName
                :: List.map (wrapArgumentInParentheses >> indentString) arguments

        printValueSyntax : Pine2024.Value -> List String
        printValueSyntax literal =
            case literal of
                Pine2024.ListValue itemsValues ->
                    case itemsValues of
                        [] ->
                            [ "Pine2024.ListValue []" ]

                        firstListItem :: otherListItems ->
                            let
                                genericList () =
                                    "Pine2024.ListValue"
                                        :: (("[ " ++ String.join "\n" (printValueSyntax firstListItem))
                                                :: List.map (printValueSyntax >> String.join "\n" >> (++) ", ") otherListItems
                                                ++ [ "]" ]
                                                |> List.map indentString
                                           )
                            in
                            if otherListItems == [] then
                                genericList ()

                            else
                                case Pine2024.stringFromValue literal of
                                    Ok asString ->
                                        [ "Pine2024.valueFromString " ++ "\"" ++ asString ++ "\"" ]

                                    Err _ ->
                                        genericList ()

                Pine2024.BlobValue blob ->
                    case Pine2024.bigIntFromBlobValue blob of
                        Ok asInt ->
                            [ "Pine2024.valueFromInt (" ++ BigInt.toString asInt ++ ")" ]

                        Err _ ->
                            [ "other-blob" ]
    in
    case maybeAsEncodedExpression of
        Just encodedExpression ->
            encodedExpression

        Nothing ->
            case expression of
                Pine2024.LiteralExpression literal ->
                    let
                        valueSyntax =
                            printValueSyntax literal
                                |> String.join "\n"

                        separator =
                            if String.contains "\n" valueSyntax then
                                "\n"

                            else
                                " "
                    in
                    [ "Pine2024.LiteralExpression" ++ separator ++ wrapArgumentInParentheses valueSyntax ]

                Pine2024.ListExpression list ->
                    let
                        listSyntax =
                            case
                                List.map
                                    (buildPineExpressionSyntax config
                                        >> String.join "\n"
                                    )
                                    list
                            of
                                [] ->
                                    [ "[]" ]

                                firstElement :: otherElements ->
                                    ("[ " ++ firstElement)
                                        :: List.map ((++) ", ") otherElements
                                        ++ [ "]" ]
                    in
                    buildFromTagNameAndArguments
                        "Pine2024.ListExpression"
                        [ String.join "\n" listSyntax ]

                Pine2024.ParseAndEvalExpression encodedExpr envExpr ->
                    buildFromTagNameAndArguments
                        "Pine2024.ParseAndEvalExpression"
                        [ buildRecordSyntax
                            [ ( "encoded"
                              , buildPineExpressionSyntax config encodedExpr
                                    |> String.join "\n"
                              )
                            , ( "environment"
                              , buildPineExpressionSyntax config envExpr
                                    |> String.join "\n"
                              )
                            ]
                            |> String.join "\n"
                        ]

                Pine2024.KernelApplicationExpression functionName input ->
                    buildFromTagNameAndArguments
                        "Pine2024.KernelApplicationExpression"
                        [ buildRecordSyntax
                            [ ( "function"
                              , "\"" ++ functionName ++ "\""
                              )
                            , ( "input"
                              , buildPineExpressionSyntax config input
                                    |> String.join "\n"
                              )
                            ]
                            |> String.join "\n"
                        ]

                Pine2024.ConditionalExpression condition falseBranch trueBranch ->
                    buildFromTagNameAndArguments
                        "Pine2024.ConditionalExpression"
                        [ buildRecordSyntax
                            [ ( "condition"
                              , buildPineExpressionSyntax config condition
                                    |> String.join "\n"
                              )
                            , ( "falseBranch"
                              , buildPineExpressionSyntax config falseBranch
                                    |> String.join "\n"
                              )
                            , ( "trueBranch"
                              , buildPineExpressionSyntax config trueBranch
                                    |> String.join "\n"
                              )
                            ]
                            |> String.join "\n"
                        ]

                Pine2024.EnvironmentExpression ->
                    [ "Pine2024.EnvironmentExpression" ]

                Pine2024.StringTagExpression tag tagged ->
                    buildFromTagNameAndArguments
                        "Pine2024.StringTagExpression"
                        [ "\"" ++ tag ++ "\""
                        , buildPineExpressionSyntax config tagged
                            |> String.join "\n"
                        ]


recursiveLiteralListToListExpression : Pine2024.Expression -> Pine2024.Expression
recursiveLiteralListToListExpression expression =
    let
        isBlobValue value =
            case value of
                Pine2024.BlobValue _ ->
                    True

                _ ->
                    False
    in
    case expression of
        Pine2024.LiteralExpression (Pine2024.ListValue list) ->
            if List.any isBlobValue list then
                expression

            else
                list
                    |> List.map (Pine2024.LiteralExpression >> recursiveLiteralListToListExpression)
                    |> Pine2024.ListExpression

        Pine2024.ListExpression list ->
            list
                |> List.map recursiveLiteralListToListExpression
                |> Pine2024.ListExpression

        _ ->
            expression
