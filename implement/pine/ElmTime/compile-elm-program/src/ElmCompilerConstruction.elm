module ElmCompilerConstruction exposing (..)

import BigInt
import FirCompiler
import Pine


{-| This function returns an expression that evaluates to the encoding of the input expression. In other words, it is an inversion of `Pine.evaluateExpression emptyEnv >> Pine.parseExpressionFromValue`
Most expressions have multiple valid encoded representations, and the one produced here supports building templates to bind the parent environment.
One typical use case for these templates is wrapping a function to support the partial application of the wrapped function.
-}
generateTemplateEvaluatingToExpression : Pine.Expression -> Pine.Expression
generateTemplateEvaluatingToExpression expression =
    let
        buildFromTagAndArgument tagName argument =
            [ Pine.LiteralExpression (Pine.valueFromString tagName)
            , argument
            ]
                |> Pine.ListExpression

        buildRecordExpression fields =
            fields
                |> List.map
                    (\( name, value ) ->
                        [ Pine.LiteralExpression (Pine.valueFromString name)
                        , value
                        ]
                            |> Pine.ListExpression
                    )
                |> Pine.ListExpression
    in
    case expression of
        Pine.ListExpression list ->
            buildFromTagAndArgument
                "List"
                (Pine.ListExpression (List.map generateTemplateEvaluatingToExpression list))

        Pine.LiteralExpression literal ->
            buildFromTagAndArgument
                "Literal"
                (Pine.LiteralExpression literal)

        Pine.ParseAndEvalExpression encodedExpr envExpr ->
            buildFromTagAndArgument
                "ParseAndEval"
                (buildRecordExpression
                    [ ( "encoded", generateTemplateEvaluatingToExpression encodedExpr )
                    , ( "environment", generateTemplateEvaluatingToExpression envExpr )
                    ]
                )

        Pine.KernelApplicationExpression functionName input ->
            buildFromTagAndArgument
                "KernelApplication"
                (buildRecordExpression
                    [ ( "function", Pine.LiteralExpression (Pine.valueFromString functionName) )
                    , ( "input", generateTemplateEvaluatingToExpression input )
                    ]
                )

        Pine.ConditionalExpression condition falseBranch trueBranch ->
            buildFromTagAndArgument
                "Conditional"
                (buildRecordExpression
                    [ ( "condition"
                      , generateTemplateEvaluatingToExpression condition
                      )
                    , ( "falseBranch"
                      , generateTemplateEvaluatingToExpression falseBranch
                      )
                    , ( "trueBranch"
                      , generateTemplateEvaluatingToExpression trueBranch
                      )
                    ]
                )

        Pine.EnvironmentExpression ->
            buildFromTagAndArgument
                "Environment"
                (Pine.ListExpression [])

        Pine.StringTagExpression tag tagged ->
            buildFromTagAndArgument
                "StringTag"
                (Pine.ListExpression
                    [ Pine.LiteralExpression (Pine.valueFromString tag)
                    , generateTemplateEvaluatingToExpression tagged
                    ]
                )


buildPineExpressionSyntax : { attemptEncodeExpression : Bool } -> Pine.Expression -> List String
buildPineExpressionSyntax config expression =
    let
        maybeAsEncodedExpression =
            if not config.attemptEncodeExpression then
                Nothing

            else if FirCompiler.pineExpressionIsIndependent expression then
                case Pine.evaluateExpression Pine.emptyEvalEnvironment expression of
                    Err _ ->
                        Nothing

                    Ok expressionValue ->
                        case Pine.parseExpressionFromValue expressionValue of
                            Err _ ->
                                Nothing

                            Ok decodedExpression ->
                                String.join "\n" (buildPineExpressionSyntax config decodedExpression)
                                    :: List.map indentString
                                        [ "|> Pine.encodeExpressionAsValue"
                                        , "|> Pine.LiteralExpression"
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

        printValueSyntax : Pine.Value -> List String
        printValueSyntax literal =
            case literal of
                Pine.ListValue listValues ->
                    case listValues of
                        [] ->
                            [ "Pine.ListValue []" ]

                        firstListItem :: otherListItems ->
                            case Pine.stringFromValue literal of
                                Ok asString ->
                                    [ "Pine.valueFromString " ++ "\"" ++ asString ++ "\"" ]

                                Err _ ->
                                    "Pine.ListValue"
                                        :: (("[ " ++ String.join "\n" (printValueSyntax firstListItem))
                                                :: List.map (printValueSyntax >> String.join "\n" >> (++) ", ") otherListItems
                                                ++ [ "]" ]
                                                |> List.map indentString
                                           )

                Pine.BlobValue blob ->
                    case Pine.bigIntFromBlobValue blob of
                        Ok asInt ->
                            [ "Pine.valueFromInt (" ++ BigInt.toString asInt ++ ")" ]

                        Err _ ->
                            [ "other-blob" ]
    in
    case maybeAsEncodedExpression of
        Just encodedExpression ->
            encodedExpression

        Nothing ->
            case expression of
                Pine.LiteralExpression literal ->
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
                    [ "Pine.LiteralExpression" ++ separator ++ wrapArgumentInParentheses valueSyntax ]

                Pine.ListExpression list ->
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
                        "Pine.ListExpression"
                        [ String.join "\n" listSyntax ]

                Pine.ParseAndEvalExpression encodedExpr envExpr ->
                    buildFromTagNameAndArguments
                        "Pine.ParseAndEvalExpression"
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

                Pine.KernelApplicationExpression functionName input ->
                    buildFromTagNameAndArguments
                        "Pine.KernelApplicationExpression"
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

                Pine.ConditionalExpression condition falseBranch trueBranch ->
                    buildFromTagNameAndArguments
                        "Pine.ConditionalExpression"
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

                Pine.EnvironmentExpression ->
                    [ "Pine.EnvironmentExpression" ]

                Pine.StringTagExpression tag tagged ->
                    buildFromTagNameAndArguments
                        "Pine.StringTagExpression"
                        [ "\"" ++ tag ++ "\""
                        , buildPineExpressionSyntax config tagged
                            |> String.join "\n"
                        ]


recursiveLiteralListToListExpression : Pine.Expression -> Pine.Expression
recursiveLiteralListToListExpression expression =
    let
        isBlobValue value =
            case value of
                Pine.BlobValue _ ->
                    True

                _ ->
                    False
    in
    case expression of
        Pine.LiteralExpression (Pine.ListValue list) ->
            if List.any isBlobValue list then
                expression

            else
                list
                    |> List.map (Pine.LiteralExpression >> recursiveLiteralListToListExpression)
                    |> Pine.ListExpression

        Pine.ListExpression list ->
            list
                |> List.map recursiveLiteralListToListExpression
                |> Pine.ListExpression

        _ ->
            expression
