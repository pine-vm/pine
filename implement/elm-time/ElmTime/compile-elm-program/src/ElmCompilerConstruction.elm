module ElmCompilerConstruction exposing (..)

import BigInt
import ElmInteractive
import Pine


{-| This function returns an expression that evaluates to the encoding of the input expression. In other words, it is an inversion of `Pine.evaluateExpression emptyEnv >> Pine.decodeExpressionFromValue`
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

        Pine.DecodeAndEvaluateExpression decodeAndEval ->
            buildFromTagAndArgument
                "DecodeAndEvaluate"
                (buildRecordExpression
                    [ ( "environment", generateTemplateEvaluatingToExpression decodeAndEval.environment )
                    , ( "expression", generateTemplateEvaluatingToExpression decodeAndEval.expression )
                    ]
                )

        Pine.KernelApplicationExpression kernelApp ->
            buildFromTagAndArgument
                "KernelApplication"
                (buildRecordExpression
                    [ ( "argument", generateTemplateEvaluatingToExpression kernelApp.argument )
                    , ( "functionName", Pine.LiteralExpression (Pine.valueFromString kernelApp.functionName) )
                    ]
                )

        Pine.ConditionalExpression conditional ->
            buildFromTagAndArgument
                "Conditional"
                (buildRecordExpression
                    [ ( "condition"
                      , generateTemplateEvaluatingToExpression conditional.condition
                      )
                    , ( "ifFalse"
                      , generateTemplateEvaluatingToExpression conditional.ifFalse
                      )
                    , ( "ifTrue"
                      , generateTemplateEvaluatingToExpression conditional.ifTrue
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

            else if ElmInteractive.pineExpressionIsIndependent expression then
                case Pine.evaluateExpression Pine.emptyEvalContext expression of
                    Err _ ->
                        Nothing

                    Ok expressionValue ->
                        case Pine.decodeExpressionFromValue expressionValue of
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
                            [ "Pine.valueFromBigInt (BigInt.fromInt " ++ BigInt.toString asInt ++ ")" ]

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

                Pine.DecodeAndEvaluateExpression decodeAndEvaluate ->
                    buildFromTagNameAndArguments
                        "Pine.DecodeAndEvaluateExpression"
                        [ buildRecordSyntax
                            [ ( "environment"
                              , buildPineExpressionSyntax config decodeAndEvaluate.environment
                                    |> String.join "\n"
                              )
                            , ( "expression"
                              , buildPineExpressionSyntax config decodeAndEvaluate.expression
                                    |> String.join "\n"
                              )
                            ]
                            |> String.join "\n"
                        ]

                Pine.KernelApplicationExpression kernelApplication ->
                    buildFromTagNameAndArguments
                        "Pine.KernelApplicationExpression"
                        [ buildRecordSyntax
                            [ ( "argument"
                              , buildPineExpressionSyntax config kernelApplication.argument
                                    |> String.join "\n"
                              )
                            , ( "functionName"
                              , "\"" ++ kernelApplication.functionName ++ "\""
                              )
                            ]
                            |> String.join "\n"
                        ]

                Pine.ConditionalExpression conditional ->
                    buildFromTagNameAndArguments
                        "Pine.ConditionalExpression"
                        [ buildRecordSyntax
                            [ ( "condition"
                              , buildPineExpressionSyntax config conditional.condition
                                    |> String.join "\n"
                              )
                            , ( "ifFalse"
                              , buildPineExpressionSyntax config conditional.ifFalse
                                    |> String.join "\n"
                              )
                            , ( "ifTrue"
                              , buildPineExpressionSyntax config conditional.ifTrue
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
