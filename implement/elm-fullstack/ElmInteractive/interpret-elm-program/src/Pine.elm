module Pine exposing (..)

import BigInt
import Json.Decode
import Json.Encode
import Maybe.Extra
import Result.Extra


type Expression
    = LiteralExpression Value
    | ListExpression ListExpressionStructure
    | ApplicationExpression ApplicationExpressionStructure
    | FunctionOrValueExpression String
    | ContextExpansionWithNameExpression ContextExpansionWithNameExpressionStructure
    | IfBlockExpression IfBlockExpressionStructure
    | FunctionExpression FunctionExpressionStructure


type alias ListExpressionStructure =
    List Expression


type alias ApplicationExpressionStructure =
    { function : Expression
    , arguments : List Expression
    }


type alias IfBlockExpressionStructure =
    { condition : Expression
    , ifTrue : Expression
    , ifFalse : Expression
    }


type alias FunctionExpressionStructure =
    { argumentName : String
    , body : Expression
    }


type alias ContextExpansionWithNameExpressionStructure =
    { name : String
    , namedValue : Value
    , expression : Expression
    }


type Value
    = BlobValue (List Int)
    | ListValue (List Value)
    | ClosureValue ExpressionContext Expression


type alias ExpressionContext =
    -- TODO: Test consolidate into simple Value
    { commonModel : List Value
    }


type PathDescription a
    = DescribePathNode a (PathDescription a)
    | DescribePathEnd a


addToContext : List Value -> ExpressionContext -> ExpressionContext
addToContext names context =
    { context | commonModel = names ++ context.commonModel }


evaluateExpression : ExpressionContext -> Expression -> Result (PathDescription String) Value
evaluateExpression context expression =
    evaluateExpressionExceptClosure context expression
        |> Result.andThen
            (\value ->
                case value of
                    ClosureValue closureContext closureExpression ->
                        case closureExpression of
                            FunctionExpression _ ->
                                Ok value

                            _ ->
                                evaluateExpression closureContext closureExpression

                    _ ->
                        Ok value
            )


evaluateExpressionExceptClosure : ExpressionContext -> Expression -> Result (PathDescription String) Value
evaluateExpressionExceptClosure context expression =
    case expression of
        LiteralExpression value ->
            Ok value

        ListExpression listElements ->
            listElements
                |> List.map (evaluateExpression context)
                |> Result.Extra.combine
                |> Result.map ListValue
                |> Result.mapError (DescribePathNode "Failed to evaluate list element")

        ApplicationExpression application ->
            evaluateFunctionApplication context application
                |> Result.mapError (DescribePathNode ("Failed application of '" ++ describeExpression application.function ++ "'"))

        FunctionOrValueExpression name ->
            case name of
                "True" ->
                    Ok trueValue

                "False" ->
                    Ok falseValue

                _ ->
                    let
                        beforeCheckForExpression =
                            lookUpNameAsStringInContext (String.split "." name) context
                                |> Result.mapError (DescribePathNode ("Failed to look up name '" ++ name ++ "'"))
                    in
                    case beforeCheckForExpression of
                        Ok ( valueFromLookup, contextFromLookup ) ->
                            case decodeExpressionFromValue valueFromLookup of
                                Ok expressionFromLookup ->
                                    Ok (ClosureValue { commonModel = contextFromLookup } expressionFromLookup)

                                _ ->
                                    Result.map Tuple.first beforeCheckForExpression

                        _ ->
                            Result.map Tuple.first beforeCheckForExpression

        IfBlockExpression ifBlock ->
            case evaluateExpression context ifBlock.condition of
                Err error ->
                    Err (DescribePathNode "Failed to evaluate condition" error)

                Ok conditionValue ->
                    evaluateExpression context
                        (if conditionValue == trueValue then
                            ifBlock.ifTrue

                         else
                            ifBlock.ifFalse
                        )

        ContextExpansionWithNameExpression expansion ->
            evaluateExpression
                { context
                    | commonModel = valueFromContextExpansionWithName ( expansion.name, expansion.namedValue ) :: context.commonModel
                }
                expansion.expression

        FunctionExpression _ ->
            Ok (ClosureValue context expression)


valueFromContextExpansionWithName : ( String, Value ) -> Value
valueFromContextExpansionWithName ( declName, declValue ) =
    ListValue [ valueFromString declName, declValue ]


namedValueFromValue : Value -> Maybe ( String, Value )
namedValueFromValue value =
    case value of
        ListValue [ elementLabelCandidate, elementValue ] ->
            case stringFromValue elementLabelCandidate of
                Ok elementLabel ->
                    Just ( elementLabel, elementValue )

                Err _ ->
                    Nothing

        _ ->
            Nothing


lookUpNameAsStringInContext : List String -> ExpressionContext -> Result (PathDescription String) ( Value, List Value )
lookUpNameAsStringInContext path =
    lookUpNameAsValueInContext (List.map valueFromString path)


lookUpNameAsValueInContext : List Value -> ExpressionContext -> Result (PathDescription String) ( Value, List Value )
lookUpNameAsValueInContext path context =
    case path of
        [] ->
            Err (DescribePathEnd "path is empty")

        pathFirstElement :: pathRemainingElements ->
            let
                getPathFirstElementAsString _ =
                    Result.withDefault "Failed to map value to string" (stringFromValue pathFirstElement)

                getMatchFromList contextList =
                    case contextList of
                        [] ->
                            Nothing

                        nextElement :: remainingElements ->
                            case nextElement of
                                ListValue [ labelValue, namedValue ] ->
                                    if labelValue == pathFirstElement then
                                        Just namedValue

                                    else
                                        getMatchFromList remainingElements

                                _ ->
                                    getMatchFromList remainingElements
            in
            case getMatchFromList context.commonModel of
                Nothing ->
                    let
                        availableNames =
                            context.commonModel |> List.filterMap namedValueFromValue
                    in
                    Err
                        (DescribePathEnd
                            ("Did not find '"
                                ++ getPathFirstElementAsString ()
                                ++ "'. There are "
                                ++ (availableNames |> List.length |> String.fromInt)
                                ++ " names available in that scope: "
                                ++ (availableNames |> List.map Tuple.first |> String.join ", ")
                            )
                        )

                Just firstNameValue ->
                    if pathRemainingElements == [] then
                        Ok ( firstNameValue, context.commonModel )

                    else
                        case firstNameValue of
                            ListValue firstNameList ->
                                lookUpNameAsValueInContext pathRemainingElements
                                    { commonModel = firstNameList }

                            _ ->
                                Err (DescribePathEnd ("'" ++ getPathFirstElementAsString () ++ "' has unexpected type: Not a list."))


evaluateFunctionApplication : ExpressionContext -> { function : Expression, arguments : List Expression } -> Result (PathDescription String) Value
evaluateFunctionApplication context application =
    application.arguments
        |> List.map
            (\argumentExpression ->
                evaluateExpression context argumentExpression
                    |> Result.mapError (DescribePathNode ("Failed to evaluate argument '" ++ describeExpression argumentExpression ++ "'"))
            )
        |> Result.Extra.combine
        |> Result.andThen
            (\arguments ->
                evaluateFunctionApplicationWithEvaluatedArgs context { function = application.function, arguments = arguments }
            )


evaluateFunctionApplicationWithEvaluatedArgs : ExpressionContext -> { function : Expression, arguments : List Value } -> Result (PathDescription String) Value
evaluateFunctionApplicationWithEvaluatedArgs context application =
    let
        functionOnTwoBigIntWithBooleanResult functionOnBigInt =
            evaluateFunctionApplicationExpectingExactlyTwoArguments
                { mapArg0 = bigIntFromValue >> Result.mapError DescribePathEnd
                , mapArg1 = bigIntFromValue >> Result.mapError DescribePathEnd
                , apply =
                    \leftInt rightInt ->
                        Ok
                            (if functionOnBigInt leftInt rightInt then
                                trueValue

                             else
                                falseValue
                            )
                }
                application.arguments

        functionOnTwoBigIntWithBigIntResult functionOnBigInt =
            evaluateFunctionApplicationExpectingExactlyTwoArguments
                { mapArg0 = bigIntFromValue >> Result.mapError DescribePathEnd
                , mapArg1 = bigIntFromValue >> Result.mapError DescribePathEnd
                , apply =
                    \leftInt rightInt ->
                        Ok (valueFromBigInt (functionOnBigInt leftInt rightInt))
                }
                application.arguments

        functionExpectingOneArgumentOfTypeList functionOnList =
            evaluateFunctionApplicationExpectingExactlyOneArgument
                { mapArg = Ok
                , apply =
                    \argument ->
                        case argument of
                            ListValue list ->
                                list |> functionOnList |> Ok

                            _ ->
                                Err (DescribePathEnd ("Argument is not a list ('" ++ describeValue argument ++ "')"))
                }
                application.arguments

        functionEquals =
            evaluateFunctionApplicationExpectingExactlyTwoArguments
                { mapArg0 = Ok
                , mapArg1 = Ok
                , apply =
                    \leftValue rightValue ->
                        Ok
                            (if leftValue == rightValue then
                                trueValue

                             else
                                falseValue
                            )
                }
                application.arguments

        functionEqualsNot =
            evaluateFunctionApplicationExpectingExactlyTwoArguments
                { mapArg0 = Ok
                , mapArg1 = Ok
                , apply =
                    \leftValue rightValue ->
                        Ok
                            (if leftValue == rightValue then
                                falseValue

                             else
                                trueValue
                            )
                }
                application.arguments

        continueIgnoringAtomBindings _ =
            evaluateFunctionApplicationIgnoringAtomBindings
                context
                application
    in
    case application.function of
        FunctionOrValueExpression functionName ->
            case functionName of
                "PineKernel.equals" ->
                    functionEquals

                "PineKernel.negate" ->
                    evaluateFunctionApplicationExpectingExactlyOneArgument
                        { mapArg = bigIntFromValue >> Result.mapError DescribePathEnd
                        , apply = BigInt.negate >> valueFromBigInt >> Ok
                        }
                        application.arguments

                "PineKernel.listHead" ->
                    functionExpectingOneArgumentOfTypeList (List.head >> Maybe.withDefault (ListValue []))

                "PineKernel.listTail" ->
                    functionExpectingOneArgumentOfTypeList (List.tail >> Maybe.withDefault [] >> ListValue)

                "PineKernel.listCons" ->
                    evaluateFunctionApplicationExpectingExactlyTwoArguments
                        { mapArg0 = Ok
                        , mapArg1 = Ok
                        , apply =
                            \leftValue rightValue ->
                                case rightValue of
                                    ListValue rightList ->
                                        Ok (ListValue (leftValue :: rightList))

                                    _ ->
                                        Err (DescribePathEnd "Right operand for listCons is not a list.")
                        }
                        application.arguments

                "PineKernel.booleanNot" ->
                    evaluateFunctionApplicationExpectingExactlyOneArgument
                        { mapArg = Ok
                        , apply =
                            \argument ->
                                if argument == trueValue then
                                    Ok falseValue

                                else
                                    Ok trueValue
                        }
                        application.arguments

                "(==)" ->
                    functionEquals

                "(/=)" ->
                    functionEqualsNot

                "(++)" ->
                    evaluateFunctionApplicationExpectingExactlyTwoArguments
                        { mapArg0 = Ok
                        , mapArg1 = Ok
                        , apply =
                            \leftValue rightValue ->
                                case ( leftValue, rightValue ) of
                                    ( BlobValue leftLiteral, BlobValue rightLiteral ) ->
                                        Ok (BlobValue (leftLiteral ++ rightLiteral))

                                    ( ListValue leftList, ListValue rightList ) ->
                                        Ok (ListValue (leftList ++ rightList))

                                    _ ->
                                        Err
                                            (DescribePathEnd
                                                ("Unexpected combination of operands for '++' ("
                                                    ++ describeValueSuperficial leftValue
                                                    ++ ", "
                                                    ++ describeValueSuperficial rightValue
                                                    ++ ")."
                                                )
                                            )
                        }
                        application.arguments

                "(&&)" ->
                    evaluateFunctionApplicationExpectingExactlyTwoArguments
                        { mapArg0 = Ok
                        , mapArg1 = Ok
                        , apply =
                            \leftValue rightValue ->
                                if leftValue == falseValue then
                                    Ok falseValue

                                else if leftValue == trueValue then
                                    if rightValue == falseValue then
                                        Ok falseValue

                                    else if rightValue == trueValue then
                                        Ok trueValue

                                    else
                                        Err (DescribePathEnd "Value right of && is not a 'Bool'")

                                else
                                    Err (DescribePathEnd "Value left of && is not a 'Bool'")
                        }
                        application.arguments

                "(+)" ->
                    functionOnTwoBigIntWithBigIntResult BigInt.add

                "(-)" ->
                    functionOnTwoBigIntWithBigIntResult BigInt.sub

                "(*)" ->
                    functionOnTwoBigIntWithBigIntResult BigInt.mul

                "(//)" ->
                    functionOnTwoBigIntWithBigIntResult BigInt.div

                "(<)" ->
                    functionOnTwoBigIntWithBooleanResult BigInt.lt

                "(<=)" ->
                    functionOnTwoBigIntWithBooleanResult BigInt.lte

                "(>)" ->
                    functionOnTwoBigIntWithBooleanResult BigInt.gt

                "(>=)" ->
                    functionOnTwoBigIntWithBooleanResult BigInt.gte

                _ ->
                    continueIgnoringAtomBindings ()

        _ ->
            continueIgnoringAtomBindings ()


evaluateFunctionApplicationIgnoringAtomBindings : ExpressionContext -> { function : Expression, arguments : List Value } -> Result (PathDescription String) Value
evaluateFunctionApplicationIgnoringAtomBindings context application =
    evaluateExpression context application.function
        |> Result.mapError (DescribePathNode ("Failed to evaluate function expression '" ++ describeExpression application.function ++ "'"))
        |> Result.andThen
            (\functionOrValue ->
                case application.arguments of
                    [] ->
                        Ok functionOrValue

                    firstArgument :: remainingArguments ->
                        let
                            continueWithClosure closureContext functionValue =
                                case functionValue of
                                    ClosureValue nextClosureContext closureExpression ->
                                        case closureExpression of
                                            FunctionExpression functionExpression ->
                                                evaluateFunctionApplicationIgnoringAtomBindings
                                                    (addToContext
                                                        [ valueFromContextExpansionWithName ( functionExpression.argumentName, firstArgument ) ]
                                                        nextClosureContext
                                                    )
                                                    { function = functionExpression.body, arguments = remainingArguments }
                                                    |> Result.mapError
                                                        (DescribePathNode
                                                            ("Failed application of '"
                                                                ++ describeExpression application.function
                                                                ++ "' with argument '"
                                                                ++ functionExpression.argumentName
                                                            )
                                                        )

                                            _ ->
                                                Err
                                                    (DescribePathEnd
                                                        ("Failed to apply: Expression "
                                                            ++ describeExpression closureExpression
                                                            ++ " is not a function (Too many arguments)."
                                                        )
                                                    )

                                    _ ->
                                        case decodeExpressionFromValue functionValue of
                                            Ok expressionFromValue ->
                                                continueWithClosure closureContext (ClosureValue closureContext expressionFromValue)

                                            Err decodeError ->
                                                Err
                                                    (DescribePathEnd
                                                        ("Failed to decode expression from value: " ++ decodeError)
                                                    )
                        in
                        continueWithClosure context functionOrValue
            )


intFromBigInt : BigInt.BigInt -> Result String Int
intFromBigInt bigInt =
    case bigInt |> BigInt.toString |> String.toInt of
        Nothing ->
            Err "Failed to String.toInt"

        Just int ->
            if String.fromInt int /= BigInt.toString bigInt then
                Err "Integer out of supported range for String.toInt"

            else
                Ok int


evaluateFunctionApplicationExpectingExactlyTwoArguments :
    { mapArg0 : Value -> Result (PathDescription String) arg0
    , mapArg1 : Value -> Result (PathDescription String) arg1
    , apply : arg0 -> arg1 -> Result (PathDescription String) Value
    }
    -> List Value
    -> Result (PathDescription String) Value
evaluateFunctionApplicationExpectingExactlyTwoArguments configuration arguments =
    case arguments of
        [ arg0, arg1 ] ->
            case configuration.mapArg0 arg0 of
                Err error ->
                    Err (DescribePathNode "Failed to map argument 0" error)

                Ok mappedArg0 ->
                    case configuration.mapArg1 arg1 of
                        Err error ->
                            Err (DescribePathNode "Failed to map argument 1" error)

                        Ok mappedArg1 ->
                            configuration.apply mappedArg0 mappedArg1

        _ ->
            Err
                (DescribePathEnd
                    ("Unexpected number of arguments for: "
                        ++ String.fromInt (List.length arguments)
                    )
                )


evaluateFunctionApplicationExpectingExactlyOneArgument :
    { mapArg : Value -> Result (PathDescription String) arg
    , apply : arg -> Result (PathDescription String) Value
    }
    -> List Value
    -> Result (PathDescription String) Value
evaluateFunctionApplicationExpectingExactlyOneArgument configuration arguments =
    case arguments of
        [ arg ] ->
            case configuration.mapArg arg of
                Err error ->
                    Err (DescribePathNode "Failed to map argument" error)

                Ok mappedArg ->
                    configuration.apply mappedArg

        _ ->
            Err
                (DescribePathEnd
                    ("Unexpected number of arguments for: "
                        ++ String.fromInt (List.length arguments)
                    )
                )


trueValue : Value
trueValue =
    tagValue "True" []


falseValue : Value
falseValue =
    tagValue "False" []


tagValue : String -> List Value -> Value
tagValue tagName tagArguments =
    ListValue [ valueFromString tagName, ListValue tagArguments ]


tagValueExpression : String -> List Expression -> Expression
tagValueExpression tagName tagArgumentsExpressions =
    ListExpression [ LiteralExpression (valueFromString tagName), ListExpression tagArgumentsExpressions ]


describeExpression : Expression -> String
describeExpression expression =
    case expression of
        FunctionOrValueExpression name ->
            "name(" ++ name ++ ")"

        ListExpression list ->
            "[" ++ String.join "," (list |> List.map describeExpression) ++ ")"

        LiteralExpression literal ->
            "literal(" ++ describeValue literal ++ ")"

        ApplicationExpression application ->
            "application(" ++ describeExpression application.function ++ ")"

        FunctionExpression functionExpression ->
            "function(" ++ functionExpression.argumentName ++ ", " ++ describeExpression functionExpression.body ++ ")"

        IfBlockExpression _ ->
            "if-block"

        ContextExpansionWithNameExpression expansion ->
            "context-expansion(" ++ expansion.name ++ ")"


describeValueSuperficial : Value -> String
describeValueSuperficial value =
    case value of
        BlobValue _ ->
            "BlobValue"

        ListValue _ ->
            "ListValue"

        ClosureValue _ _ ->
            "ClosureValue"


describeValue : Value -> String
describeValue value =
    case value of
        BlobValue blob ->
            "BlobValue 0x" ++ hexadecimalRepresentationFromBlobValue blob

        ListValue list ->
            "[" ++ String.join ", " (List.map describeValue list) ++ "]"

        ClosureValue _ expression ->
            "closure(" ++ describeExpression expression ++ ")"


valueFromString : String -> Value
valueFromString =
    String.toList
        >> List.map valueFromChar
        >> ListValue


valueFromChar : Char -> Value
valueFromChar =
    Char.toCode >> BigInt.fromInt >> unsignedBlobValueFromBigInt >> Maybe.withDefault [] >> BlobValue


stringFromValue : Value -> Result String String
stringFromValue value =
    case value of
        ListValue charsValues ->
            case charsValues |> List.map bigIntFromUnsignedValue |> Maybe.Extra.combine of
                Nothing ->
                    Err "Failed to map list elements to unsigned integers."

                Just chars ->
                    chars
                        |> List.map (BigInt.toString >> String.toInt >> Maybe.map Char.fromCode)
                        |> Maybe.Extra.combine
                        |> Maybe.map String.fromList
                        |> Result.fromMaybe "Programming error: Failed to map from integers to chars."

        _ ->
            Err "Only a ListValue can represent a string."


valueFromBigInt : BigInt.BigInt -> Value
valueFromBigInt =
    blobValueFromBigInt >> BlobValue


blobValueFromBigInt : BigInt.BigInt -> List Int
blobValueFromBigInt bigint =
    let
        value =
            BigInt.abs bigint

        signByte =
            if value == bigint then
                0

            else
                0x80

        unsignedBytesFromIntValue intValue =
            if BigInt.lt intValue (BigInt.fromInt 0x0100) then
                String.toInt (BigInt.toString intValue) |> Maybe.map List.singleton

            else
                case BigInt.divmod intValue (BigInt.fromInt 0x0100) of
                    Nothing ->
                        Nothing

                    Just ( upper, lower ) ->
                        case unsignedBytesFromIntValue upper of
                            Nothing ->
                                Nothing

                            Just upperBytes ->
                                case String.toInt (BigInt.toString lower) of
                                    Nothing ->
                                        Nothing

                                    Just lowerByte ->
                                        Just (upperBytes ++ [ lowerByte ])
    in
    signByte :: Maybe.withDefault [] (unsignedBytesFromIntValue value)


unsignedBlobValueFromBigInt : BigInt.BigInt -> Maybe (List Int)
unsignedBlobValueFromBigInt bigint =
    case blobValueFromBigInt bigint of
        [] ->
            Nothing

        signByte :: unsignedBytes ->
            if signByte == 0 then
                Just unsignedBytes

            else
                Nothing


bigIntFromValue : Value -> Result String BigInt.BigInt
bigIntFromValue value =
    case value of
        BlobValue blobValue ->
            bigIntFromBlobValue blobValue

        _ ->
            Err "Only a BlobValue can represent an integer."


bigIntFromBlobValue : List Int -> Result String BigInt.BigInt
bigIntFromBlobValue blobValue =
    case blobValue of
        [] ->
            Err "Empty blob is not a valid integer because the sign byte is missing. Did you mean to use an unsigned integer?"

        sign :: intValueBytes ->
            if sign /= 0 && sign /= 0x80 then
                Err ("Unexpected value for sign byte of integer: " ++ String.fromInt sign)

            else
                intValueBytes
                    |> bigIntFromUnsignedBlobValue
                    |> (if sign == 0 then
                            identity

                        else
                            BigInt.negate
                       )
                    |> Ok


bigIntFromUnsignedValue : Value -> Maybe BigInt.BigInt
bigIntFromUnsignedValue value =
    case value of
        BlobValue intValueBytes ->
            Just (bigIntFromUnsignedBlobValue intValueBytes)

        _ ->
            Nothing


bigIntFromUnsignedBlobValue : List Int -> BigInt.BigInt
bigIntFromUnsignedBlobValue intValueBytes =
    intValueBytes
        |> List.foldl
            (\nextByte aggregate ->
                BigInt.add (BigInt.fromInt nextByte) (BigInt.mul (BigInt.fromInt 0x0100) aggregate)
            )
            (BigInt.fromInt 0)


hexadecimalRepresentationFromBlobValue : List Int -> String
hexadecimalRepresentationFromBlobValue =
    List.map BigInt.fromInt
        >> List.map (BigInt.toHexString >> String.padLeft 2 '0')
        >> String.join ""


encodeExpressionAsValue : Expression -> Value
encodeExpressionAsValue expression =
    [ ( "pine_expression", jsonEncodeExpression expression ) ]
        |> Json.Encode.object
        |> Json.Encode.encode 0
        |> valueFromAsciiString


decodeExpressionFromValue : Value -> Result String Expression
decodeExpressionFromValue value =
    case asciiStringFromValue value of
        Err error ->
            Err ("Failed to decode as string: " ++ error)

        Ok string ->
            Json.Decode.decodeString (Json.Decode.field "pine_expression" jsonDecodeExpression) string
                |> Result.mapError Json.Decode.errorToString


jsonEncodeExpression : Expression -> Json.Encode.Value
jsonEncodeExpression expression =
    case expression of
        LiteralExpression literal ->
            Json.Encode.object [ ( "LiteralExpression", jsonEncodeValue literal ) ]

        ListExpression list ->
            Json.Encode.object [ ( "ListExpression", Json.Encode.list jsonEncodeExpression list ) ]

        ApplicationExpression applicationExpression ->
            Json.Encode.object
                [ ( "ApplicationExpression"
                  , Json.Encode.object
                        [ ( "function", jsonEncodeExpression applicationExpression.function )
                        , ( "arguments", Json.Encode.list jsonEncodeExpression applicationExpression.arguments )
                        ]
                  )
                ]

        FunctionOrValueExpression functionOrValueExpression ->
            Json.Encode.object [ ( "FunctionOrValueExpression", Json.Encode.string functionOrValueExpression ) ]

        ContextExpansionWithNameExpression contextExpansionWithNameExpression ->
            Json.Encode.object
                [ ( "ContextExpansionWithNameExpression"
                  , jsonEncodeContextExpansionWithNameExpression contextExpansionWithNameExpression
                  )
                ]

        IfBlockExpression ifBlock ->
            Json.Encode.object
                [ ( "IfBlockExpression"
                  , Json.Encode.object
                        [ ( "condition", jsonEncodeExpression ifBlock.condition )
                        , ( "ifTrue", jsonEncodeExpression ifBlock.ifTrue )
                        , ( "ifFalse", jsonEncodeExpression ifBlock.ifFalse )
                        ]
                  )
                ]

        FunctionExpression functionExpression ->
            Json.Encode.object
                [ ( "FunctionExpression"
                  , Json.Encode.object
                        [ ( "argumentName", Json.Encode.string functionExpression.argumentName )
                        , ( "body", jsonEncodeExpression functionExpression.body )
                        ]
                  )
                ]


jsonDecodeExpression : Json.Decode.Decoder Expression
jsonDecodeExpression =
    Json.Decode.oneOf
        [ Json.Decode.field "LiteralExpression"
            (Json.Decode.map LiteralExpression jsonDecodeValue)
        , Json.Decode.field "ListExpression"
            (Json.Decode.map ListExpression (Json.Decode.list (Json.Decode.lazy (\() -> jsonDecodeExpression))))
        , Json.Decode.field "ApplicationExpression"
            (Json.Decode.map ApplicationExpression
                (Json.Decode.map2 ApplicationExpressionStructure
                    (Json.Decode.field "function" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                    (Json.Decode.field "arguments" (Json.Decode.lazy (\() -> Json.Decode.list jsonDecodeExpression)))
                )
            )
        , Json.Decode.field "FunctionOrValueExpression"
            (Json.Decode.map FunctionOrValueExpression Json.Decode.string)
        , Json.Decode.field "ContextExpansionWithNameExpression"
            (Json.Decode.map ContextExpansionWithNameExpression jsonDecodeContextExpansionWithNameExpression)
        , Json.Decode.field "IfBlockExpression"
            (Json.Decode.lazy
                (\() ->
                    Json.Decode.map IfBlockExpression
                        (Json.Decode.map3 IfBlockExpressionStructure
                            (Json.Decode.field "condition" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                            (Json.Decode.field "ifTrue" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                            (Json.Decode.field "ifFalse" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                        )
                )
            )
        , Json.Decode.field "FunctionExpression"
            (Json.Decode.lazy
                (\() ->
                    Json.Decode.map FunctionExpression
                        (Json.Decode.map2 FunctionExpressionStructure
                            (Json.Decode.field "argumentName" Json.Decode.string)
                            (Json.Decode.field "body" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                        )
                )
            )
        ]


jsonEncodeContextExpansionWithNameExpression : ContextExpansionWithNameExpressionStructure -> Json.Encode.Value
jsonEncodeContextExpansionWithNameExpression contextExpansionWithNameExpression =
    Json.Encode.object
        [ ( "name", Json.Encode.string contextExpansionWithNameExpression.name )
        , ( "namedValue", jsonEncodeValue contextExpansionWithNameExpression.namedValue )
        , ( "expression", jsonEncodeExpression contextExpansionWithNameExpression.expression )
        ]


jsonDecodeContextExpansionWithNameExpression : Json.Decode.Decoder ContextExpansionWithNameExpressionStructure
jsonDecodeContextExpansionWithNameExpression =
    Json.Decode.map3 ContextExpansionWithNameExpressionStructure
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "namedValue" jsonDecodeValue)
        (Json.Decode.field "expression" (Json.Decode.lazy (\() -> jsonDecodeExpression)))


jsonEncodeValue : Value -> Json.Encode.Value
jsonEncodeValue value =
    case value of
        BlobValue blob ->
            Json.Encode.object [ ( "BlobValue", Json.Encode.list Json.Encode.int blob ) ]

        ListValue list ->
            Json.Encode.object [ ( "ListValue", Json.Encode.list jsonEncodeValue list ) ]

        ClosureValue _ _ ->
            Json.Encode.object [ ( "ClosureValue", Json.Encode.string "not_implemented" ) ]


jsonDecodeValue : Json.Decode.Decoder Value
jsonDecodeValue =
    Json.Decode.oneOf
        [ Json.Decode.field "BlobValue"
            (Json.Decode.map BlobValue (Json.Decode.list Json.Decode.int))
        , Json.Decode.field "ListValue"
            (Json.Decode.map ListValue (Json.Decode.list (Json.Decode.lazy (\() -> jsonDecodeValue))))
        , Json.Decode.field "ClosureValue"
            (Json.Decode.fail "Decoding of ClosureValue not implemented")
        ]


asciiStringFromValue : Value -> Result String String
asciiStringFromValue value =
    case value of
        BlobValue charsValues ->
            charsValues
                |> List.map Char.fromCode
                |> String.fromList
                |> Ok

        _ ->
            Err "Only a BlobValue can represent an ASCII string."


valueFromAsciiString : String -> Value
valueFromAsciiString =
    String.toList >> List.map Char.toCode >> BlobValue
