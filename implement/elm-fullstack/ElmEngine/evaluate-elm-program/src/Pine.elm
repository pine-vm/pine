module Pine exposing (..)

import BigInt
import Json.Encode
import Maybe.Extra
import Result.Extra


type Expression
    = LiteralExpression Value
    | ListExpression (List Expression)
    | ApplicationExpression { function : Expression, arguments : List Expression }
    | FunctionOrValueExpression String
    | ContextExpansionWithNameExpression ( String, Value ) Expression
    | IfBlockExpression Expression Expression Expression
    | FunctionExpression String Expression


type Value
    = BlobValue (List Int)
    | ListValue (List Value)
      -- TODO: Replace ExpressionValue with convention for mapping value to expression.
    | ExpressionValue Expression
    | ClosureValue ExpressionContext String Expression


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
                        Ok ( ExpressionValue expressionFromLookup, contextFromLookup ) ->
                            evaluateExpression (addToContext contextFromLookup context) expressionFromLookup
                                |> Result.mapError (DescribePathNode "Failed to evaluate expression from name")

                        _ ->
                            Result.map Tuple.first beforeCheckForExpression

        IfBlockExpression condition expressionIfTrue expressionIfFalse ->
            case evaluateExpression context condition of
                Err error ->
                    Err (DescribePathNode "Failed to evaluate condition" error)

                Ok conditionValue ->
                    evaluateExpression context
                        (if conditionValue == trueValue then
                            expressionIfTrue

                         else
                            expressionIfFalse
                        )

        ContextExpansionWithNameExpression expansion expressionInExpandedContext ->
            evaluateExpression
                { context | commonModel = valueFromContextExpansionWithName expansion :: context.commonModel }
                expressionInExpandedContext

        FunctionExpression argumentName expressionInExpandedContext ->
            Ok (ClosureValue context argumentName expressionInExpandedContext)


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

                maybeMatchingValue =
                    context.commonModel
                        |> List.filterMap
                            (\candidate ->
                                case candidate of
                                    ListValue [ labelValue, namedValue ] ->
                                        if labelValue == pathFirstElement then
                                            Just namedValue

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                        |> List.head
            in
            case maybeMatchingValue of
                Nothing ->
                    let
                        availableNames =
                            context.commonModel |> List.filterMap namedValueFromValue
                    in
                    Err
                        (DescribePathEnd
                            ("Did not find '"
                                ++ getPathFirstElementAsString ()
                                ++ "'. "
                                ++ (availableNames |> List.length |> String.fromInt)
                                ++ " names available: "
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
                                Err (DescribePathEnd ("Argument is not a list ('" ++ describeValue argument ++ ")"))
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

                "String.fromInt" ->
                    case application.arguments of
                        [ argument ] ->
                            case bigIntFromValue argument of
                                Err error ->
                                    Err (DescribePathEnd ("Failed to map to integer: " ++ error))

                                Ok bigInt ->
                                    bigInt |> BigInt.toString |> valueFromString |> Ok

                        _ ->
                            Err
                                (DescribePathEnd
                                    ("Unexpected number of arguments for String.fromInt: "
                                        ++ String.fromInt (List.length application.arguments)
                                    )
                                )

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

                "not" ->
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
                            continueWithClosure closureContext argumentName functionExpression =
                                evaluateFunctionApplicationIgnoringAtomBindings
                                    (addToContext
                                        [ valueFromContextExpansionWithName ( argumentName, firstArgument ) ]
                                        closureContext
                                    )
                                    { function = functionExpression, arguments = remainingArguments }
                                    |> Result.mapError
                                        (DescribePathNode
                                            ("Failed application of '"
                                                ++ describeExpression application.function
                                                ++ "' with argument '"
                                                ++ argumentName
                                            )
                                        )
                        in
                        case functionOrValue of
                            ExpressionValue (FunctionExpression argumentName functionExpression) ->
                                continueWithClosure context argumentName functionExpression

                            ClosureValue closureContext argumentName functionExpression ->
                                continueWithClosure closureContext argumentName functionExpression

                            _ ->
                                Err
                                    (DescribePathEnd
                                        ("Failed to apply: Value "
                                            ++ describeValue functionOrValue
                                            ++ " is not a function (Too many arguments)."
                                        )
                                    )
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

        FunctionExpression argumentName functionExpression ->
            "function(" ++ argumentName ++ ", " ++ describeExpression functionExpression ++ ")"

        IfBlockExpression _ _ _ ->
            "if-block"

        ContextExpansionWithNameExpression ( newName, _ ) _ ->
            "context-expansion(" ++ newName ++ ")"


describeValueSuperficial : Value -> String
describeValueSuperficial value =
    case value of
        BlobValue _ ->
            "BlobValue"

        ListValue _ ->
            "ListValue"

        ExpressionValue _ ->
            "ExpressionValue"

        ClosureValue _ _ _ ->
            "ClosureValue"


describeValue : Value -> String
describeValue value =
    case value of
        BlobValue blob ->
            "BlobValue 0x" ++ Json.Encode.encode 0 (Json.Encode.string (hexadecimalRepresentationFromBlobValue blob))

        ListValue list ->
            "[" ++ String.join ", " (List.map describeValue list) ++ "]"

        ExpressionValue expression ->
            "expression(" ++ describeExpression expression ++ ")"

        ClosureValue _ argumentName expression ->
            "closure(" ++ argumentName ++ "," ++ describeExpression expression ++ ")"


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
