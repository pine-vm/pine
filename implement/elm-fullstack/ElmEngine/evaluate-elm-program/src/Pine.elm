module Pine exposing (..)

import BigInt
import Json.Encode
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
    = StringOrIntegerValue String
    | ListValue (List Value)
      -- TODO: Replace ExpressionValue with convention for mapping value to expression.
    | ExpressionValue Expression


type alias ExpressionContext =
    -- TODO: Test consolidate into simple Value
    { commonModel : List Value
    , provisionalArgumentStack : List Value
    }


addToContext : List Value -> ExpressionContext -> ExpressionContext
addToContext names context =
    { context | commonModel = names ++ context.commonModel }


evaluateExpression : ExpressionContext -> Expression -> Result String Value
evaluateExpression context expression =
    case expression of
        LiteralExpression value ->
            Ok value

        ListExpression listElements ->
            listElements
                |> List.map (evaluateExpression context)
                |> Result.Extra.combine
                |> Result.map ListValue
                |> Result.mapError (\error -> "Failed to evaluate list element: " ++ error)

        ApplicationExpression application ->
            case evaluateFunctionApplication context application of
                Err error ->
                    Err ("Failed application: " ++ error)

                Ok (ExpressionValue expressionAfterApplication) ->
                    evaluateExpression context expressionAfterApplication

                otherResult ->
                    otherResult

        FunctionOrValueExpression name ->
            case name of
                "True" ->
                    Ok trueValue

                "False" ->
                    Ok falseValue

                _ ->
                    let
                        beforeCheckForExpression =
                            lookUpNameInContext name context
                                |> Result.mapError
                                    (\error -> "Failed to look up name '" ++ name ++ "': " ++ error)
                    in
                    case beforeCheckForExpression of
                        Ok ( ExpressionValue expressionFromLookup, contextFromLookup ) ->
                            evaluateExpression (addToContext contextFromLookup context) expressionFromLookup

                        _ ->
                            Result.map Tuple.first beforeCheckForExpression

        IfBlockExpression condition expressionIfTrue expressionIfFalse ->
            case evaluateExpression context condition of
                Err error ->
                    Err ("Failed to evaluate condition: " ++ error)

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
            case context.provisionalArgumentStack of
                nextArgumentValue :: remainingArgumentValues ->
                    evaluateExpression
                        { context | provisionalArgumentStack = remainingArgumentValues }
                        (ContextExpansionWithNameExpression ( argumentName, nextArgumentValue ) expressionInExpandedContext)

                [] ->
                    Ok (ExpressionValue expression)


valueFromContextExpansionWithName : ( String, Value ) -> Value
valueFromContextExpansionWithName ( declName, declValue ) =
    ListValue [ StringOrIntegerValue declName, declValue ]


namedValueFromValue : Value -> Maybe ( String, Value )
namedValueFromValue value =
    case value of
        ListValue [ StringOrIntegerValue elementLabel, elementValue ] ->
            Just ( elementLabel, elementValue )

        _ ->
            Nothing


lookUpNameInContext : String -> ExpressionContext -> Result String ( Value, List Value )
lookUpNameInContext name context =
    case name |> String.split "." of
        [] ->
            Err "nameElements is empty"

        nameFirstElement :: nameRemainingElements ->
            let
                availableNames =
                    context.commonModel |> List.filterMap namedValueFromValue

                maybeMatchingValue =
                    availableNames
                        |> List.filter (Tuple.first >> (==) nameFirstElement)
                        |> List.head
                        |> Maybe.map Tuple.second
            in
            case maybeMatchingValue of
                Nothing ->
                    Err
                        ("Did not find '"
                            ++ nameFirstElement
                            ++ "'. "
                            ++ (availableNames |> List.length |> String.fromInt)
                            ++ " names available: "
                            ++ (availableNames |> List.map Tuple.first |> String.join ", ")
                        )

                Just firstNameValue ->
                    if nameRemainingElements == [] then
                        Ok ( firstNameValue, context.commonModel )

                    else
                        case firstNameValue of
                            ListValue firstNameList ->
                                lookUpNameInContext (String.join "." nameRemainingElements)
                                    { commonModel = firstNameList, provisionalArgumentStack = [] }

                            _ ->
                                Err ("'" ++ nameFirstElement ++ "' has unexpected type: Not a list.")


evaluateFunctionApplication : ExpressionContext -> { function : Expression, arguments : List Expression } -> Result String Value
evaluateFunctionApplication context application =
    case application.arguments |> List.map (evaluateExpression context) |> Result.Extra.combine of
        Err evalArgError ->
            Err ("Failed to evaluate argument: " ++ evalArgError)

        Ok arguments ->
            let
                functionOnTwoBigIntWithBooleanResult functionOnBigInt =
                    evaluateFunctionApplicationExpectingExactlyTwoArguments
                        { mapArg0 = evaluateExpression context >> Result.andThen parseAsBigInt
                        , mapArg1 = evaluateExpression context >> Result.andThen parseAsBigInt
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
                        { mapArg0 = evaluateExpression context >> Result.andThen parseAsBigInt
                        , mapArg1 = evaluateExpression context >> Result.andThen parseAsBigInt
                        , apply =
                            \leftInt rightInt ->
                                Ok (StringOrIntegerValue (functionOnBigInt leftInt rightInt |> BigInt.toString))
                        }
                        application.arguments

                functionExpectingOneArgumentOfTypeList functionOnList =
                    evaluateFunctionApplicationExpectingExactlyOneArgument
                        { mapArg = evaluateExpression context
                        , apply =
                            \argument ->
                                case argument of
                                    ListValue list ->
                                        list |> functionOnList |> Ok

                                    _ ->
                                        Err ("Argument is not a list ('" ++ describeValue argument ++ ")")
                        }
                        application.arguments

                functionEquals =
                    evaluateFunctionApplicationExpectingExactlyTwoArguments
                        { mapArg0 = evaluateExpression context
                        , mapArg1 = evaluateExpression context
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
            in
            case application.function of
                FunctionOrValueExpression functionName ->
                    case functionName of
                        "PineKernel.equals" ->
                            functionEquals

                        "PineKernel.negate" ->
                            evaluateFunctionApplicationExpectingExactlyOneArgument
                                { mapArg = evaluateExpression context >> Result.andThen parseAsBigInt
                                , apply = BigInt.negate >> BigInt.toString >> StringOrIntegerValue >> Ok
                                }
                                application.arguments

                        "PineKernel.listHead" ->
                            functionExpectingOneArgumentOfTypeList (List.head >> Maybe.withDefault (ListValue []))

                        "PineKernel.listTail" ->
                            functionExpectingOneArgumentOfTypeList (List.tail >> Maybe.withDefault [] >> ListValue)

                        "PineKernel.listCons" ->
                            evaluateFunctionApplicationExpectingExactlyTwoArguments
                                { mapArg0 = evaluateExpression context
                                , mapArg1 = evaluateExpression context
                                , apply =
                                    \leftValue rightValue ->
                                        case rightValue of
                                            ListValue rightList ->
                                                Ok (ListValue (leftValue :: rightList))

                                            _ ->
                                                Err "Right operand for listCons is not a list."
                                }
                                application.arguments

                        "String.fromInt" ->
                            case application.arguments of
                                [ argument ] ->
                                    evaluateExpression context argument

                                _ ->
                                    Err
                                        ("Unexpected number of arguments for String.fromInt: "
                                            ++ String.fromInt (List.length application.arguments)
                                        )

                        "(==)" ->
                            functionEquals

                        "(++)" ->
                            evaluateFunctionApplicationExpectingExactlyTwoArguments
                                { mapArg0 = evaluateExpression context
                                , mapArg1 = evaluateExpression context
                                , apply =
                                    \leftValue rightValue ->
                                        case ( leftValue, rightValue ) of
                                            ( StringOrIntegerValue leftLiteral, StringOrIntegerValue rightLiteral ) ->
                                                Ok (StringOrIntegerValue (leftLiteral ++ rightLiteral))

                                            ( ListValue leftList, ListValue rightList ) ->
                                                Ok (ListValue (leftList ++ rightList))

                                            _ ->
                                                Err "Unexpected combination of operands."
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
                                { mapArg = evaluateExpression context
                                , apply =
                                    \argument ->
                                        if argument == trueValue then
                                            Ok falseValue

                                        else
                                            Ok trueValue
                                }
                                application.arguments

                        _ ->
                            case lookUpNameInContext functionName context of
                                Err lookupError ->
                                    Err ("Failed to look up name '" ++ functionName ++ "': " ++ lookupError)

                                Ok ( ExpressionValue expression, contextFromLookup ) ->
                                    evaluateExpression
                                        (addToContext
                                            contextFromLookup
                                            { commonModel = []
                                            , provisionalArgumentStack = arguments ++ context.provisionalArgumentStack
                                            }
                                        )
                                        expression

                                _ ->
                                    Err "Unexpected value for function in application: Not an expression."

                FunctionExpression argumentName functionExpression ->
                    case arguments of
                        [] ->
                            Ok (ExpressionValue application.function)

                        firstArgument :: remainingArguments ->
                            evaluateExpression
                                (addToContext
                                    [ valueFromContextExpansionWithName ( argumentName, firstArgument ) ]
                                    { commonModel = [], provisionalArgumentStack = remainingArguments }
                                )
                                functionExpression

                _ ->
                    Err "Application not implemented yet."


parseAsBigInt : Value -> Result String BigInt.BigInt
parseAsBigInt value =
    case value of
        StringOrIntegerValue stringOrInt ->
            BigInt.fromIntString stringOrInt
                |> Result.fromMaybe ("Failed to parse as integer: " ++ stringOrInt)

        ListValue _ ->
            Err "Unexpected type of value: List"

        ExpressionValue _ ->
            Err "Unexpected type of value: ExpressionValue"


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
    { mapArg0 : Expression -> Result String arg0
    , mapArg1 : Expression -> Result String arg1
    , apply : arg0 -> arg1 -> Result String Value
    }
    -> List Expression
    -> Result String Value
evaluateFunctionApplicationExpectingExactlyTwoArguments configuration arguments =
    case arguments of
        [ arg0, arg1 ] ->
            case configuration.mapArg0 arg0 of
                Err error ->
                    Err ("Failed to map argument 0: " ++ error)

                Ok mappedArg0 ->
                    case configuration.mapArg1 arg1 of
                        Err error ->
                            Err ("Failed to map argument 1: " ++ error)

                        Ok mappedArg1 ->
                            configuration.apply mappedArg0 mappedArg1

        _ ->
            Err
                ("Unexpected number of arguments for: "
                    ++ String.fromInt (List.length arguments)
                )


evaluateFunctionApplicationExpectingExactlyOneArgument :
    { mapArg : Expression -> Result String arg
    , apply : arg -> Result String Value
    }
    -> List Expression
    -> Result String Value
evaluateFunctionApplicationExpectingExactlyOneArgument configuration arguments =
    case arguments of
        [ arg ] ->
            case configuration.mapArg arg of
                Err error ->
                    Err ("Failed to map argument: " ++ error)

                Ok mappedArg ->
                    configuration.apply mappedArg

        _ ->
            Err
                ("Unexpected number of arguments for: "
                    ++ String.fromInt (List.length arguments)
                )


trueValue : Value
trueValue =
    tagValue "True" []


falseValue : Value
falseValue =
    tagValue "False" []


tagValue : String -> List Value -> Value
tagValue tagName tagArguments =
    ListValue [ StringOrIntegerValue tagName, ListValue tagArguments ]


tagValueExpression : String -> List Expression -> Expression
tagValueExpression tagName tagArgumentsExpressions =
    ListExpression [ LiteralExpression (StringOrIntegerValue tagName), ListExpression tagArgumentsExpressions ]


describeValue : Value -> String
describeValue value =
    case value of
        StringOrIntegerValue string ->
            "StringOrIntegerValue " ++ Json.Encode.encode 0 (Json.Encode.string string)

        ListValue list ->
            "[" ++ String.join ", " (List.map describeValue list) ++ "]"

        ExpressionValue _ ->
            "<expression>"
