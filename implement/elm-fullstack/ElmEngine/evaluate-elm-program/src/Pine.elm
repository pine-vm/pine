module Pine exposing (..)

import BigInt
import Result.Extra


type PineExpression
    = PineLiteral PineValue
    | PineListExpr (List PineExpression)
    | PineApplication { function : PineExpression, arguments : List PineExpression }
    | PineFunctionOrValue String
    | PineContextExpansionWithName ( String, PineValue ) PineExpression
    | PineIfBlock PineExpression PineExpression PineExpression
    | PineFunction String PineExpression


type PineValue
    = PineStringOrInteger String
    | PineList (List PineValue)
      -- TODO: Replace PineExpressionValue with convention for mapping value to expression.
    | PineExpressionValue PineExpression


type alias PineExpressionContext =
    -- TODO: Test consolidate into simple PineValue
    { commonModel : List PineValue
    , provisionalArgumentStack : List PineValue
    }


addToContext : List PineValue -> PineExpressionContext -> PineExpressionContext
addToContext names context =
    { context | commonModel = context.commonModel ++ names }


evaluatePineExpression : PineExpressionContext -> PineExpression -> Result String PineValue
evaluatePineExpression context expression =
    case expression of
        PineLiteral pineValue ->
            Ok pineValue

        PineListExpr listElements ->
            listElements
                |> List.map (evaluatePineExpression context)
                |> Result.Extra.combine
                |> Result.map PineList
                |> Result.mapError (\error -> "Failed to evaluate list element: " ++ error)

        PineApplication application ->
            case evaluatePineApplication context application of
                Err error ->
                    Err ("Failed application: " ++ error)

                Ok (PineExpressionValue expressionAfterApplication) ->
                    evaluatePineExpression context expressionAfterApplication

                otherResult ->
                    otherResult

        PineFunctionOrValue name ->
            case name of
                "True" ->
                    Ok truePineValue

                "False" ->
                    Ok falsePineValue

                _ ->
                    let
                        beforeCheckForExpression =
                            lookUpNameInContext name context
                                |> Result.mapError
                                    (\error -> "Failed to look up name '" ++ name ++ "': " ++ error)
                    in
                    case beforeCheckForExpression of
                        Ok ( PineExpressionValue expressionFromLookup, contextFromLookup ) ->
                            evaluatePineExpression (addToContext contextFromLookup context) expressionFromLookup

                        _ ->
                            Result.map Tuple.first beforeCheckForExpression

        PineIfBlock condition expressionIfTrue expressionIfFalse ->
            case evaluatePineExpression context condition of
                Err error ->
                    Err ("Failed to evaluate condition: " ++ error)

                Ok conditionValue ->
                    evaluatePineExpression context
                        (if conditionValue == truePineValue then
                            expressionIfTrue

                         else
                            expressionIfFalse
                        )

        PineContextExpansionWithName expansion expressionInExpandedContext ->
            evaluatePineExpression
                { context | commonModel = pineValueFromContextExpansionWithName expansion :: context.commonModel }
                expressionInExpandedContext

        PineFunction argumentName expressionInExpandedContext ->
            case context.provisionalArgumentStack of
                nextArgumentValue :: remainingArgumentValues ->
                    evaluatePineExpression
                        { context | provisionalArgumentStack = remainingArgumentValues }
                        (PineContextExpansionWithName ( argumentName, nextArgumentValue ) expressionInExpandedContext)

                [] ->
                    Ok (PineExpressionValue expression)


pineValueFromContextExpansionWithName : ( String, PineValue ) -> PineValue
pineValueFromContextExpansionWithName ( declName, declValue ) =
    PineList [ PineStringOrInteger declName, declValue ]


pineNamedValueFromValue : PineValue -> Maybe ( String, PineValue )
pineNamedValueFromValue value =
    case value of
        PineList [ PineStringOrInteger elementLabel, elementValue ] ->
            Just ( elementLabel, elementValue )

        _ ->
            Nothing


lookUpNameInContext : String -> PineExpressionContext -> Result String ( PineValue, List PineValue )
lookUpNameInContext name context =
    case name |> String.split "." of
        [] ->
            Err "nameElements is empty"

        nameFirstElement :: nameRemainingElements ->
            let
                availableNames =
                    context.commonModel
                        |> List.filterMap pineNamedValueFromValue

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
                            PineList firstNameList ->
                                lookUpNameInContext (String.join "." nameRemainingElements)
                                    { commonModel = firstNameList, provisionalArgumentStack = [] }

                            _ ->
                                Err ("'" ++ nameFirstElement ++ "' has unexpected type: Not a list.")


evaluatePineApplication : PineExpressionContext -> { function : PineExpression, arguments : List PineExpression } -> Result String PineValue
evaluatePineApplication context application =
    case application.arguments |> List.map (evaluatePineExpression context) |> Result.Extra.combine of
        Err evalArgError ->
            Err ("Failed to evaluate argument: " ++ evalArgError)

        Ok arguments ->
            let
                functionOnTwoBigIntWithBooleanResult functionOnBigInt =
                    evaluatePineApplicationExpectingExactlyTwoArguments
                        { mapArg0 = evaluatePineExpression context >> Result.andThen parseAsBigInt
                        , mapArg1 = evaluatePineExpression context >> Result.andThen parseAsBigInt
                        , apply =
                            \leftInt rightInt ->
                                Ok
                                    (if functionOnBigInt leftInt rightInt then
                                        truePineValue

                                     else
                                        falsePineValue
                                    )
                        }
                        application.arguments

                functionOnTwoBigIntWithBigIntResult functionOnBigInt =
                    evaluatePineApplicationExpectingExactlyTwoArguments
                        { mapArg0 = evaluatePineExpression context >> Result.andThen parseAsBigInt
                        , mapArg1 = evaluatePineExpression context >> Result.andThen parseAsBigInt
                        , apply =
                            \leftInt rightInt ->
                                Ok (PineStringOrInteger (functionOnBigInt leftInt rightInt |> BigInt.toString))
                        }
                        application.arguments

                functionExpectingOneArgumentOfTypeList functionOnList =
                    evaluatePineApplicationExpectingExactlyOneArgument
                        { mapArg = evaluatePineExpression context
                        , apply =
                            \argument ->
                                case argument of
                                    PineList list ->
                                        list |> functionOnList |> Ok

                                    _ ->
                                        Err "Argument is not a list."
                        }
                        application.arguments
            in
            case application.function of
                PineFunctionOrValue functionName ->
                    case functionName of
                        "PineKernel.listHead" ->
                            functionExpectingOneArgumentOfTypeList (List.head >> Maybe.withDefault (PineList []))

                        "PineKernel.listTail" ->
                            functionExpectingOneArgumentOfTypeList (List.tail >> Maybe.withDefault [] >> PineList)

                        "String.fromInt" ->
                            case application.arguments of
                                [ argument ] ->
                                    evaluatePineExpression context argument

                                _ ->
                                    Err
                                        ("Unexpected number of arguments for String.fromInt: "
                                            ++ String.fromInt (List.length application.arguments)
                                        )

                        "(==)" ->
                            evaluatePineApplicationExpectingExactlyTwoArguments
                                { mapArg0 = evaluatePineExpression context
                                , mapArg1 = evaluatePineExpression context
                                , apply =
                                    \leftValue rightValue ->
                                        Ok
                                            (if leftValue == rightValue then
                                                truePineValue

                                             else
                                                falsePineValue
                                            )
                                }
                                application.arguments

                        "(++)" ->
                            evaluatePineApplicationExpectingExactlyTwoArguments
                                { mapArg0 = evaluatePineExpression context
                                , mapArg1 = evaluatePineExpression context
                                , apply =
                                    \leftValue rightValue ->
                                        case ( leftValue, rightValue ) of
                                            ( PineStringOrInteger leftLiteral, PineStringOrInteger rightLiteral ) ->
                                                Ok (PineStringOrInteger (leftLiteral ++ rightLiteral))

                                            ( PineList leftList, PineList rightList ) ->
                                                Ok (PineList (leftList ++ rightList))

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
                            evaluatePineApplicationExpectingExactlyOneArgument
                                { mapArg = evaluatePineExpression context
                                , apply =
                                    \argument ->
                                        if argument == truePineValue then
                                            Ok falsePineValue

                                        else
                                            Ok truePineValue
                                }
                                application.arguments

                        _ ->
                            case lookUpNameInContext functionName context of
                                Err lookupError ->
                                    Err ("Failed to look up name '" ++ functionName ++ "': " ++ lookupError)

                                Ok ( PineExpressionValue expression, contextFromLookup ) ->
                                    evaluatePineExpression
                                        (addToContext
                                            contextFromLookup
                                            { context
                                                | provisionalArgumentStack = arguments ++ context.provisionalArgumentStack
                                            }
                                        )
                                        expression

                                _ ->
                                    Err "Unexpected value for function in application: Not an expression."

                PineFunction argumentName functionExpression ->
                    case arguments of
                        [] ->
                            Ok (PineExpressionValue application.function)

                        firstArgument :: remainingArguments ->
                            evaluatePineExpression
                                (addToContext
                                    [ pineValueFromContextExpansionWithName ( argumentName, firstArgument ) ]
                                    { context | provisionalArgumentStack = remainingArguments }
                                )
                                functionExpression

                _ ->
                    Err "Application not implemented yet."


parseAsBigInt : PineValue -> Result String BigInt.BigInt
parseAsBigInt value =
    case value of
        PineStringOrInteger stringOrInt ->
            BigInt.fromIntString stringOrInt
                |> Result.fromMaybe ("Failed to parse as integer: " ++ stringOrInt)

        PineList _ ->
            Err "Unexpected type of value: List"

        PineExpressionValue _ ->
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


evaluatePineApplicationExpectingExactlyTwoArguments :
    { mapArg0 : PineExpression -> Result String arg0
    , mapArg1 : PineExpression -> Result String arg1
    , apply : arg0 -> arg1 -> Result String PineValue
    }
    -> List PineExpression
    -> Result String PineValue
evaluatePineApplicationExpectingExactlyTwoArguments configuration arguments =
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


evaluatePineApplicationExpectingExactlyOneArgument :
    { mapArg : PineExpression -> Result String arg
    , apply : arg -> Result String PineValue
    }
    -> List PineExpression
    -> Result String PineValue
evaluatePineApplicationExpectingExactlyOneArgument configuration arguments =
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


truePineValue : PineValue
truePineValue =
    tagValue "True" []


falsePineValue : PineValue
falsePineValue =
    tagValue "False" []


tagValue : String -> List PineValue -> PineValue
tagValue tagName tagArguments =
    PineList [ PineStringOrInteger tagName, PineList tagArguments ]
