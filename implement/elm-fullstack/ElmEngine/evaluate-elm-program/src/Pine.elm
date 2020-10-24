module Pine exposing (..)

import BigInt


type PineExpression
    = PineLiteral PineValue
    | PineApplication { function : PineExpression, arguments : List PineExpression }
    | PineFunctionOrValue String


type PineValue
    = PineStringOrInteger String


evaluatePineExpression : PineExpression -> Result String PineValue
evaluatePineExpression expression =
    case expression of
        PineLiteral pineValue ->
            Ok pineValue

        PineApplication application ->
            evaluatePineApplication application

        PineFunctionOrValue _ ->
            Err "PineFunctionOrValue not implemented yet."


evaluatePineApplication : { function : PineExpression, arguments : List PineExpression } -> Result String PineValue
evaluatePineApplication application =
    case application.function of
        PineFunctionOrValue functionName ->
            case functionName of
                "String.fromInt" ->
                    case application.arguments of
                        [ argument ] ->
                            evaluatePineExpression argument

                        _ ->
                            Err
                                ("Unexpected number of arguments for String.fromInt: "
                                    ++ String.fromInt (List.length application.arguments)
                                )

                "(++)" ->
                    evaluatePineApplicationExpectingExactlyTwoArguments
                        { mapArg0 = evaluatePineExpression
                        , mapArg1 = evaluatePineExpression
                        , apply =
                            \leftValue rightValue ->
                                case ( leftValue, rightValue ) of
                                    ( PineStringOrInteger leftLiteral, PineStringOrInteger rightLiteral ) ->
                                        Ok (PineStringOrInteger (leftLiteral ++ rightLiteral))
                        }
                        application.arguments

                "(+)" ->
                    evaluatePineApplicationExpectingExactlyTwoArguments
                        { mapArg0 = evaluatePineExpression >> Result.andThen parseAsBigInt
                        , mapArg1 = evaluatePineExpression >> Result.andThen parseAsBigInt
                        , apply =
                            \leftInt rightInt ->
                                Ok (PineStringOrInteger (BigInt.add leftInt rightInt |> BigInt.toString))
                        }
                        application.arguments

                "(*)" ->
                    evaluatePineApplicationExpectingExactlyTwoArguments
                        { mapArg0 = evaluatePineExpression >> Result.andThen parseAsBigInt
                        , mapArg1 = evaluatePineExpression >> Result.andThen parseAsBigInt
                        , apply =
                            \leftInt rightInt ->
                                Ok (PineStringOrInteger (BigInt.mul leftInt rightInt |> BigInt.toString))
                        }
                        application.arguments

                "(//)" ->
                    evaluatePineApplicationExpectingExactlyTwoArguments
                        { mapArg0 = evaluatePineExpression >> Result.andThen parseAsBigInt
                        , mapArg1 = evaluatePineExpression >> Result.andThen parseAsBigInt
                        , apply =
                            \leftInt rightInt ->
                                Ok (PineStringOrInteger (BigInt.div leftInt rightInt |> BigInt.toString))
                        }
                        application.arguments

                _ ->
                    Err ("Function " ++ functionName ++ " not implemented yet.")

        _ ->
            Err "Application not implemented yet."


parseAsBigInt : PineValue -> Result String BigInt.BigInt
parseAsBigInt value =
    case value of
        PineStringOrInteger stringOrInt ->
            BigInt.fromIntString stringOrInt
                |> Result.fromMaybe ("Failed to parse as integer: " ++ stringOrInt)


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
