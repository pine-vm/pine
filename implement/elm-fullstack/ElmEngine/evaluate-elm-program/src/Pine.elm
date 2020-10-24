module Pine exposing (..)


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
        PineFunctionOrValue "String.fromInt" ->
            case application.arguments of
                [ argument ] ->
                    case argument of
                        PineLiteral literal ->
                            Ok literal

                        _ ->
                            Err "Unexpected argument shape for String.fromInt"

                _ ->
                    Err
                        ("Unexpected number or arguments for String.fromInt: "
                            ++ String.fromInt (List.length application.arguments)
                        )

        _ ->
            Err "Application not implemented yet."
