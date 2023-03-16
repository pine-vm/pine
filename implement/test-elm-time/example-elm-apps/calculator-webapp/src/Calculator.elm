module Calculator exposing (..)


type CalculatorOperation
    = AddOperation Int


applyCalculatorOperation : CalculatorOperation -> Int -> Int
applyCalculatorOperation operation numberBefore =
    case operation of
        AddOperation addition ->
            numberBefore + addition
