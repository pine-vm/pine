module Backend.ExposeFunctionsToAdmin exposing (..)

import Backend.State
import Calculator


type alias CustomUsageReport =
    { httpRequestCount : Int
    , operationsViaHttpRequestCount : Int
    , anotherField : String
    }


applyCalculatorOperation : Calculator.CalculatorOperation -> Backend.State.State -> Backend.State.State
applyCalculatorOperation operation stateBefore =
    { stateBefore
        | resultingNumber = stateBefore.resultingNumber |> Calculator.applyCalculatorOperation operation
    }


customUsageReport : String -> Backend.State.State -> CustomUsageReport
customUsageReport customArgument state =
    { httpRequestCount = state.httpRequestCount
    , operationsViaHttpRequestCount = state.operationsViaHttpRequestCount
    , anotherField = "Custom content from argument: " ++ customArgument
    }
