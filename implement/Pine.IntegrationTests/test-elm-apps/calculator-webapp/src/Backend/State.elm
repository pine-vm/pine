module Backend.State exposing (..)


type alias State =
    { httpRequestCount : Int
    , operationsViaHttpRequestCount : Int
    , resultingNumber : Int
    }
