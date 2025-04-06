module Backend.Generated.StateShimTypes exposing (..)

import Json.Encode


type alias ResponseOverSerialInterface =
    Result String StateShimResponse


type StateShimRequest
    = ListExposedFunctionsShimRequest


type StateShimResponse
    = ListExposedFunctionsShimResponse (List { functionName : String, functionDescription : ExposedFunctionDescription })


type alias ApplyFunctionArguments state =
    { stateArgument : state
    , serializedArgumentsJson : List Json.Encode.Value
    }


type alias ExposedFunctionDescription =
    { returnType : ExposedFunctionReturnTypeDescription
    , parameters : List ExposedFunctionParameterDescription
    }


type alias ExposedFunctionReturnTypeDescription =
    { sourceCodeText : String
    , containsAppStateType : Bool
    }


type alias ExposedFunctionParameterDescription =
    { patternSourceCodeText : String
    , typeSourceCodeText : String
    , typeIsAppStateType : Bool
    }


type alias FunctionApplicationResult =
    { resultLessStateJson : Maybe Json.Encode.Value
    , producedStateDifferentFromStateArgument : Bool
    }

