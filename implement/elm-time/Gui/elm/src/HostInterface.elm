module HostInterface exposing (..)

import Json.Encode


type MessageToHost
    = ReadAdminInterfaceConfigRequest


type EventFromHost
    = ReadAdminInterfaceConfigEvent AdminInterfaceConfig


type alias AdminInterfaceConfig =
    { elmTimeVersionId : String
    , httpRoutes : List HttpRoute
    , databaseFunctions : List DatabaseFunctionDescription
    }


type alias DatabaseFunctionDescription =
    { functionName : String
    , functionDescription : ExposedFunctionDescription
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


type alias HttpRoute =
    { methods : List String
    , path : String
    }


type alias ApplyFunctionOnDatabaseRequest =
    { functionName : String
    , serializedArgumentsJson : List String
    , commitResultingState : Bool
    }


type alias ApplyFunctionOnDatabaseSuccess =
    { functionApplicationResult : FunctionApplicationResult
    , committedResultingState : Bool
    }


type alias FunctionApplicationResult =
    { resultLessStateJson : Maybe Json.Encode.Value
    , producedStateDifferentFromStateArgument : Bool
    }
