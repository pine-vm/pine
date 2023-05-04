module HostInterface exposing (..)


type MessageToHost
    = ReadAdminInterfaceConfigRequest


type EventFromHost
    = ReadAdminInterfaceConfigEvent AdminInterfaceConfig


type alias AdminInterfaceConfig =
    { elmTimeVersionId : String
    , httpRoutes : List HttpRoute
    }


type alias HttpRoute =
    { methods : List String
    , path : String
    }
