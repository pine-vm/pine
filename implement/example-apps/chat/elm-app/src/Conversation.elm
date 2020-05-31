module Conversation exposing (..)


type EventOrigin
    = FromUser EventOriginUser
    | FromSystem


type alias Event =
    { posixTimeMilli : Int
    , origin : EventOrigin
    , message : EventContentNode
    }


type EventContentNode
    = LeafUser EventOriginUser
    | LeafPlainText String
    | LeafLinkToUrl { url : String }
    | SequenceOfNodes (List EventContentNode)


type alias EventOriginUser =
    { userId : Int
    }
