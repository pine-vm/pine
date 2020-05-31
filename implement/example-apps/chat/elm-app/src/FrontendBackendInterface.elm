module FrontendBackendInterface exposing (..)

import Conversation


type alias MessageToClient =
    { currentPosixTimeMilli : Int
    , currentUserId : Maybe Int
    , conversationHistory : List Conversation.Event
    , usersOnline : List Conversation.EventOriginUser
    , messageToUser : Maybe Conversation.EventContentNode
    }


type RequestFromUser
    = BeOnline
    | AddTextMessage String
