module FrontendBackendInterface exposing (..)

import Conversation exposing (UserId)


type RequestFromUser
    = ShowUpRequest
    | AddTextMessageRequest String
    | ChooseNameRequest String
    | ReadUserProfileRequest UserId


{-| Note on design of the `responseToUser`:
With more powerful tooling, we might avoid responding to individual requests, and instead simplify the app code by using a representation of the complete state.
A difference with better tooling is that it saves us from actually transferring the complete state with each change because it can make the acquisition of that data non-strict/lazy.
-}
type alias MessageToClient =
    { currentPosixTimeMilli : Int
    , currentUserId : Maybe Int
    , responseToUser : ResponseToUser
    }


type ResponseToUser
    = SeeingLobby SeeingLobbyStructure
    | ReadUserProfile { chosenName : String }
    | MessageToUser Conversation.EventContentNode


type alias SeeingLobbyStructure =
    { conversationHistory : List Conversation.Event
    , usersOnline : List UserId
    }
