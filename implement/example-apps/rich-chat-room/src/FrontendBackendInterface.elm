module FrontendBackendInterface exposing (..)

import Bytes
import Conversation exposing (UserId)
import SHA256
import Url
import Url.Parser exposing ((</>))


type RequestFromUser
    = ShowUpRequest { lastSeenEventPosixTimeMilli : Maybe Int }
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


type Route
    = ApiRoute
    | StaticContentRoute String
    | FrontendWithInspectorRoute


routeFromUrl : Url.Url -> Maybe Route
routeFromUrl =
    Url.Parser.parse
        (Url.Parser.oneOf
            [ Url.Parser.map ApiRoute (Url.Parser.s "api")
            , Url.Parser.map StaticContentRoute (Url.Parser.s "static-content" </> Url.Parser.string)
            , Url.Parser.map FrontendWithInspectorRoute (Url.Parser.s "with-inspector")
            ]
        )


urlPathFromRoute : Route -> List String
urlPathFromRoute route =
    case route of
        ApiRoute ->
            [ "api" ]

        StaticContentRoute contentName ->
            [ "static-content", contentName ]

        FrontendWithInspectorRoute ->
            [ "with-inspector" ]


staticContentFileName : Bytes.Bytes -> String
staticContentFileName =
    SHA256.fromBytes >> SHA256.toHex
