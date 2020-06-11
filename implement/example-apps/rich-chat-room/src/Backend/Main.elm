module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Bytes
import Bytes.Decode
import Bytes.Encode
import Conversation exposing (UserId)
import Dict
import ElmFullstackCompilerInterface.ElmMakeFrontendWeb as ElmMakeFrontendWeb
import ElmFullstackCompilerInterface.GenerateJsonCoders as GenerateJsonCoders
import FrontendBackendInterface
import Json.Decode
import Json.Encode
import SHA1
import Url


type alias State =
    { posixTimeMilli : Int
    , conversationHistory : List Conversation.Event
    , usersSessions : Dict.Dict String UserSessionState
    , usersProfiles : Dict.Dict UserId UserProfile
    , usersLastSeen : Dict.Dict UserId { posixTime : Int }
    }


type alias UserSessionState =
    { beginPosixTime : Int
    , clientAddress : Maybe String
    , userId : Maybe UserId
    , lastUsePosixTime : Int
    }


type alias UserProfile =
    { chosenName : String
    }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            processEventHttpRequest httpRequestEvent { stateBefore | posixTimeMilli = httpRequestEvent.posixTimeMilli }

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


processEventHttpRequest : InterfaceToHost.HttpRequestEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEventHttpRequest httpRequestEvent stateBefore =
    if
        httpRequestEvent.request.uri
            |> Url.fromString
            |> Maybe.map urlLeadsToFrontendHtmlDocument
            |> Maybe.withDefault False
    then
        ( stateBefore
        , [ { httpRequestId = httpRequestEvent.httpRequestId
            , response =
                { statusCode = 200
                , body = Just ElmMakeFrontendWeb.elm_make_frontendWeb_html_debug
                , headersToAdd = []
                }
            }
                |> InterfaceToHost.CompleteHttpResponse
          ]
        )

    else
        case
            httpRequestEvent.request.body
                |> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string")
                |> Maybe.withDefault "Missing HTTP body"
                |> Json.Decode.decodeString GenerateJsonCoders.jsonDecodeRequestFromUser
        of
            Err decodeError ->
                let
                    httpResponse =
                        { httpRequestId = httpRequestEvent.httpRequestId
                        , response =
                            { statusCode = 400
                            , body =
                                ("Failed to decode request: " ++ (decodeError |> Json.Decode.errorToString))
                                    |> encodeStringToBytes
                                    |> Just
                            , headersToAdd = []
                            }
                        }
                in
                ( stateBefore, [ httpResponse |> InterfaceToHost.CompleteHttpResponse ] )

            Ok requestFromUser ->
                let
                    ( userSessionId, userSessionStateBefore ) =
                        userSessionIdAndStateFromRequestOrCreateNew httpRequestEvent.request stateBefore

                    userSessionState =
                        userSessionStateBefore

                    usersLastSeen =
                        case userSessionStateBefore.userId of
                            Nothing ->
                                stateBefore.usersLastSeen

                            Just userId ->
                                stateBefore.usersLastSeen
                                    |> Dict.insert userId { posixTime = stateBefore.posixTimeMilli // 1000 }

                    usersSessions =
                        stateBefore.usersSessions
                            |> Dict.insert userSessionId userSessionState

                    ( state, responseToUser ) =
                        processMessageFromClient
                            { posixTimeMilli = stateBefore.posixTimeMilli, userId = userSessionStateBefore.userId, loginUrl = "" }
                            requestFromUser
                            { stateBefore
                                | usersSessions = usersSessions
                                , usersLastSeen = usersLastSeen
                            }

                    responseToClient =
                        { currentPosixTimeMilli = state.posixTimeMilli
                        , currentUserId = userSessionState.userId
                        , responseToUser = responseToUser
                        }

                    httpResponse =
                        { httpRequestId = httpRequestEvent.httpRequestId
                        , response =
                            { statusCode = 200
                            , body =
                                responseToClient
                                    |> GenerateJsonCoders.jsonEncodeMessageToClient
                                    |> Json.Encode.encode 0
                                    |> encodeStringToBytes
                                    |> Just
                            , headersToAdd = []
                            }
                                |> addCookieUserSessionId userSessionId
                        }
                            |> InterfaceToHost.CompleteHttpResponse
                in
                ( state, [ httpResponse ] )


urlLeadsToFrontendHtmlDocument : Url.Url -> Bool
urlLeadsToFrontendHtmlDocument url =
    not (url.path == "/api" || (url.path |> String.startsWith "/api/"))


seeingLobbyFromState : State -> FrontendBackendInterface.SeeingLobbyStructure
seeingLobbyFromState state =
    let
        usersOnline =
            state.usersLastSeen
                |> Dict.filter (\_ lastSeen -> state.posixTimeMilli // 1000 - 10 < lastSeen.posixTime)
                |> Dict.keys
    in
    { conversationHistory = state.conversationHistory
    , usersOnline = usersOnline
    }


processMessageFromClient :
    { userId : Maybe Int, posixTimeMilli : Int, loginUrl : String }
    -> FrontendBackendInterface.RequestFromUser
    -> State
    -> ( State, FrontendBackendInterface.ResponseToUser )
processMessageFromClient context requestFromUser stateBefore =
    let
        ( state, responseToUser ) =
            case requestFromUser of
                FrontendBackendInterface.ShowUpRequest ->
                    ( stateBefore, stateBefore |> seeingLobbyFromState |> FrontendBackendInterface.SeeingLobby )

                FrontendBackendInterface.AddTextMessageRequest message ->
                    case context.userId of
                        Nothing ->
                            ( stateBefore
                            , FrontendBackendInterface.MessageToUser
                                ([ Conversation.LeafPlainText "⚠️ To add a message, please sign in first at "
                                 , Conversation.LeafLinkToUrl { url = context.loginUrl }
                                 ]
                                    |> Conversation.SequenceOfNodes
                                )
                            )

                        Just userId ->
                            let
                                conversationHistory =
                                    { posixTimeMilli = stateBefore.posixTimeMilli
                                    , origin = Conversation.FromUser { userId = userId }
                                    , message = Conversation.LeafPlainText message
                                    }
                                        :: stateBefore.conversationHistory

                                stateAfterAddingMessage =
                                    { stateBefore | conversationHistory = conversationHistory }
                            in
                            ( stateAfterAddingMessage
                            , stateAfterAddingMessage |> seeingLobbyFromState |> FrontendBackendInterface.SeeingLobby
                            )

                FrontendBackendInterface.ChooseNameRequest chosenName ->
                    case context.userId of
                        Nothing ->
                            ( stateBefore
                            , FrontendBackendInterface.MessageToUser
                                ([ Conversation.LeafPlainText "⚠️ To choose a name, please sign in first at "
                                 , Conversation.LeafLinkToUrl { url = context.loginUrl }
                                 ]
                                    |> Conversation.SequenceOfNodes
                                )
                            )

                        Just userId ->
                            let
                                userProfileBefore =
                                    stateBefore.usersProfiles |> Dict.get userId |> Maybe.withDefault initUserProfile

                                userProfile =
                                    { userProfileBefore | chosenName = chosenName }

                                usersProfiles =
                                    stateBefore.usersProfiles |> Dict.insert userId userProfile
                            in
                            ( { stateBefore | usersProfiles = usersProfiles }
                            , FrontendBackendInterface.ReadUserProfile userProfile
                            )

                FrontendBackendInterface.ReadUserProfileRequest userId ->
                    ( stateBefore
                    , stateBefore.usersProfiles
                        |> Dict.get userId
                        |> Maybe.withDefault initUserProfile
                        |> FrontendBackendInterface.ReadUserProfile
                    )
    in
    ( state, responseToUser )


initUserProfile : UserProfile
initUserProfile =
    { chosenName = "" }


userSessionIdAndStateFromRequestOrCreateNew : InterfaceToHost.HttpRequestProperties -> State -> ( String, UserSessionState )
userSessionIdAndStateFromRequestOrCreateNew httpRequest state =
    state |> getFirstMatchingUserSessionOrCreateNew (getSessionIdsFromHttpRequest httpRequest)


getFirstMatchingUserSessionOrCreateNew : List String -> State -> ( String, UserSessionState )
getFirstMatchingUserSessionOrCreateNew sessionIds state =
    sessionIds
        |> List.filterMap
            (\requestSessionId ->
                state.usersSessions
                    |> Dict.get requestSessionId
                    |> Maybe.map
                        (\sessionState ->
                            ( requestSessionId, sessionState )
                        )
            )
        |> List.sortBy
            (\( _, sessionState ) ->
                if sessionState.userId == Nothing then
                    1

                else
                    0
            )
        |> List.head
        |> Maybe.withDefault (state |> getNextUserSessionIdAndState)


getSessionIdsFromHttpRequest : InterfaceToHost.HttpRequestProperties -> List String
getSessionIdsFromHttpRequest httpRequest =
    let
        cookies =
            httpRequest.headers
                |> List.filter (\{ name } -> (name |> String.toLower) == "cookie")
                |> List.head
                |> Maybe.map .values
                |> Maybe.withDefault []
                |> List.concatMap (String.split ";")
                |> List.map String.trim

        prefix =
            httpSessionIdCookieName ++ "="
    in
    cookies
        |> List.filter (String.startsWith prefix)
        |> List.map (\sessionIdCookie -> sessionIdCookie |> String.dropLeft (prefix |> String.length))


getNextUserSessionIdAndState : State -> ( String, UserSessionState )
getNextUserSessionIdAndState state =
    let
        posixTime =
            state.posixTimeMilli // 1000
    in
    ( state |> getNextUserSessionId
    , { userId = Just ((state |> getLastUserId |> Maybe.withDefault 0) + 1)
      , beginPosixTime = posixTime
      , lastUsePosixTime = posixTime
      , clientAddress = Nothing
      }
    )


getLastUserId : State -> Maybe Int
getLastUserId state =
    [ state.usersProfiles |> Dict.keys
    , state.usersSessions |> Dict.values |> List.filterMap .userId
    ]
        |> List.concat
        |> List.maximum


getNextUserSessionId : State -> String
getNextUserSessionId state =
    let
        otherSessionsIds =
            state.usersSessions |> Dict.keys

        source =
            (otherSessionsIds |> String.concat) ++ (state.posixTimeMilli |> String.fromInt)
    in
    source |> SHA1.fromString |> SHA1.toHex |> String.left 30


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


encodeStringToBytes : String -> Bytes.Bytes
encodeStringToBytes =
    Bytes.Encode.string >> Bytes.Encode.encode


addCookieUserSessionId : String -> InterfaceToHost.HttpResponse -> InterfaceToHost.HttpResponse
addCookieUserSessionId userSessionId httpResponse =
    let
        cookieHeader =
            { name = "Set-Cookie", values = [ httpSessionIdCookieName ++ "=" ++ userSessionId ++ "; Path=/; Max-Age=3600" ] }
    in
    { httpResponse | headersToAdd = cookieHeader :: httpResponse.headersToAdd }


httpSessionIdCookieName : String
httpSessionIdCookieName =
    "sessionid"


interfaceToHost_initState : State
interfaceToHost_initState =
    { posixTimeMilli = 0
    , conversationHistory = []
    , usersProfiles = Dict.empty
    , usersSessions = Dict.empty
    , usersLastSeen = Dict.empty
    }
