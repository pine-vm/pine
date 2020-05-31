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
import Conversation
import Dict
import ElmFullstackCompilerInterface.GenerateJsonCoders as GenerateJsonCoders
import FrontendBackendInterface
import Json.Decode
import Json.Encode
import SHA1


type alias State =
    { posixTimeMilli : Int
    , conversationHistory : List Conversation.Event
    , usersSessions : Dict.Dict String UserSessionState
    , usersProfiles : Dict.Dict Int {}
    }


type alias UserSessionState =
    { beginPosixTime : Int
    , clientAddress : Maybe String
    , userId : Maybe Int
    , lastUsePosixTime : Int
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

                usersProfiles =
                    case userSessionStateBefore.userId of
                        Nothing ->
                            stateBefore.usersProfiles

                        Just userId ->
                            stateBefore.usersProfiles |> Dict.insert userId {}

                usersSessions =
                    stateBefore.usersSessions
                        |> Dict.insert userSessionId userSessionStateBefore

                ( state, responseToClient ) =
                    processMessageFromClient
                        { posixTimeMilli = stateBefore.posixTimeMilli, userId = userSessionStateBefore.userId, loginUrl = "" }
                        requestFromUser
                        { stateBefore | usersSessions = usersSessions, usersProfiles = usersProfiles }

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


processMessageFromClient : { userId : Maybe Int, posixTimeMilli : Int, loginUrl : String } -> FrontendBackendInterface.RequestFromUser -> State -> ( State, FrontendBackendInterface.MessageToClient )
processMessageFromClient context requestFromUser stateBefore =
    let
        ( state, messageToUser ) =
            case requestFromUser of
                FrontendBackendInterface.BeOnline ->
                    ( stateBefore, Nothing )

                FrontendBackendInterface.AddTextMessage message ->
                    case context.userId of
                        Nothing ->
                            ( stateBefore
                            , Just
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
                            in
                            ( { stateBefore | conversationHistory = conversationHistory }, Nothing )

        messageToClient =
            { currentPosixTimeMilli = context.posixTimeMilli
            , conversationHistory = state.conversationHistory
            , usersOnline = []
            , currentUserId = context.userId
            , messageToUser = messageToUser
            }
    in
    ( state, messageToClient )


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
    }
